import operator
import argparse
import sys
from collections import namedtuple
try:
    # Unix-like systems
    import tty
    import termios
    PLATFORM = "unix"
except ImportError:
    try:
        # Windows
        import msvcrt
        PLATFORM = "windows"
    except ImportError:
        # Other OS or environments (like WASM) where raw I/O is not available
        PLATFORM = "unsupported"

# ===================================================================
#      PARSER: Turns a source code stream into tokens, one by one.
# ===================================================================

# === New Custom Types ===

_WordTuple = namedtuple("Word", "value")
class Word(_WordTuple):
    """A token representing a word. Its repr is just its name."""
    def __repr__(self):
        return str(self.value)

class CatString:
    """A custom string type for the language stack.
    - __str__ returns the raw content (for 'print', '.')
    - __repr__ returns a quoted representation (for '.s', debug)
    """
    def __init__(self, value: str):
        self.value = value

    def __str__(self):
        # For end-user display
        return self.value
    
    def __repr__(self):
        # For developer/stack display. Use Python's repr for proper escaping.
        return repr(self.value)
    
    def __len__(self):
        return len(self.value)
        
    def __eq__(self, other):
        # Allow comparison with other CatStrings or raw Python strings
        if isinstance(other, CatString):
            return self.value == other.value
        if isinstance(other, str):
            return self.value == other
        return NotImplemented


class Parser:
    """
    Parses a string of source code for the concatenative language.
    This version is designed to be used as a stream, yielding one
    token at a time.
    """
    def __init__(self, code: str):
        self.code = code
        self.pos = 0

    def next_token(self): # <--- CHANGED FROM parse()
        """
        Parses and returns the very next token from the input stream.
        Returns None if the end of the stream is reached.
        """
        self._skip_whitespace_and_comments()
        if self.pos >= len(self.code):
            return None # Signal end of input
        return self._parse_next_token()

    def next_raw_token(self):
        r = self.next_token()
        if not isinstance(r, Word):
            raise TypeError(f"Symbol expected, got {r}")
        return r
        

    def _skip_whitespace_and_comments(self):
        while self.pos < len(self.code):
            if self.code[self.pos].isspace():
                self.pos += 1
            else:
                break

    def _parse_next_token(self):
        char = self.code[self.pos]
        if char == '"':
            return self._parse_string_literal()
        if char == '|': # <--- NEW CASE
            return self._parse_piped_word()
        return self._parse_atom()
    
    def _parse_piped_word(self) -> Word:
        self.pos += 1  # Consume the opening '"'
        
        value_chars = []
        is_escaped = False

        while self.pos < len(self.code):
            char = self.code[self.pos]
            self.pos += 1 # Consume character now

            if is_escaped:
                # Last char was '\', so treat this one literally
                value_chars.append(char)
                is_escaped = False
            elif char == '\\':
                # This is an escape char, note it for the next loop iteration
                is_escaped = True
            elif char == '|':
                # It's an unescaped quote, so we are done.
                return Word("".join(value_chars))
            else:
                # It's a normal character
                value_chars.append(char)
                
        # If we fall out of the loop, the string was not terminated.
        raise ValueError("Unterminated piped word")

    def _parse_string_literal(self) -> CatString:
        self.pos += 1  # Consume the opening '"'
        
        value_chars = []
        is_escaped = False

        while self.pos < len(self.code):
            char = self.code[self.pos]
            self.pos += 1 # Consume character now

            if is_escaped:
                # Last char was '\', so treat this one literally
                value_chars.append(char)
                is_escaped = False
            elif char == '\\':
                # This is an escape char, note it for the next loop iteration
                is_escaped = True
            elif char == '"':
                # It's an unescaped quote, so we are done.
                return CatString("".join(value_chars))
            else:
                # It's a normal character
                value_chars.append(char)
                
        # If we fall out of the loop, the string was not terminated.
        raise ValueError("Unterminated string literal")

    def _parse_atom(self):
        start = self.pos
        while self.pos < len(self.code) and not self.code[self.pos].isspace():
            self.pos += 1
        
        token_str = self.code[start:self.pos]
        
        if token_str == 'true': return True
        if token_str == 'false': return False
        
        try: return int(token_str)
        except ValueError:
            try: return float(token_str)
            except ValueError: return Word(token_str)

# ===================================================================
#      INTERPRETER: Executes tokens as they are parsed
# ===================================================================

class Interpreter:
    """
    An extensible implementation of a concatenative language.
    Holds a Parser instance and executes tokens as they are generated.
    """

    def __init__(self, code: str): # <--- MODIFIED
        self.stack = []
        self.parser = Parser(code) # Interpreter now owns the parser
        
        self.quotation_level = 0
        self.quotation_stack = [] # A stack to hold lists being built
        
        self.comment_level = 0
        
        self.words = self._create_core_words()

    def add_word(self, name: str, func: callable):
        self.words[name] = func

    # In the Interpreter class

    def run(self):
        """
        Executes a program. Manages raw terminal mode for interactive I/O.
        """
        is_interactive = PLATFORM == "unix" and sys.stdin.isatty()
        fd = None # Initialize fd to None

        if is_interactive:
            # Save old terminal settings before entering the try block
            fd = sys.stdin.fileno()
            self.old_settings = termios.tcgetattr(fd)
            tty.setraw(fd)

        try:
            # --- Main Execution Block ---
            self.stack.clear()
            
            while (token := self.parser.next_token()) is not None:
                # This is the main loop from before
                in_comment = self.comment_level > 0
                is_comment_control_word = token in (Word('(//'), Word('//)'))

                if is_comment_control_word:
                    self._eval_one(token)
                    continue
                if in_comment:
                    continue
                
                is_eval_word = self.quotation_level == 0 or token in (Word('['), Word(']'))
                if is_eval_word:
                    self._eval_one(token)
                else:
                    self._compile_to_quotation(token)

        except (IndexError, TypeError, NameError, ValueError) as e:
            # --- Language Error Handling ---
            # The \r moves to the start of the line, \n goes to a new line.
            # This is important for clean output in raw mode.
            print(f"\r\nRuntime Error: {e}", file=sys.stderr)
            print(f"Execution halted. Current stack: {self.stack}", file=sys.stderr)
            # We don't exit here; the finally block must run first.
        
        except KeyboardInterrupt:
            # --- Handle Ctrl+C ---
            print("\r\nInterrupted by user.", file=sys.stderr)

        finally:
            # --- CRITICAL Cleanup Block ---
            # This block runs NO MATTER WHAT: success, language error, or Ctrl+C.
            if is_interactive:
                # Restore the terminal to its original state
                termios.tcsetattr(fd, termios.TCSADRAIN, self.old_settings)
            
            # Now we can do final checks that might exit the program
            if self.comment_level > 0:
                print(f"\r\nRuntime Error: Unterminated comment.", file=sys.stderr)
                sys.exit(1)
                
        return self.stack


    def _compile_to_quotation(self, token):
        """Appends a token to the quotation currently being built."""
        self.quotation_stack[-1].append(token)

    def _eval_one(self, token):
        """Evaluates a single token."""
        if isinstance(token, Word):
            if token.value in self.words:
                self.words[token.value]()
            else:
                raise NameError(f"Unknown word: '{token.value}'")
        elif isinstance(token, (int, float, bool, list,CatString)):
            self.stack.append(token)
        else:
            raise TypeError(f"Invalid token type encountered: {type(token)}")

    def _eval_quotation(self, tokens: list):
        """Evaluates a list of tokens (typically from a quotation)."""
        for token in tokens:
            self._eval_one(token)
    
    def _create_core_words(self):
        words = {
            # === Metaprogramming ===
            'define': self._word_define,     # ( quot name -- )
            'read-word': self._word_read_word,
            # === Parsing Words (New Category!) ===
            'hex:':    self._word_hex,        # ( -- ) Reads next word as hex
            # === Quotation / Execution ===
            'call':   self._word_call,       # ( quot -- ? )
            'dip':    self._word_dip,        # ( item quot -- ? )
            
            # ... rest of the words are the same ...
            'dup':    lambda: self.stack.append(self.stack[-1]),
            'drop':   lambda: self.stack.pop(),
            'swap':   self._word_swap,
            'over':   lambda: self.stack.append(self.stack[-2]),
            'rot':    self._word_rot,
            '-rot':   self._word_neg_rot,
            'nip':    lambda: self.stack.pop(-2),
            'tuck':   lambda: self.stack.insert(-2, self.stack[-1]),
            'depth':  lambda: self.stack.append(len(self.stack)),
            '+':      self._word_add,
            '-':      self._make_binary_op(operator.sub),
            '*':      self._make_binary_op(operator.mul),
            '/':      self._make_binary_op(operator.truediv),
            'mod':    self._make_binary_op(operator.mod),
            'negate': lambda: self.stack.append(-self.stack.pop()),
            '==':     self._make_binary_op(operator.eq),
            '!=':     self._make_binary_op(operator.ne),
            '<':      self._make_binary_op(operator.lt),
            '>':      self._make_binary_op(operator.gt),
            '<=':     self._make_binary_op(operator.le),
            '>=':     self._make_binary_op(operator.ge),
            'and':    self._make_binary_op(operator.and_),
            'or':     self._make_binary_op(operator.or_),
            'not':    lambda: self.stack.append(not self.stack.pop()),
            'if':     self._word_if,       # ( bool then-quot else-quot -- ? )
            'times':  self._word_times,      # ( n quot -- )
            'map':    self._word_map,        # ( seq quot -- seq' )
            'compose': self._word_compose,   # ( q1 q2 -- q_new )
            'cons':    self._word_cons,      # ( item list -- list' )
            'unit':    lambda: self.stack.append([self.stack.pop()]),
            'empty?':  lambda: self.stack.append(len(self.stack.pop()) == 0),
            '.s':      lambda: print(f"Stack: {self.stack}"),
            '.':       lambda: print(self.stack.pop()),
            'print':   lambda: print(str(self.stack.pop()), end=""),
            '[': self._word_left_bracket,
            ']': self._word_right_bracket,
            "(//": self._word_comment,
            "//)": self._word_end_comment,
            "filter": self._word_filter,
            "reduce": self._word_reduce,
            "each": self._word_each,
            "uncons": self._word_uncons,
            "append": self._word_append,
            "concat": self._word_concat,
            "size": self._word_size,
            "bi": self._word_bi,
            "tri": self._word_tri,
            "cleave": self._word_cleave,
            "spread": self._word_spread,
            "empty": self._word_empty,
            "key": self._word_key,
            "emit": self._word_emit,
            "while": self._word_while,
        }
        return words

    def _word_while(self):
        """
        Executes a body quotation as long as a condition quotation is true.
        Stack: ( condition-quot body-quot -- )
        """
        body_quot, cond_quot = self.stack.pop(), self.stack.pop()
        if not isinstance(body_quot, list) or not isinstance(cond_quot, list):
            raise TypeError("'while' requires two quotations.")
        
        # Evaluate the condition for the first time
        self._eval_quotation(cond_quot)
        
        while self.stack.pop():
            # If true, run the body
            self._eval_quotation(body_quot)
            # And check the condition again for the next iteration
            self._eval_quotation(cond_quot)

    # In Interpreter class
    def _word_key(self):
        """
        Reads one character from stdin and pushes its UTF-32 code point.
        Handles raw mode on supported platforms.
        Stack: ( -- n )
        """
        if PLATFORM == "unix":
            char = sys.stdin.read(1)
            # In raw mode, a Ctrl+D (EOF) might be read as an EOT character (ASCII 4)
            # or it might close the stream, resulting in an empty string.
            # We also check for Ctrl+C (ASCII 3) to allow breaking the program.
            if not char or ord(char) == 4: # EOF
                self.stack.append(-1)
            elif ord(char) == 3: # Ctrl+C
                raise KeyboardInterrupt()
            else:
                self.stack.append(ord(char))

        elif PLATFORM == "windows":
            # msvcrt.getch() reads a single keypress and returns it as a bytes object
            char_byte = msvcrt.getch()
            if char_byte in (b'\x03', b'\x1a'): # Ctrl+C or Ctrl+Z (EOF)
                 self.stack.append(-1)
            else:
                # We must decode the byte into a string to get the code point
                self.stack.append(ord(char_byte.decode('utf-8')))

        else: # "unsupported" platform
            # Fallback to buffered input
            char = sys.stdin.read(1)
            if not char:
                self.stack.append(-1)
            else:
                self.stack.append(ord(char))


    def _word_emit(self):
        """
        Pops an integer code point and prints the corresponding character to stdout.
        Handles raw mode newline translation.
        Stack: ( n -- )
        """
        codepoint = self.stack.pop()
        if not isinstance(codepoint, int):
            raise TypeError("'emit' requires an integer code point on the stack.")
        
        # --- NEW LOGIC for handling newlines ---
        # The Enter key sends a carriage return (ASCII 13). In raw mode, we must
        # manually output a full newline sequence ('\r\n') to move the cursor
        # to the start of the next line. This works robustly on both Windows
        # and Unix-like terminals in raw mode.
        if codepoint == 13:
            print("\r\n", end="", flush=True)
        else:
            try:
                # For all other characters, print them as-is.
                print(chr(codepoint), end="", flush=True)
            except (ValueError, OverflowError):
                # This handles cases where the number is not a valid Unicode code point.
                raise ValueError(f"Cannot 'emit' code point {codepoint}: not a valid Unicode value.")

    def _word_add(self):
        b, a = self.stack.pop(), self.stack.pop()
        
        # Case 1: Numbers
        if isinstance(a, (int, float)) and isinstance(b, (int, float)):
            self.stack.append(a + b)
        # Case 2: Strings
        elif isinstance(a, CatString) and isinstance(b, CatString):
            self.stack.append(CatString(a.value + b.value))
        # Case 3: Lists (concatenation)
        elif isinstance(a, list) and isinstance(b, list):
            self.stack.append(a + b)
        else:
            raise TypeError(f"Cannot add types {type(a)} and {type(b)}")

    def _word_empty(self):
        self.stack.append([])

    def _word_spread(self):
        """
        Applies a sequence of quotations to a sequence of values.
        Stack: ( x_n ... x_1 {q_1 ... q_n} -- r_1 ... r_n )
        """
        quot_seq = self.stack.pop()
        if not isinstance(quot_seq, list):
            raise TypeError("'spread' requires a sequence of quotations on top.")

        n = len(quot_seq)
        if len(self.stack) < n:
            raise IndexError(f"'spread' requires {n} items on the stack, but only found {len(self.stack)}.")

        # Pop n items. The first item popped is x_1, the second is x_2, etc.
        # This naturally pairs them with q_1, q_2 from the quot_seq.
        items = [self.stack.pop() for _ in range(n)]

        results = []
        # zip pairs (x_1, q_1), (x_2, q_2), ...
        for item, quot in zip(items, quot_seq):
            if not isinstance(quot, list):
                raise TypeError("The sequence for 'spread' must only contain quotations.")
            
            # Perform the call for one pair
            self.stack.append(item)
            self._eval_quotation(quot)
            results.append(self.stack.pop())
        
        # The results are [r_1, r_2, ...]. Pushing them back in this
        # order will result in r_n being on top, which is the correct
        # final stack state.
        self.stack.extend(results)


    # In Interpreter class
    def _word_cleave(self):
        quot_seq, x = self.stack.pop(), self.stack.pop()
        if not isinstance(quot_seq, list):
            raise TypeError("'cleave' requires a sequence of quotations.")
        
        results = []
        for quot in quot_seq:
            if not isinstance(quot, list):
                raise TypeError("The sequence for 'cleave' must only contain quotations.")
            self.stack.append(x)
            self._eval_quotation(quot)
            results.append(self.stack.pop())
            
        self.stack.extend(results)


    def _word_tri(self):
        q3, q2, q1, x = self.stack.pop(), self.stack.pop(), self.stack.pop(), self.stack.pop()
        if not isinstance(q1, list) or not isinstance(q2, list) or not isinstance(q3, list):
            raise TypeError("'tri' requires three quotations.")
        
        # First branch
        self.stack.append(x)
        self._eval_quotation(q1)
        r1 = self.stack.pop() # Temporarily store the result

        # Second branch
        self.stack.append(x)
        self._eval_quotation(q2)
        r2 = self.stack.pop() # Get the second result
        
        # Third branch
        self.stack.append(x)
        self._eval_quotation(q3)
        r3 = self.stack.pop() # Get the third result

        # Push both results back
        self.stack.append(r1)
        self.stack.append(r2)
        self.stack.append(r3)

    def _word_bi(self):
        q2, q1, x = self.stack.pop(), self.stack.pop(), self.stack.pop()
        if not isinstance(q1, list) or not isinstance(q2, list):
            raise TypeError("'bi' requires two quotations.")
        
        # First branch
        self.stack.append(x)
        self._eval_quotation(q1)
        r1 = self.stack.pop() # Temporarily store the result

        # Second branch
        self.stack.append(x)
        self._eval_quotation(q2)
        r2 = self.stack.pop() # Get the second result

        # Push both results back
        self.stack.append(r1)
        self.stack.append(r2)

    def _word_size(self):
        seq = self.stack.pop()
        if not hasattr(seq, '__len__'): # Works for lists, strings, etc.
            raise TypeError("'size' requires a collection with a length.")
        self.stack.append(len(seq))

    def _word_concat(self):
        seq2, seq1 = self.stack.pop(), self.stack.pop()
        if not isinstance(seq1, list) or not isinstance(seq2, list):
            raise TypeError("'concat' requires two lists.")
        self.stack.append(seq1 + seq2)

    def _word_append(self):
        item, seq = self.stack.pop(), self.stack.pop()
        if not isinstance(seq, list):
            raise TypeError("'append' requires a list.")
        self.stack.append(seq + [item]) # Simple but creates a new list; could also use seq.append(item) if mutation is ok

    def _word_uncons(self):
        seq = self.stack.pop()
        if not isinstance(seq, list) or len(seq) == 0:
            raise TypeError("'uncons' requires a non-empty list.")
        self.stack.append(seq[1:]) # Tail
        self.stack.append(seq[0])  # Head
    
    def _word_filter(self):
        quotation, seq = self.stack.pop(), self.stack.pop()
        if not isinstance(seq, list) or not isinstance(quotation, list):
            raise TypeError("'filter' requires a sequence and a quotation.")
        
        result = []
        for item in seq:
            self.stack.append(item)
            self._eval_quotation(quotation)
            if self.stack.pop(): # Check if the result is true
                result.append(item)
        self.stack.append(result)

    def _word_reduce(self):
        quotation, initial, seq = self.stack.pop(), self.stack.pop(), self.stack.pop()
        if not isinstance(seq, list) or not isinstance(quotation, list):
            raise TypeError("'reduce' requires a sequence, an initial value, and a quotation.")
        
        accumulator = initial
        for item in seq:
            self.stack.append(accumulator)
            self.stack.append(item)
            self._eval_quotation(quotation)
            accumulator = self.stack.pop()
        self.stack.append(accumulator)

    def _word_each(self):
        quotation, seq = self.stack.pop(), self.stack.pop()
        if not isinstance(seq, list):
            raise TypeError("'each' requires a sequence.")
        
        for item in seq:
            self.stack.append(item)
            self._eval_quotation(quotation)

    def _word_comment(self):
        """Increments the comment nesting level."""
        self.comment_level += 1

    def _word_end_comment(self):
        """Decrements the comment nesting level. Raises error if not in a comment."""
        if self.comment_level <= 0:
            raise ValueError("'end-comment' found without matching 'comment'")
        self.comment_level -= 1

    def _word_left_bracket(self):
        """Starts a new quotation."""
        self.quotation_level += 1
        self.quotation_stack.append([]) # Push a new empty list to build upon

    def _word_right_bracket(self):
        """Ends the current quotation."""
        self.quotation_level -= 1
        if self.quotation_level < 0:
            raise ValueError("Mismatched brackets: extra ']' found.")

        # Pop the completed quotation from our build stack
        completed_quot = self.quotation_stack.pop()

        if self.quotation_level == 0:
            # We are back at the top level. Push the final quotation to the main data stack.
            self.stack.append(completed_quot)
        else:
            # This was a nested quotation. Append it as data to its parent.
            self.quotation_stack[-1].append(completed_quot)

    def _word_read_word(self):
        next_word_token = self.parser.next_raw_token()
        self.stack.append(next_word_token.value)


    def _word_hex(self):
        """A parsing word. It asks the parser for the next token,
        interprets its value as a hex number, and pushes it to the stack."""
        next_word_token = self.parser.next_token()
        if not isinstance(next_word_token, Word):
            raise TypeError("HEX must be followed by a word to be interpreted as hex.")
        
        try:
            # Convert the word's string value from base 16
            value = int(next_word_token.value, 16)
            self.stack.append(value)
        except ValueError:
            raise ValueError(f"Invalid hexadecimal number: '{next_word_token.value}'")

    # --- Word Implementations (updated to use _eval_quotation) ---
    def _make_binary_op(self, op: callable):
        def word():
            b, a = self.stack.pop(), self.stack.pop()
            self.stack.append(op(a, b))
        return word
    
    def _word_swap(self):
        a, b = self.stack.pop(), self.stack.pop()
        self.stack.extend((a, b))

    def _word_rot(self):
        c, b, a = self.stack.pop(), self.stack.pop(), self.stack.pop()
        self.stack.extend((b, c, a))

    def _word_neg_rot(self):
        c, b, a = self.stack.pop(), self.stack.pop(), self.stack.pop()
        self.stack.extend((c, a, b))

    def _word_define(self):
        name, quotation = self.stack.pop(), self.stack.pop()
        if not isinstance(name, str) or not isinstance(quotation, list):
            raise TypeError("'define' requires a quotation and a name (string).")
        # When the new word is called, it will execute the pre-parsed quotation
        self.words[name] = lambda: self._eval_quotation(quotation)

    def _word_call(self):
        quotation = self.stack.pop()
        if not isinstance(quotation, list): raise TypeError("'call' requires a quotation.")
        self._eval_quotation(quotation)
    
    def _word_dip(self):
        quotation, item_to_save = self.stack.pop(), self.stack.pop()
        if not isinstance(quotation, list): raise TypeError("'dip' requires a quotation.")
        self._eval_quotation(quotation)
        self.stack.append(item_to_save)

    def _word_if(self):
        else_quot, then_quot, condition = self.stack.pop(), self.stack.pop(), self.stack.pop()
        if not isinstance(then_quot, list) or not isinstance(else_quot, list):
            raise TypeError("'if' requires two quotations.")
        if condition:
            self._eval_quotation(then_quot)
        else:
            self._eval_quotation(else_quot)

    def _word_times(self):
        quotation, n = self.stack.pop(), self.stack.pop()
        if not isinstance(n, int) or n < 0: raise TypeError("'times' requires a non-negative integer count.")
        for _ in range(n):
            self._eval_quotation(quotation)
    
    def _word_map(self):
        quotation, seq = self.stack.pop(), self.stack.pop()
        if not isinstance(seq, list): raise TypeError("'map' requires a sequence (list).")
        result = []
        for item in seq:
            self.stack.append(item)
            self._eval_quotation(quotation)
            result.append(self.stack.pop())
        self.stack.append(result)

    def _word_compose(self):
        q2, q1 = self.stack.pop(), self.stack.pop()
        if not isinstance(q1, list) or not isinstance(q2, list):
            raise TypeError("'compose' requires two quotations.")
        self.stack.append(q1 + q2)

    def _word_cons(self):
        item, seq = self.stack.pop(), self.stack.pop()
        if not isinstance(seq, list): raise TypeError("'cons' requires a list.")
        self.stack.append([item] + seq)


# ===================================================================
#      COMMAND-LINE RUNNER (updated for new workflow)
# ===================================================================

def main():
    parser = argparse.ArgumentParser(
        description="A concatenative language interpreter.",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument("filepath", help="Path to the source file to execute.")
    parser.add_argument("-d", "--debug", action="store_true", help="Print the final stack state after execution.")
    args = parser.parse_args()

    try:
        with open(args.filepath, 'r',encoding='utf-8') as f:
            code = f.read()
    except FileNotFoundError:
        print(f"Error: File not found at '{args.filepath}'", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: Could not read file '{args.filepath}': {e}", file=sys.stderr)
        sys.exit(1)

    # --- New, Simpler Workflow ---
    # 1. Create an interpreter with the source code
    interpreter = Interpreter(code)
    
    # 2. Run it. The interpreter handles all parsing and execution internally.
    final_stack = interpreter.run()

    if args.debug:
        print("\n--- Execution Finished ---")
        print(f"Final stack state: {final_stack}")

if __name__ == "__main__":
    main()

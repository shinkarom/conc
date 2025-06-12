import operator
import argparse
import sys
import locale
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
        
from parser import *
from conc_types import *

# ===================================================================
#      INTERPRETER: Executes tokens as they are parsed
# ===================================================================

class Interpreter:
    """
    An extensible implementation of a concatenative language.
    Holds a Parser instance and executes tokens as they are generated.
    """

    def __init__(self, code: str):
        self.stack = []
        self.parser = Parser(code)
        
        self.words = self._create_core_words()
        self.immediate_words = set([Word('['), Word(']')])

    def run(self):
        """
        Executes a program. Manages raw terminal mode for interactive I/O.
        """
        is_interactive = PLATFORM == "unix" and sys.stdin.isatty()
        fd = None
        if is_interactive:
            fd = sys.stdin.fileno()
            self.old_settings = termios.tcgetattr(fd)
            tty.setraw(fd)
        try:
            self.stack.clear()
            while (token := self.parser.next_token()) is not None: 
                self._eval_one(token)
        except (IndexError, TypeError, NameError, ValueError) as e:
            print(f"\r\nRuntime Error: {e}", file=sys.stderr)
            print(f"Execution halted. Current stack: {self.stack}", file=sys.stderr)
        except KeyboardInterrupt:
            print("\r\nInterrupted by user.", file=sys.stderr)
        finally:
            if is_interactive:
                termios.tcsetattr(fd, termios.TCSADRAIN, self.old_settings)   
        return self.stack      

    def _eval_one(self, token):
        if isinstance(token, Word):
            if token.value in self.words:
                definition = self.words[token.value]
                if callable(definition):
                    definition()
                elif isinstance(definition, list):
                    self._eval_list(definition)
                else:
                    raise TypeError(f"Invalid definition type for '{token.value}'")
            else:
                raise NameError(f"Unknown word: '{token.value}'")
        elif isinstance(token, (int, float, bool, list, str)):
            self.stack.append(token)
        else:
            raise TypeError(f"Invalid token type encountered: {type(token)}")
    
    def _eval_list(self,quotation):
        for token in quotation:
            self._eval_one(token)
    
    def _create_core_words(self):
        words = {
            'define': self._word_define,     # ( quot name -- )
            'read-word-string': self._word_read_word_string,
            'read-word': self._word_read_word,
            "'": self._word_quote,
            'hex:':    self._word_hex,        # ( -- ) Reads next word as hex
            'call':   self._word_call,       # ( quot -- ? )
            'dip':    self._word_dip,        # ( item quot -- ? )
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
            '//':      self._make_binary_op(operator.floordiv),
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
            '[]?':  lambda: self.stack.append(len(self.stack.pop()) == 0),
            '.s':      lambda: print(f"Stack: {self.stack}"),
            '.':       lambda: print(self.stack.pop()),
            'print':   lambda: print(str(self.stack.pop()), end=""),
            '[': self._word_left_bracket,
            "(//": self._word_comment,
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
            "[]": self._word_empty,
            "get-char": self._word_key,
            "emit": self._word_emit,
            "while": self._word_while,
            "{}": self._word_table_new,
            "table-get": self._word_table_get,
            "table-set": self._word_table_set,
            "str-concat": self._word_string_concat,
            "parse-list": self._word_parse_list,
            ":": self._word_colon,
        }
        return words

    def _word_colon(self):
        n = self.parser.next_token()
        if not isinstance(n, Word):
            raise ValueError(f"Word expected but got {n}")
        self.stack.append(":")
        self.stack.append(";")
        self._word_parse_list()
        q = self.stack.pop()
        self.words[n.value] = q

    def _word_parse_list(self):
        end_delimiter = self.stack.pop()
        if not isinstance(end_delimiter, str):
            raise ValueError(f"Expected a string but got {end_delimiter}")
        start_delimiter = self.stack.pop()
        if not isinstance(start_delimiter, str):
            raise ValueError(f"Expected a string but got {start_delimiter}")
        l = len(self.stack)
        terminated = False
        while (token := self.parser.next_token()) is not None: 
            if isinstance(token,Word):
                if token.value == end_delimiter:
                    terminated = True
                    break
                if token.value == start_delimiter: # recursion
                    self.stack.append(start_delimiter)
                    self.stack.append(end_delimiter)
                    self._word_parse_list()
                elif token in self.immediate_words:
                    self._eval_one(token)
                else:
                    self.stack.append(token)
            else:
                self.stack.append(token)
        if not terminated:
            raise ValueError(f"Unterminated quotation: {end_delimiter} expected")
        quotation = list()
        while len(self.stack) != l:
            quotation.append(self.stack.pop())
        self.stack.append(quotation[::-1])

    def _word_string_concat(self):
        b, a = self.stack.pop(), self.stack.pop()
        if isinstance(a, str) and isinstance(b, str):
            self.stack.append(str(a + b))
        else:
            raise TypeError(f"Cannot concat types {type(a)} and {type(b)}, not strings")

    def _word_table_new(self):
        self.stack.append({})
        
    def _word_table_get(self):

        key = self.stack.pop()
        table = self.stack.pop()
        
        if not isinstance(table, dict):
            raise TypeError("'tbl.get' requires a dict as the first argument")

        value = table.get(key)
        self.stack.append(value)
        
    def _word_table_set(self):
        key = self.stack.pop()
        value = self.stack.pop()
        table = self.stack.pop()

        if not isinstance(table, dict):
            raise TypeError("'tbl.set' requires a dict as the first argument")

        table[key] = value
        

    def _word_while(self):
        """
        Executes a body quotation as long as a condition quotation is true.
        Stack: ( condition-quot body-quot -- )
        """
        body_quot, cond_quot = self.stack.pop(), self.stack.pop()
        if not isinstance(body_quot, list) or not isinstance(cond_quot, list):
            raise TypeError("'while' requires two quotations.")
        
        # Evaluate the condition for the first time
        self._eval_list(cond_quot)
        
        while self.stack.pop():
            # If true, run the body
            self._eval_list(body_quot)
            # And check the condition again for the next iteration
            self._eval_list(cond_quot)

    def _word_key(self):
        if PLATFORM == "unix":
            char = sys.stdin.read(1)
            if not char or ord(char) == 4:
                self.stack.append(-1)
            elif ord(char) == 3:
                raise KeyboardInterrupt()
            else:
                self.stack.append(ord(char))

        elif PLATFORM == "windows":
            char_byte = msvcrt.getch()
            if char_byte in (b'\x03', b'\x1a'):
                 self.stack.append(-1)
            else:
                try:
                    encoding = 'utf-8'
                    decoded_char = char_byte.decode(encoding)
                except UnicodeDecodeError:
                    encoding = locale.getpreferredencoding()
                    decoded_char = char_byte.decode(encoding, errors='ignore')
                
                if decoded_char:
                    self.stack.append(ord(decoded_char))

        else:
            char = sys.stdin.read(1)
            if not char:
                self.stack.append(-1)
            else:
                self.stack.append(ord(char))


    def _word_emit(self):
        codepoint = self.stack.pop()
        if not isinstance(codepoint, int):
            raise TypeError("'emit' requires an integer code point on the stack.")
        
        if codepoint == 13:
            print("\r\n", end="", flush=True)
        else:
            try:
                print(chr(codepoint), end="", flush=True)
            except (ValueError, OverflowError):
                raise ValueError(f"Cannot 'emit' code point {codepoint}: not a valid Unicode value.")

    def _word_add(self):
        b, a = self.stack.pop(), self.stack.pop()
        
        if isinstance(a, (int, float)) and isinstance(b, (int, float)):
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
            self._eval_list(quot)
            results.append(self.stack.pop())
        
        # The results are [r_1, r_2, ...]. Pushing them back in this
        # order will result in r_n being on top, which is the correct
        # final stack state.
        self.stack.extend(results)

    def _internal_cleave(self, x, quot_seq: list):
        """
        Internal helper. Applies a list of quotations to a single item x.
        Pushes all results back onto the stack.
        """
        results = []
        for quot in quot_seq:
            if not isinstance(quot, list):
                # Centralized error checking
                raise TypeError("A sequence provided to a combinator must only contain quotations.")
            
            # The core operation: push x, run quot, store result
            self.stack.append(x)
            self._eval_list(quot)
            results.append(self.stack.pop())
            
        self.stack.extend(results)

    def _word_bi(self):
        # Stack: ( x q1 q2 -- r1 r2 )
        q2, q1, x = self.stack.pop(), self.stack.pop(), self.stack.pop()
        if not isinstance(q1, list) or not isinstance(q2, list):
            raise TypeError("'bi' requires two quotations.")
        self._internal_cleave(x, [q1, q2])

    def _word_tri(self):
        # Stack: ( x q1 q2 q3 -- r1 r2 r3 )
        q3, q2, q1, x = self.stack.pop(), self.stack.pop(), self.stack.pop(), self.stack.pop()
        if not isinstance(q1, list) or not isinstance(q2, list) or not isinstance(q3, list):
            raise TypeError("'tri' requires three quotations.")
        self._internal_cleave(x, [q1, q2, q3])

    def _word_cleave(self):
        # Stack: ( x {q1...qN} -- r1...rN )
        quot_seq, x = self.stack.pop(), self.stack.pop()
        if not isinstance(quot_seq, list):
            raise TypeError("'cleave' requires a sequence of quotations.")
        self._internal_cleave(x, quot_seq)

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
            self._eval_list(quotation)
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
            self._eval_list(quotation)
            accumulator = self.stack.pop()
        self.stack.append(accumulator)

    def _word_each(self):
        quotation, seq = self.stack.pop(), self.stack.pop()
        if not isinstance(seq, list):
            raise TypeError("'each' requires a sequence.")
        for item in seq:
            self.stack.append(item)
            self._eval_list(quotation)

    def _word_comment(self):
        nesting_level = 1
        while nesting_level > 0:
            token = self.parser.next_token()

            if token is None:
                raise ValueError("Unterminated nested comment: end of input reached.")

            if token == Word('(//'):
                nesting_level += 1
            elif token == Word('//)'):
                nesting_level -= 1

    def _word_left_bracket(self):
        self.stack.append("[")
        self.stack.append("]")
        self._word_parse_list()

    def _word_read_word(self):
        next_word_token = self.parser.next_token()
        if not isinstance(next_word_token,Word):
            raise ValueError(f"'read-word' expected a word but got {next_word_token}")
        self.stack.append(next_word_token)
      
    def _word_read_word_string(self):
        next_word_token = self.parser.next_raw_token()
        self.stack.append(str(next_word_token.value))
      
    def _word_quote(self):
        next_word_token = self.parser.next_raw_token()
        self.stack.append(next_word_token)

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

    # --- Word Implementations (updated to use _eval_one) ---
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
        self.words[name] = quotation

    def _word_call(self):
        quotation = self.stack.pop()
        self._eval_list(quotation)
        #raise TypeError("'call' requires a quotation.")
        
    
    def _word_dip(self):
        quotation, item_to_save = self.stack.pop(), self.stack.pop()
        if not isinstance(quotation, list): raise TypeError("'dip' requires a quotation.")
        self._eval_list(quotation)
        self.stack.append(item_to_save)

    def _word_if(self):
        else_quot, then_quot, condition = self.stack.pop(), self.stack.pop(), self.stack.pop()
        if not isinstance(then_quot, list) or not isinstance(else_quot, list):
            raise TypeError("'if' requires two quotations.")
        if condition:
            self._eval_list(then_quot)
        else:
            self._eval_list(else_quot)

    def _word_times(self):
        quotation, n = self.stack.pop(), self.stack.pop()
        if not isinstance(n, int) or n < 0: raise TypeError("'times' requires a non-negative integer count.")
        for _ in range(n):
            self._eval_list(quotation)
    
    def _word_map(self):
        quotation, seq = self.stack.pop(), self.stack.pop()
        if not isinstance(seq, list): raise TypeError("'map' requires a sequence (list).")
        result = []
        for item in seq:
            self.stack.append(item)
            self._eval_list(quotation)
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

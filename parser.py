from conc_types import *

class Parser:
    """
    Parses a string of source code for the concatenative language.
    This version is designed to be used as a stream, yielding one
    token at a time.
    """
    def __init__(self, code: str):
        self.code = code
        self.pos = 0

    def next_token(self):
        """
        Parses and returns the very next token from the input stream.
        Returns None if the end of the stream is reached.
        """
        self._skip_whitespace()
        if self.pos >= len(self.code):
            return None # Signal end of input
        return self._parse_next_token()

    def next_raw_token(self):
        r = self.next_token()
        if not isinstance(r, Word):
            raise TypeError(f"Word expected, got {r}")
        return r
        

    def _skip_whitespace(self):
        while self.pos < len(self.code):
            if self.code[self.pos].isspace():
                self.pos += 1
            else:
                break

    def _parse_next_token(self):
        char = self.code[self.pos]
        if char == '"':
            return self._parse_string_literal()
        return self._parse_atom()
    
    def _parse_atom(self):
        start = self.pos
        while self.pos < len(self.code) and not self.code[self.pos].isspace():
            self.pos += 1
        
        token_str = self.code[start:self.pos]
        
        try: return int(token_str)
        except ValueError:
            try: return float(token_str)
            except ValueError: return Word(token_str)

    def _parse_hex_escape(self, num_digits: int, escape_type: str) -> str:
        start_pos = self.pos
        end_pos = self.pos + num_digits
        if end_pos > len(self.code):
            raise ValueError(f"Incomplete \\{escape_type} escape sequence.")

        hex_str = self.code[start_pos:end_pos]
        self.pos = end_pos # Consume the hex digits

        try:
            codepoint = int(hex_str, 16)
        except ValueError:
            raise ValueError(f"Invalid characters in \\{escape_type} escape sequence: '{hex_str}'")
        
        try:
            return chr(codepoint)
        except (ValueError, OverflowError):
            raise ValueError(f"Invalid Unicode codepoint U+{codepoint:04X} in \\{escape_type} escape sequence.")

    def _parse_string_literal(self):
        """
        Parses a "processed" string literal with advanced escape sequences.
        """
        self.pos += 1  # Consume the opening '"'
        value_chars = []

        while self.pos < len(self.code):
            char = self.code[self.pos]
            self.pos += 1

            if char == '\\':  # --- Start of an escape sequence ---
                if self.pos >= len(self.code):
                    raise ValueError("Unterminated escape sequence at end of input.")
                
                escape_char = self.code[self.pos]
                self.pos += 1 # Consume the character after '\'

                # Standard single-character escapes
                if   escape_char == 'n': value_chars.append('\n')
                elif escape_char == 't': value_chars.append('\t')
                elif escape_char == 'r': value_chars.append('\r')
                elif escape_char == 'b': value_chars.append('\b')
                elif escape_char == 'f': value_chars.append('\f')
                elif escape_char == '"': value_chars.append('"')
                elif escape_char == '\\': value_chars.append('\\')
                
                # Hex and Unicode escapes
                elif escape_char == 'x':
                    value_chars.append(self._parse_hex_escape(2, "hex"))
                elif escape_char == 'u':
                    value_chars.append(self._parse_hex_escape(4, "unicode"))
                elif escape_char == 'U':
                    value_chars.append(self._parse_hex_escape(8, "unicode"))
                
                else: # Unknown escape sequence: treat as literal backslash + char
                    value_chars.append('\\')
                    value_chars.append(escape_char)
            
            elif char == '"': # --- End of the string ---
                return str("".join(value_chars))

            elif char == '\n': # --- Error case: unescaped newline ---
                raise ValueError("Unterminated string literal: newline encountered.")
            
            else: # --- A normal character ---
                value_chars.append(char)
        
        raise ValueError("Unterminated string literal: end of input reached.")
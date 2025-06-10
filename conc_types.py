from collections import namedtuple

class Word(namedtuple("Word", "value")):
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
        
    def __hash__(self):
        # Delegate the hash calculation to the underlying string.
        return hash(self.value)

# --- types.py ---

class Table:
    """
    A Python implementation of a Lua-style table.
    - Acts as both a list (array part) and a dict (hash part).
    - Array keys are 1-based integers.
    - Accessing a non-existent key returns None (our 'nil').
    """
    def __init__(self):
        self._array_part = []
        self._hash_part = {}
        self.metatable = None # Placeholder for future metatable support

    def __repr__(self):
        # A helpful representation for debugging your stack
        return f"<LuaTable: {len(self._array_part)} array, {len(self._hash_part)} hash>"

    def set(self, key, value):
        """Sets a key-value pair, routing to array or hash part."""
        if isinstance(key, (int, float)) and key > 0 and key == int(key):
            idx = int(key) - 1  # Convert 1-based key to 0-based index
            
            if idx >= len(self._array_part):
                padding_needed = idx - len(self._array_part) + 1
                self._array_part.extend([None] * padding_needed)
            
            self._array_part[idx] = value
        else:
            self._hash_part[key] = value

    def get(self, key):
        """Gets a value, checking the array part first, then the hash part."""
        if isinstance(key, (int, float)) and key > 0 and key == int(key):
            idx = int(key) - 1
            if 0 <= idx < len(self._array_part):
                return self._array_part[idx]

        return self._hash_part.get(key)



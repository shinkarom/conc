from collections import namedtuple

class Word(namedtuple("Word", "value")):
    """A token representing a word. Its repr is just its name."""
    def __repr__(self):
        return "|" + str(self.value) + "|"
        
    def __eq__(self,other):
        if isinstance(other,Word):
            return self.value == other.value
        
        return NotImplemented

    def __hash__(self):
        return hash(self.value)


from dataclasses import dataclass, field
from numpy import uint8, int8, uint16, int16, uint32, int32


def initmem():
    """Return a default value for our main memory."""
    return [uint8(0)] * (2 ** 20)


@dataclass
class Memory:
    """Serves as simultaor main memory with associated read/write functions."""

    _mainmem: list[uint8] = field(default_factory=initmem)

    def store_byte(self, addr: uint32, data: uint8) -> None:
        """Stores a single byte in memory."""
        self._mainmem[addr] = data
        return None

    def load_byte(self, addr: uint32, signed: bool = True) -> int8:
        """Load and return a single byte from memory."""
        if signed:
            return int8(self._mainmem[addr])
        else:
            return uint8(self._mainmem[addr])

    def store_halfword(self, addr: uint32, data: uint16) -> None:
        """Stores a 16-bit halfword in memory."""
        d0 = uint8(data & 0xFF)
        d1 = uint8(data >> 8 & 0xFF)

        self._mainmem[addr] = d0
        self._mainmem[addr + 1] = d1

        return None

    def load_halfword(self, addr: uint32, signed: bool = True) -> int16:
        """Load and return a 16-bit halfword from memory"""
        d0 = self._mainmem[addr]
        d1 = self._mainmem[addr + 1]
        if signed:
            return int16(d1 << 8 | d0)
        else:
            return uint16(d1 << 8 | d0)

    def store_word(self, addr: uint32, data: uint32) -> None:
        """Stores a 32-bit word in memory."""
        d0 = uint8(data & 0xFF)
        d1 = uint8(data >> 8 & 0xFF)
        d2 = uint8(data >> 16 & 0xFF)
        d3 = uint8(data >> 24 & 0xFF)

        self._mainmem[addr] = d0
        self._mainmem[addr + 1] = d1
        self._mainmem[addr + 2] = d2
        self._mainmem[addr + 3] = d3

        return None

    def load_word(self, addr: uint32) -> uint32:
        """Load and return a 16-bit halfword from memory"""
        d0 = self._mainmem[addr]
        d1 = self._mainmem[addr + 1]
        d2 = self._mainmem[addr + 2]
        d3 = self._mainmem[addr + 3]

        print(d0, d1, d2, d3)

        return int32((((d3 << 8 | d2) << 8 | d1) << 8 | d0))

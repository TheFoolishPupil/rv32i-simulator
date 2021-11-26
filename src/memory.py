from dataclasses import dataclass, field
from numpy import uint8, int8, uint16, int16, uint32, int32, array

MEMORY_SIZE = 2 ** 20


def init_mem():
    """Return a default value for our memory."""

    return [uint8(0)] * (MEMORY_SIZE)


@dataclass
class Memory:
    """Serves as simultaor memory with associated read and write functions."""

    _mem: list[uint8] = field(default_factory=init_mem)

    def __len__(self):
        return len(self._mem)

    def copy_to_mem(self, data: list[uint8]):
        """Copies elements from provided list into begining of memory.

        Args:
            data (list[uint8]): The data to copy over.

        """
        i = 0
        for byte in data:
            self._mem[i] = byte
            i += 1

    def store_byte(self, addr: uint32, data: uint8) -> None:
        """Stores a single byte in memory.

        Args:
            addr (uint32): The memory address to write to.
            data (uint8): The data to write.
        """

        self._mem[addr] = data
        return None

    def load_byte(self, addr: uint32, signed: bool = True) -> int8:
        """Load and return a single byte from memory.

        Args:
            addr (uint32): The memory address to load from.
            signed (bool): Determines if the return data should be signed.

        Return:
            The data stored at the address.
        """

        if signed:
            return int8(self._mem[addr])
        else:
            return uint8(self._mem[addr])

    def store_halfword(self, addr: uint32, data: uint16) -> None:
        """Stores a 16-bit halfword in memory.

        Args:
            addr (uint32): The memory address to write to.
            data (uint16): The data to write.
        """

        d0 = uint8(data & 0xFF)
        d1 = uint8(data >> 8 & 0xFF)

        self._mem[addr] = d0
        self._mem[addr + 1] = d1

        return None

    def load_halfword(self, addr: uint32, signed: bool = True) -> int16:
        """Load and return a 16-bit halfword from memory

        Args:
            addr (uint32): The memory address to load from.
            signed (bool): Determines if the return data should be signed.

        Return:
            The data stored at the address.
        """

        d0 = self._mem[addr]
        d1 = self._mem[addr + 1]
        if signed:
            return int16(d1 << 8 | d0)
        else:
            return uint16(d1 << 8 | d0)

    def store_word(self, addr: uint32, data: uint32) -> None:
        """Stores a 32-bit word in memory.

        Args:
            addr (uint32): The memory address to write to.
            data (uint32): The data to write.
        """

        d0 = uint8(data & 0xFF)
        d1 = uint8(data >> 8 & 0xFF)
        d2 = uint8(data >> 16 & 0xFF)
        d3 = uint8(data >> 24 & 0xFF)

        self._mem[addr] = d0
        self._mem[addr + 1] = d1
        self._mem[addr + 2] = d2
        self._mem[addr + 3] = d3

        return None

    def load_word(self, addr: uint32) -> int32:
        """Load and return a 32-bit word from memory

        Args:
            addr (uint32): The memory address to load from.

        Return:
            The data stored at the address.
        """

        d0 = self._mem[addr]
        d1 = self._mem[addr + 1]
        d2 = self._mem[addr + 2]
        d3 = self._mem[addr + 3]

        return int32((((d3 << 8 | d2) << 8 | d1) << 8 | d0))

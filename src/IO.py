from dataclasses import dataclass
from numpy import uint8, uint32, frombuffer, array


@dataclass
class File:
    """Performs IO operations"""

    _path: str

    def load_program(self) -> list[uint8]:
        """Returns a list of np.uint8, read from the provided file path."""

        with open(self._path, "rb") as f:
            data = f.read()
            return frombuffer(buffer=data, dtype=uint8)

    def write_program_results(self, path: str, reg: list[uint32]) -> None:
        """Writes the resulting uint32 register list to a file."""

        with open(path, "wb") as f:
            f.write(array(reg, dtype="uint32").tobytes())

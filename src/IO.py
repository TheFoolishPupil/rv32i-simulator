from dataclasses import dataclass
from numpy import uint32, frombuffer


@dataclass
class File:
    """Performs IO operations

    Attributes:
        path (str): A file path

    """

    _path: str

    def load_program(self) -> list[uint32]:
        """Returns a list of np.uint32, read from the provided file path."""

        with open(self._path, "rb") as f:
            data = f.read()
            return frombuffer(buffer=data, dtype=uint32)

    def write_program_results(self, path: str, reg: list[uint32]) -> None:
        """Writes the resulting uint32 register list to a file."""

        with open(path, "wb") as f:
            f.write(bytearray(reg))

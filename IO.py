from dataclasses import dataclass
from numpy import uint32, frombuffer


@dataclass
class File:
    """Performs IO operations a binary file.

    Attributes:
        path (str): A file path

    """

    _path: str

    def load_program(self) -> list[uint32]:
        """Returns an list of np.uint32, read from the provided file path."""

        with open(self._path, "rb") as f:
            data = f.read()
            return frombuffer(buffer=data, dtype=uint32)

from dataclasses import dataclass
from numpy import uint8, uint32, frombuffer, array


@dataclass
class File:
    """Performs IO operations

    Args:
        _path (str): a path to a .bin file

    """

    _path: str

    @property
    def save_path(self):
        return self._path[:-4] + "_.res"

    def set_program(self, program_path: str):
        """Sets the path attribute, pointig File object to other file path."""
        self._path = program_path

    def load_program(self) -> list[uint8]:
        """Returns a list of np.uint8, read from the provided file path."""

        with open(self._path, "rb") as f:
            data = f.read()
            return frombuffer(buffer=data, dtype=uint8)

    def write_program_results(self, path: str, reg: list[uint32]) -> None:
        """Writes the resulting uint32 register list to a file."""

        with open(path, "wb") as f:
            f.write(array(reg, dtype="uint32").tobytes())

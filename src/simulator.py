import argparse
from dataclasses import dataclass

from CPU import CPU
from IO import File
from memory import Memory


@dataclass
class Simulator:
    """Provides an interface for loading and executing programs.

    Args:
        _io (File): A file object used for loading programs from disk and saving registers.
    """

    _io: File
    _cpu: CPU = CPU()

    def run(self, save_result: bool = False):
        """Executes the program stored loaded from _io on the _cpu."""
        program = self._io.load_program()
        registers = self._cpu.execute_program(program)
        if save_result:
            self._io.write_program_results(self._io.save_path, registers)

    def load_program(self, program_path: str):
        """Allows for reprogramming of the simulator."""
        self._io.set_program(program_path)


parser = argparse.ArgumentParser()
parser.add_argument(
    "--program", help="path to binary file containing program to execute"
)
args = parser.parse_args()


def main() -> None:
    """Provides a CLI to run provided program files, and save the results."""

    simulator = Simulator(File(args.program))
    simulator.run(save_result=True)


if __name__ == "__main__":
    main()

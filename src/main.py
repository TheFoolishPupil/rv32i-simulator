import argparse

from CPU import CPU
from IO import File

parser = argparse.ArgumentParser()
parser.add_argument(
    "--program", help="path to binary file containing program to execute"
)
args = parser.parse_args()

cpu = CPU()


def main() -> None:
    """Executes a program and saves the resulting registers to a file."""

    # Load program and run it through the cpu, store the resulting registers.
    file = File(args.program)
    program = file.load_program()
    cpu = CPU()
    result = cpu.execute_program(program)
    file.write_program_results(args.program[:-4] + "_.res", result)

    # file = File(args.program)
    # print(file.load_program())


if __name__ == "__main__":
    main()

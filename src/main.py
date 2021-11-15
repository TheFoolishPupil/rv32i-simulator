import argparse

from CPU import CPU
from IO import File


parser = argparse.ArgumentParser()
parser.add_argument(
    "--program", help="path to binary file containing program to execute"
)
args = parser.parse_args()


def main() -> None:
    """Executes a program and saves the resulting registers to a file."""

    # Load program and run it through the cpu, store the resulting registers.
    file = File(args.program)
    cpu = CPU()
    print(cpu._mem)
    program = file.load_program()
    result = cpu.execute_program(program)
    file.write_program_results(args.program[:-4] + "_.res", result)


if __name__ == "__main__":
    main()

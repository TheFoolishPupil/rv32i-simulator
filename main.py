from CPU import CPU
from IO import File


file = File("tests/task2/branchcnt.bin")
program = file.load_program()

cpu = CPU()

print(cpu.execute_program(program))

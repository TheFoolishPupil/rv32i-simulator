from IO import File

file = File("tests/task4/t14.res")

program = file.load_program()
print(program)

[         0         23          5          0          0         19
          5          5          0 4294967171         29          0
          5          0         19          5         21         10
          3          1          5          0         19          5
         21          0 4294967171          1          5          0
         19          5]
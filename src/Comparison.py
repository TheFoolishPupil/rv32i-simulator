import filecmp
import os

complete = 1

path = "C:\\Users\\natha\\Desktop\\Travail\\DD\\Cours\\Fall 2021\\Computer Architecture and Engineering\\Finale Assignment\\rv32i-simulator\\tests"

original_path = os.path.dirname(os.path.abspath(__file__))  # /a/b/c/d/e
source_dir = original_path.replace("src", "")
test_path = source_dir + "tests"
print("Building Simulator")

# 1st Test folder
names1 = ["addlarge", "addneg", "addpos", "bool", "set", "shift", "shift2"]
# Run them and compare
for test_file in names1:
    error = os.system(
        "C:\\Users\\natha\\AppData\\Local\\Programs\\Python\\Python310\\python.exe main.py --program ..\\tests\\task1\\"
        + test_file
        + ".bin"
    )

    if error:
        complete = 0
        print("Error, Program Crashed")
    else:
        # If it is executed without error, we check the result.
        same = filecmp.cmp(
            "..\\tests\\task1\\" + test_file + ".res",
            "..\\tests\\task1\\" + test_file + "_.res",
        )
        if not same:
            complete = 0
            print("\n" + test_file + " failed")
        else:
            print("\n" + test_file + " passed")

# 2nd Test folder
names2 = ["branchcnt", "branchmany", "branchtrap"]
# Run them and compare
for test_file in names2:
    error = os.system(
        "C:\\Users\\natha\\AppData\\Local\\Programs\\Python\\Python310\\python.exe main.py --program ..\\tests\\task2\\"
        + test_file
        + ".bin"
    )
    if error:
        complete = 0
        print("Error, Program Crashed")
    else:
        # If it is executed without error, we check the result.
        same = filecmp.cmp(
            "..\\tests\\task2\\" + test_file + ".res",
            "..\\tests\\task2\\" + test_file + "_.res",
        )
        if not same:
            complete = 0
            print("\n" + test_file + " failed")
        else:
            print("\n" + test_file + " passed")

# 3rd Test folder
names3 = ["loop", "recursive", "string", "width"]
# Run them and compare
for test_file in names3:
    error = os.system(
        "C:\\Users\\natha\\AppData\\Local\\Programs\\Python\\Python310\\python.exe main.py --program ..\\tests\\task3\\"
        + test_file
        + ".bin"
    )
    if error:
        complete = 0
        print("Error, Program Crashed")
    else:
        # If it is executed without error, we check the result.
        same = filecmp.cmp(
            "..\\tests\\task3\\" + test_file + ".res",
            "..\\tests\\task3\\" + test_file + "_.res",
        )
        if not same:
            complete = 0
            print("\n" + test_file + " failed")
        else:
            print("\n" + test_file + " passed")

if complete:
    print("All tests passed :) !!!")
else:
    print("Not all tests passed... :( ")

# RV32I CPU Simulator

## Installation

In order to use the RISCV simulator you must use python version 3.10.x with the modules provided in `requirements.txt` installed. Additionally, the convenient test script `run_tests.sh` assumes that such a python environment is activated when run. General steps for getting everything in place are provided below:

1. Clone the repository and change directory with `git clone https://github.com/TheFoolishPupil/rv32i-simulator.git & cd rv32i-simulator`
2. Install python 3.10 on your machine, see [here](https://www.python.org/downloads/release/python-3100/)
3. Create a virtual environment with `\your\path\to\python3.10\bin\python -m venv .venv`
4. Active the environment with `source .venv/bin/active`

Your system should now be ready to use the simulator!

## Usage

### Automated Test Script

Firstly, there is a convenient test script prepared for you. It will iterate over each subdirectory in `tests/` running all contained programs through the simulator, comparing the simulators generated .res file with the provided one. It will report if the a given test passes or fails. If you have another set of test cases you can simply add another directory to `tests/` and execute `run_tests.sh`. Added directories are assumed to follow the same convention as the test cases provided in the assignment.

### Command Line

You can call the simulator directly from the command line as well. to do this you merely call `src/main.py` and provide a path to a program that you wish to execute. For example: `python src/main --program /tests/task3/loop.bin`. A result file will be generated in the same directory that contains the program, with the format `<filename>_.res`. This file contains the content of the registers post-execution. It can be directly compared with provided `.res` files to test the execution.

from dataclasses import dataclass, field
from numpy import uint32

from decode import Decoder, Opcode, Funct3, Funct7


def uint32list():
    """Return a default value for the 32 registers."""
    return [0] * 32


@dataclass
class CPU:
    """RISCV CPU simulator.

    The simulator is capable of executing a subset of the RV32I spec.

    Attributes:
        reg (list[uint32]): The 32 registers of the CPU.

    """

    _pc: int = 0
    _reg: list[uint32] = field(default_factory=uint32list)

    def execute_program(self, program: list[uint32]) -> list[uint32]:
        """Executes a program and returns the resulting register values.

        Args:
            program (list[uint32]): The instructions of a program to be executed.

        Retuns:
            list[uint32]: The computed values in the 32 registers.

        """

        while self._pc < len(program):

            instruction = Decoder(_instruction=program[self._pc])

            match instruction.opcode:
                case Opcode.LUI.value:
                    print("LUI")

                case Opcode.AUIPC.value:
                    print("AUIPC")

                case Opcode.JAL.value:
                    print("JAL")

                case Opcode.JALR.value:
                    print("JALR")


                case Opcode.B_TYPE.value:
                    match instruction.funct3:

                        case Funct3.BEQ.value:
                            print("BEQ")

                        case Funct3.BNE.value:
                            print("BNE")

                        case Funct3.BLT.value:
                            print("BLT")

                        case Funct3.BGE.value:
                            print("BGE")

                        case Funct3.BLTU.value:
                            print("BLTU")

                        case Funct3.BGEU.value:
                            print("BGEU")


                case Opcode.I_TYPE_LOAD.value:
                    match instruction.funct3:

                        case Funct3.LB.value:
                            print("LB")

                        case Funct3.LH.value:
                            print("LH")

                        case Funct3.LW.value:
                            print("LW")

                        case Funct3.LBU.value:
                            print("LBU")

                        case Funct3.LHU.value:
                            print("LHU")


                case Opcode.S_TYPE.value:
                    match instruction.funct3:

                        case Funct3.SB.value:
                            print("SB")

                        case Funct3.SH.value:
                            print("SH")

                        case Funct3.SW.value:
                            print("SW")


                case Opcode.I_TYPE.value:
                    match instruction.funct3:

                        case Funct3.ADDI.value:
                            print("ADDI")

                        case Funct3.SLTI.value:
                            print("SLTI")

                        case Funct3.SLTIU.value:
                            print("SLTIU")

                        case Funct3.XORI.value:
                            print("XORI")

                        case Funct3.ORI.value:
                            print("ORI")

                        case Funct3.ANDI.value:
                            print("ANDI")

                        case Funct3.SLLI.value:
                            print("SLLI")

                        case Funct3.SRLI_SRAI.value:
                            match instruction.funct7:

                                case Funct7.SRLI.value:
                                    print("SRLI")
                                case Funct7.SRAI.value:
                                    print("SRAI")


                case Opcode.R_TYPE.value:
                    match instruction.funct3:

                        case Funct3.ADD_SUB.value:
                            match instruction.funct7:

                                case Funct7.ADD.value:
                                    print("ADD")
                                case Funct7.SUB.value:
                                    print("SUB")

                        case Funct3.SLL.value:
                            print("SLL")

                        case Funct3.SLT.value:
                            print("SLT")

                        case Funct3.SLTU.value:
                            print("SLTU")

                        case Funct3.XOR.value:
                            print("XOR")

                        case Funct3.SRL_SRA.value:
                            match instruction.funct7:
                                case Funct7.SRL.value:
                                    print("SRL")

                                case Funct7.SRA.value:
                                    print("SRA")

                        case Funct3.OR.value:
                            print("OR")

                        case Funct3.AND.value:
                            print("AND")


                case Opcode.ECALL.value:
                    print("ECALL")

            self._pc += 1

        return self._reg

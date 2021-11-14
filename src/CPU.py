from dataclasses import dataclass, field
from numpy import uint32, int32

from decode import Decoder, Opcode, Funct3, Funct7


def initlist():
    """Return a default value for the 32 registers."""
    return [int32(0)] * 32


@dataclass
class CPU:
    """RISCV CPU simulator.

    The simulator is capable of executing a subset of the RV32I spec.

    Attributes:
        reg (list[uint32]): The 32 registers of the CPU.

    """

    _pc: int = 0
    _reg: list[int32] = field(default_factory=initlist)

    def execute_program(self, program: list[uint32]) -> list[int32]:
        """Executes a program and returns the resulting register values.

        Args:
            program (list[uint32]): The instructions of a program to be executed.

        Retuns:
            list[int32]: The computed values in the 32 registers.

        """

        while self._pc >> 2 < len(program):

            inst = Decoder(_instruction=program[self._pc >> 2])

            match inst.opcode:

                case Opcode.LUI.value:
                    self._reg[inst.rd] = inst.imm_u

                case Opcode.AUIPC.value:
                    self._pc = self._pc + inst.imm_u - 4

                case Opcode.JAL.value:
                    pass

                case Opcode.JALR.value:
                    pass


                case Opcode.B_TYPE.value:
                    match inst.funct3:

                        case Funct3.BEQ.value:
                            if self._reg[inst.rs1] == self._reg[inst.rs2]:
                                self._pc = self._pc + inst.imm_b - 4

                        case Funct3.BNE.value:
                            if self._reg[inst.rs1] != self._reg[inst.rs2]:
                                self._pc = self._pc + inst.imm_b - 4

                        case Funct3.BLT.value:
                            if self._reg[inst.rs1] < self._reg[inst.rs2]:
                                self._pc = self._pc + inst.imm_b - 4

                        case Funct3.BGE.value:
                            if self._reg[inst.rs1] > self._reg[inst.rs2]:
                                self._pc = self._pc + inst.imm_b - 4

                        case Funct3.BLTU.value:
                            if uint32(self._reg[inst.rs1]) < uint32(self._reg[inst.rs2]):
                                self._pc = self._pc + inst.imm_b - 4

                        case Funct3.BGEU.value:
                            if uint32(self._reg[inst.rs1]) > uint32(self._reg[inst.rs2]):
                                self._pc = self._pc + inst.imm_b - 4


                case Opcode.I_TYPE_LOAD.value:
                    match inst.funct3:

                        case Funct3.LB.value:
                            pass

                        case Funct3.LH.value:
                            pass

                        case Funct3.LW.value:
                            pass

                        case Funct3.LBU.value:
                            pass

                        case Funct3.LHU.value:
                            pass


                case Opcode.S_TYPE.value:
                    match inst.funct3:

                        case Funct3.SB.value:
                            pass

                        case Funct3.SH.value:
                            pass

                        case Funct3.SW.value:
                            pass


                case Opcode.I_TYPE.value:
                    match inst.funct3:

                        case Funct3.ADDI.value:
                            self._reg[inst.rd] = self._reg[inst.rs1] + inst.imm_i

                        case Funct3.SLTI.value:
                            if self._reg[inst.rs1] < inst.imm_i:
                                self._reg[inst.rd] = 1
                            else:
                                self._reg[inst.rd] = 0

                        case Funct3.SLTIU.value:
                            if uint32(self._reg[inst.rs1]) < uint32(inst.imm_i):
                                self._reg[inst.rd] = 1
                            else:
                                self._reg[inst.rd] = 0

                        case Funct3.XORI.value:
                            self._reg[inst.rd] = self._reg[inst.rs1] ^ inst.imm_i

                        case Funct3.ORI.value:
                            self._reg[inst.rd] = self._reg[inst.rs1] | inst.imm_i

                        case Funct3.ANDI.value:
                            self._reg[inst.rd] = self._reg[inst.rs1] & inst.imm_i

                        case Funct3.SLLI.value:
                            self._reg[inst.rd] = int32(self._reg[inst.rs1] << inst.shamt)

                        case Funct3.SRLI_SRAI.value:
                            match inst.funct7:

                                case Funct7.SRLI.value:
                                    self._reg[inst.rd] = int(uint32(self._reg[inst.rs1]) >> inst.shamt)

                                case Funct7.SRAI.value:
                                    self._reg[inst.rd] = self._reg[inst.rs1] >> inst.shamt



                case Opcode.R_TYPE.value:
                    match inst.funct3:

                        case Funct3.ADD_SUB.value:
                            match inst.funct7:

                                case Funct7.ADD.value:
                                    self._reg[inst.rd] = self._reg[inst.rs1] + self._reg[inst.rs2]

                                case Funct7.SUB.value:
                                    self._reg[inst.rd] = self._reg[inst.rs1] - self._reg[inst.rs2]

                        case Funct3.SLL.value:
                            self._reg[inst.rd] = int32(self._reg[inst.rs1] << self._reg[inst.rs2])

                        case Funct3.SLT.value:
                            if self._reg[inst.rs1] < self._reg[inst.rs2]:
                                self._reg[inst.rd] = 1
                            else:
                                self._ref[inst.rd] = 0

                        case Funct3.SLTU.value:
                            if uint32(self._reg[inst.rs1]) < uint32(self._reg[inst.rs2]):
                                self._reg[inst.rd] = 1
                            else:
                                self._ref[inst.rd] = 0

                        case Funct3.XOR.value:
                            self._reg[inst.rd] = self._reg[inst.rs1] ^ self._reg[inst.rs2]

                        case Funct3.SRL_SRA.value:
                            match inst.funct7:
                                case Funct7.SRL.value:
                                    self._reg[inst.rd] = int(uint32(self._reg[inst.rs1]) >> self._reg[inst.rs2])

                                case Funct7.SRA.value:
                                    self._reg[inst.rd] = self._reg[inst.rs1] >> self._reg[inst.rs2]

                        case Funct3.OR.value:
                            self._reg[inst.rd] = self._reg[inst.rs1] | self._reg[inst.rs2]

                        case Funct3.AND.value:
                            self._reg[inst.rd] = self._reg[inst.rs1] & self._reg[inst.rs2]


                case Opcode.ECALL.value:
                    pass

                case _:
                    pass

            for reg in self._reg:
                print(str(reg) + " ", end="")
            print()

            self._pc += 4

        return self._reg


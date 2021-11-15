from dataclasses import dataclass, field
from numpy import uint32, int32, uint16, int16, uint8, int8, seterr

from decode import Decoder, Opcode, Funct3, Funct7
from memory import Memory

# ignore overflow warnings. Overflows are expected.
seterr(over="ignore")


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
    _mem: Memory = Memory()


    def _writeToRegister(self, val: uint32, instruction: uint32) -> None:
        """Writes value to rd Register
        Args:
            val (uint32) : value to be written
            instruction (uint32) : RISC-V instruction

        """
        if instruction.rd !=0:
            self._reg[instruction.rd] = val

    def execute_program(self, program: list[uint32]) -> list[int32]:
        """Executes a program and returns the resulting register values.

        Args:
            program (list[uint32]): The instructions of a program to be executed.

        Retuns:
            list[int32]: The computed values in the 32 registers.

        """

        while uint32(self._pc >> 2) < len(program):

            inst = Decoder(_instruction=program[self._pc >> 2])

            match inst.opcode:


                case Opcode.LUI.value:
                    self._writeToRegister(inst.imm_u, inst)


                case Opcode.AUIPC.value:
                    self._writeToRegister(self._pc + inst.imm_u - 4, inst)



                case Opcode.JAL.value:
                    self._writeToRegister(self._pc + 4, inst)
                    self._pc = self._pc + inst.imm_j - 4


                case Opcode.JALR.value:
                    self._writeToRegister(self._pc + 4, inst)
                    self._pc = inst.rs1 + inst.imm_i - 4


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
                            self._writeToRegister(self._mem.load_byte(inst.rs1 + inst.imm_i), inst)

                        case Funct3.LH.value:
                            self._writeToRegister(self._mem.load_halfword(inst.rs1 + inst.imm_i), inst)

                        case Funct3.LW.value: # signed?
                            self._writeToRegister(self._mem.load_word(inst.rs1 + inst.imm_i), inst)

                        case Funct3.LBU.value:
                            self._writeToRegister(self._mem.load_byte(inst.rs1 + inst.imm_i, signed=False), inst)

                        case Funct3.LHU.value:
                            self._writeToRegister(self._mem.load_halfword(inst.rs1 + inst.imm_i, signed=False), inst)


                case Opcode.S_TYPE.value:
                    match inst.funct3:

                        case Funct3.SB.value:
                            byte = uint8(self._reg[inst.rs2] & 0xFF)
                            self._mem.store_byte(addr=inst.rs1+inst.imm_s, data=byte)

                        case Funct3.SH.value:
                            halfword = uint16(self._reg[inst.rs2] & 0xFFFF)
                            self._mem.store_halfword(addr=inst.rs1+inst.imm_s, data=halfword)

                        case Funct3.SW.value:
                            word = uint32(self._reg[inst.rs2])
                            self._mem.store_halfword(addr=inst.rs1+inst.imm_s, data=word)


                case Opcode.I_TYPE.value:
                    match inst.funct3:

                        case Funct3.ADDI.value:
                            self._writeToRegister(self._reg[inst.rs1] + inst.imm_i, inst)

                        case Funct3.SLTI.value:
                            if self._reg[inst.rs1] < inst.imm_i:
                                self._writeToRegister(1, inst)
                            else:
                                self._writeToRegister(0, inst)

                        case Funct3.SLTIU.value:
                            if uint32(self._reg[inst.rs1]) < uint32(inst.imm_i):
                                self._writeToRegister(1, inst)
                            else:
                                self._writeToRegister(0, inst)

                        case Funct3.XORI.value:
                            self._writeToRegister(self._reg[inst.rs1] ^ inst.imm_i, inst)

                        case Funct3.ORI.value:
                            self._writeToRegister(self._reg[inst.rs1] | inst.imm_i, inst)

                        case Funct3.ANDI.value:
                            self._writeToRegister(self._reg[inst.rs1] & inst.imm_i, inst)

                        case Funct3.SLLI.value:
                            self._writeToRegister(int32(self._reg[inst.rs1] << inst.shamt), inst)

                        case Funct3.SRLI_SRAI.value:
                            match inst.funct7:

                                case Funct7.SRLI.value:
                                    self._writeToRegister(int(uint32(self._reg[inst.rs1]) >> inst.shamt), inst)

                                case Funct7.SRAI.value:
                                    self._writeToRegister(self._reg[inst.rs1] >> inst.shamt, inst)


                case Opcode.R_TYPE.value:
                    match inst.funct3:

                        case Funct3.ADD_SUB.value:
                            match inst.funct7:

                                case Funct7.ADD.value:
                                    self._writeToRegister(self._reg[inst.rs1] + self._reg[inst.rs2], inst)

                                case Funct7.SUB.value:
                                    self._writeToRegister(self._reg[inst.rs1] - self._reg[inst.rs2], inst)

                        case Funct3.SLL.value:
                            self._writeToRegister(int32(self._reg[inst.rs1] << self._reg[inst.rs2]), inst)

                        case Funct3.SLT.value:
                            if self._reg[inst.rs1] < self._reg[inst.rs2]:
                                self._writeToRegister(1, inst)
                            else:
                                self._writeToRegister(0, inst)

                        case Funct3.SLTU.value:
                            if uint32(self._reg[inst.rs1]) < uint32(self._reg[inst.rs2]):
                                self._writeToRegister(1, inst)
                            else:
                                self._writeToRegister(0, inst)

                        case Funct3.XOR.value:
                            self._writeToRegister(self._reg[inst.rs1] ^ self._reg[inst.rs2], inst)

                        case Funct3.SRL_SRA.value:
                            match inst.funct7:
                                case Funct7.SRL.value:
                                    self._writeToRegister(int(uint32(self._reg[inst.rs1]) >> self._reg[inst.rs2]), inst)

                                case Funct7.SRA.value:
                                    self._writeToRegister(self._reg[inst.rs1] >> self._reg[inst.rs2], inst)

                        case Funct3.OR.value:
                            self._writeToRegister(self._reg[inst.rs1] | self._reg[inst.rs2], inst)

                        case Funct3.AND.value:
                            self._writeToRegister(self._reg[inst.rs1] & self._reg[inst.rs2], inst)


                case Opcode.ECALL.value:
                    # TODO: implement other ecalls

                    # ECALL 10
                    return self._reg


                case _:
                    pass

            print()
            for reg in self._reg:
                print(str(reg) + " ", end="")
            print()

            self._pc += 4

        return self._reg


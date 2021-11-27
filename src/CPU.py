from dataclasses import dataclass, field
from numpy import uint32, int32, uint16, uint8, seterr

from decode import Decoder, Opcode, Funct3, Funct7
from memory import Memory, MEMORY_SIZE


# ignore overflow warnings. Overflows are expected.
seterr(over="ignore")


def init_registers():
    """Return initial state of the registers."""
    reg = [int32(0)] * 32
    reg[1] = MEMORY_SIZE  # Initialize stack pointer
    return [int32(0)] * 32


@dataclass
class CPU:
    """RISCV CPU simulator.

    The simulator is capable of executing a subset of the RV32I spec.
    """

    _pc: int = 0
    _reg: list[int32] = field(default_factory=init_registers)
    _mem: Memory = Memory()


    def _write_to_register(self, val: int, instruction: Decoder) -> None:
        """Writes value to rd Register.

        This checks if the register pointed to by rd is 0, in which case it doesn't
        write, ensuring x0 is always 0.

        Args:
            val (uint32) : value to be written
            instruction (Decoder) : Decoded RISC-V instruction

        """
        if instruction.rd != 0:
            self._reg[instruction.rd] = val

    def execute_program(self, program: list[uint8]) -> list[int32]:
        """Executes a program and returns the resulting register values.

        Args:
            program (list[uint32]): The instructions of a program to be executed.

        Retuns:
            list[int32]: The computed values in the 32 registers.

        """

        # Load program into memory
        self._mem.copy_to_mem(data=program)

        while uint32(self._pc) < len(self._mem):

            inst = Decoder(_instruction=self._mem.load_word(self._pc))

            match inst.opcode:

                case Opcode.LUI.value:
                    self._write_to_register(inst.imm_u, inst)

                case Opcode.AUIPC.value:
                    self._write_to_register(self._pc + inst.imm_u, inst)

                case Opcode.JAL.value:
                    self._write_to_register(self._pc + 4, inst)
                    self._pc = self._pc + inst.imm_j - 4  # Subtract 4 to compensate for pc increment

                case Opcode.JALR.value:
                    self._write_to_register(self._pc + 4, inst)
                    self._pc = ((inst.rs1 + inst.imm_i) & 0xFFFFFFFE) - 4

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
                            self._write_to_register(self._mem.load_byte(self._reg[inst.rs1] + inst.imm_i), inst)

                        case Funct3.LH.value:
                            self._write_to_register(self._mem.load_halfword(self._reg[inst.rs1] + inst.imm_i), inst)
                            
                        case Funct3.LW.value:  # signed?
                            self._write_to_register(self._mem.load_word(self._reg[inst.rs1] + inst.imm_i), inst)

                        case Funct3.LBU.value:
                            self._write_to_register(self._mem.load_byte(self._reg[inst.rs1] + inst.imm_i, signed=False), inst)

                        case Funct3.LHU.value:
                            self._write_to_register(self._mem.load_halfword(self._reg[inst.rs1] + inst.imm_i, signed=False), inst)

                case Opcode.S_TYPE.value:
                    match inst.funct3:

                        case Funct3.SB.value:
                            byte = uint8(self._reg[inst.rs2] & 0xFF)
                            self._mem.store_byte(addr=self._reg[inst.rs1]+inst.imm_s, data=byte)

                        case Funct3.SH.value:
                            halfword = uint16(self._reg[inst.rs2] & 0xFFFF)
                            self._mem.store_halfword(addr=self._reg[inst.rs1]+inst.imm_s, data=halfword)

                        case Funct3.SW.value:
                            word = uint32(self._reg[inst.rs2])
                            self._mem.store_word(addr=self._reg[inst.rs1]+inst.imm_s, data=word)

                case Opcode.I_TYPE.value:
                    match inst.funct3:

                        case Funct3.ADDI.value:
                            self._write_to_register(self._reg[inst.rs1] + inst.imm_i, inst)

                        case Funct3.SLTI.value:
                            if self._reg[inst.rs1] < inst.imm_i:
                                self._write_to_register(1, inst)
                            else:
                                self._write_to_register(0, inst)

                        case Funct3.SLTIU.value:
                            if uint32(self._reg[inst.rs1]) < uint32(inst.imm_i):
                                self._write_to_register(1, inst)
                            else:
                                self._write_to_register(0, inst)

                        case Funct3.XORI.value:
                            self._write_to_register(self._reg[inst.rs1] ^ inst.imm_i, inst)

                        case Funct3.ORI.value:
                            self._write_to_register(self._reg[inst.rs1] | inst.imm_i, inst)

                        case Funct3.ANDI.value:
                            self._write_to_register(self._reg[inst.rs1] & inst.imm_i, inst)

                        case Funct3.SLLI.value:
                            self._write_to_register(int32(self._reg[inst.rs1] << inst.shamt), inst)

                        case Funct3.SRLI_SRAI.value:
                            match inst.funct7:

                                case Funct7.SRLI.value:
                                    self._write_to_register(int(uint32(self._reg[inst.rs1]) >> inst.shamt), inst)

                                case Funct7.SRAI.value:
                                    self._write_to_register(self._reg[inst.rs1] >> inst.shamt, inst)

                case Opcode.R_TYPE.value:
                    match inst.funct3:

                        case Funct3.ADD_SUB.value:
                            match inst.funct7:

                                case Funct7.ADD.value:
                                    self._write_to_register(self._reg[inst.rs1] + self._reg[inst.rs2], inst)

                                case Funct7.SUB.value:
                                    self._write_to_register(self._reg[inst.rs1] - self._reg[inst.rs2], inst)

                        case Funct3.SLL.value:
                            self._write_to_register(int32(self._reg[inst.rs1] << self._reg[inst.rs2]), inst)

                        case Funct3.SLT.value:
                            if self._reg[inst.rs1] < self._reg[inst.rs2]:
                                self._write_to_register(1, inst)
                            else:
                                self._write_to_register(0, inst)

                        case Funct3.SLTU.value:
                            if uint32(self._reg[inst.rs1]) < uint32(self._reg[inst.rs2]):
                                self._write_to_register(1, inst)
                            else:
                                self._write_to_register(0, inst)

                        case Funct3.XOR.value:
                            self._write_to_register(self._reg[inst.rs1] ^ self._reg[inst.rs2], inst)

                        case Funct3.SRL_SRA.value:
                            match inst.funct7:
                                case Funct7.SRL.value:
                                    self._write_to_register(int(uint32(
                                            self._reg[inst.rs1]) >> self._reg[inst.rs2]), inst)

                                case Funct7.SRA.value:
                                    self._write_to_register(self._reg[inst.rs1] >> self._reg[inst.rs2], inst)

                        case Funct3.OR.value:
                            self._write_to_register(self._reg[inst.rs1] | self._reg[inst.rs2], inst)

                        case Funct3.AND.value:
                            self._write_to_register(self._reg[inst.rs1] & self._reg[inst.rs2], inst)

                case Opcode.ECALL.value:
                    # TODO: implement other ecalls

                    return self._reg  # ECALL 10

                case _:
                    pass

            # for reg in self._reg:
            #     print(str(reg) + " ", end="")
            # print()
            # # print(self._mem._mem[256:350])
            # print()
            self._pc += 4

        return self._reg

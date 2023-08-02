/**
 * CPU class. Documentation:
 * https://www.masswerk.at/6502/6502_instruction_set.html
 */
export class CPU {
    program = [];

    program_counter = -1;
    register_a = 0x0;
    register_x = 0x0;
    register_y = 0x0;
    cycles = 0;
    stopped = false;
    status_register = 0;
    memory = new Map();

    opcodes = new Map<number, [(data?: number) => void, () => number, number]>([
        [0xa9, [this.lda, this.mode_imm, 2]],
        [0x85, [this.sta, this.mode_zp0, 3]],
        [0x65, [this.adc, this.mode_zp0, 3]],
        [0xe6, [this.inc, this.mode_zp0, 5]],
        [0xa4, [this.ldy, this.mode_zp0, 3]],
        [0xc8, [this.iny, this.mode_imp, 2]],
        [0x00, [this.brk, this.mode_imp, 7]],
    ]);

    pc() {
        return (this.program_counter += 1);
    }

    load_program(bytes) {
        this.program = bytes;
    }

    run_program() {
        while (!this.stopped) {
            this.step();
        }
    }

    step() {
        const opcode = this.program[this.pc()];
        const op_def = this.opcodes.get(opcode);
        if (!op_def) {
            console.log('unknown op code: 0x' + opcode.toString(16));
        }

        const [op, mode, cycles] = op_def;
        const data = mode.bind(this)();
        op.bind(this)(data);
        this.cycles += cycles;
    }

    peek(addr) {
        return this.memory.get(addr);
    }

    set_flag(flag: Flags, val: boolean) {
        if (val) {
            this.status_register |= flag;
        } else {
            this.status_register &= ~flag;
        }
    }

    get_flag(flag: Flags) {
        return !!(this.status_register & flag);
    }

    get_flag_bit(flag: Flags) {
        return this.get_flag(flag) ? 1 : 0;
    }

    /**
     * IMM: Immediate Mode. Take the next byte as the value for the op code. Does not touch memory
     * Ex: 0xa9, 0x10 = LDA #$10 = Load 0x10 into A
     */
    mode_imm() {
        this.program_counter++;
        return this.program[this.program_counter];
    }

    /**
     * ZP0: Zero Page.
     * OPC $LL; operand is zeropage address (hi-byte is zero, address = $00LL)
     */
    mode_zp0() {
        this.program_counter++;
        return this.program[this.program_counter];
    }

    /**
     * IMP: Implied
     * operand implied
     */
    mode_imp() {
        return;
    }

    lda(data) {
        this.register_a = data;
        this.set_flag(Flags.Z, this.register_a === 0);
        this.set_flag(Flags.N, !!(this.register_a & (1 << 7)));
    }

    sta(data) {
        this.memory.set(data, this.register_a);
    }

    /**
     * This instruction adds the value of memory and carry from the previous operation to the value of the accumulator and stores the result in the accumulator.
     *
     * This instruction affects the accumulator; sets the carry flag when the sum of a binary add exceeds 255 or when the sum of a decimal add exceeds 99, otherwise carry is reset. The overflow flag is set when the sign or bit 7 is changed due to the result exceeding +127 or -128, otherwise overflow is reset. The negative flag is set if the accumulator result contains bit 7 on, otherwise the negative flag is reset. The zero flag is set if the accumulator result is 0, otherwise the zero flag is reset.
     */
    adc(data) {
        var fetched = this.memory.get(data);
        const tmp = this.register_a + fetched + this.get_flag_bit(Flags.C);

        // Overflow: https://forums.nesdev.org/viewtopic.php?t=6331
        // https://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        this.set_flag(
            Flags.V,
            !!(~(this.register_a ^ fetched) & (this.register_a ^ tmp) & 0x0080)
        );

        this.register_a = tmp & 0x00ff; // truncate to 255

        this.set_flag(Flags.C, tmp > 255);
        this.set_flag(Flags.Z, this.register_a === 0);
        this.set_flag(Flags.N, !!(tmp & 0x0080));
    }

    inc(data) {
        this.memory.set(data, this.memory.get(data) + 1);
    }

    ldy(data) {
        this.register_y = this.memory.get(data);
    }

    iny(_) {
        this.register_y += 1;
    }

    brk(_) {
        this.stopped = true;
    }
}

enum Flags {
    /**
     * The carry flag (C) flag is used as a buffer and as a borrow in arithmetic operations. Any comparisons will update this additionally to the Z and N flags, as do shift and rotate operations.
     */
    C,
    /**
     * The zero flag (Z) indicates a value of all zero bits and the negative flag (N) indicates the presence of a set sign bit in bit-position 7. These flags are always updated, whenever a value is transferred to a CPU register (A,X,Y) and as a result of any logical ALU operations. The Z and N flags are also updated by increment and decrement operations acting on a memory location.
     */
    Z = 1 << 1,
    /**
     * The interrupt inhibit flag (I) blocks any maskable interrupt requests (IRQ).
     */
    I = 1 << 2,
    /**
     * The decimal flag (D) sets the ALU to binary coded decimal (BCD) mode for additions and subtractions (ADC, SBC). This flag is not used in the NES.
     */
    D = 1 << 3,
    /**
     * The break flag (B) is not an actual flag implemented in a register, and rather appears only, when the status register is pushed onto or pulled from the stack. When pushed, it will be 1 when transfered by a BRK or PHP instruction, and zero otherwise (i.e., when pushed by a hardware interrupt). When pulled into the status register (by PLP or on RTI), it will be ignored.
     *
     * In other words, the break flag will be inserted, whenever the status register is transferred to the stack by software (BRK or PHP), and will be zero, when transferred by hardware. Since there is no actual slot for the break flag, it will be always ignored, when retrieved (PLP or RTI). The break flag is not accessed by the CPU at anytime and there is no internal representation. Its purpose is more for patching, to discern an interrupt caused by a BRK instruction from a normal interrupt initiated by hardware.
     */
    B = 1 << 4,
    /**
     * Unused
     */
    _ = 1 << 5,
    /**
     * The overflow flag (V) indicates overflow with signed binary arithmetics. As a signed byte represents a range of -128 to +127, an overflow can never occur when the operands are of opposite sign, since the result will never exceed this range. Thus, overflow may only occur, if both operands are of the same sign. Then, the result must be also of the same sign. Otherwise, overflow is detected and the overflow flag is set. (I.e., both operands have a zero in the sign position at bit 7, but bit 7 of the result is 1, or, both operands have the sign-bit set, but the result is positive.)
     */
    V = 1 << 6,
    /**
     * The zero flag (Z) indicates a value of all zero bits and the negative flag (N) indicates the presence of a set sign bit in bit-position 7. These flags are always updated, whenever a value is transferred to a CPU register (A,X,Y) and as a result of any logical ALU operations. The Z and N flags are also updated by increment and decrement operations acting on a memory location.
     */
    N = 1 << 7,
}

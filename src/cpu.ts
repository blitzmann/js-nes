export class CPU {
    program = [];

    program_counter = -1;
    register_a = 0x0;
    register_x = 0x0;
    register_y = 0x0;
    cycles = 0;
    stopped = false;
    memory = new Map();

    opcodes = new Map([
        [0xa9, this.lda_immediate],
        [0x85, this.sta_zeropage],
        [0x65, this.adc_zeropage],
        [0xe6, this.inc_zeropage],
        [0xa4, this.ldy_zeropage],
        [0xc8, this.iny],
        [0x00, this.brk],
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
        const func = this.opcodes.get(opcode);
        if (!func) {
            console.log("sdasd'");
        }
        func.bind(this)();
    }

    peek(addr) {
        return this.memory.get(addr);
    }

    /**
     * IMM: Immediate Mode. Take the next byte as the value for the op code. Does not touch memory
     * Ex: 0xa9, 0x10 = LDA #$10 = Load 0x10 into A
     */
    mode_imm() {
        this.program_counter++;
        return this.program[this.program_counter];
    }

    lda_immediate() {
        const data = this.program[this.pc()];
        this.register_a = data;
        this.cycles += 2;
    }

    sta_zeropage() {
        const data = this.program[this.pc()];
        this.memory.set(data, this.register_a);
        this.cycles += 3;
    }

    adc_zeropage() {
        // todo: deal with carry
        const data = this.program[this.pc()];
        this.register_a += this.memory.get(data);
        this.cycles += 3;
    }

    inc_zeropage() {
        const data = this.program[this.pc()];
        this.memory.set(data, this.memory.get(data) + 1);
        this.cycles += 5;
    }

    ldy_zeropage() {
        const data = this.program[this.pc()];
        this.register_y = this.memory.get(data);
        this.cycles += 3;
    }

    iny() {
        this.register_y += 1;
        this.cycles += 2;
    }

    brk() {
        this.stopped = true;
        this.cycles += 7;
    }
}

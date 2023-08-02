import test from 'ava';
import { CPU } from './cpu';

test('simple program', (t) => {
    const cpu = new CPU();
    // prettier-ignore
    cpu.load_program([
        0xa9, 0x10, // LDA #$10     -> A = #$10 (value $10, or 16)
        0x85, 0x20, // STA $20      -> $20 = #$10 (address $20 = $10, or 16)
        0xa9, 0x01, // LDA #$1      -> A = #$1
        0x65, 0x20, // ADC $20      -> A = #$11
        0x85, 0x21, // STA $21      -> $21=#$11
        0xe6, 0x21, // INC $21      -> $21=#$12
        0xa4, 0x21, // LDY $21      -> Y=#$12
        0xc8,       // INY          -> Y=#$13
        0x00,       // BRK
    ]);

    cpu.run_program();

    t.is(cpu.peek(0x20), 0x10);
    t.is(cpu.peek(0x21), 0x12);
    t.is(cpu.register_a, 0x11);
    t.is(cpu.register_y, 0x13);
});

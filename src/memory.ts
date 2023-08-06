export class Memory {
    data;

    constructor(size) {
        this.data = new Uint8Array(size + 1);
    }

    get(addr) {
        return this.data[addr];
    }

    set(addr, data) {
        this.data[addr] = data;
    }

    load(bytes, offset = 0x00) {
        // todo: check bounds
        for (let i = 0; i < bytes.length; i++) {
            this.data[offset + i] = bytes[i];
        }
    }
}

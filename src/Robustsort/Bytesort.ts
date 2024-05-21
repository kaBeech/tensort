type Bit = number

type Byte = Bit[]

interface Bytemap {
    pointers: number[],
    bytes: Byte[],
}

type Metabyte = Bytemap[] | Metabytemap[]

interface Metabytemap {
    pointers: number[],
    metabytes: Metabyte[],
}

interface Package {
    reference: number,
    value: Bit
}

type Bytestack = Metabyte

type Stackmap = Metabytemap


type Bit = number

type Byte = Bit[]

interface Bytemap {
    pointers: number[],
    bytes: Byte[],
}

type Metabyte = Bytemap | Metabytemap

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

const bubblesort = (a: number[]) => a.sort()

const bytesort4Bit = (a: number[]) => bytesort(a, 4)

const randomizeArray = (a: number[]): number[] => {
    const newArray: number[] = []
    for (const x of a) {
        if (Math.random() > 0.5) {
            newArray.push(x)
        } else {
            newArray.unshift(x)
        }
    }
    return newArray
}

const bytesort = (bits: number[], bytesize: number): number[] => {
    bits = randomizeArray(bits)
    const bytes = convertRawBitsToBytes(bits, bytesize)
    let bytestacks = getBytestacksFromBytes(bytes)
    while (bytestacks.length > bytesize) {
        bytestacks = reduceBytestacks(bytestacks)
    }
    return getSortedArrayFromBytestacks(bytestacks)
}

const convertRawBitsToBytes = (bits: number[], bytesize: number): Byte[] => {
    const bytes: Byte[] = []
    while (bits.length > 0) {
        let byte = bits.slice(0, bytesize)
        byte = bubblesort(byte)
        bytes.push(byte)
        bits = bits.slice(bytesize)
    }
    return bytes
}

const getBytestacksFromBytes = (bytes: Byte[]): Bytestack[] => {
    const bytestacks: Bytestack[] = []
    while (bytes.length > 0) {
        bytestacks.push(getBytemapFromBytes(bytes.slice(0, 4)))
        bytes = bytes.slice(4)
    }
    return bytestacks
}

const getBytemapFromBytes = (bytes: Byte[]): Bytemap => {
    const pointers: number[] = []
    let i = 0
    while (i < bytes.length) {
        pointers.push(i)
        i++
    }
    return { pointers, bytes }
}

const reduceBytestacks = (bytestacks: Bytestack[]): Bytestack[] => {
    return bytestacks
}

const getSortedArrayFromBytestacks = (bytestacks: Bytestack[]) => {
    return [1, 2, 3, 4, 5, bytestacks.length]
}

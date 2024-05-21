type Bit = number

type Byte = Bit[]

interface Bytemap {
    pointers: number[],
    data: Byte[],
}

type Metabyte = Bytemap | Metabytemap

interface Metabytemap {
    pointers: number[],
    data: Metabyte[],
}

interface Package {
    reference: number,
    bit: Bit
}

type Bytestack = Metabyte

type Stackmap = Metabytemap

// Dummy function in place of a true bubblesort
const bubblesort = (a: number[]) => a.sort()

// Dummy function in place of a true bubblesort
const bubblesortPackages = (packages: Package[]) => packages.sort((a, b) => a.bit - b.bit)

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
        bytestacks = reduceBytestacks(bytestacks, 4)
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
    return { pointers, data: bytes }
}

const reduceBytestacks = (bytestacks: Bytestack[], bytesize: number): Bytestack[] => {
    const newBytestacks: Bytestack[] = []
    while (bytestacks.length > 0) {
        const bytestack = createBytestack(bytestacks.slice(0, bytesize))
        newBytestacks.push(bytestack)
        bytestacks = bytestacks.slice(bytesize)
    }
    return newBytestacks
}

const createBytestack = (metabytes: Metabyte[]): Bytestack => {
    const packages: Package[] = []
    let i = 0
    for (const metabyte of metabytes) {
        packages.push({ reference: i, bit: getTopValueFromBytestack(metabyte) })
    }
    let packagesSorted = bubblesortPackages(packages)
    const pointers: number[] = []
    for (const package of packagesSorted) {
        pointers.push(package.reference)
    }
    return { pointers, data: metabytes }
}

const getTopValueFromBytestack = (bytestack: Bytestack): number => {
    const topByteOrMetabyte: Byte | Metabyte = bytestack.data[bytestack.pointers[bytestack.pointers.length - 1]]
    if (typeof topByteOrMetabyte[0] == "number") {
        const topByte = topByteOrMetabyte as Byte
        return topByte[topByte.length - 1]
    } else return getTopValueFromBytestack(topByteOrMetabyte as Metabyte)
}

const getSortedArrayFromBytestacks = (bytestacks: Bytestack[]) => {
    return [1, 2, 3, 4, 5, bytestacks.length]
}

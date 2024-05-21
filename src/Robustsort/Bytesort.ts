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
    const packages = getPackagesFromMetabytes(metabytes)
    let packagesSorted = bubblesortPackages(packages)
    const pointers: number[] = []
    for (const package of packagesSorted) {
        pointers.push(package.reference)
    }
    return { pointers, data: metabytes }
}

const getPackagesFromMetabytes = (metabytes: Metabyte[]): Package[] => {
    const packages: Package[] = []
    let i = 0
    for (const metabyte of metabytes) {
        packages.push({ reference: i, bit: getTopValueFromBytestack(metabyte)! })
    }
    return packages
}

const getTopValueFromBytestack = (bytestack: Bytestack): number | null => {
    const topByteOrMetabyte: Byte | Metabyte = bytestack.data[bytestack.pointers[bytestack.pointers.length - 1]]
    switch (topByteOrMetabyte[0]) {
        case "undefined": {
            return null
        }
        case "number": {
            const topByte = topByteOrMetabyte as Byte
            return topByte[topByte.length - 1]
        }
        default: return getTopValueFromBytestack(topByteOrMetabyte as Metabyte)
    }
}

const getSortedArrayFromBytestacks = (bytestacks: Bytestack[]): number[] => {
    const sortedArray: number[] = []
    while (!(areAllEmpty(bytestacks))) {
        const { nextElement, newBytestacks } = getNextElementFromBytestacks(bytestacks)
        sortedArray.unshift(nextElement)
        bytestacks = newBytestacks
    }
    return sortedArray
}

const areAllEmpty = (metabytes: Metabyte[]): boolean => {
    let allEmpty = true
    for (const metabyte of metabytes) {
        if (getTopValueFromBytestack(metabyte) !== null) {
            allEmpty = false
            break
        }
    }
    return allEmpty
}

const getNextElementFromBytestacks = (bytestacks: Bytestack[]): { nextElement: number, newBytestacks: Bytestack[] } => {
    const packages = getPackagesFromMetabytes(bytestacks)
    const sortedPackages = bubblesortPackages(packages)
    const topBytestackIndex = getTopPackage(sortedPackages).reference
    const topBytestack: Bytestack = bytestacks[topBytestackIndex]
    const result = removeTopValueFromBytestack(topBytestack)
    const topValue: number = result.topValue
    const newBytestack: Bytestack[] = [result.newBytestack]
    const newBytestacks: Bytestack[] = bytestacks.slice(0, topBytestackIndex).concat(newBytestack, bytestacks.slice(topBytestackIndex + 1))
    return { nextElement: topValue, newBytestacks }
}

const getTopPackage = (packages: Package[]): Package => {
    while (packages.length > 0) {
        let package = packages.pop()!
        if (typeof package.bit == "number") {
            return package
        }
    }
    throw Error("No package in input contains a numerical value")
}

const removeTopValueFromBytestack = (bytestack: Bytestack): { topValue: number, newBytestack: Bytestack } => {
    const topPointer = bytestack.pointers[bytestack.pointers.length - 1]
    const topByteOrMetabyte: Byte | Metabyte = bytestack.data[topPointer]
    switch (topByteOrMetabyte[0]) {
        // This case should be unnecessary now
        // case "undefined": {
        //     bytestack.pointers.pop()
        //     return removeTopValueFromBytestack(bytestack)
        // }
        case "number": {
            let topByte = topByteOrMetabyte as Byte
            const topValue = topByte.pop()!
            if (topByte.length > 0) {
                topByte = bubblesort(topByte)
            } else {
                bytestack.pointers.pop()
            }
            return { topValue, newBytestack: bytestack }
        }
        default: return removeTopValueFromBytestack(topByteOrMetabyte as Bytestack)
    }
}

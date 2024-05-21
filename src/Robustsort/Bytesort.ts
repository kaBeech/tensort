// Note: this is valid TypeScript, but really it's pseudocode to guide me in 
// writing the Haskell version

// Notably, some of the subsidiary sorting 
// functions (like bubblesort) are cheated using regular sort() methods that 
// will function equivalently in code without errors, but do not provide the 
// same benefits to robustness that the real methods do. These real subsidiary 
// sorting functions will of course be defined in the Haskell version of the 
// code

// Also the Haskell version will likely be more optimized.

type Bit = number

type Byte = Bit[]

interface Bytestore {
    register: Reference[],
    data: Byte[],
}

type Metabyte = Bytestore | Metabytestore

interface Metabytestore {
    register: Reference[],
    data: Metabyte[],
}

interface Reference {
    address: number,
    topBit: Bit
}

type Bytestack = Metabyte

// type Stackmap = Metabytestore

// Dummy function in place of a true bubblesort
const bubblesort = (a: number[]) => a.sort()

// Dummy function in place of a true bubblesort
const bubblesortRefs = (refs: Reference[]) => refs.sort((a, b) => a.topBit - b.topBit)

const bytesort4Bit = (a: number[]) => bytesort(a, 4)

const bytesort = (bits: number[], bytesize: number): number[] => {
    bits = randomizeArray(bits)
    const bytes = convertRawBitsToBytes(bits, bytesize)
    let bytestacks = getBytestacksFromBytes(bytes)
    while (bytestacks.length > bytesize) {
        bytestacks = reduceBytestacks(bytestacks, 4)
    }
    return getSortedArrayFromBytestacks(bytestacks)
}

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
        bytestacks.push(getBytestoreFromBytes(bytes.slice(0, 4)))
        bytes = bytes.slice(4)
    }
    return bytestacks
}

const getBytestoreFromBytes = (bytes: Byte[]): Bytestore => {
    const register: Reference[] = []
    let i = 0
    for (const byte of bytes) {
        const topBit = byte[byte.length - 1]
        const ref = { address: i, topBit }
        register.push(ref)
        i++
    }
    return { register, data: bytes }
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

// Returns a Bytestack with the collated References from the Metabytes as the 
// Register and the original Metabytes as the data
const createBytestack = (metabytes: Metabyte[]): Bytestack => {
    const refs = getRefsFromMetabytes(metabytes)
    const register = bubblesortRefs(refs)
    // I'm pretty sure this code I commented out is no longer needed
    // let i = 0
    // for (const ref of refsSorted) {
    //     const newRef = { address: i, topBit }
    //     register.push(newRef)
    //     i++
    // }
    return { register, data: metabytes }
}

// For each Metabyte, produces a Reference by combining the top bit of the 
// Metabyte with an index value for its address
const getRefsFromMetabytes = (metabytes: Metabyte[]): Reference[] => {
    const refs: Reference[] = []
    let i = 0
    for (const metabyte of metabytes) {
        refs.push({ address: i, topBit: getTopValueFromBytestack(metabyte)! })
    }
    return refs
}

const getTopValueFromBytestack = (bytestack: Bytestack): number | null => {
    const topRef = bytestack.register[bytestack.register.length - 1]
    switch (typeof topRef) {
        case "undefined": {
            return null
        }
        default: return topRef.topBit
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
        if (metabyte.register.length > 0) {
            allEmpty = false
            break
        }
    }
    return allEmpty
}

const getNextElementFromBytestacks = (bytestacks: Bytestack[]): { nextElement: number, newBytestacks: Bytestack[] } => {
    const refs = getRefsFromMetabytes(bytestacks)
    const sortedPackages = bubblesortRefs(refs)
    const topBytestackIndex = getTopRef(sortedPackages).address
    const topBytestack: Bytestack = bytestacks[topBytestackIndex]
    const result = removeTopValueFromBytestack(topBytestack)
    const topValue: number = result.topValue
    const newBytestack: Bytestack[] = [result.newBytestack]
    const newBytestacks: Bytestack[] = bytestacks.slice(0, topBytestackIndex).concat(newBytestack, bytestacks.slice(topBytestackIndex + 1))
    return { nextElement: topValue, newBytestacks }
}

const getTopRef = (refs: Reference[]): Reference => {
    while (refs.length > 0) {
        let ref = refs.pop()!
        if (typeof ref.topBit == "number") {
            return ref
        }
    }
    throw Error("No package in input contains a numerical value")
}

const removeTopValueFromBytestack = (bytestack: Bytestack): { topValue: number, newBytestack: Bytestack } => {
    const topRef = bytestack.register[bytestack.register.length - 1]
    const topByteOrMetabyte: Byte | Metabyte = bytestack.data[topRef.address]
    switch (topByteOrMetabyte[0]) {
        // This case should be unnecessary now
        // case "undefined": {
        //     bytestack.register.pop()
        //     return removeTopValueFromBytestack(bytestack)
        // }
        case "number": {
            let topByte = topByteOrMetabyte as Byte
            const topValue = topByte.pop()!
            if (topByte.length > 0) {
                // Actually, this extra bubblesort step isn't necessary for COE 
                // Bytesort. Save it for Robustsort
                // topByte = bubblesort(topByte)
                topRef.topBit = topByte[topByte.length - 1]
            } else {
                bytestack.register.pop()
            }
            const newRegister: Reference[] = bubblesortRefs(bytestack.register)
            const bytes = bytestack.data as Byte[]
            const newBytestore: Bytestore = { register: newRegister, data: bytes }
            return { topValue, newBytestack: newBytestore }
        }
        default: {
            const result = removeTopValueFromBytestack(topByteOrMetabyte as Bytestack)
            const topValue = result.topValue
            if (result.newBytestack.register.length > 0) {
                topRef.topBit = getTopValueFromBytestack(result.newBytestack)!
            } else {
                bytestack.register.pop()
            }
            const newRegister: Reference[] = bubblesortRefs(bytestack.register)
            const metabytes = bytestack.data as Metabyte[]
            const newMetabytestore: Metabytestore = { register: newRegister, data: metabytes }
            return { topValue, newBytestack: newMetabytestore }
        }
    }
}

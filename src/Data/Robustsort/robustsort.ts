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
    topBit: Bit,
}

type Bytestack = Metabyte

// type Stackmap = Metabytestore

// Dummy function in place of a true bubblesort
const bubblesort = (a: number[]) => a.sort()

// Dummy function in place of a true bubblesort
const bubblesortRefs = (refs: Reference[]) => refs.sort((a, b) => a.topBit - b.topBit)

// Dummy function in place of a true reverseExchangesort
const reverseExchangesort = (a: number[]) => a.sort()

// Dummy function in place of a true reverseExchangesort
const reverseExchangesortRefs = (refs: Reference[]) => refs.sort((a, b) => a.topBit - b.topBit)

// Dummy function in place of a true bogosort
const bogosort = (a: number[]) => a.sort()

// Dummy function in place of a true bogosort
const bogosortRefs = (refs: Reference[]) => refs.sort((a, b) => a.topBit - b.topBit)

// Dummy function in place of a true permutationsort
const permutationsort = (a: number[]) => a.sort()

// Dummy function in place of a true permutationsort
const permutationsortRefs = (refs: Reference[]) => refs.sort((a, b) => a.topBit - b.topBit)

// Dummy function for verifying that two Bytes or Bytestacks contain exactly 
// the same values. I know that's not what this function does in TypeScript; 
// this is for demonstration purposes only. In the Haskell version all the data 
// will be Ords of Ints so I think we can just use a comparison operator there 
const areStacksEqual = (stack1, stack2) => {
    return stack1 === stack2
}

// Similar disclaimer as above for areStacksEqual(), except this one does 
// actually work if the inputs are arrays of integers
const areTopsOfStacksEqual = (stack1, stack2) => {
    return stack1[stack1.length - 1] === stack2[stack2.length - 1]
}

const magicsort = (a: number[]): number[] => {
    let permutationsorted = permutationsort(a)
    let bogosorted = bogosort(a)
    while (!areStacksEqual(permutationsorted, bogosorted)) {
        permutationsorted = permutationsort(a)
        bogosorted = bogosort(a)
    }
    return bogosorted
}

const magicsortRefs = (refs: Reference[]): Reference[] => {
    const permutationsorted = permutationsortRefs(refs)
    let bogosorted = bogosortRefs(refs)
    while (!areStacksEqual(permutationsorted, bogosorted)) {
        bogosorted = bogosortRefs(refs)
    }
    return bogosorted
}

const supersort = (a: number[]): number[] => {
    const bubblesorted = bubblesort(a)
    const reverseExchangesorted = reverseExchangesort(a)
    if (areStacksEqual(bubblesorted, reverseExchangesorted)) {
        return bubblesorted
    } else {
        const magicsorted = magicsort(a)
        if (
            areTopsOfStacksEqual(magicsorted, bubblesorted) ||
            areTopsOfStacksEqual(magicsorted, reverseExchangesorted)
        ) {
            return magicsorted
        } else if (areTopsOfStacksEqual(bubblesorted, reverseExchangesorted)) {
            return bubblesorted
        } else {
            return magicsorted
        }
    }
}

const supersortRefs = (refs: Reference[]): Reference[] => {
    const bubblesorted = bubblesortRefs(refs)
    const reverseExchangesorted = reverseExchangesortRefs(refs)
    if (areStacksEqual(bubblesorted, reverseExchangesorted)) {
        return bubblesorted
    } else {
        const magicsorted = magicsortRefs(refs)
        if (
            areTopsOfStacksEqual(magicsorted, bubblesorted) ||
            areTopsOfStacksEqual(magicsorted, reverseExchangesorted)
        ) {
            return magicsorted
        } else if (areTopsOfStacksEqual(bubblesorted, reverseExchangesorted)) {
            return bubblesorted
        } else {
            return magicsorted
        }
    }
}

export default (bits: number[]): number[] => {
    const bytesize = 3
    bits = randomizeArray(bits)
    const bytes = convertRawBitsToBytes(bits, bytesize)
    let bytestacks = getBytestacksFromBytes(bytes, bytesize)
    while (bytestacks.length > bytesize) {
        bytestacks = reduceBytestacks(bytestacks, bytesize)
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
        byte = supersort(byte)
        bytes.push(byte)
        bits = bits.slice(bytesize)
    }
    return bytes
}

const getBytestacksFromBytes = (bytes: Byte[], bytesize: number): Bytestack[] => {
    const bytestacks: Bytestack[] = []
    while (bytes.length > 0) {
        bytestacks.push(getBytestoreFromBytes(bytes.slice(0, bytesize)))
        bytes = bytes.slice(bytesize)
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
    const register = supersortRefs(refs)
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
    const sortedRefs = supersortRefs(refs)
    const topBytestackIndex = getTopRef(sortedRefs).address
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
    throw Error("No ref in input contains a numerical value")
}

const removeTopValueFromBytestack = (bytestack: Bytestack): { topValue: number, newBytestack: Bytestack } => {
    const topRef = bytestack.register[bytestack.register.length - 1]
    const topByteOrMetabyte: Byte | Metabyte = bytestack.data[topRef.address]
    switch (topByteOrMetabyte[0]) {
        case "number": {
            let topByte = topByteOrMetabyte as Byte
            const topValue = topByte.pop()!
            if (topByte.length > 0) {
                topByte = supersort(topByte)
                topRef.topBit = topByte[topByte.length - 1]
            } else {
                bytestack.register.pop()
            }
            const newRegister: Reference[] = supersortRefs(bytestack.register)
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
            const newRegister: Reference[] = supersortRefs(bytestack.register)
            const metabytes = bytestack.data as Metabyte[]
            const newMetabytestore: Metabytestore = { register: newRegister, data: metabytes }
            return { topValue, newBytestack: newMetabytestore }
        }
    }
}

import { greaterThan } from "./compare.ts";

export default (array: number[]) => {
  array = array.reduceRight((accumulator) => {
    for (let i = 0; i < accumulator.length - 1; i++) {
      if (greaterThan(accumulator[i], accumulator[i + 1])) {
        [accumulator[i], accumulator[i + 1]] = [accumulator[i + 1], accumulator[i]];
      }
    }
    return accumulator;
  }
    , array);
  return array;
}

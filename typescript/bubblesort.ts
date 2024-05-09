export const bubblesort = (array: number[]) => {
  let counter = 0
  array = array.reduceRight((accumulator) => {
    for (let i = 0; i < accumulator.length; i++) {
      if (accumulator[i] > accumulator[i + 1]) {
        [accumulator[i], accumulator[i + 1]] = [accumulator[i + 1], accumulator[i]];
      }
      counter++;
    }
    return accumulator;
  }
    , array);
  console.log(counter);
  return array;
}

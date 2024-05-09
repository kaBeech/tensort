import { bubblesort } from "./bubblesort.ts";

export function add(a: number, b: number): number {
  return a + b;
}

let unsortedArray = [5, 3, 1, 4, 6, 2, 0, 7, 9, 8];

// Learn more at https://deno.land/manual/examples/module_metadata#concepts
if (import.meta.main) {
  console.log("Add 2 + 3 =", add(2, 3));
  console.log(bubblesort(unsortedArray));
}

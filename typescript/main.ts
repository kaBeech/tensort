import bubblesort from "./bubblesort.ts";
import { setRandomComparitorPercentage } from "./globalState.ts";
import unsortedArray from "./unsortedArray.ts";

console.log(bubblesort(unsortedArray()));

setRandomComparitorPercentage(50);

console.log(bubblesort(unsortedArray()));

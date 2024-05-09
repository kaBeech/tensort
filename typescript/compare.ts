import { getRandomComparitorPercentage } from "./globalState.ts";

export const trinaryCompare = (x: number, y: number) => {
  if (getRandomComparitorPercentage() > Math.floor(Math.random() * 100) + 1) {
    return Math.floor(Math.random() * 3) - 1;
  } else {
    switch (true) {
      case x > y:
        return 1;
      case x < y:
        return -1;
      default:
        return 0;
    }
  }
}

export const lessThan = (x: number, y: number) => trinaryCompare(x, y) < 0;

export const greaterThan = (x: number, y: number) => trinaryCompare(x, y) > 0;

export const lessThanOrEqualTo = (x: number, y: number) => trinaryCompare(x, y) <= 0;

export const greaterThanOrEqualTo = (x: number, y: number) => trinaryCompare(x, y) >= 0;

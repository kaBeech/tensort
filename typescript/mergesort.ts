export default (array: number[]): number[] => {
  if (array.length <= 1) {
    return array;
  } else {
    return array;
  }
}

type mergeBranch = number[] | mergeBranch[];

const constructMergeTree = (array: number[]) => {
  const midpoint = Math.floor(array.length / 2);
  let leftArray = array.slice(0, midpoint)
  let rightArray = array.slice(midpoint);
  if (leftArray.length > 1) {
    leftArray = constructMergeTree(leftArray);
  }
  if (rightArray.length > 1) {
    rightArray = constructMergeTree(rightArray);
  }
  return [...leftArray, ...rightArray];
}

const merge = (left: number[], right: number[]): number[] => {
  if (left.length === 0) {
    return right;
  } else if (right.length === 0) {
    return left;
  } else return left[0] < right[0]
    ? [left[0], ...merge(left.slice(1), right)]
    : [right[0], ...merge(left, right.slice(1))];
}

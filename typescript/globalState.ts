let randomComparitorPercentage = 0;

export const getRandomComparitorPercentage = () => randomComparitorPercentage;

export const setComparitorPercentage = (percentage: number) => {
  randomComparitorPercentage = percentage;
};

let stuckComparitorPercentage = 0;

export const getStuckComparitorPercentage = () => stuckComparitorPercentage;

export const setStuckComparitorPercentage = (percentage: number) => {
  stuckComparitorPercentage = percentage;
};

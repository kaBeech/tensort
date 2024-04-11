# Robust Sort

The goal of this project is to explore what a sorting algorithm that 
prioritizes robustness over efficiency would look like

## Inspiration

- [Beyond Efficiency](https://www.cs.unm.edu/~ackley/be-201301131528.pdf) by David H. Ackley
- Future of Coding's [podcast episode](https://futureofcoding.org/episodes/070) on the same paper

## Strategy

1. Implement one or more Robust Sort prototypes

2. Test the prototypes for error rate and efficiency as compared with other 
    sorting algorithms, particularly Quick Sort

## Implementation

### Idea One (Naive)

1. Start with an array of integers, calling it `unsorted_array`

2. Clone `unsorted_array`, calling the clone `sorted_array_1`

3. Run Quick Sort on `sorted_array_1`

4. Clone `unsorted_array` again, calling the clone `sorted_array_2`

5. Run Quick Sort on `sorted_array_2`

6. Compare each element in `sorted_array_1` and `sorted_array_2`

7. If Step 6 finds any elements that are not equal, repeat Steps 4-6, then go 
    to Step 8. Otherwise, go to Step 9

8. If Step 6 finds any elements that are not equal, repeat Steps 2, 3 and 6, 
    then go to Step 7. Otherwise, go to Step 9

9. Return `sorted_array_2`

#### Notes

- My intuition is that this algorithm will be less robust with very short array 
    lengths. Maybe add a condition where if the array is short (say, less than 
    8 to 10 elements), the algorithm will run a very naive but robust sorting 
    algorithm instead of Quick Sort

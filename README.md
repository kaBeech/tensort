# RobustSort

The goal of this project is to explore what a sorting algorithm that 
prioritizes robustness would look like

DISCLAIMER: This project is still under construction. The basic ideas are all there, but need to be 
tied together and made to look pretty =)

## Inspiration

- [Beyond Efficiency](https://www.cs.unm.edu/~ackley/be-201301131528.pdf) by David H. Ackley
- Future of Coding's [podcast episode](https://futureofcoding.org/episodes/070) on the same paper

## Project structure

- `src/` contains the RobustSort library
- `app/` contains the suite for comparing different sorting algorithms in terms of robustness and time efficiency

## Implementation Overview

NOTE! This description is somewhat out of date. Please see the code for details.
The code for `Bytesort.hs` and its dependencies\ is fairly well documented and is 
*almost* completely valid Haskell. The `robustsort.ts` file is valid TypeScript, 
but really it's pseudocode to guide me in writing the Haskell version. It's not 
as well documented as the Haskell code, but is likely easy enough to follow
once you understand bytesort

Unfortunately I also have to get paid and can't spend *all* my time writing 
Haskell, but I intend to have everything functioning soon =)

...

Please note that we will discuss a few algorithms that I've either made up or 
am just not familiar with by other names. If any of these algorithms have 
previously been named, please let me know. Prior to this project I really 
only had a rudimentary understanding of insertionsort, quicksort, mergesort, 
and bogosort, so it's entirely possible that I've reinvented a few things 
that already exist

It also may be helpful to note that this project was undertaken in an 
endeavor to come up with a solution naively, for the practice, before 
researching other algorithms built to tackle the same problem. I did very 
briefly check out [Demon Horde Sort](https://www.youtube.com/watch?v=helScS3coAE&t=260s), 
but only enough to verify that it is different from this algorithm. For the 
record, if you do actually want a real, professional approach to robust 
sorting, Demon Horde Sort is the place to look

The algorithms used here that I have made up or renamed are, in order of 
appearance, Bytesort, Robustsort, Permutaionsort, and Magicsort. Get ready!

### Bytesort

NOTE! This description is somewhat out of date. Please see `Bytesort.hs` for 
details

Bytesort is my attempt to write the most robust O(n log n) sorting algorithm 
possible while avoiding anything that Ackley might consider a "cheap hack." 
My hope is that it will be, if not competitive with Bubblesort in robustness, 
it at least is a major improvement over Quicksort and Mergesort. 

Again, I'm not well-studied in sorting algorithms, so this may well be known 
already under another name. After settling on this algorithm, I looked into 
several other sorting algorithms for comparison and found a few that I think 
are similar - significantly Blocksort, Bucketsort, and Patiencesort. If you are 
familiar with these algorithms, you may recognize that they each have a 
structure that aids in understanding them.

Bytesort uses an underlying structure as well. We will discuss this structure 
before going over the algorithm's actual steps. If this doesn't make sense yet,
fear not!

<!-- [image1] -->

In Bytesort, the smallest unit of information is a Bit. Each bit stores one 
element of the list to be sorted. A group of Bits is known as a Byte. 

A Byte is an array of Bits. The maximum length of a Byte is set according to an 
argument passsed to Bytesort. In practice, almost all Bytes will be of maximum 
length until the final steps of Bytesort. In this explanation we will use a 
4-Bit Bytesort as our example. Several Bytes are grouped together in a Bytestore.

A Bytestore is an array with two elements. The second element is an array of 
Bytes known as Memory. The length of this Memory array is equal to the Bytesize.
The first element is a Register of Records, each of which has an Address 
pointing to a Byte in memory and a copy of the TopBit in the referenced Byte. 
These Records are arranged in the order that the Bytes are sorted (this will be 
clarified soon). Several Bytestores are grouped together in a Metabyte.

A Metabyte is an array with two elements. The second element is an array of 
either Bytestores or other Metabytes. The length of this array is equal to the 
Bytesize. Similar to as in a Bytestore, the first 
element in a Metabyte is an array of integer pointers representing the indices 
of the Bytestores/Metabytes appearing in the second element.

A Bytestack is a top-level Metabyte along with all the Bits, Bytes, Bytestores, 
and Metabytes it contains. Once the Metabytes are fully built, the total number 
of Bytestacks will equal the Bytesize, but before that point there will be many 
more Bytestacks.

Now, on to the algorithm!

The first step in Bytesort is to randomize the input list. I'll explain why we 
do this in more detail later - for now just know that it's easier for Bytesort 
to make mistakes when the list is already nearly sorted.

1 - Randomize the input list of elements (Bits)

2 - Assemble Bytes by using Bubblesort on the Bits. After this, we will do no 
more Write operations on the Bits until the final steps. Instead, we will make 
copies of the Bits and sort the copies alongside their pointers.

3 - Assemble Bytestores by grouping Bytes together (setting them as the Bytestore's 
second element), making Records from their top bits, running Bubblesort on the 
Records, and then recording the Pointers from the Records (after being 
sorted) as the Bytestore's first element.

4 - Assemble Metabytes by grouping Bytestores together (setting them as the 
Metabyte's second element), making Records from their top Bits, running 
Bubblesort on the Records, and then recording the Pointers from the Records 
(after being sorted) as the Bytestore's first element.

5 - Assemble Metabytes by grouping existing Metabytes together, using a similar 
process as in Steps 3 and 4.

6 - Repeat Step 5 until the number of Bytestacks equals the Bytesize

7 - Make Records from the top Bits on each Bytestack and use Bubblesort on the 
Records.

8 - Remove the top Bit from the top Bytestack, add it to the final sorted list, 
and rebalance the Bytestack

9 - Repeat Step 8 until all Bytestacks are empty
...

<!-- It seems to me that ... is more time  -->
<!-- efficient than... , though it is less space-efficient. It also seems more  -->
<!-- efficient than ... . It's worth noting though that I'm not 100%  -->
<!-- sure about any of this - I used intuition more than anything to make these  -->
<!-- determinations. It's entirely possible that this method could be improved  -->
<!-- (perhaps by using an [in-place data structure] and some clever swaperations).  -->
<!-- As always, constructive feedback is welcome! -->

...

Now that we know all the steps, it's easier to see why we randomize the list. 
as the beginning step. This way, if the list is already nearly 
sorted, values close to each other don't get stuck under each other in their 
Byte. Ideally, we want the top Bits from all Bytestacks to be close to 
each other. Say for example, the first three elements in a 1,000,000-element 
list are 121, 122, 123, and 124. If we don't randomize the list, these 3 
elements get grouped together in the first byte. That's all well and good if 
everything performs as expected, but if something unexpected happens 
during an operation where we intend to add 124 to the final list  
and we add a different element instead, three of the best-case elements to have
mistakenly added (121, 122, and 123) are impossible to have been selected.

...

Alright! Now we have a simple sorting algorithm absent of cheap hacks that is 
both relatively fast and relatively robust. I'm pretty happy with that!

<!-- [image2] -->

Now for some cheap hacks!

### Robustsort

NOTE! This description is somewhat out of date. Please see `robustsort.ts` for 
current pseudocode

In Beyond Efficiency, Ackley augmented Mergesort and Quicksort with what he 
called "cheap hacks" in order to give them a boost in robustness to get them to 
compare with Bubblesort. This amounted to adding a quarum system to the 
unpredictable comparison operator and choosing the most-agreed-apon answer. 

I agree that adding a quorum for the unpredictable comparison operator is a bit 
of a cheap hack, or at least a post-hoc solution to a known problem. Instead of 
retrying a specific component again because we know it to be unpredictable, 
let's build redundancy into the system at the (sub-)algorithmic level. A simple 
way to do this is by asking different components the same question and see if 
they agree.

Robustsort is my attempt to make the most robust sorting algorithm possible 
utilizing some solution-checking on the (sub-)algorithmic level while still:

    - Keeping runtime somewhat reasonable

    - Never re-running a sub-algorithm that is expected to act deterministicly 
      looking for a non-deterministic result (i.e. expect that if a components 
      gives a wrong answer, running it again won't somehow yield a right answer)

    - Using a minimal number of different sub-algorithms (i.e. doesn't just 
      use every O(n log n) sorting algorithm I can think of and compare all 
      their results)

With those ground rules in place, let's get to Robustsort!

Once we have Bytesort in our toolbox, the road to Robustsort is pretty simple. 
At its core, Robustsort is a 3-bit Bytesort with some extra parallelism baked 
in. Why 3-bit? It's because of the power of threes.

In Bytesort, we repeatedly run Bubblesort to sort our pointers. In Robustsort, 
instead of simply taking the output from Bubblesort, we compare it to another 
algorithm to see if they match.

We use a 4-bit Bytesort as the standard above because there's something 
magical that happens around these numbers. Robust sorting algorithms tend to be 
slow. Bubblesort, for example, has an average time efficiency of O(n^2), 
compared with Quicksort and Mergesort, which both have an average of (n log n).

Here's the trick though: with small numbers the difference between these values 
is minimal. For example, when n=4, Mergesort will make 6 comparisons, while 
Bubblesort will make 12. A Bit Array with 4 members is both small enough to run 
the Bubblesort quickly and large enough to allow multiple opportunities for a 
mistake to be corrected. Since we don't have much built-in parallelism, we want 
to weight more heavily on the side of making more checks.

In Robustsort, however, we do have built-in parallelism, so we can afford to 
make less checks during this step. We choose a byte size of 9 because a Bit 
Array with 3 elements has some special properties. For one thing, sorting at 
this length greatly reduces the time it takes to run our slow-but-robust 
algorithms. For example, at this size, Bubblesort will make only 6 comparisons. 
Mergesort still makes 6 as well.

In addition, when making a mistake while sorting 3 elements, the mistake is 
most likely to displace an element by only 1 position (or 2 positions at the
maximum), no matter which algorithm is used.

This is all to say that using a 3-bit byte size allows us to have our pick of 
algorithms to compare with!

When choosing our comparison algorithm, we want something with logic 
substancially different than Bubblesort, for the sake of robustness. We do, 
however, want something similar to Bubblesort in that it compares our elements 
multiple times. And, as mentioned above, the element that is most important to 
our sorting is the top (biggest) element, by a large degree.

With these priorities in mind, the comparison algorithm we choose shall be a 
reverse Exchangesort.

...

...the most robust, most correct, and all-around best algorithm of all time: 
Bogosort

...

Well now that's pretty cool! But I wonder... can we make this more robust, if 
we relax the rules just a little more?

<!-- (image3) -->

Of course we can! And we will. To do so, we will simply replace ... with 
another newly-named sorting algorithm: Magicsort!

### Magicsort

...

### Comparing it all

Now let's take a look at how everything compares. Here is a graph showing the 
benchmarking results in both in both robustness and time efficiency for 
Quicksort, Mergesort, Bytesort, Rule-Abiding Robustsort, Robustsort With Magic, 
and Bubblesort:

...


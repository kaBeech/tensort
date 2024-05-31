# Tensort

Tensort is a tensor-based sorting algorithm that is tunable to adjust to 
the priorities of the task at hand.

This project started as an exploration of what a sorting algorithm that 
prioritizes robustness would look like. As such it also describes and provides
implementations of Robustsort, a group of Tensort variants designed to 
prioritize Robustness in conditions defined in David H. Ackley's
[Beyond Efficiency](https://www.cs.unm.edu/~ackley/be-201301131528.pdf).

Note: This project is still under construction. The Library is 
functional but I have yet to add much documentation and benchmarking.
There's likely a lot of room for improvement in the code as well.

## Table of Contents

- [Introduction](#introduction)
  - [Inspiration](#inspiration)
  - [Why?](#why)
  - [But why would anyone care about this in the first place?](#but-why-would-anyone-care-about-this-in-the-first-place)
  - [Why Haskell?](#why-haskell)
- [Project structure](#project-structure)
- [Algorithms overview](#algorithms-overview)
  - [Tensort](#tensort-1)
    - [Preface](#preface)
    - [Structure](#structure)
    - [Algorithm](#algorithm)
    - [What are the benefits?](#what-are-the-benefits)
    - [Logarithmic Tensort](#logarithmic-tensort)
  - [Robustsort](#robustsort)
    - [Preface](#preface-1)
    - [Overview](#overview)
    - [Examining Bubblesort](#examining-bubblesort)
    - [Exchangesort](#exchangesort)
    - [Introducing Supersort](#introducing-supersort)
    - [Permutationsort](#permutationsort)
    - [Supersort Adjudication](#supersort-adjudication)
  - [Magicsort](#magicsort)
    - [Supersort adjudication with Magic](#supersort-adjudication-with-magic)
  - [A note on Robustsort and Bogosort](#a-note-on-robustsort-and-bogosort)
- [Comparing it all](#comparing-it-all)
- [Library](#library)

## Introduction

### Inspiration

  - [Beyond Efficiency](https://www.cs.unm.edu/~ackley/be-201301131528.pdf) by 
  David H. Ackley
    
  - Future of Coding's 
  [podcast episode](https://futureofcoding.org/episodes/070) on the same paper

### Why?

Because near the end of [that podcast episode](https://futureofcoding.org/episodes/070), 
[Ivan Reese](https://github.com/ivanreese) said "Why are we comparing Bubblesort 
versus Quicksort and Mergesort? Well, because no one's made Robustsort yet."

And I thought, "Why not?"

### But why would anyone care about this in the first place?

Well, a tunable sorting algorithm is a really cool thing to have!

This can have many different uses, one of which is prioritizing robustness.

[Ackley](https://www.cs.unm.edu/~ackley/be-201301131528.pdf) has some really 
compelling things to say about why prioritizing robustness is important and 
useful, and I'd highly recommend you read that paper!

Or listen to [this podcast](https://futureofcoding.org/episodes/070)!

If you want my elevator pitch, it's because we eventually want to build things
like [Dyson Spheres](https://en.wikipedia.org/wiki/Dyson_sphere). Doing so will 
likely involve massively distributed systems being constantly pelted by 
radiation. In circumstances like that, robustnesss is key.

Another other example I like to consider is artificial cognition. When working 
in a non-determinative system (or a system so complex as to be considered
non-determinative), it can be helpful to have systems in place to make sure 
that the answer we come to is really valid.

Incidentally, while I was preparing for this project, we experienced 
[the strongest solar storm to reach Earth in 2 decades](https://science.nasa.gov/science-research/heliophysics/how-nasa-tracked-the-most-intense-solar-storm-in-decades/). 
I don't know for certain whether the solar activity caused any computer errors, 
but we had some anomalies at work and certainly joked about them being caused by
the Sun.

Also during the same period, 
[one of the Internet's root-servers glitched out for unexplained reasons](https://arstechnica.com/security/2024/05/dns-glitch-that-threatened-internet-stability-fixed-cause-remains-unclear/).

As Ackley mentions, as a culture we have tended to prioritize correctness and 
efficiency to the exclusion of robustness. The rate of our technological 
progression precludes us from continuing to do so.

### Why Haskell?

[Obviously](https://www.youtube.com/shorts/LGZKXZQeEBg).

## Project structure

- `src/` contains the Tensort library
    
- `app/` contains the suite for comparing different sorting algorithms in terms of robustness and time efficiency

## Algorithms overview

This README assumes some general knowledge of basic sorting algoritms. If you
would like a refresher, I recommend 
[this video](https://www.youtube.com/watch?v=kgBjXUE_Nwc) which touches on 
Bubblesort, MergeSort, and Bogosort, and 
[this video](https://www.youtube.com/watch?v=XE4VP_8Y0BU) which discusses
Quicksort.

It also assumes you've read 
[Beyond Efficiency](https://www.cs.unm.edu/~ackley/be-201301131528.pdf) by 
David H. Ackley. Go read it!

Please note that we will discuss a few algorithms that I've either made up or 
am just not familiar with by other names. If any of these algorithms have 
previously been named, please let me know. Prior to this project I really 
only had a rudimentary understanding of Insertionsort, Quicksort, Mergesort,
Bubblesort and Bogosort, so it's entirely possible that I've reinvented a few 
things that already exist.

It also may be helpful to note that this project was originally undertaken in
an endeavor to come up with a solution naively, for the practice, before 
researching other algorithms built to tackle the same problem. I did very 
briefly check out Ackley's 
[Demon Horde Sort](https://www.youtube.com/watch?v=helScS3coAE&t=260s), 
but only enough (about 5 seconds of that video) to verify that it is different 
from this algorithm. I've been purposefully avoiding learning much about Demon 
Horde Sort before publishing v1.0.0.0 of this package, but Ackley is way 
smarter than me so if you do actually want a real, professional approach to 
robust sorting, Demon Horde Sort is likely the place to look.

The algorithms used here that I have made up or renamed are, in order of 
introduction, Tensort, Robustsort, Permutationsort, and Magicsort. Get ready!

### Tensort

#### Preface

Tensort is my attempt to write the most robust O(n log n) sorting algorithm 
possible while avoiding anything that Ackley might consider a "cheap hack." 
My hope is that it will be, if not competitive with Bubblesort in robustness, 
at least a major improvement over Quicksort and Mergesort. 

Again, I'm not well-studied in sorting algorithms, so this may well be known 
already under another name. After settling on this algorithm, I looked into 
several other sorting algorithms for comparison and found a few that I think 
are similar - significantly Blocksort, Bucketsort, and Patiencesort. If you are 
familiar with these algorithms, you may recognize that they each have a 
structure that aids in understanding them.

Tensort uses an underlying structure as well. We will discuss this structure 
before going over the algorithm's actual steps. If this doesn't make sense yet,
fear not!

<!-- [image1] -->

#### Structure

  - Bit <- Element of the list to be sorted
    
  - Byte <- List of Bits

  - Bytesize <- Maximum length of a Byte
    
  - Tensor <- Tuple of a Register list and a Memory list
    
  - Memory <- List of Bytes or Tensors contained in the current Tensor.
    
  - Register <- List of Records referencing each Byte or Tensor in Memory
    
  - Record <- Tuple of the Address and the TopBit of the referenced Byte or Tensor
    
  - Address <- Pointer to a Byte or Tensor in Memory
    
  - TopBit <- Value of the Bit at the top of the stack in a Byte or Tensor

  - TensorStack <- A top-level Tensor along with all the Bits, Bytes, and Tensors it contains
    
  - SubAlgorithm <- The sorting sub-algorithm used at various stages

In Tensort, the smallest unit of information is a Bit. Each Bit stores one 
element of the list to be sorted. A group of Bits is known as a Byte. 

A Byte is a list of Bits. The maximum length of a Byte is set according to an 
argument passed to Tensort. In practice, almost all Bytes will be of maximum 
length until the final steps of Tensort. Several Bytes are grouped together 
in a Tensor.

A Tensor is a tuple with two elements: Register and Memory.

Memory is the second element in a Tensor tuple. It is a list of Bytes or 
other Tensors. The length of this Memory list is equal to the Bytesize.

A Register is the first element in a Tensor tuple. It is a list of Records, 
each of which has an Address pointing to an element in its Tensor's Memory 
and a copy of the TopBit in the referenced element. These Records are arranged 
in the order that the elements of the Tensor's Memory are sorted (this will be 
clarified soon).

A TensorStack is a top-level Tensor along with all the Bits, Bytes, and 
Tensors it contains. Once the Tensors are fully built, the total number 
of TensorStacks will equal the Bytesize, but before that point there will 
be many more TensorStacks.

The sorting SubAlgorithm will be used any time we sort something within 
Tensort. The choice of this SubAlgorithm is very important. For reasons that 
will become clear soon, the SubAlgorithm for Standard Tensort will be 
Bubblesort, but the major part of Tensort's tunability is  the ability to 
substitute another sorting algorithm based on current priorities.

Now, on to the algorithm!

#### Algorithm

The first step in Tensort is to randomize the input list. I'll explain why we 
do this in more detail later - for now just know that it's easier for Tensort 
to make mistakes when the list is already nearly sorted.

  1. Randomize the input list of elements (Bits)

  2. Assemble Bytes by sorting the Bits using the SubAlgorithm. After this, we 
    will do no more write operations on the Bits until the final steps. Instead, we 
    will make copies of the Bits and sort the copies alongside their pointers.

  3. Assemble TensorStacks by creating Tensors from the Bytes. Tensors are 
    created by grouping Bytes together (setting them as the Tensor's 
    second element), making Records from their top bits, sorting the records, and 
    then recording the Pointers from the Records (after being sorted) as the 
    Tensor's first element.

  4. Reduce the number of TensorStacks by creating a new layer of Tensors from 
    the Tensors created in Step 3. These new Tensors are created by grouping 
    the first layer of Tensors together (setting them as the new Tensor's 
    second element), making Records from their top Bits, sorting the Records, and 
    then recording the Pointers from the Records 
    (after being sorted) as the Tensor's first element.

  5. Continue in the same manner as in Step 4 until the number of TensorStacks 
    equals the Bytesize

  6. Assemble a top Register by Making Records from the Top Bits on each 
    TensorStack and sort the Records.

  7. Remove the Top Bit from the top Byte in the top TensorStack and add it 
    to the final Sorted List. If the top Byte has more than one But in it stll, 
    Re-sort the Byte for good measure (technically this is 
    running the algorithm on different arguments - if anyone wants to me about 
    this I'll update this README)

  8. If the top Byte in the top TensorStack is empty, remove the Record that 
    points to it from its Tensor's Register. If the Tensor is empty, remove
    the Record that points to it from its Tensor's Register. Do this recursively 
    until the Tensor is not empty or the top of the TensorStack is reached. If the 
    entire TensorStack is empty of Bits, remove its Record from the top Register. If 
    all TensorStacks are empty of Bits, return the final Sorted List. Otherwise, 
    re-sort the top Register

  9. Otherwise (the top Byte (or a Tensor that contains it) is not empty), 
    update the top Byte's (or Tensor's) Record with its 
    new Top Bit and re-sort its Tensor's Register. Then jump up a level to 
    the Tensor that contains that Tensor and update the top Tensor's Record
    with its new Top Bit and re-sort its Register. Do this recursively until
    the whole TensorStack is rebalanced. Then update the TensorStack's Record in the 
    top Register with its new Top Bit and re-sort the top Register.

Now that we know all the steps, it's easier to see why we randomize the list
as the beginning step. This way, if the list is already nearly 
sorted, values close to each other don't get stuck under each other in their 
Byte. Ideally, we want the top Bits from all TensorStacks to be close to 
each other. Say for example, the first three elements in a 1,000,000-element 
list are 121, 122, 123, and 124. If we don't randomize the list, these 3 
elements get grouped together in the first byte. That's all well and good if 
everything performs as expected, but if something unexpected happens 
during an operation where we intend to add 124 to the final list  
and we add a different element instead, three of the best-case elements to have
mistakenly added (121, 122, and 123) are impossible to have been selected.

#### What are the benefits?

The core idea of Tensort is breaking the input into smaller pieces along an 
ever-expanding rank, and sorting the smaller pieces. Once we understand the
overall structure, we can design the SubAlgorithm (and Bytesize) to suit our 
needs.

Standard Tensort leverages the robustness of Bubblesort while reducing the time 
required by never Bubblesorting the entire input. 

We are able to do this because A) Bubblesort is really good at making sure the 
last element is in the final position of a list, and B) at each step of Tensort 
the only element we *really* care about is the last element in a given list 
(or to look at it another way, the TopBit of a given Tensor).

#### Logarithmic Tensort

When using standard Tensort (i.e. using Bubblesort as the SubAlgoritm), as the 
Bytesize approaches the square root of the number of elements in the 
input list, its time efficiency approaches O(n^2).

Standard Tensort is most time efficient when the Bytesize is close 
to the natural log of the number of elements in the input list. A logarithmic 
Bytesize is likely to be ideal for most use cases of standard Tensort.

Alright! Now we have a simple sorting algorithm absent of cheap hacks that is 
both relatively fast and relatively robust. I'm pretty happy with that!

<!-- [image2] -->

Now for some cheap hacks!

### Robustsort

#### Preface

In Beyond Efficiency, Ackley augmented Mergesort and Quicksort with what he 
called "cheap hacks" in order to give them a boost in robustness to get them to 
compare with Bubblesort. This amounted to adding a quorum system to the 
unpredictable comparison operator and choosing the most-agreed-upon answer. 

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
      on the same arguments looking for a non-deterministic result (i.e. expect 
      that if a components gives a wrong answer, running it again won't somehow 
      yield a right answer)

  - Using a minimal number of different sub-algorithms (i.e. doesn't just 
      use every O(n log n) sorting algorithm I can think of and compare all 
      their results)

With those ground rules in place, let's get to Robustsort!

#### Overview

Once we have Tensort in our toolbox, the road to Robustsort is pretty simple. 
Robustsort is a 3-bit Tensort with a custom SubAlgorithm that compares other 
sub-algorithms. For convenience, we will call this custom SubAlgorithm 
Supersort. We use a 3-bit Tensort here because there's something 
magical that happens around these numbers.

Robust sorting algorithms tend to be 
slow. Bubblesort, for example, has an average time efficiency of O(n^2), 
compared with Quicksort and Mergesort, which both have an average of (n log n).

Here's the trick though: with small numbers the difference between these values 
is minimal. For example, when n=4, Mergesort will make 6 comparisons, while 
Bubblesort will make 12. A Byte holding 4 Bites is both small enough to run 
the Bubblesort quickly and large enough to allow multiple opportunities for a 
mistake to be corrected. Since we don't as much built-in parallelism in 
Tensort, it can make sense to weight more heavily on the side of making more 
checks.

In Robustsort, however, we have parallelism built into the Supersort 
SubAlgorithm, so we can afford to make less checks during this step. 
We choose a Bytesize of 
3 because a list of
3 Bits has some special properties. For one thing, sorting at 
this length greatly reduces the time it takes to run our slow-but-robust 
algorithms. For example, at this size, Bubblesort will make only 6 comparisons. 
Mergesort still makes 6 as well.

In addition, when making a mistake while sorting 3 elements, the mistake 
will displace an element by only 1 or 2 positions at the, no matter which 
algorithm is used.

This is all to say that using a 3-bit byte size allows us to have our pick of 
algorithms to compare with!

Note: One might ask why we don't use a Bytesize of 2, since it would be even faster
and still have the same property of displacing an element by only 1 or 2
positions. Well, how many different algorithms can you use to sort 2 elements?
At this length, most algorithms function equivalently (in terms of the 
sub-operations performed) and in my mind running two such algorithms is 
equivalent to re-running a single algorithm (which violates the requirements 
of this project).

#### Examining Bubblesort

Before moving further, let's talk a little about Bubblesort, and why we're 
using it in our SubAlgorithm.

As a reminder, Bubblesort will make an average of 6 comparisons when sorting
a 3-element list.

We've said before that Bubblesort is likely to put the last element in the 
correct position. Let's examine this in the context of Bubblesorting a 
3-element list.

Our implementation of Bubblesort (which mirrors Ackley's) will perform three
iterations over a 3-element list. After the second iteration, if everything
goes as planned, the list will be sorted and the final iteration is an extra
verification step. Therefore, to simplify the analysis, we will consider
what happens with a faulty comparator during the final iteration, assuming the
list has been correctly sorted up to that point.

Given a Byte of [1,2,3], here are the chances of various outcomes from using a 
faulty comparator that gives a random result 10% of the time:

    81% <- [1,2,3] (correct - no swaps made)

    9% <- [2,1,3] (faulty first swap)

    9% <- [1,3,2] (faulty second swap)

    1% <- [2,3,1] (faulty first and second swap)

In these cases, 90% of the time the Top Bit will be in the correct position, 
and in the other cases it will be off by one position, and in no case will the 
Byte be reverse sorted.

#### Exchangesort

When choosing an algorithm to compare with Bubblesort, we want something with 
substantially different logic, for the sake of robustness. We do, 
however, want something similar to Bubblesort in that it compares our elements 
multiple times. And, as mentioned above, the element that is most important to 
our sorting is the top (biggest) element, by a large degree.

With these priorities in mind, the comparison algorithm we choose shall be 
Exchangesort. If you're not familiar with this algorithm, I'd recommend
checking out [this video](https://youtu.be/wqibJMG42Ik?feature=shared&t=143). 

The Exchangesort we use is notable in two ways. Firstly, it is a Reverse 
Exchangesort, as explained in that video.

Secondly, the algorithm as described in the video only compares selected element 
with elements that appear after (or before, as in Reverse Exchangesort) it in 
the list, swapping them if the compared element is larger. This functions 
similarly to an optimized Bubblesort where after the each round the last 
element compared that round is no longer compared in following rounds. Our 
implementation will compare the selected element with all other elements in the 
list, swapping them if the element that appears later is larger. Ackley 
uses an unoptimized Bubblesort in Beyond Efficiency, so I feel comfortable 
using this variation for our Exchangesort.

Exchangesort will also make an average of 6 comparisons when sorting a
3-element list.

As with Bubblesort, Exchangesort will perform three iterations over a 3-element
list, with the final iteration being redundant.

Given a Byte of [1,2,3], here are the chances of various outcomes from using a 
faulty comparator that gives a random result 10% of the time:

    81% <- [1,2,3] (correct - no swaps made)

    9% <- [2,1,3] (faulty first swap)

    9% <- [3,2,1] (faulty second swap)

    1% <- [3,1,2] (faulty first and second swap)

In these cases, 90% of the time the Top Bit will have the correct value. 
Notably there is a 9% chance that the Byte will be reverse sorted, but we will 
exploit this trait later on in the Supersort SubAlgorithm. Note also that the 
only possible outcomes shared between this example and the Bubblesort example
are the correct outcome and [2,1,3], which retains the TopBit with the correct 
value.

#### Introducing Supersort

Supersort is a SubAlgorithm that compares the results of two different
sorting algorithms, in our case Bubblesort and Exchangesort. If both 
algorithms agree on the result, that result is used. 

Looking at our analysis on Bubblesort and Exchangesort, we can 
approximate the chances of various outcomes when comparing the results of 
running these two algorithms in similar conditions:

    65.61% <- [1,2,3], [1,2,3] (Agree Correctly)

    7.29% <- [1,2,3], [2,1,3] (Disagree - TopBit agrees correctly)

    7.29% <- [1,2,3], [3,2,1] (Disagree Fully)

    7.29% <- [2,1,3], [1,2,3] (Disagree - TopBit agrees correctly)

    7.29% <- [1,3,2], [1,2,3] (Disagree Fully)

    0.81% <- [2,1,3], [2,1,3] (Agree Incorrectly - TopBit correct)

    0.81% <- [2,1,3], [3,2,1] (Disagree Fully)

    0.81% <- [1,3,2], [2,1,3] (Disagree Fully)

    0.81% <- [1,3,2], [3,2,1] (Disagree Fully)

    0.09% <- [2,1,3], [3,1,2] (Disagree Fully)

    0.09% <- [1,3,2], [3,1,2] (Disagree - TopBit agrees incorrectly)

    0.09% <- [2,3,1], [2,1,3] (Disagree Fully)
  
    0.09% <- [2,3,1], [3,2,1] (Disagree - TopBit agrees incorrectly)

    0.01% <- [2,3,1], [3,1,2] (Disagree Fully)

In total, that makes:

    65.61% <- Agree Correctly

    17.2% <- Disagree Fully

    14.58% <- Disagree - TopBit agrees correctly

    0.81% <- Agree Incorrectly - TopBit correct

    0.18% <- Disagree - TopBit agrees incorrectly

    [no outcome] <- Agree with TopBit incorrect

The first thing that might stand out is that around 34% of the time, these 
sub-algorithms will disagree with each other. What happens then?

Well, in that case we run a third sub-algorithm to compare the results with: 
Permutationsort.

#### Permutationsort

Permutationsort is a simple, brute-force sorting algorithm. As a first step we 
generate all the different ways the elements could possibly be arranged in the 
list. Then we loop over this list of permutations until we find one that is in 
the right order. We check if a permutation is in the right order by comparing
the first two elements, if they are in the right order comparing the next two
elements, and so on until we either find two elements that are out of order or
we confirm that the list is in order.

Permutationsort will also make an average of 7 comparisons when sorting a 
3-element list. This is slightly more than the other algorithms examined but
it's worth it because A) the spread of outcomes is favorable for our needs, and 
B) it uses logic that is completely different from Bubblesort and Exchangesort. 
Using different manners of reasoning to reach an agreed-upon answer greatly 
increases the robustness of the system.

Given a Byte of [1,2,3], here are the chances of various outcomes from using a
faulty comparator that gives a random result 10% of the time:

    ~68.67% <- [1,2,3] (correct)

    ~7.63% <- [2,1,3] (faulty first comparator)
  
    ~7.63% <- [3,1,2] (faulty first comparator)

    ~7.63% <- [1,3,2] (faulty second comparator)

    ~7.63% <- [2,3,1] (faulty second comparator)

    ~0.85% <- [3,2,1] (faulty first and second comparator)

In these cases, 76.6% of the time the Top Bit will be in the correct position. 
Notably the least likely outcome is a reverse-sorted Byte and the other 
possible incorrect outcomes are in even distribution with each other.

#### Supersort Adjudication

Supposing that our results from Bubblesort and Exchangesort disagree 
and we now have our result from Permutationsort, how do we choose which to
use?

First we check to see whether the result from Permutationsort agrees with
the results from either Bubblesort or Exchangesort. To keep things 
simple, let's just look at the raw chances that 
Permutationsort will agree on results with Bubblesort or Exchangesort.

Permutationsort and Bubblesort:

    ~55.62% <- [1,2,3] (Correct)

    ~0.69% <- [2,1,3] (Correct TopBit)

    ~0.69% <- [1,3,2] (Incorrect)

    ~0.08% <- [2,3,1] (Incorrect)

Permutationsort and Exchangesort:

    ~55.62% <- [1,2,3] (Correct)

    ~0.69% <- [2,1,3] (Correct TopBit)

    ~0.08% <- [3,1,2] (Incorrect)

    ~0.08% <- [3,2,1] (Reverse)

As we can see, it is very unlikely that Permutationsort will agree with
either Bubblesort or Exchangesort incorrectly. It is even less likely
that they will do so when the TopBit is incorrect. However, there are many 
cases in which they do not agree, so let's handle those.

If there is no agreed-upon result between these three algorithms, we will look 
at the top bit only.

First we check if the results from Bubblesort and Exchangesort agree on the 
TopBit. This is because the chance is very unlikely 
(0.18%) that they will agree on an incorrect TopBit. If they do agree, we use 
the result from Bubblesort (as it will not return a reverse-sorted list).

If they do not agree, we will check the TopBit results from Bubblesort and 
Permutationsort. This is because it is unlikely 
(~0.92%) that they will agree on an incorrect TopBit, and the chance of them 
incorrectly agreeing on the highest Bit as the TopBit is even lower (~0.16%). 
If they do agree, we use the result from Bubblesort.

If they do not agree, we will check the TopBit results from Exchangesort 
and Permutationsort. The chance that they will agree on an 
incorrect TopBit is about 1.55%, with the chances of them incorrectly agreeing
on the highest Bit as the TopBit also around 0.16%. If they do agree, we use
the result from Exchangesort.

If after all this adjudication we still do not have an agreed-upon result, we
will use the result from Bubblesort.

Now obviously we have made some approximations in our analysis (and I may have
made some mistakes in my calculations), but in general I think we can conclude 
that it is very unlikely that this Supersort process will return an incorrect 
result, and that if an incorrect result is returned, it is very likely to still 
have a correct TopBit.

We now have the basic form of Robustsort: a 3-bit Tensort with a Supersort 
adjudicating Bubblesort, Exchangesort, and Permutationsort as its
SubAlgorithm.

Well that's pretty cool! But I wonder... can we make this more robust, if 
we relax the rules just a little more?

<!-- (image3) -->

Of course we can! And we will. To do so, we will simply replace Permutationsort
with another newly-named sorting algorithm: Magicsort!

### Magicsort

For our most robust iteration of Robustsort we will relax the requirement on
never re-running the same deterministic sub-algorithm in one specific context.
Magicsort is an algorithm that will re-run Permutationsort only if it disagrees 
with an extremely reliable algorithm algorithm - one that's so good it's robust 
against logic itself...

<!-- (image4) -->

Bogosort!

<!-- (image5) -->

Magicsort simply runs both Permutationsort and Bogosort on the same input and 
checks if they agree. If they do, the result is used and if not, both 
algorithms are run again. This process is repeated until the two algorithms
agree on a result.

Strong-brained readers may have already deduced that Permutationsort functions
nearly identically to Bogosort. Indeed, their approximate analysis results are
the same. Magicsort is based on the idea that if you happen to pull the right 
answer out of a hat once, it might be random chance, but if you do it twice,
it might just be magic!

Given a Byte of [1,2,3], here are the approximate chances of various outcomes 
from Magicsort using a faulty comparator that gives a random result 10% of the 
time:

    ~95.27% <- [1,2,3] (Correct)

    ~1.18% <- [2,1,3] (Correct TopBit)

    ~1.18% <- [1,3,2] (Incorrect)

    ~1.18% <- [3,1,2] (Incorrect)

    ~1.18% <- [2,3,1] (Incorrect)

    ~0.02% <- [3,2,1] (Reverse)

The downside here is that Magisort can take a long time to run. I don't know 
how many comparisons are made on average, but it's well over 14.

Thankfully, Magicsort will only be run in our algorithm if Bubblesort and
Exchangesort disagree on an answer. Overall the Robustsort we're building that 
uses Magicsort will still have an average of O(n log n) time efficiency.

#### Supersort adjudication with Magic

Since we have replaced Permutationsort with Magicsort (which is far more robust 
than Bubblesort or Exchangesort), we will adjust our adjudication
within the Supersort SubAlgorithm.

If Bubblesort and Exchangesort disagree, we will run Magicsort on the
input. If Magicsort agrees with either Bubblesort or Exchangesort, we
will use the result from Magicsort. Otherwise, if Magicsort agrees on the 
TopBit with either Bubblesort or Exchangesort, we will use the result
from Magicsort. Otherwise, if Bubblesort and Exchangesort agree on the
TopBit, we will use the result from Bubblesort.

If no agreement is reached at this point, we abandon all logic and just use
Magicsort.

### A note on Robustsort and Bogosort

It is perfectly valid to use Bogosort in place of Permutationsort in Robustsort's 
standard Supersort SubAlgorithm. It may be argued that doing so is even more 
robust, since it barely even relies on logic. Here are some considerations to
keep in mind:

  - Permutationsort uses additional space and may take slightly longer on average 
      due to computing all possible permutations of the input and storing them in a 
      list.

  - Bogosort could theoretically run forever without returning a result, even 
      when no errors occur.
  
## Comparing it all

Now let's take a look at how everything compares. Here is a graph showing the 
benchmarking results in both in both robustness and time efficiency for 
Quicksort, Mergesort, Standard Logarithmic Tensort, Robustsort (Permutations), 
Robustsort (Bogo), Robustsort (Magic), and Bubblesort:

...Coming Soon!

## Library

This package contains implementations of each algorithm discussed above. 
Notably, it provides the following:

  - Customizable Tensort

  - Standard Logarithmic Tensort

  - Standard Tensort with customizable Bytesize

  - Mundane Robustsort with Permutationsort adjudicator

  - Mundane Robustsort with Bogosort adjudicator

  - Magic Robustsort

Check the code in `src/` or the documentation on Hackage/Hoogle (Coming Soon!) 
for more details.

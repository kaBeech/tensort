# Tensort [![Hackage](https://img.shields.io/hackage/v/tensort.svg)](https://hackage.haskell.org/package/tensort)

Tensort is a family of sorting algorithm that are tunable to adjust to the
priorities of the task at hand.

This project started as an exploration of what a sorting algorithm that
prioritizes robustness would look like. As such it also describes and provides
implementations of Robustsort, a group of Tensort variants designed for
robustness in conditions defined in David H. Ackley's
[Beyond Efficiency](https://www.cs.unm.edu/~ackley/be-201301131528.pdf).

Simply put, Tensort takes an input list, transforms the list into a
multi-dimensional tensor field, then transforms that tensor field back into a
sorted list. These transformations provide opportunities to increase redundancy
for improved robustness and can be leveraged to include any further processing
we wish to do on the elements.

Note: This project is still under construction. Everything works and performs
excellently under Ackley's testing conditions. Still to add: documentation
additions/revisions, convenience wrappers for the top-level functions, memes.
There's likely a lot of room for improvement in the code as well.

## Table of Contents

- [Introduction](#introduction)
  - [Inspiration](#inspiration)
  - [Why?](#why)
  - [But why would anyone care about this in the first place?
    ](#but-why-would-anyone-care-about-this-in-the-first-place)
  - [Why Haskell?](#why-haskell)
- [Project structure](#project-structure)
- [Algorithms overview](#algorithms-overview)
  - [Tensort](#tensort)
    - [Preface](#preface)
    - [Structure](#structure)
    - [Algorithm](#algorithm)
    - [What are the benefits?](#what-are-the-benefits)
    - [Logarithmic Bytesize](#logarithmic-bytesize)
  - [Robustsort](#robustsort)
    - [Preface](#preface-1)
    - [Overview](#overview)
    - [Examining Bubblesort](#examining-bubblesort)
    - [Rotationsort](#rotationsort)
    - [Introducing Supersort](#introducing-supersort)
    - [Permutationsort](#permutationsort)
    - [Supersort Adjudication](#supersort-adjudication)
    - [Recursion](#recursion)
  - [Magicsort](#magicsort)
  - [A note on Robustsort and Bogosort](#a-note-on-robustsort-and-bogosort)
- [Comparing it all](#comparing-it-all)
- [Library](#library)
- [Development Environment](#development-environment)

## Introduction

### Inspiration

  - [Beyond Efficiency](https://www.cs.unm.edu/~ackley/be-201301131528.pdf) by
  [David H. Ackley](https://github.com/DaveAckley)
    
  - [Beyond Efficiency by Dave Ackley](https://futureofcoding.org/episodes/070)
  by Future of Coding ([Lu Wilson](https://github.com/TodePond),
  [Jimmy Miller](https://github.com/jimmyhmiller),
  [Ivan Reese](https://github.com/ivanreese))

### Why?

Because near the end of
[that podcast episode](https://futureofcoding.org/episodes/070),
[Ivan](https://github.com/ivanreese) said "Why are we comparing Bubblesort
versus Quicksort and Mergesort? Well, because no one's made Robustsort yet."

And I thought, "Why not?"

### But why would anyone care about this in the first place?

Being adaptable to different scenarios, a tunable sorting algorithm has many
potential applications. This README will focus on robustness in sorting.

[Ackley](https://www.cs.unm.edu/~ackley/be-201301131528.pdf) has some
compelling things to say about why prioritizing robustness is important and
useful. I'd highly recommend you read that paper!

Or listen to [this podcast](https://futureofcoding.org/episodes/070)!

If you want my elevator pitch, it's because we eventually want to build things
like [Dyson Spheres](https://en.wikipedia.org/wiki/Dyson_sphere). Doing so will
involve massively distributed systems that are constantly pelted by radiation.
In such circumstances, robustness is key.

Another example I like to consider is artificial cognition. When working
in a non-deterministic system (or a system so complex as to be considered
non-deterministic), it can be helpful to have systems in place to verify that
the answer we come to is valid.

Incidentally, while I was preparing for this project, we experienced
[the strongest solar storm to reach Earth in 2 decades](https://science.nasa.gov/science-research/heliophysics/how-nasa-tracked-the-most-intense-solar-storm-in-decades/).
I don't know for certain whether the solar activity caused any computer errors,
but we had some anomalies at work and certainly joked about them being caused
by the Sun.

Also during the same period,
[one of the Internet's root-servers glitched out for unexplained reasons](https://arstechnica.com/security/2024/05/dns-glitch-that-threatened-internet-stability-fixed-cause-remains-unclear/).

As Ackley asserts, as a culture we have tended to prioritize correctness and
efficiency to the detriment of robustness. The rate of our technological
progression precludes us from continuing to do so.

### Why Haskell?

1. Tensort can involve a lot of recursion, which Haskell handles well

2. All the other benefits we get from using a purely functional language, such
as strict dependency management, which
[even the smartest among us](http://livingcomputation.com/robusort2.tar)
sometimes falter without:

  ![Comment from Ackley in the Beyond Efficiency code about Perl updates
  breaking their code](./assets/images/ackley_deps.png)

3. [Obviously](https://www.youtube.com/shorts/LGZKXZQeEBg)

### What's a tensor?

If you want an in-depth explanation,
[Wikipedia](https://en.wikipedia.org/wiki/Tensor) is usually a good starting
place.

If you just want to understand Tensort, you can think of 'tensor' as a fancy
word for a multi-dimensional array.

Every tensor has a degree, which is the number of dimensions it has. A 0-degree
tensor is a scalar (like an integer), a 1-degree tensor is a vector (like a
list), and a 2-degree tensor is a matrix.

Each dimension of a tensor has a rank, which can be thought of as the length of
the dimension. A tensor's shape can be described by a tensor denoting the ranks
of each of its dimensions. For example. [1,2,3] is an instance of a 1-degree
tensor. Its single dimension is 3 elements long, so it has a rank 3. Thus its
shape is [3].

For another example, consider the following tensor which has the shape [3,2]:
[[1,2,3],
 [4,5,6]]

Tensort transforms a list into the highest-degree tensors possible while
giving most of its dimensions a specified rank size to achieve maximum
parallelism. This provides opportunities to add processing tailored to current
goals while preserving time efficiency.

## Project structure

- `src/` contains the Tensort library
    
- `app/` contains the suite for comparing different sorting algorithms in terms
of robustness and time efficiency (only in the benchmarking branch)

- `data/` contains benchmarking data

## Algorithms overview

This README assumes some general knowledge of basic sorting algoritms. If you
would like a refresher, I recommend
[this video](https://www.youtube.com/watch?v=kgBjXUE_Nwc) which touches on
Bubblesort, Mergesort, and Bogosort, and
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
introduction, Tensort, Robustsort, Rotationsort, Permutationsort, and
Magicsort. Get ready!

### Tensort

#### Preface

Tensort is my original attempt to write the most robust sorting algorithm
possible with O(n log n) average time efficiency while avoiding anything that
Ackley might consider a "cheap hack." Starting out, my hope was that it would
be, if not competitive with Bubblesort in robustness, at least a major
improvement over Quicksort and Mergesort.

Again, I haven't studied sorting algorithms long, so if you know this algorithm
under another name, please let me know! After settling on this algorithm, I
looked into several other sorting algorithms for comparison and found a few
that have some similarities with Tensort - significantly Blocksort, Bucketsort,
and Patiencesort. If you are familiar with these algorithms, you may recognize
that they each have a structure that aids in understanding them.

Tensort uses an underlying structure as well. We will discuss this structure 
before going over the algorithm's actual steps. If this doesn't make sense yet,
fear not!

<!-- [image1] -->

#### Structure

  - Bit <- Element of the list to be sorted

  - Byte <- List of Bits

  - Bytesize <- Maximum length of a Byte

  - Tensor <- Tuple of a Register list and a Memory list

  - Memory <- List of Bytes or other Tensors contained in the current Tensor

  - Register <- List of Records, each Record referencing one Byte or Tensor
  in Memory

  - Record <- Tuple of the Address and a copy of the TopBit of the referenced
  Byte or Tensor

  - Address <- Pointer to a Byte or Tensor in Memory

  - TopBit <- Value of the Bit at the top of the stack in a Byte or Tensor

  - TensorStack <- A top-level Tensor along with all the Bits, Bytes, and
  Tensors contained within it. Structurally equivalent to a Tensor

  - TopRegister <- List of Records that is built after all Tensors are built.
  Each Record references one TensorStack. Structurally equivalent to a Register

  - SubAlgorithm <- The sorting sub-algorithm used at various stages

In Tensort, the smallest unit of information is a Bit. Each Bit stores one
element of the list to be sorted. A group of Bits is known as a Byte.

A Byte is a list of Bits. The maximum length of a Byte (known as the Bytesize)
is set according to an argument passed to Tensort. This Bytesize can also be
thought of as the maximum rank (not degree) of a tensor in Tensort.
Ideally, all Bytes will be of maximum length until the final steps of Tensort.
Several Bytes are grouped together in a Tensor.

A Tensor is a tuple with two elements: Register and Memory.

Memory is the second element in a Tensor tuple. It is a list of Bytes or
other Tensors. The maximum length of this Memory list is equal to the Bytesize.

A Register is the first element in a Tensor tuple. It is a list of Records,
each of which has an Address pointing to an element in its Tensor's Memory
and a copy of the TopBit in the referenced element.

Each Record is a simplification of a Byte or Tensor in a Tensor's memory. It
is a tuple comprised of an Address and a TopBit

The Address of a Record is an integer representing the index of the referenced
Byte or Tensor in its containing Tensor's memory

The TopBit in a Byte (which is copied into the Byte's referencing Record) is
the Bit at the end of the Byte list. If everything functions correctly, this
will be the highest value Bit in the Byte.

The TopBit in a Tensor (which is also copied into the Tensor's referencing
Record) is the TopBit of the Byte referenced by the Record at the end of the
Register list of the Tensor referenced by the Record at the end of the
Register list of the Tensor... and so on until the original (containing) Tensor
is reached.

A TensorStack is a top-level Tensor (i.e. a Tensor not contained within another
Tensor) along with all the Bits, Bytes, and Tensors it contains. Once the
Tensors are fully built, the total number of TensorStacks will be equal to (or
sometimes less than) the Bytesize, but before that point there will be many
more TensorStacks.

Once all Tensors are built, a TopRegister is assembled as a list of Records,
each Record referencing one TensorStack.

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

  1. Randomize the input list of elements (Bits).

  2. Assemble Bytes by grouping the Bits into lists of lengths equal to the
    Bytesize, then sorting the Bits in each Byte using the SubAlgorithm. After
    this, we will do no more write operations on the Bits until the final
    steps. Instead, we will make copies of the Bits and sort the copies
    alongside their pointers.

  3. Assemble TensorStacks by creating Tensors from the Bytes:
        1. Group the Bytes together in Memory lists of Bytesize length.
        2. Assign each Memory to a newly-created Tensor.
        3. Make Records for each Byte in Memory by combining its index in the
          Memory list with a copy of its TopBit.
        4. Group the Records together in Register lists and assign them to
          their respective Tensors.
        5. Sort each Register list in order of its Records' TopBits.

  4. Reduce the number of TensorStacks by creating a new layer of Tensors from
    the Tensors created in Step 3:
        1. Group the first layer of Tensors together in Tensor lists of
          Bytesize length.
        2. Assign each Memory to a newly-created Tensor.
        3. Make Records for each Tensor in Memory by combining its index in the
          Memory list with a copy of its TopBit.
        4. Group the Records together in Register lists and assign them to
          their respective Tensors.
        5. Sort each Register list in order of its Records' TopBits.

  5. Continue in the same manner as in Step 4 until the number of TensorStacks
    is equal to or less than the Bytesize.

  6. Assemble a TopRegister by making Records from the Top Bits on each
    TensorStack and sorting the Records.

  7. Remove the Top Bit from the top Byte in the top TensorStack and add it
    to the final Sorted List. If the top Byte has more than one Bit in it
    still, re-sort the Byte for good measure

  8. If the top Byte in the top TensorStack is empty:
      1. Remove the Record that points to the top Byte from its containing 
        Tensor's Register.
      2. If the Tensor containing that byte is empty, remove the
        Record that points to it from its containing Tensor's Register. Do this
        recursively until the Tensor is not empty or the top of the
        TensorStack is reached.
      3. If the entire TensorStack is empty of Bits, remove its Record from the
        TopRegister.
      4. If all TensorStacks are empty of Bits, return the final
        Sorted List. Otherwise, re-sort the TopRegister.

  9. Otherwise (i.e. the top Byte or a Tensor that contains it is not empty):
      1. Update the top Byte's (or Tensor's) Record with its new TopBit.
      2. Re-sort the top Byte's (or Tensor's) containing Tensor's Register.
      3. Then jump up a level to the Tensor that contains that Tensor,
        update the containing Tensor's Record with its new TopBit, and re-sort
        its Register. Do this recursively until the whole TensorStack is 
        rebalanced.
      4. Update the TensorStack's Record in the TopRegister with its new TopBit
      5. Re-sort the TopRegister.

  10. Repeat Steps 7-9 until the final Sorted List is returned.

Now that we know all the steps, it's easier to see why we randomize the list
as the beginning step. This way, if the list is already nearly
sorted, values close to each other don't get stuck under each other in their
Byte. Ideally, we want the top Bits from all TensorStacks to be close to
each other. Say for example that we're using a Bytesize of 4 and the first four
elements in a 1,000,000-element list are 121, 122, 123, and 124. If we don't
randomize the list, these 4 elements get grouped together in the first byte.
That's all well and good if everything performs as expected, but if something
unexpected happens during an operation where we intend to add 124 to the final
list and we add a different element instead, three of the best-case elements to
have mistakenly added (121, 122, and 123) are impossible to have been selected.

#### What are the benefits of using Tensort?

The core idea of Tensort is breaking the input into smaller pieces across many
dimensions and sorting the smaller pieces. Once we understand the overall
structure, we can design a SubAlgorithm (and Bytesize) to suit our needs.

Standard Tensort leverages the robustness of Bubblesort while reducing the time
required by never Bubblesorting the entire input.

We are able to do this because A) Bubblesort is very good at making sure the
last element is in the final position of a list, and B) at each step of Tensort
the only element we *really* care about is the last element in a given list (or
to look at it another way, the TopBit of a given Tensor).

#### Logarithmic Bytesize

When using standard Tensort (i.e. using Bubblesort as the SubAlgoritm), as the
Bytesize approaches the square root of the number of elements in the
input list, its average time efficiency approaches O(n^2).

Standard Tensort is most time efficient when the Bytesize is close
to the natural log of the number of elements in the input list. A logarithmic
Bytesize is likely to be ideal for most use cases of standard Tensort.

-------

Alright! We now have a simple sorting algorithm absent of cheap hacks that
both maintains O(n log n) average time efficiency and is relatively robust. I'm
pretty happy with that!

But now that we understand Tensort's basic structure, let's tune it for even
more robustness!

<!-- [image2] -->

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

  - Keeping to O(n log n) average time efficiency

  - Never re-running a sub-algorithm that is expected to act deterministicly
      on the same arguments looking for a non-deterministic result (i.e. expect
      that if a components gives a wrong answer, running it again the same way
      won't somehow yield a right answer)

  - Using a minimal number of different sub-algorithms (i.e. doesn't just
      use every sorting algorithm I can think of and compare all their results)

With those ground rules in place, let's get to Robustsort!

#### Overview

Once we have Tensort in our toolbox, the road to Robustsort is not long.
Robustsort is a potentially recursive version of Tensort, but first we'll look
at the basic variant: a 3-bit Tensort with a custom SubAlgorithm that compares
other sub-algorithms. For convenience, we will call this custom SubAlgorithm
Supersort. We use a 3-bit Tensort here because there's something magical that
happens around the number 3.

Robust sorting algorithms tend to be slow. Bubblesort, for example, having an
average time efficiency of O(n^2), is practically glacial compared with
Quicksort and Mergesort (which both have an average of O(n log n)).

Here's the trick though: with small numbers the difference between these values
is minimal. For example, when n=4, Mergesort will make 6 comparisons, while
Bubblesort will make 12. A Byte holding 4 Bites is both small enough to run
the Bubblesort quickly and large enough to allow multiple opportunities for a
mistake to be corrected.

In Robustsort, we choose a Bytesize of 3 because a list of 3 Bits has some
special properties. For one thing, sorting at this length greatly reduces the
time it takes to run our slow-but-robust algorithms. For example, at this size,
Bubblesort will make only 6 comparisons. Mergesort still makes 6 as well.

Furthermore, when making a mistake while sorting 3 elements, the mistake
will displace an element by only 1 or 2 positions at most, no matter which
algorithm is used.

This is all to say that using a 3-bit Bytesize allows us to have our pick of
sub-algorithms to compare with!

#### Examining Bubblesort

Before moving further, let's talk a little about Bubblesort and why we're
using it in our SubAlgorithm.

We've said before that Bubblesort is likely to put the last element in the
correct position. Let's examine this in the context of Bubblesorting a
3-element list.

I ran Bubblesort 1000 times on Bytes of random permutations of [1,2,3] using a
faulty comparator that gives a random result 10% of the time. Here is how often
each outcome was returned:

    87.1% <- [1,2,3]

    4.1% <- [1,3,2]

    6.6% <- [2,1,3]

    0.2% <- [2,3,1]

    1.8% <- [3,1,2]

    0.2% <- [3,2,1]

In these results, 93.7% of the time the Top Bit was returned in the correct
position, and the bottom value was returned in the top position only 0.4% of
the time.

Notably, the far more likely result where the Top Bit was at the bottom was
[3,1,2], with [3,2,1] occurring only 0.2% of the time.

#### Rotationsort

When choosing an algorithm to compare with Bubblesort, we want something with
substantially different logic, for the sake of robustness. We do,
however, want something similar to Bubblesort in that it compares our elements
multiple times. And, as mentioned above, the element that is most important to
our sorting is the top (highest value) element, by a large degree.

In terms of the probability of different outcomes, if our algorithm returns
an incorrect result, we want that result to be different than what Bubblesort
is likely to return.

I originally chose an algorithm that balances these properties and I retain the
above paragraphs for the sake of theory. However, we're going to choose a
highly accurate algorithm instead: Rotationsort.

...

Here are the results of running Rotationsort 1000 times on Bytes of random
permutations of [1,2,3] using a faulty comparator that gives a random result
10% of the time:

    91.8% <- [1,2,3]

    3.4% <- [1,3,2]

    2.5% <- [2,1,3]

    0% <- [2,3,1]

    0.2% <- [3,1,2]

    2.1% <- [3,2,1]

In these results, 94.3% of the time the Top Bit was returned in the correct
position and it returned the bottom value in the top position only 2.1% of the
time.

In results where the Top Bit was in the
bottom position, [3,2,1] was the most likely outcome, with [3,1,2] occurring
only 0.2% of the time. This is opposite to what happens in Bubblesort,
making cases in which they agree with the Top Bit in the bottom position
very rare.

Overall, there is a modest probability (about 0.14% according to these results)
that Bubblesort and Rotationsort will agree on [1,3,2] as the result, but it is
very unlikely that they will agree on any other result that does not have the
Top Bit in the correct position.

#### Introducing Supersort

Supersort is a SubAlgorithm that compares the results of two different
sorting algorithms, in our case Bubblesort and Rotationsort. If both
algorithms agree on the result, that result is used.

Looking at our analysis on Bubblesort and Rotationsort, we can
approximate the chances of how often they will agree in similar conditions:

    ~79.96% <- Agree Correctly

    ~19.73% <- Disagree

    ~0.17% <- Agree Incorrectly - TopBit correct

    ~0.14% <- Agree Incorectly - TopBit incorrect

Hey, that's pretty good! If they agree, then return the results from
Rotationsort because if for some reason the module that compares the full Bytes
is also faulty (outside the scope of these benchmarks), Rotationsort is more
likely to have an accurate result.

Around 20% of the time, these sub-algorithms will disagree with each other.
If this happens we run our third sub-algorithm: Permutationsort.

#### Permutationsort

Permutationsort is a simple, brute-force sorting algorithm.

As a first step we generate all the different ways the elements could possibly
be arranged in the list. Then we loop over this list of permutations until we
find one that is in the right order. We check if a permutation is in the right
order by comparing the first two elements. If the first element is smaller,
we compare the next two elements, and so on until we either find two elements
that are out of order or we reach the end of the list, confirming that the list
is in order.

Permutationsort is a good choice for our adjudication algorithm because A) the
spread of outcomes is favorable for our needs, and B) it uses logic that is
completely different from Bubblesort and Rotationsort. Using different manners
of reasoning to reach an agreed-upon answer increases the robustness of the
system.

Here are the results of running Permutationsort 1000 times on Bytes of random
permutations of [1,2,3] using a faulty comparator that gives a random result
10% of the time:

    81.9% <- [1,2,3]

    4.1% <- [2,1,3]

    4.5% <- [3,1,2]

    5.3% <- [1,3,2]

    3.4% <- [2,3,1]

    0.8% <- [3,2,1]

In these cases, 86% of the time the Top Bit was in the correct position.
Notably the least likely outcome is a reverse-sorted Byte and the other
possible incorrect outcomes are in approximately even distribution with
each other.

#### Supersort Adjudication

Supposing that our results from Bubblesort and Exchangesort disagree 
and we now have our result from Permutationsort, how do we choose which to
use?

First we check to see whether the result from Permutationsort agrees with
the results from either Bubblesort or Exchangesort. To keep things 
simple, let's just look at the raw chances that 
Permutationsort will agree on results with Bubblesort or Exchangesort.

Permutationsort and Bubblesort:

    ~71.33% <- Agree Correctly

    ~28.13% <- Disagree

    ~0.30% <- Agree Incorrectly - TopBit correct

    ~0.24% <- Agree Incorectly - TopBit incorrect

Permutationsort and Exchangesort:

    ~75.18% <- Agree Correctly

    ~25.44% <- Disagree

    ~0.11% <- Agree Incorrectly - TopBit correct

    ~0.16% <- Agree Incorectly - TopBit incorrect

If Permutationsort agrees with either Bubblesort or Exchangesort, then it's 
easy - just use that result!

According to these results, Permutationsort is likely to disagree with both
Bubblesort and Exchangesort about 7.16% of the time if all three are run 
indepedently. In practice this will happen more often than that because in 
order to reach the point of doing Permutationsort, either Bubblesort or 
Exchangesort must have sorted the list incorrectly (which makes it less likely
to agree with Permutationsort).

In any case, if all three algorithms disagree, use the results from Bubblesort.

#### Recursion

You'll remember that our standard Tensort uses a logarithmic Bytesize. Our base
Robustsort uses a Bytesize of 3, but we can use a logarithmic Bytesize by 
adding recursion.

<!-- (image3) -->

Let's take our base Robustsort example above and make it recursive.

First, instead of using a 3-bit Bytesize, we will use a logarithmic Bytesize.
Then, instead of using our Supersort directly as our
SubAlgorithm, we will use Robustsort itself to sort the records.

At the base case, this Robustsort will have a Bytesize of 3. If the logarithmic 
Bytesize of the input list is greater than 27, then the SubAlgorithm of the 
top-level Robustsort will be a recursive Robustsort with a logarithmic 
Bytesize.

The number 27 is chosen because we want a number that has a natural log that is
close to 3 (27's is about 3.3) and since 3 ^ 3 = 27, it is easy to sort lists 
of 27 elements in groups of 3.

This recursive version of Robustsort is more tailored to large input lists 
(it doesn't add another layer of recursion until the input list is
is longer than 500 billion elements), but differences can be noticed when 
sorting smaller lists as well.

We now have the standard form of Robustsort: a potentially recursive Tensort 
with a 3-bit base case using a Supersort adjudicating Bubblesort, Exchangesort,
and Permutationsort as its base SubAlgorithm.

Well that's pretty cool! But I wonder... can we make this more robust, if 
we relax the rules just a little more?

<!-- (image4) -->

Of course we can! And we will. To do so, we will simply replace Permutationsort
with another newly-named sorting algorithm: Magicsort!

### Magicsort

For our most robust iteration of Robustsort we will relax the requirement on
never re-running the same deterministic sub-algorithm in one specific context.
Magicsort is an algorithm that will re-run Permutationsort only if it disagrees 
with an extremely reliable algorithm - one that's so good it's robust 
against logic itself...

<!-- (image4) -->

Bogosort!

<!-- (image5) -->

Magicsort simply runs both Permutationsort and Bogosort on the same input and 
checks if they agree. If they do, the result is used and if not, both 
algorithms are run again. This process is repeated until the two algorithms
agree on a result.

Strong-brained readers may have already deduced that Permutationsort functions
nearly identically to Bogosort. Here are the results of running Bogosort 1000 
times on Bytes of random permutations of [1,2,3] using a faulty comparator that 
gives a random result 10% of the time:

    81.3% <- [1,2,3]

    3.0% <- [2,1,3]
  
    3.8% <- [3,1,2]

    5.8% <- [1,3,2]

    5.7% <- [2,3,1]

    0.8% <- [3,2,1]

In these cases, 84.3% of the time the Top Bit was in the correct position. 
Note that even though both Bogosort and Permutationsort were ran with the same 
random seeds, they gave slightly different results because their methodology 
is slightly different the least likely outcome is a reverse-sorted Byte and the other 
possible incorrect outcomes are in approximately even distribution with 
each other.

Magicsort is based on the notion that if you happen to pull the right 
answer out of a hat once, it might be random chance, but if you do it twice,
it might just be magic!

Here are the results of running Magicsort 1000 
times on Bytes of random permutations of [1,2,3] using a faulty comparator that 
gives a random result 10% of the time:

    ~94.0% <- [1,2,3] (Correct)

    ~1.5% <- [2,1,3] (Correct TopBit)

    ~1.4% <- [1,3,2] (Incorrect)

    ~1.5% <- [3,1,2] (Incorrect)

    ~1.5% <- [2,3,1] (Incorrect)

    ~0.1% <- [3,2,1] (Reverse)

94% of the time we got the absolutely correct answer! In total, 95.5% of the 
time we got the Top Bit in the correct position and only 1.6% of the time did
we get the bottom value in the top position.

The downside here is that Magisort can take a long time to run. I don't know 
how many comparisons are made on average, but it's well over 14.

Thankfully, Magicsort will only be run in our algorithm if Bubblesort and
Exchangesort disagree on an answer, and then only with 3 elements to sort.
Overall, the Robustsort we're building that uses Magicsort will still have an
average of O(n log n) time efficiency.

#### Supersort adjudication with Magic

Since we have replaced Permutationsort with Magicsort (which is far more robust
than Bubblesort or Exchangesort), we will adjust our adjudication
within the Supersort SubAlgorithm. If Bubblesort and Exchangesort don't come 
to an agreement, we just use Magicsort.

You might expect that we'd check to see whether Magicsort agrees with any of 
the other algorithms before just using its results, but even if we were to 
check we would end up using the results from Magicsort in all pass cases and 
all fail cases.

### A note on Robustsort and Bogosort

It is perfectly valid to use Bogosort in place of Permutationsort in 
Robustsort's standard Supersort SubAlgorithm. It may be argued that doing so is
even more robust, since it barely even relies on logic. Here are some
considerations to
keep in mind:

  - Permutationsort uses additional space and may take slightly longer on
      average due to computing all possible permutations of the input and
      storing them in a list.

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

In the 1.0.0.0 release there will also be a wrapper around top-level 
Tensort (Logarithmic) and Robustsort (Magic) that will allow for easy
use without dealing with Type conversions.

Check the code in `src/` or the documentation on Hackage/Hoogle
for more details.

## Development Environment

This project is wrapped in a Nix Flake, so it's easy to hack on yourself!

Note that (unless otherwise specified) all instructions assume you are in the 
repository root, have Nix installed, and have entered the development shell.

### Entering the Dev Shell

Note that these instructions don't make the assumptions listed above

  * [Install Nix](https://nixos.org/download/)
  * [Enable Flakes](https://nixos.wiki/wiki/Flakes)
  * [Clone this repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)
  * Run `nix develop` in the repository root 

### Run main test suite (QuickCheck)

  * Run `cabal test`

### Run DocTest

  * Run `cabal repl --with-compiler=doctest`

### Print Benchmarking Data

  * [Checkout to the 'benchmarking' branch](https://git-scm.com/docs/git-checkout)
  * Uncomment the desired benchmarking process(es) in `app/Main.hs`
  * Run `cabal run`

# Tensort [![Hackage](https://img.shields.io/hackage/v/tensort.svg)](https://hackage.haskell.org/package/tensort)

Tensort is a tensor-based sorting algorithm that is tunable to adjust to 
the priorities of the task at hand.

This project started as an exploration of what a sorting algorithm that 
prioritizes robustness would look like. As such it also describes and provides
implementations of Robustsort, a group of Tensort variants designed to 
prioritize robustness in conditions defined in David H. Ackley's
[Beyond Efficiency](https://www.cs.unm.edu/~ackley/be-201301131528.pdf).

Simply put, Tensort takes an input list, transforms the list into a tensor field, 
then transforms the tensor field back into a sorted list. These transformations
provide opportunities to increase redundancy for improved robustness and can
be leveraged to include any further processing we wish to do on the elements.

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
    - [Exchangesort](#exchangesort)
    - [Introducing Supersort](#introducing-supersort)
    - [Permutationsort](#permutationsort)
    - [Supersort Adjudication](#supersort-adjudication)
    - [Recursion](#recursion)
  - [Magicsort](#magicsort)
    - [Supersort adjudication with Magic](#supersort-adjudication-with-magic)
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

Because near the end of [that podcast episode
](https://futureofcoding.org/episodes/070), 
[Ivan](https://github.com/ivanreese) said "Why are we comparing Bubblesort 
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

Another example I like to consider is artificial cognition. When working 
in a non-deterministic system (or a system so complex as to be considered
non-deterministic), it can be helpful to have systems in place to make sure 
that the answer we come to is really valid.

Incidentally, while I was preparing for this project, we experienced 
[the strongest solar storm to reach Earth in 2 decades
](https://science.nasa.gov/science-research/heliophysics/how-nasa-tracked-the-most-intense-solar-storm-in-decades/). 
I don't know for certain whether the solar activity caused any computer errors, 
but we had some anomalies at work and certainly joked about them being caused 
by the Sun.

Also during the same period, 
[one of the Internet's root-servers glitched out for unexplained reasons
](https://arstechnica.com/security/2024/05/dns-glitch-that-threatened-internet-stability-fixed-cause-remains-unclear/).

As Ackley mentions, as a culture we have tended to prioritize correctness and 
efficiency to the exclusion of robustness. The rate of our technological 
progression precludes us from continuing to do so.

### Why Haskell?

1. Tensort can involve a lot of recursion, which Haskell handles well

2. All the other benefits we get with using a purely functional language, such 
as strict dependency management, which 
[even the smartest of us](http://livingcomputation.com/robusort2.tar) sometimes 
falter without:

  ![Comment from Ackley in the Beyond Efficiency code about Perl updates 
  breaking their code](./assets/images/ackley_deps.png)

3. [Obviously](https://www.youtube.com/shorts/LGZKXZQeEBg)

## Project structure

- `src/` contains the Tensort library
    
- `app/` contains the suite for comparing different sorting algorithms in terms
of robustness and time efficiency

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
    
  - Record <- Tuple of the Address and the TopBit of the referenced Byte or 
  Tensor
    
  - Address <- Pointer to a Byte or Tensor in Memory
    
  - TopBit <- Value of the Bit at the top of the stack in a Byte or Tensor

  - TensorStack <- A top-level Tensor along with all the Bits, Bytes, and 
  Tensors it contains
    
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
    will do no more write operations on the Bits until the final steps. 
    Instead, we will make copies of the Bits and sort the copies alongside 
    their pointers.

  3. Assemble TensorStacks by creating Tensors from the Bytes. Tensors are 
    created by grouping Bytes together (setting them as the Tensor's 
    second element), making Records from their top bits, sorting the records, 
    and then recording the Pointers from the Records (after being sorted) as 
    the Tensor's first element.

  4. Reduce the number of TensorStacks by creating a new layer of Tensors from 
    the Tensors created in Step 3. These new Tensors are created by grouping 
    the first layer of Tensors together (setting them as the new Tensor's 
    second element), making Records from their top Bits, sorting the Records, 
    and then recording the Pointers from the Records (after being sorted) as 
    the Tensor's first element.

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

#### Logarithmic Bytesize

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

Once we have Tensort in our toolbox, the road to Robustsort is not long. 
Robustsort is a recursive version of Tensort, so first we'll look at its base 
case, a 3-bit Tensort with a custom SubAlgorithm that compares other 
sub-algorithms. For convenience, we will call this custom SubAlgorithm 
Supersort. We use a 3-bit Tensort here because there's something magical that 
happens around the number 3.

Robust sorting algorithms tend to be 
slow. Bubblesort, for example, has an average time efficiency of O(n^2), 
compared with Quicksort and Mergesort, which both have an average of (n log n).

Here's the trick though: with small numbers the difference between these values 
is minimal. For example, when n=4, Mergesort will make 6 comparisons, while 
Bubblesort will make 12. A Byte holding 4 Bites is both small enough to run 
the Bubblesort quickly and large enough to allow multiple opportunities for a 
mistake to be corrected. 

In Robustsort, we choose a Bytesize of 
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
sub-operations performed) and as we will see, it pays to have some diversity
in our sub-algorithms.

#### Examining Bubblesort

Before moving further, let's talk a little about Bubblesort, and why we're 
using it in our SubAlgorithm.

As a reminder, Bubblesort will make an average of 6 comparisons when sorting
a 3-element list.

We've said before that Bubblesort is likely to put the last element in the 
correct position. Let's examine this in the context of Bubblesorting a 
3-element list.

<!-- Our implementation of Bubblesort (which mirrors Ackley's) will perform three -->
<!-- iterations over a 3-element list. After the second iteration, if everything -->
<!-- goes as planned, the list will be sorted and the final iteration is an extra -->
<!-- verification step. Therefore, to simplify the analysis, we will consider -->
<!-- what happens with a faulty comparator during the final iteration, assuming the -->
<!-- list has been correctly sorted up to that point. -->

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

#### Exchangesort

When choosing an algorithm to compare with Bubblesort, we want something with 
substantially different logic, for the sake of robustness. We do, 
however, want something similar to Bubblesort in that it compares our elements 
multiple times. And, as mentioned above, the element that is most important to 
our sorting is the top (biggest) element, by a large degree.

In terms of the probability of different outcomes, if our algorithm returns 
an incorrect result, we want that result to be different than what Bubblesort
is likely to return.

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

<!-- As with Bubblesort, Exchangesort will perform three iterations over a 3-element -->
<!-- list, with the final iteration being redundant. -->

Here are the results of running Exchangesort 1000 times on Bytes of random 
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
that Bubblesort and Exchangesort will agree on [1,3,2] as the result, but it is
very unlikely that they will agree on any other result that does not have the
Top Bit in the correct position.

#### Introducing Supersort

Supersort is a SubAlgorithm that compares the results of two different
sorting algorithms, in our case Bubblesort and Exchangesort. If both 
algorithms agree on the result, that result is used. 

Looking at our analysis on Bubblesort and Exchangesort, we can 
approximate the chances of how often they will agree in similar conditions:

    ~79.96% <- Agree Correctly

    ~19.73% <- Disagree

    ~0.17% <- Agree Incorrectly - TopBit correct

    ~0.14% <- Agree Incorectly - TopBit incorrect

Hey, that's pretty good! If they agree, then return the results from 
Bubblesort because if for some reason the module that compares the full Bytes
is also faulty (outside the scope of these benchmarks), Bubblesort is less 
likely to have a result with the bottom value set as the Top Bit.

Around 20% of the time, these sub-algorithms will disagree with each other.
What happens then?

First we check to see if they agree on the Top Bit. If they do, we return the 
results from Exchangesort, since it is more likely to have the results exactly
correct. Otherwise, we run a third sub-algorithm to compare the results with.

The reason we check to see if Bubblesort and Exchangesort agree on the Top Bit
is that based on our results, there is only about a 0.07% chance that they will
agree on the incorrect Top Bit while disagreeing on the total results. This is 
due to the fact that these algorithms have only one result with a incorrect 
Top Bit ([1,3,2]) that they both return more than 1% of the time, so one of 
them has to return a rare result for this peculiar semi-agreement to occur.

If the first two sub-algorithms don't agree on a Top Bit then we run our third 
sub-algorithm: Permutationsort.

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

We now have the basic form of Robustsort: a recursive Tensort with a 3-bit 
base case using a Supersort adjudicating Bubblesort, Exchangesort, and 
Permutationsort as its base SubAlgorithm.

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
  * [Clone this repository
    ](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)
  * Run `nix develop` in the repository root 

### Run main test suite (QuickCheck)

  * Run `cabal test`

### Run DocTest

This is planned to be folded into the main test suite with the 1.0.0.0 release

  * Run `cabal repl --with-compiler=doctest`

### Get Benchmarking Data

  * Run `cabal run`

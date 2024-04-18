# Robust Sort

The goal of this project is to explore what a sorting algorithm that 
prioritizes robustness over efficiency would look like

## Inspiration

- [Beyond Efficiency](https://www.cs.unm.edu/~ackley/be-201301131528.pdf) by David H. Ackley
- Future of Coding's [podcast episode](https://futureofcoding.org/episodes/070) on the same paper

## Implementation

...

Please note that we will discuss a few algorithms that I might have made up, or 
I'm just not familiar with them under other names. If these algorithms have 
previously been defined and named, please let me know. I don't know algorithms, 
I'm just rhyming words together =)

The algorithms used here that I have made up or renamed are Robustsort, 
Bytesort, Comparisonsort, and Magicsort. Get ready!

We are accompanied on this journey by the image of Sir Michael Caine, who will 
be watching over my work to make sure I stay on the straight and narrow. He's 
also here as a bit of insurance to make sure you've read Beyond Efficiency; my 
own version of Van Halen's "No Brown M&M's", if you will. If you don't 
understand why he's here, especially by the end of this README, go back and 
read the paper =)

Alright, let's get started! Ready, Michael?

### Bytesort

...

The first bit in each metabyte... . It seems to me that ... is more time 
efficient than... , though it is less space-efficient. It also seems more 
efficient than ... . It's worth noting though that I'm not 100% 
sure about any of this - I used intuition more than anything to make these 
determinations. It's entirely possible that this method could be improved 
(perhaps by using an [in-place data structure] and some clever swaperations). 
As always, constructive feedback is welcome!

...

Alright! Now we have a simple sorting algorithm absent of cheap hacks that is 
both relatively fast and relatively robust. I'm pretty happy with that!

Now for some cheap hacks!

### Enter Robustsort

Robustsort is my attempt to make the most robust sorting algorithm possible 
that still runs in a somewhat reasonable amount of time, using some techniques 
that Ackley might consider "cheap hacks." Even while permitting these 
tricks, I'm keeping myself to a few rules:

    - Never run a determinitive algorithm more than once on the same arguments 
      hoping to get a different result

    - Once the result of a non-determinitive algorithm is compared with a 
      the results of a determinitive algorithm, the non-determinitive algorithm 
      cannot be run again on the same arguments to get a different result

With those ground rules in place, let's get to Robustsort!

Once we have Bytesort in our toolbox, the road to Robustsort is pretty simple. 
At its core, Robustsort is...

...

...a reverse Comparisonsort.

But what is Comparisonsort [Exchangesort], you ask?

### Comparisonsort

Comparisonsort is the algorithm presented here that is the most likely to 
already be well-known under another name.

...

### Fitting Comparisonsort into Robustsort

...

Now we've made it to the algorithm I know you've all been waiting for: Magicsort!

### Magicsort

Magicsort is designed to be the most robust sorting algorithm used in 
Robustsort. It is, of course, based on the most robust, simplest, and 
all-around greatest sorting algorithm of of them all: ...

...

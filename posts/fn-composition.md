Last year, I distractedly worked through the first 3 chapters of *The Structure and Interpretation of Computer Programs*.
It had come highly recommended to me from programmers I respected. But I had only begun my journey into computer science, and so *SICP*
didn't really do it for me. I read it as a "How to Code" text. Which was the wrong way to read it.

After a full year of diving deeper into "How to Code" (and subsequently into theory), I am now revisiting this text and so far it is reading like the Talmud. A lot of things I didn't notice
the first time around I find myself reading and re-reading and pondering over.

In this post, I'll be listing (and trying to keep up to date), some "aha!" moments. All mistakes are mine, please call them out when you see them :)

###On the Magic of Processes:

> Computational processes are abstract beings that inhabit computers. As they evolve, processes manipulate other abstract things called *data*.
> The evolution of a process is directed by a pattern of rules called a *program*. People create programs to direct processes.
> In effect, we conjure the spirits of the computer with our spells.

This passage gets less esoteric after studying computer architecture and operating systems (re: processes, threads
of execution, and how these are represented and instantiated in memory by the kernel).

###Tail-Recursive & Registers:

> (A process) characterized by a chain of deferred operations is called a *recursive* process. Carrying out this process requires that the 
> interpreter keep track of the (computations) to be performed later on.
> By contrast [...] an *iterative* process is one whose state can be summarized by a fixed number of state variables, together with a 
> fixed rule that describes how the state variables should be updated as the process moves from state to state.

Hiding in the footnotes:

> When we discuss the implementation of procedures on register machines, we will see that any iterative process can be realized "in hardware"
> as a machine taht has a fixed set of registers and no auxiliary memory. In contrast, realizing a recursive process requires a machine that
> uses an auxiliary data structure known as a *stack*.

When you've written enough Javascript and read enough trendy Functional Programming Lite blog posts, you run into the term "TCO" -- tail-call optimization.
Many articles write about how you can make your programs more efficient with "tail recursion" because the v8 engine can optimize these tail calls
such that calling that function won't lead to the dreaded "Maximum call stack size exceeded" error.

It all sounds like magic until you realize they're talking about recursive v. iterative processes, stacks v. registers, 
statefulness v. state encapsulation. Let's take the simple example Abelson/Sussman use of computing `b^n` (exponentiation).

A *linearly recursive procedure* that calculates `b^n`:

```js
const expR = (b, n) => {
  if (n === 0) return 1
  else return b * expR(b, n - 1)
}
```

A *tail recursive procedure* that does the same thing:

```js
const exp = (b, n) => {
  const expTCO = (b, counter, product) => {
    if (counter === 0) return product
    else return expTCO(b, counter - 1, b * product)
  }
  
  return expTCO(b, n, 1)
}
```

In the former case, not only does the return value of each function call depend on what its recursive call returns, the process must
also somehow maintain state about this growing stack of calls. Why? Because when a recursive call returns, it must be then be applied
to the "deferred operation" (`multiplying b * expR(b, n -1)`). This gets computationally expensive.

In the latter case, all state transformations are represented in the parameters of each recursive call. As each function call goes along,
it carries with it all state transformations. The product is calculated immediately as opposed to deferred.

An interpreter that handles tail-recursive procedures, then, will notice the "tail" position of the recursive procedure, identify the
pattern of state transformation it makes from one call to the next, and then perhaps represent it in assembly code as a tight, efficient
loop of computation. Perhaps similar to:

```js
const expIter = (b, n) => {
  let counter = n, product = 1
  while (counter--) {
    product *= b
  }
  
  return counter
}
```

###Function Composition

> Often the same programming pattern will be used with a number of different procedures. To express such patterns as concepts, we will need
> to construct procedures that can accept procedures as arguments or return procedures as values. Procedures that manipulate procedures
> are called *higher-order procedures*.

This passage drills in (at least to me) the notion that code is just data. Whether we're dealing with a function or a list or an integer
or some other primitive, it doesn't matter. We're ultimately performing a series of instructions that manipulate the registers and auxiliary memory structures
located in the hardware of our computers.

The implication of this is that, if our logic is tight, we can then start viewing our higher-level abstractions as procedures
being applied to a pattern of other procedures.

A cool example:

```js
const iter = (a, b, map, reduce) => {
  if (a >= b) return map(a)
  else return reduce(map(a), iter(++a, b, map, reduce))
}
```

The above function is an iterator from some integer `a` to `b`. On each iteration, it applies a mapping function of the programmer's choice
to the current integer (let's say `i`), then applies a programmer-supplied reduce function to this transformed `i`, moving along until
we reach the end of the range of integers.

It can be used to define some `factorial` process:

```js
const factorial = x => {
  const identity = x => x
  const mult = (a, b) => a * b
  return iter(1, x, identity, mult)
}
```
Iterate from 1 to `x`, multiplying by `identity(i)` as you go. `factorial(4) -> identity(1) * identity(2) * identity(3) * identity(4)`

And an interpreter that handles tail-call optimization would optimize this code path to look something like this:

```js
const fact = x => {
  let total = 1
  for (let i = 1; i <= x; i++) {
    total *= i
  }
  
  return total
}
```


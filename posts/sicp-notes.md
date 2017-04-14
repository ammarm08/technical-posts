I started my programming journey in mid-2015. My wisest CS friends recommended *The Structure and Interpretation of Computer Programs*, but it entirely flew over my head. Now I'm revisiting it 18 months later, and it's reading like the Talmud.

In this post, I'll be listing (and trying to keep up to date), some "aha!" moments. All mistakes are mine, please call them out when you see them :)

### Chapter 1:
- [Processes, Programs, and Data](#processes)
- [Tail-Recursive Procedures and Registers](#recursion)
- [Higher-Order Procedures and Composition](#composition)
- [Search as an Interval Minimizing Function](#search)
### Chapter 2:
- [Data Objects and Message Passing](#data-objects)
- [Hierarchical Data Structures](#hierarchical-data)
- [Symbolic Representation](#symbolic)
- [Sets](#set)
### Chapter 3:
### Chapter 4:
### Chapter 5:


### <a name="processes">On the Magic of Processes</a>:

> Computational processes are abstract beings that inhabit computers. As they evolve, processes manipulate other abstract things called *data*.
> The evolution of a process is directed by a pattern of rules called a *program*. People create programs to direct processes.
> In effect, we conjure the spirits of the computer with our spells.

Where to start: [Nand2Tetris](http://www.nand2tetris.org/) (write your own logic gates, ALUs, memory units, assembler, compiler, and virtual machine); [Operating Systems: Three Easy Pieces](http://pages.cs.wisc.edu/~remzi/OSTEP/) (exercise-driven discussion on how operating systems work)

### <a name="recursion">Tail-Recursive & Registers</a>:

> (A process) characterized by a chain of deferred operations is called a *recursive* process. Carrying out this process requires that the 
> interpreter keep track of the (computations) to be performed later on.
> By contrast [...] an *iterative* process is one whose state can be summarized by a fixed number of state variables, together with a 
> fixed rule that describes how the state variables should be updated as the process moves from state to state.

Hiding in the footnotes:

> When we discuss the implementation of procedures on register machines, we will see that any iterative process can be realized "in hardware"
> as a machine that has a fixed set of registers and no auxiliary memory. In contrast, realizing a recursive process requires a machine that
> uses an auxiliary data structure known as a *stack*.

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
also somehow maintain state about this growing stack of calls. Why? Because when a recursive call returns, it must then be applied
to the "deferred operation" (`multiplying b * expR(b, n -1)`). This gets computationally expensive.

In the latter case, all state transformations are represented in the parameters of each recursive call. As each function call goes along,
it carries with it all state transformations. The product is calculated immediately as opposed to being deferred.

An interpreter that handles tail-recursive procedures, then, will notice the "tail" position of the recursive procedure, identify the
pattern of state transformation it makes from one call to the next, and then perhaps represent it in assembly code as a tight, efficient
loop of computation. Perhaps similar to:

```js
const expIter = (b, n) => {
  let counter = n, product = 1
  while (counter--) {
    product *= b
  }
  
  return product
}
```

### <a name="composition">Function Composition</a>

> Often the same programming pattern will be used with a number of different procedures. To express such patterns as concepts, we will need
> to construct procedures that can accept procedures as arguments or return procedures as values. Procedures that manipulate procedures
> are called *higher-order procedures*.

Whether we're dealing with a function or a list or an integer
or some other primitive, we're ultimately performing a series of instructions that manipulate the registers and auxiliary memory structures
located in the hardware of our computers.

The implication of this is that, if our logic is tight, we can then start viewing our higher-level abstractions as procedures
being applied to a pattern of other procedures.

A cool example:

```js
const iter = (a, b, transform, reduce) => {
  if (a >= b) return transform(a)
  else return reduce(transform(a), iter(++a, b, transform, reduce))
}
```

The above function is an iterator from some integer `a` to `b`. On each iteration, it applies a transform function of the programmer's choice to the current integer (let's say `i`), then applies a programmer-supplied reduce function to this transformed `i`, moving along until we reach the end of the range of integers.

It can be used to define some `factorial` process:

```js
const factorial = x => {
  const identity = x => x
  const mult = (a, b) => a * b
  return iter(1, x, identity, mult)
}
```
Iterate from 1 to `x`, multiplying by `identity(i)` as you go. `factorial(4) -> identity(1) * identity(2) * identity(3) * identity(4)`

### <a name="search">Search as an Interval Minimizing Function</a>

While explaining an algorithm to find the root of a continuous function (aka `x where f(x) = 0`):

> Since the interval of uncertainty is reduced by half at each step of the process, the number of steps required grows as
> O(log(*L*/*T*)), where *L* is the length of the original interval and where *T* is the error tolerance.

Then, while explaining an algorithm to find the global maximum of a unimodal function (single-peak):

> [...] we compare the function value at two intermediate points *x* and *y* in order to reduce the interval.
> Thus, each reduction step requires that we find the value of the function at two intermediate points.

Search is the process of minimizing an interval to within a particular error tolerance.

In a sorted list of unique integers where the error tolerance is 0, your initial
interval lies between the first element and the last. A naive search of this type of list would look like:

```js
const list = [1, 2, 3, 4, 5]
const search = (x, xs, tolerance=0) => {
  let a = 0, b = xs.length-1
  
  while (a <= b) {
    if (Math.abs(xs[a++] - x) <= tolerance) return true
    if (Math.abs(xs[b--] - x) <= tolerance) return true
  }
  
  return false
}
```

Of course this is a pretty shitty search algorithm -- I'd be better off just starting from the beginning and iterating until I find what I'm looking for.

Still, this algorithm works. But we can more efficiently reduce the interval via a binary search. This evolution requires a third pointer `middle` that incrementally cleaves the interval (search space) in half. 

```js
const list = [1, 2, 3, 4, 5]
const binarySearch = (x, xs, tolerance=0) => {
  let a = 0, b = xs.length-1, middle
  
  while (a <= b) {
    middle = Math.floor((a + b)/2)
    if (Math.abs(xs[middle] - x) <= tolerance) return true
    
    if (x > xs[middle]) {
      a = middle + 1
    } else {
      b = middle - 1
    }
  }
  
  return false
}
```

A search can often be re-characterized as minimizing the interval of uncertainty. You improve the search by identifying a better interval to minimize, then minimizing that interval more and more efficiently. Then depending on the type of data and requirements of your search, you think about what room for error you can allow.

### <a name="data-objects">Data Objects and Message Dispatching</a>

> We see that the ability to manipulate procedures as objects automatically provides the ability to represent compound data.
> This may seem a curiosity now, but procedural representations of data will play a central role in our programming repertoire.

Data abstractions become extremely useful constructs when we're managing the flow and access patterns of information we're dealing with. What if we wanted to implement a `Fraction` data type as a numerator-denominator pair? We could define the following procedure:

```js
const fraction = (numerator, denominator) => (dispatch) => dispatch(numerator, denominator)
```

This procedure takes a numerator and denominator, then returns a function. This returning function takes as an argument a dispatcher function (that a programmer can define) that will apply the numerator and denominator as its arguments.

Now we have a procedure that has abstracted out the concept of a fraction. The numerator and denominator are simply variables bound by the `fraction` procedure. If we want to access the numerator, we could define a procedure such as:

```js
const getNumerator = (z) => z((n, d) => n)

// const f = fraction(3, 4)
// getNumerator(f) = 3
```

The Fraction data type return a function that accepts a dispatcher which has access to the numerator and denominator as arguments. In `getNumerator`, the dispatcher simply returns the numerator (first arg).

Similarly, we can define a denominator dispatcher:

```js
const getDenominator = (z) => z((n, d) => d)

// const f = fraction(3, 4)
// getDenominator(f) = 4
```

And now we could add more specific operations to further manipulate the Fraction data type.

```js
const addFractions = (a, b) => {
  const num_a = getNumerator(a), denom_a = getDenominator(a)
  const num_b = getNumerator(b), denom_b = getDenominator(b)
  
  const numerator = ( (num_a * denom_b) + (num_b * denom_a) )
  const denominator = ( denom_a * denom_b )
  
  return fraction(numerator, denominator)
}

const printFraction = (fraction) => console.log(getNumerator(fraction) + '/' + getDenominator(fraction))
```

But say we invoke `fraction(6, 8)`. This fraction in its reduced form is `3 / 4`, but do we make that computation at construction time or at access time? That's up to the programmer to decide, depending on read/write ratio. If you anticipate writing fractions more often than reading them, then maybe reducing upfront is unnecessary. But if you're reading fractions a lot, you don't want to slow things down to constantly reduce the fraction on access, so in this case you may consider reducing the fraction at construction time.

As Abelman/Sussman say, all of this "further blurs the distinction between 'procedure' and 'data'."

### <a name="hierarchical-data">Hierarchical Data Structures</a>

Working with Lisp reorders your thinking to work in binaries/pairs. Some extremely powerful procedures can be cooked up from the ground up.

```js

// construct a pair
const cons = (x, y) => (dispatch) => dispatch(x, y)

// access first/last item in pair
const car = (z) => z((x, y) => x)
const cdr = (z) => z((x, y) => y)

// examples:
let x = cons(1, 2)
car(x) // 1
cdr(y) // 2

let y = cons(x, cons(3, 4))
car(y) // cons(1, 2)
cdr(y) // cons(3, 4)
cdr(car(y)) // 2
cdr(cdr(y)) // 4

// the List primitive constructs a pair of pair of pairs ... of arbitrary length
const some_list = cons(1, cons(2, cons(3, null))) = list(1, 2, 3)

// append/concatenate two lists
const append = (a, b) => null?(a) : b ? cons(car(a), append(cdr(a), b))

// list operations
const nth = (list, n) => null?(list) ? null : (n == 0 ? car(list) : nth(cdr(list), n - 1))
const len = (list) => null?(list) ? 0 : 1 + len(cdr(list))
const includes = (list, x) => null?(list) ? false : (car(list) == x ? true : includes(cdr(list), x))

// functional operations (yee haw)
const map = (list, fn) => null?(list) ? null : cons(fn(car(list)), map(cdr(list), fn))
const filter = (list, fn) => null?(list) ? null : (fn(car(list)) ? cons(car(list), filter(cdr(list), fn)) : filter(cdr(list), fn))
const reduce = (list, fn, memo) => null?(list) ? memo : reduce(cdr(list), fn, fn(memo, car(list)))

// examples
const xs = list(1, 2, 3, 4, 5)
const odds = filter(xs, (x) => x % 2 != 0) // odds: cons(1, cons(3, cons(5, nil))) = list(1, 3, 5)
const doubles = map(xs, (x) => x * 2) // doubles: cons(2, cons(4, ... you get the idea
const sum = reduce(xs, (total, x) => total + x, 0) // sum: 1 + 2 + 3 + 4 + 5 = 15
```

This is thinking in "binary" hierarchies (a collection is a pair formed by one thing and a collection of another pair formed by one thing and a collection of another pair formed by ... etc).

Pros: this is a powerful problem-solving mindset: it reduces a problem to a set of subproblems. You can literally "feel the bits sliding in between your fingers."

Cons: "LISP programmers know the value of everything and the cost of nothing" - Alan Perlis. Recursive procedures, as we've learned, aren't always the fastest or most memory-efficient.

### <a name="symbolic">Symbolic Representation</a>

> Symbolic differentiation is of special historical significance in Lisp. It was one of the motivating examples behind the
> development of a computer language for symbol manipulation. Furthermore, it marked the beginning of the line of research that
> led to the development of powerful systems for symbolic mathematical work.

The early history of computing owes a lot to logicians and mathematicians, especially in the realm of calculus. As my understanding of it goes, calculus preoccupies itself with the study of continuous change -- how a quantity moves from one discrete value to the next.

We can use computing to approximate differentials with increasing precision. To be able to do this though, we'd need a way to represent data abstractly without immediately binding values to symbols until we absolutely needed to.

In other words, how do can we work with and evaluate expressions like this abstractly (aka without actual values attached to the variables?):

```
f(x) = x^2 when x = (a + b)^3
```

In Lisp, this becomes possible with the use of the primitive `quote`:

```lisp 
(car (quote (list (1 2 3)))) ; 1
(cdr (quote (list (1 2 3)))) ; (2 3)
(car '(list (1 2 3))) ; 1
(cdr '(list (1 2 3))) ; (2 3)
```

`quote` provides an abstract layer for representing data without needing to evaluate the underlying data. These become *symbols*, hence expressions can now be *represented symbolically*. You could, for example, define a series of functions that create and destructure symbolic representations of sums:

```lisp
(define (make-sum a b) (list '(+ a b))) ; (+ a b) now an unevaluated list 
(define (sum? s) (if (not (atom? s)) (eq? (car s) '+) nil)) ; check if first element is a '+' symbol
(define (addend s) (cadr s)) ; ex. (addend '(+ 1 2)) => (car (cdr '(+ 1 2))) => (car '(1 2)) => 1
(define (augend s) (caddr s)) ; ex. (augend '(+ 1 2)) => (car (cdr (cdr '(+ 1 2)))) => (car (cdr '(1 2))) => (car '(2)) => 2

(make-sum '(x + 1) '(y + 1)) ; you now symbolically represented the sum (x + 1) + (y + 1) without needing to define either variable
```

Abelson/Sussman then touch upon a vexing problem with symbolic representation -- how do you determine whether two symbols are equal? How does a programming language actually implement these equality checks? Alas, they defer this discussion for later.

### <a name="set"> Sets </a>

A set is a collection of distinct objects. We define it in terms of its interface, which is: `has?`, `add`, `union`, and `intersection`. How do we represent this compound data structure?

One option is to implement it as an unordered list:

```js
const make_set = (x) => cons(x || null, null)

const has = (x, set) => {
  if (isNull(set)) return false
  else if (x === car(set)) return true
  else return has(x, cdr(set))
}

const add = (x, set) => {
  if (has(x, set)) return set
  else return cons(x, set)
}
```

Representing a set as an unordered list carries an `O(n)` on all `add` and `has` operations. 

We could also implement a set as an ordered list.

```js
// same as above
const make_set = (x) => cons(x || null, null)

// assumes sorted
const has = (x, set) => {
  if (isNull(set)) return false
  else if (x === car(set)) return true
  else if (x < car(set)) return false
  else return has(x, cdr(set))
}

// sorting algorithm
const add = (x, set) => {
  if (isNull(set)) return make_set(x)
  else if (x === car(set)) return set
  else if (x < car(set)) return make_set(x, cdr(set))
  else return make_set(car(set), add(x, cdr(set)))
}
```

On `add`, we incur some time cost to sort the list (`O(n log(n))`. But now the set has some underlying order, and all subsequent operations don't have to always scan through the entire set.

We can do better and implement a set as a binary tree:

```js
const tree = (x, l, r) => list(x, l, r)
const entry = (tree) => car(tree)
const left_branch = (tree) => car(cdr(tree))
const right_branch = (tree) => car(cdr(cdr(tree)))

const make_set = (x, l, r) => tree(x, list(), list())

const has = (set, x) => {
  if (isNull(set)) return false
  else if (x === entry(set)) return true
  else if (x < entry(set)) return has(left_branch(set), x)
  else return has(right_branch(set), x)
}

const add = (x, set) => {
  if (isNull(set)) return make_set(x)
  else if (x === entry(set)) return set
  else if (x < entry(set)) return make_set(entry(set), add(x, left_branch(set)), right_branch(set))
  else return make_set(entry(set), left_branch(set), add(x, right_branch(set)))
}
```

And this representation of a set gives us the magical `O(log(n))` lookup time.

Each of these set representations are immutable -- that is, adding or removing elements from the set returns a *new* set with the target element added/removed.

Another thing to note is that the performance benefits of a binary tree only apply if the tree remains balanced over the long-haul. Imagine inserting elements in the order 1 - 2 - 3 - 4 - 5. We'd just have a linked list (therefore `O(n)` lookup). This is where balancing tree algorithms find much of their utility.

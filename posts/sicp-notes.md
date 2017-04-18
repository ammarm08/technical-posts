I started my programming journey in mid-2015. My wisest CS friends recommended *The Structure and Interpretation of Computer Programs*, but it entirely flew over my head. Now I'm revisiting it 18 months later, and it's reading like the Talmud.

In this post, I'll be listing (and trying to keep up to date), some "aha!" moments. All mistakes are mine, please call them out when you see them :)

### Chapter 1: Building Abstractions with Procedures
- [Processes, Programs, and Data](#processes)
- [Tail-Recursive Procedures and Registers](#recursion)
- [Higher-Order Procedures and Composition](#composition)
- [Search as an Interval Minimizing Function](#search)
### Chapter 2: Building Abstractions with Data
- [Data Objects and Message Passing](#data-objects)
- [Hierarchical Data Structures](#hierarchical-data)
- [Symbolic Representation](#symbolic)
- [Representing Sets](#set)
- [Type Systems](#manifest)
### Chapter 3: Modularity, Objects, and State
- [Local State and Assignment](#state)
- [Environments and Contexts](#environment)
- [Representing Tables](#tables)
- [Streams](#streams)
- [Infinite Streams](#infinite-streams)
- [Normal-Order and Delayed Evaluation](#eval)
### Chapter 4: Metalinguistic Abstraction
### Chapter 5: Computing with Register Machines


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
to the "deferred operation" (`multiplying b * expR(b, n -1)`). This gets computationally expensive because each recursive call thus needs to maintain knowledge of this in its stack frame. These stack frames build up as each recursive function call maintains its own stack frame.

In the latter case, all state transformations are represented in the parameters of each recursive call. As each function call goes along,
it carries with it all state transformations.

A smart compiler that handles tail-recursive procedures, then, will notice that a tail-recursive procedure's last instruction simply returns the value computed by calling another function. So the compiler will call that function and not reserve any stack space for each recursive call.

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

### <a name="manifest"> Manifest Types, Dispatching on Type, and Message Passing </a>

> One way to view data abstraction is as an application to program design of the "principle of least commitment." By setting up
> selectors and constructors as an abstraction barrier, we can defer to the last possible moment the choice of a concrete representation
> for our data objects and thus retain maximum flexibility in our system design.

If we want to continue building abstract data types like those we've run into in the wild (binary trees, sets, arrays, streams, etc), then we need a way to represent them, identify them, and create rules on how we can use them. Can I multiply a number by a string? Can I add a list to a set? How do we represent valid (and invalid) operations?

A simple abstraction we can use to represent our data objects could be an `attach-type` function:

```js
const attach_type = (type, contents) => cons(type, contents)
const type = (datum) => !atom(datum) ? car(datum) : Error(`Bad typed datum -- TYPE, ${datum}`)
const contents = (datum) => !atom(datum) ? cdr(datum) : Error(`Bad typed datum -- CONTENTS, ${datum}`)
```

This very basic type system means that each data object is a tuple, with the `car` maintaining type information, and the `cdr` maintaining the contents/payload. Now we can identify our data types like this:

```js
const is_array = (z) => type(z) === 'array'
const array = type('array', make_array())
```

This is very reminiscent of what Eric Raymond writes about in *The Cathedral and the Bazaar* re: the primary of data structures:

> Smart data structures and dumb code works a lot better than the other way around.

Of course, a data structure isn't just a vessel for data; it is also a means by which to operate on the data. Ways we can define expected behavior of certain operations are generally separated by either *dispatching on type* or *message passing*.

Here's an example of dispatching on type against "real" and "imaginary" numbers:

```js
const op_table = make_table() // don't worry about this for now

// add to op_table using a predefined 'put' procedure
put('rectangular', 'real-part', real-part-rectangular)
put('rectangular', 'imag-part', imag-part-rectangular)

const operate = (op, obj) => {
  let proc = get(type(obj), op) // from the op_table, get the op for the given obj type
  return !isNull(proc) ? proc(contents(obj)) : Error(`Operator undefined -- OPERATE, ${list(op, obj)}`)
}

const real_part = (obj) => operate('real-part', obj)
const imag_part = (obj) => operate('imag-part', obj)
```
The main thing to notice is that this program maintains a global table with operations that are available to every type. So we look up the object type, then look up the operation we're trying to run. If this operation is found for that type, we run that procedure; otherwise we throw an error.

In contrast, messaging passing looks like this:

```js
const make_rectangular = (x, y) => {
  const dispatch = (m) => {
    if (m === 'real-part') return x
    else if (m === 'imag-part') return y
    else return `Unknown op -- MAKE-RECTANGULAR, ${m}`
  }
  
  return dispatch
}
```

When we dispatch by type, we look up an object's type, then look up the desired operation within this entry.
When we dispatch by message passing, the object itself encloses its own valid operations.

A brief note about interoperability and coercion -- now that we have a couple ways to get started on a type system, we have to think about the strategies we wish to use to run an operation against several data objects. Should we only allow objects of the same type to operate against each other? Or will we allow coercion?

The pros of fully static typing and operability are that it is strict on what it allows, which means less space for programmer error and possibly an easier time for the compiler to optimize code paths. The cons are that a programming language designer has to think very carefully about how to implement these data types and how she imagines programmers to be using them day to day. It also limits programmer flexibility in how he writes his programs.

The pros of coercion are that we can be more permissive with how we program, for example, multiplying strings by integers (`3 * 's' === 'sss'`). Some cons are that i) compilers may have a tougher time optimizing dynamically typed languages, and ii) type coercion can involve defining type hierarchies (x is a type of y is a type of z), which can further complicate a programmer's understanding of what is actually going on under the hood.

### <a name="state"> Local State and Assignment </a>

> In a system composed of many objects, the objects are rarely completely independent. Each may influence the states of others through
> interactions, which serve to couple the state variables of one object to those of other objects.

So far, Abelson & Sussman have ignored assignment and state. All of our data structures and procedures have been immutable. To insert or remove an element from a set, for example, we constructed a *new* set, bound to a new variable.

However, certain data objects have coupled relationships (ex. withdrawing from a bank requires a stateful "balance"). This balance must be able to be mutated and accessed and persisted from outside of the `withdraw` procedure.

Up until now, our concept of variables and values was this -- that a variable was merely a symbolic representation of an underlying value. We could continually evaluate our programs using the substitution model, knowing that whenever we encountered `x`, it would indeed be `4` or whatever we bound it to. 

But *assignment* changes the mental model of computation -- variables aren't just symbols for values. Rather, they are *locations* for mutable values.

Compare immutable variable expression ...

```js
const x = make_set()
const y = add(5, x)
const z = add(6, y)

// x is empty, y has 5, z has 5 and 6
```

... to mutable variable assignment:

```js
let x = make_set()
add(5, x)
add(6, x)

// x is now a set with 5 and 6
```

### <a name="environment"> Environments and Contexts </a>

> An environment is a sequence of frames. Each frame is a table (possibly empty) of bindings, which associate variable names 
> with their corresponding values. (A single frame may contain at most one binding for any variable.) Each frame also has a
> pointer to its enclosing environment, unless, for the purposes of discussion, the frame is considered to be global.

In the following example:

```js
const map = (f, list) => isNull(list) ? null : cons(f(car(list)), map(f, cdr(list)))

const square = (x) => x * x

const map_square = (list) => map(square, list)
```

How does `square_map` know anything about `square` or `map`? How does each procedure know anything about its arguments? This is where *environments* come into place. These are sequences of tables that store bindings to particular values that are progressively looked up, first locally, then to each subsequent parent frame until no more frames can be looked up (at which point the program assumes that value a procedure called is undefined). Roughly like this ...

```bash
GLOBAL FRAME:
... 
a bunch of other bindings
...

'map': map,
'square': square
'map_square': map_square

MAP_SQUARE FRAME:
'parameters': { 'list': some_list }
'parent': pointer_to_global_frame
```

To visualize this, take a look at this [diagram](https://mitpress.mit.edu/sicp/full-text/book/ch3-Z-G-2.gif)

### <a name="tables"> Representing Tables </a>

A table is a generic data structure that stores data as key-value or key-record relations. In the above example with environments and frames, looking up `square` in the global table would retrieve the "record" or "value" associated with it (in this case, `lambda (x) x * x`).

So how can we implement this? One straightforward method: a one-dimensional table with linear gets and sets:

```js

// first item in list is a TABLE symbol. thus recurring on cdr(table) lets us linearly scan the records
const make_table = () => list('TABLE')

const lookup = (key, table) => {
  const record = assq(key, cdr(table))
  return isNull(record) ? null : recrd
}

const assq = (key, records) => {
  if (isNull?(records)) return null
  else if (key == car(car(records))) return car(records)
  else return assq(key, cdr(records))
}

const insert = (key, value, table) => {
  const record = assq(key, cdr(table))
  return isNull(record) ? set-cdr(table, cons( cons(key, value), cdr(table) )
                        : set-cdr(record, value)
}
```

In the above example, a table could be visualized as a list of key-value pairs:

```bash
[ TABLE, 
  [ ('k1', 'v1'), 
    [ ('k2', v2'), 
      [ ..., nil ] 
    ] 
  ] 
]
```

Another useful way to represent tables is as two-dimensional data structures. In this representation, any given value is indexed by *two* keys. The first key maps to a particular subtable, and the second key maps to a value/record in that subtable. As we'll see later, this form of "misdirection" gives our tables more flexibility in how they store and retrieve data. For example, we could certainly improve performance on lookups by somehow arranging data to allow binary searches.

### <a name="streams"> Stream Operations </a>

> Instead of using objects with changing local state, we construct a stream that represents the *time history* of the 
> system being modeled. A consequence of this strategy is that it allows us to model systems that have state without ever
> using assignment or mutable data.

When we define a list, we already know a lot about it -- namely, its length, elements, and order.

Here, we define another data structure called a stream, which treats data as *signals* flowing independently through stages. They can be *enumerated* one by one, *filtered* to keep only valid signals, *transduced* to change each signal into some standard form/shape, then *accumulated* into a single form.

Before showing a basic implementation of a stream, Abelson & Sussman instead us through these stream operations (enumerators, filters, tranducers, and acccumulators).

```js
const map = (fn, stream) => {
  return emptyStream(stream) ? 
         cons_stream()  : 
         cons_stream(head(stream), map(fn, tail(stream)))
}

const filter = (predicate, stream) => {
  return emptyStream(stream) ? 
         con_stream()   : 
         (predicate(head(stream) ?
          cons_stream(car(stream), filter(predicate, tail(stream)) :
          filter(predicate, tail(stream))
}

const accumulate = (fn, init, stream) => {
  return emptyStream(stream) ? 
         init :
         fn(head(stream), accumulate(fn, init, tail(stream))
}
```

As we'll see below with the stream implementation, we can apply these operations on streams *just in time* using delayed evaluation. That means `map(square, some_stream)` doesn't map the entire stream upfront, but only as the stream is processed signal by signal.

### <a name="infinite-streams"> Infinite Streams </a>

> Our implementation of streams will be based on a special form called `delay`. Evaluating the form `delay <exp>` does not evaluate the
> expression `<exp>`, but rather returns a so-called *delayed object*, which we can think of as a "promise" to evaluate `<exp>` at
> some future time.

A basic stream implementation that can handle infinitely long streams.

```js
const cons_stream = (a, b) => cons(a, delay(b))
const head = (stream) => car(stream)
const tail = (stream) => force(cdr(stream))

const force = (delayed_object) => delayed_object()

const memoize = (proc) => {
  let ran, result
  return () => {
    if (!ran) {
      result = proc
      ran = !ran
      return result
    }
    
    return result
  }
}

const delay = memoize(fn)
```

Accessing the `head` of the stream is familiar (a simple `car` op).

Accessing the rest of the stream involves using the special form `delay`, which informs the interpreter to delay evaluation of (`cdr stream`). The special form `force` informs the interpreter when to actually evaluate.

The `memoize` function is a delayed-eval optimization technique (often referenced as `call-by-need` evaluation). It prevents a procedure from having to compute the same operation more than once.

Using delayed evaluation, we can redefine many of our stream operations, construct infinitely long streams, and merge/combine multiple streams.

For example, we can model the *Sieve of Eratosthenes* using infinite streams:

```js
const sieve = (stream) => {
  return cons-stream(head(stream), sieve(
    filter(x => x % head(stream) !== 0,
           tail(stream))))
}

const integers-from = (n) => cons-stream(n, integers-from(n + 1))

const primes = sieve(integers-from(2))

```

The Sieve of Eratosthenes constructs a stream of prime numbers using a stream that progressively filters the tail stream of all integers not divisible by the head.

So when the head is 2, the tail stream, when evaluated, will not include any integers divisible by 2. When the head is 3, the tail stream, when evaluated, will not include any integers divisible by 2 or 3, etc etc.

### <a name="eval"> Normal-Order and Delayed Evaluation </a>

To-do

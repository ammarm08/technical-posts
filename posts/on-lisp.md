Notes from Paul Graham's *On Lisp*

> The productivity of a group of programmers does not grow linearly with its size.

Graham makes the case that a system grows more complicated as more people become involved in building ("too many chefs in the kitchen"). Programming, as Common Lisp encourages it, is a solve-while-you-build process -- you refactor your thoughts and your code *as you go along*. He points to the sheer importance of small teams that pick abstractions well and understand this "refactoring" process intimately.

> A functional program tells you what it wants. An imperative program tells you what to do.

In software and in life, this appears to be a very important distinction. Functional programs help maintain referential transparency. For example, you could have an imperative `sum` program that looks like this:

```js
const sum = xs => {
  let sum = 0;
  for (let x of xs) {
    sum += x;
  }
  
  return sum;
}
```

In the simple example above, you learn about the "what is being done" before figuring out "what is desired". This could in more complex programs cause nasty bugs, though in this case the above solution works just fine. But a more functional version of `sum` might take the following shape:

```js
const add = (acc, x) => acc + x;
const sum = xs => xs.reduce(add, 0)
```

This is more referentially transparent. We want the sum of a list of integers; this is computed by adding each integer in the list.

> As capital expenditures, utilities demand extra attention. [...] A language augmented with the right utiliites will lead us to write more abstract programs.

Graham is a proponent of writing bulletproof, generalized-case utilities that can be reused all across your program. To illustrate this, he uses the example of a program that returns a list of bookshops in the town nearest to the user. A first stab:

```lisp
(let ((town (find-if #'bookshops towns)))
  (values town (bookshops town)))
```

This works. First, `town` is bound to the result of finding the first case where there's a bookshop found in a list of towns. Then it returns the name of that town, as well as the bookshops in that town. Notice, though, that `bookshops` is called twice.

So we rewrite this.

```lisp
(defun find-books (towns)
  (if (null towns)
      nil
      (let ((shops (bookshops (car towns))))
        (if shops
          (values (car towns) shops)
          (find-books (cdr towns))))))
```

Now we only do one pass through towns. If there are no towns in the list, we return `nil`. Otherwise, we check the first town in the list. If it has bookshops in it, we return. If not, we call `find-books` on the rest of the list of towns.

This is much better, but we at this point may realize that this procedure isn't useful just for bookshops, but also for any generalizable search for a list within a list.

```lisp
(defun find (fn list)
  (if (null list)
    nil
    (let ((val (funcall fn (car list))))
      (if val
        (values (car list) val)
        (find fn (cdr list))))))

(find #'bookshops towns)
(find #'bugs code)
```

And now we can use the `find` function anywhere in our program, changing its implementation as we see fit. This entire process occurs incrementally, as our first efforts show us a solution's potential as a generalizable procedure.

## Writing Macros

> The definition of a macro is essentially a function that generates Lisp code -- a program that writes programs. From those small
> beginnings arise great possibilities, and also unexpected hazards.

How would we write a macro for `dolist` with the desired shape:

```lisp
(dolist (x '(a b c))
  (print x))
```

That is, given each element `x` in a list `lst`, and a body `body`, how can we iterate across the list to execute a certain body of functions on each `x`? Of course we could try and just do a `(defun dolist ...)`. But what if the syntax rules of this new operation doesn't translate very cleanly to standard out-of-the-box Lisp? Macros let us define our *own* languages using our own syntax -- they take code as parameters, and based on the rules we provide them, inform the compiler/interpreter how to expand and substitute Lisp syntax at runtime/compile-time.

```lisp
(def macro dolist ((var list &optional result) &body body)
  `(progn
    (mapc #'(lambda (,var) ,@body) ,list)
    (let ((,var nil))
      ,result)))
```

The above macro takes as parameters a list `(var list &optional result)` and a `body`. It then returns an unevaluated `progn` function that maps across every element in the list, and for each element `var`, applying it to the entire `body`. 

So here's the macro usage and macro-expansion:

```lisp

; macro usage
(dolist x '(a b c)
  (print x))
  
; macro expanded
(progn
  (mapc #'(lambda (x)
    (print x)) '(a b c))
  (let ((x nil))
    nil)))
```

Things to note:

1. The macro destructures the first argument into `(var list &optional result)`.
2. The macro uses backtick notation to keep most things unsubstituted *except* for `var` + `list` (get subbed in) and `body` (gets spliced in)
3. The macro uses `&body body`, which is basically saying (any arguments beyond this point, put them in a list and call taht list `body`). It's essentially the same as `&rest <some-name>`.

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

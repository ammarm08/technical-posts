Notes from Paul Graham's *ANSI Lisp*

## Bottom-Up Programming

> Bottom-up programming leads naturally to extensible software. [...] Instead of devoting all your effort to writing a single
> monolithic application, you devote part of your effort to building a language, and part to writing a (proportionately smaller)
> application on top of it. What's specific to this application will be concentrated in the topmost layer.

## New Approaches to Solving Problems

> In the old model, you are betting that specs won't contain serious flaws, and that implementing them will be a simple matter of
> translating them into code. Experience has shown this to be a very bad bet indeed. It would be safer to bet that specifications
> will be misguided, and that code will be full of bugs.
> [...]

> This is just what the new model of programming does assume. Instead of hoping that people won't make mistakes, it tries to make
> the cost of mistakes very low. The cost of a mistake is the time required to correct it.

## No Pointers in a World of Pointers

> The reason Lisp has no pointers is that every value is conceptually a pointer. When you assign a value to a variable or
> store it in a data structure, what gets stored is actually a pointer to a value.

Example #1:
```lisp
(setf x '(a b c))
```
`x` is stored in memory as a *pointer* to the memory address of the list `'(abc)`

Example #2:
```lisp
(setf y x)
```
`y` is stored in memory as a copy to the same pointer that `x` had above. Both `x` and `y` point to the *same* list.

On a related note, Lisp has two primary equality functions: `eql` and `eq`. `eql` checks the equivalence of two expressions if they were *printed* out. `eq` checks if two pointers are the same. So:

```lisp
(eq x y) ; True! Examples #1 and #2. x and y point to the same memory address
(eql x y) ; True! If they point to the same place, they print the same thing

(eq x '(a b c)) ; False! the second argument does not point to the SAME memory address as x does
(eql x '(a b c)) ; True! Nonetheless, the two print out equivalent characters
```

## A Compression Algorithm

The key mental leap I'm finding with Lisp is understanding the "shape" of a problem.

Take a list of bits which we want to compress somehow. A straightforward compressor would convert
`11101000` into something that resembles `[3, 1] 0 1 [3, 0]`. That is, when a sequence repeats, we
compress that into a tuple.

The shape this takes I mentally imagine as a main process (iterating across the list) with an embedded process (checking for sequence repeats). Perhaps like the Earth's path around the Sun -- a main elliptical orbit throughout which the Earth rotates.

In the example below, `compress` gets the process going, and `compr` keeps track of the number of bits `n` in the current sequence. If the next bit in the list is the same as the current bit, we increment `n`; otherwise, we lay down a `[n, bit]` tuple and check the rest of the list.

```lisp
(defun compress (x) 
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

; example
(compress '(1 1 1 0 1 0 0 0 0 1)) ; returns ((3 1) 0 1 (4 0) 1)
```

The shape of uncompression is a linear map. We iterate straight across the compressed input, and for each element in the input list, we check if its a tuple or not. If it is, we construct an `n`-length list of `bit` bits. Otherwise we just return the current element. We accumulate these results into a flattened list.

```lisp
(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
        (if (consp elt)
            (append (apply #'unroll elt) rest)
            (cons elt rest)))))

(defun unroll (n bit)
  (if (zerop n)
      nil
      (cons bit (list-of (- n 1) bit)))


; example
(uncompress '((3 1) 0 1 (4 0) 1)) ; returns (1 1 1 0 1 0 0 0 0 1)
```

## Shortest Path Algorithm

```lisp
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

; main problem: which path of nodes lead to shortest distance from start to end?
(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (bfs end
                  (append (cdr queue)
                          (new-paths path node net))
                   net))))))

; subproblem: which nodes can we immediately get to from 'node'?
(defun new-paths (path node net)
  (mapcar #'(lambda (n) (cons n path))
          (cdr (assoc node net))))

; example
(shortest-path 'a 'c '('(a b c) '(b c) '(c d)))  ; returns (A C)
```

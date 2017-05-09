Notes from Peter Norvig's *Paradigms of Artificial Intelligence Programming*

## Evaluation Rules for Lisp

1. Every expression is a *list* or an *atom*

```lisp
(1 2 3 4) ; list
1         ; atom
(+ 1 2)   ; list
nil       ; list (and atom!)
```
2. Every evaluated list is either a *special form expression* or a *function expression*

```lisp
(setf x 10) ; special form expression with special form operator "setf"
(append '(1 2) '(3 4)) ; function expression with function "append"
```
3. Special form expressions have *special form operators*

4. A *function application* evaluates its arguments before applying the function to them

```lisp
(+ (* 3 4) (/ 10 2)) ; this entire expression is read by interpreter/compiler before any evaluation happens
```
5. Every atom is a *symbol* or a *non-symbol*

```lisp
4        ; non-symbol
nil      ; non-symbol
`(a b c) ; symbol
`John    ; symbol
```
6. Every symbol evaluates to the most recent value assigned to it

7. Every non-symbol evaluates to itself

## Context-Free Phrase-Structure Grammars and Kleene Star Notation

Imagine a simplified language with the following rules ...

```bash
Sentence => Noun-phrase + Verb-phrase
Noun-phrase => Article + Adj* + Noun + PP*
Verb-phrase => Verb + Noun-phrase
PP* => _, PP + PP*
PP => Prep + Noun-phrase
Adj* => _, Adj + Adj*
Adj => <list of adjectives>
Prep => <list of prepositions>
Verb => <list of verbs>
Noun => <list of nouns>
```

A sentence is composed of a Noun-phrase followed by a Verb-phrase.
A noun-phrase consists of an Article followed by an arbitrary number of Adjectives, then followed by a Noun, then an arbitrary number of preposition phrases.

Using Lisp, we can define these rules into a simple grammer using `defparameter`:

```lisp
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun)
    (verb-phrase -> (Verb noun-phrase)
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)))

(defvar *grammar* *simple-grammar*)

; sample access
(assoc 'noun *grammar) ; will return list of Nouns
```

To generate arbitrary sentences using this rule-based approach, we define a few helper functions:
```lisp
(defun rule-lhs (rule) (first rule))
(defun rule-rhs (rule) (rest rule))
(defun rewrites (category) (rule-rhs (assoc category *grammar*)))
(defun random-elt (lst) (elt lst (random (length lst))))
(defun mappend (fn lst) (apply #'append (mapcar fn lst)))
```

Finally, a `generate` function glues all the relevant rules together:
```lisp
(defun generate(phrase)
  (cond ((listp phrase) (mappend #'generate phrase))
        ((rewrites phrase) (generate (random-elt (rewrites phrase)))
        (t (list phrase))))

(generate 'sentence) ; sample use
```

We spend most of our time upfront defining our rules and writing the "language" for our grammar. Making changes to our grammar, then, is as simple as changing our list of rules.

This is an example of data-driven programming. The data drives what the program does next.

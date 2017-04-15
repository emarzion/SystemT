# An interpreter for Gödel's System T

Try in browser [here.](https://emarzion.github.io/SystemT/)

System T is a system of arithmetic originally formulated by Gödel in order to study the computational content of proofs in Heyting Arithmetic.  System T extends the typical notion of a primitive recursive function to the entire hierarchy of finite types, although it also defines more number-theoretic functions than the traditional primitive recursive functions (for instance, the Ackermann function is definable in System T).

To read more about System T, chapter 7 of [Proofs and Types](http://www.paultaylor.eu/stable/prot.pdf) is a nice resource.

## Syntax

Our presentation of System T will include the following types: 

```
types :=  N
         |B
         |U
         |s1 -> s2
         |s1 * s2
         |s1 + s2
```
with `N` being the type of natural numbers, `B` being the type of booleans, `U` being a unit type, and `-> , * , +` producing function types, product types, and sum types, respectively.

Terms are built up from constants using the typed lambda calculus.  As is standard, parentheses are omitted as much as possible, and application is left-associative, so for instance `f x y` will be written in place of `((f x) y)`.  The keyword `fun` denotes a lambda abstraction, and the token `=>` separates the abstracted variable from the remainder of the term. Multiple abstractions can be stacked under a single `fun`, e.g. one can write `fun f x => f x` instead of `fun f => fun x => f x`.

### Naturals

Natural numbers are represented by their usual decimal representations.  `S : N -> N` denotes the successor function.  System T allows for iteration over arbitrary type `a`, and we denote this operation by `iter : N -> (a -> a) -> a -> a`, with the rule that `iter n f x = f^n x` i.e. `f` applied to `x` `n` times in succession.

### Booleans

The two booleans are given the names `false`  and `true`.  For arbitrary type `a`, there is an if-then-else operator `if : B -> a -> a -> a` with the rules `if true x y = x` and  `if false x y = y`.

### Unit

The unit type has a single term `tt`.

### Products

For any two types `a,b` there is a pairing constant `pair : a -> b -> a * b`.  When `pair` is fully applied, one may use the syntax `[x,y]` in place of `pair x y`.  The two projections are named `p1` and `p2` and are given the rules `p1 [x,y] = x` and `p2 [x,y] = y`.

### Sums

For any two types `a,b` there are inclusion terms `i1 : a -> a + b` and `i2 : b -> a + b`.  Arguments of a sum type are handled using `case`:  for `f : a -> c` and `g : b -> c` arbitrary, we take it that `case f g (i1 x) = f x` and `case f g (i2 y) = g y`.

## Some tips on defining functions

### Using `iter`

Iteration is most useful for inductively defining functions `N -> a` where only the function's previous value is needed at the inductive step.  Most generally, if we want to define a function `f` by the equations

```
f 0   = t
f n+1 = T[f n]
```

where `T` is an already defined expression containing some number of occurrences of `f n` then, we may define

```
f := fun n => iter n (fun x => T[x]) t
```

where `T[x]` denotes the expression resulting from subbing in the fresh variable `x` in for each occurrence of `f n`. For instance, a function `double` which doubles its input can be specified by the following two equations:

```
double 0   = 0
double n+1 = S (S (double n))
```

Thus, we take `double := fun n => iter n (fun x => S (S x)) 0`.

For a trickier example, suppose we wish to define the equality operation `eq : N -> N -> B`.  Assume we have already defined predecessor (`P`) as well as the zero (`isZ`) and non-zero (`nonZ`) test functions.  If the first argument of `eq` is zero, then the second is equal to the first only if it itself is zero; thus, we have that

```
eq 0 = isZ
```

If the first argument of `eq` is `n+1`, then the second argument is equal to it only if it itself is non-zero, and its predecessor is equal to `n`; thus,

```
eq n+1 = fun m => and (nonZ m) (eq n (P m))
```

Notice that we now have an expression for `eq n+1` in terms of `eq n`.  Therefore, we may give `eq` the following defininition:

```
eq := fun n => iter n (fun f m => and (nonZ m) (f (P m))) isZ
```

### Using `rec`
Suppose we are trying to define the factorial function. The factorial is specified by the following two equations:

```
0!     = 1
(n+1)! = (n+1)*n!
```
We run into a problem:  the expression `(n+1)*n!` contains not only an instance of `n!`, but also an instance of `n` itself.  The iterator doesn't keep track of `n`, only the function's previous value.   `iter` can't be used directly in this instance.

To get around this, we define the recursor, `rec : N -> (a -> N -> a) -> a -> a` which satisfies the following two equations:

```
rec 0 f a   = a
rec n+1 f a = f (rec n f a) n
```
The general use case for `rec` is when we wish to inductively define a function `f : N -> a` in a way where both the function's previous value and the value of `n` are needed at the inductive step.  Specifically, if `f` is specified by

```
f 0   = t
f n+1 = T[f n,n]
```

where `T` is now an expression with occurrences of both `f n` and `n`, we can then use `rec` to define `f` in the following manner:

```
f := fun n => rec n (fun x m => T[x,m]) t
```

Revisiting our factorial example:

```
fact := fun n => rec n (fun x m => mult (S m) x) 1
```

The interesting thing is that `rec` is actually definable from `iter`, so we need not add it as a new primitive.  A definition for `rec` is provided, but it is a good challenge to see if you can define it yourself. 


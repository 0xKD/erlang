### Recursion

"I'm not going to apologise for using factorial." 😂

Introduced to the `when` keyword (look at `recurse.erl`). It makes up a _guard_, which can contain arithmetical operations and comparisons, but not user-defined functions (because it must always terminate).

### "Let it fail"

Aka, let the caller deal with it. Relatable to Python's "ask for forgiveness...".

A function will throw an error if the input doesn't match any of its clauses.

```erlang
fact:fac(-2).
** exception error: no function clause matching fact:fac(-2) (fact.erl, line 4)
```

### Tail Recursion

Absolves the need for maintaining intermediate results, acting more like a loop. See file `tail.erl`


---

### Complex Pattern Matching

Tuples used to represent complex types.

```erlang
{circle, {X, Y}, R}.
{rectangle, {X,Y}, H, W}.
```


Pattern matching is used to distinguish between different types. <br>
And also extract components in each case


```erlang
area({circle, {X,Y}, R}) ->
	math:pi()*R*R;
area({rectangle, {X,Y}, H, W}) ->
	H*W.
```

Using the `case` statement

```erlang
case [3] of
	[X] -> X;
	[] -> 0
end.

% try changing the [3,4] to something else
case [3,4] of
	[X,X] -> 2*X;
	[X,Y] -> -1;
	_ -> 0
end.
```


### Collections

List is a collection of elements:

```erlang
[2,3,4].
[3,2,4].
[].
```

`[X|Xs]` matches a non-empty list (used presciently in `ex1.erl`). <br>
`X` matches the head of the list (one element), `Xs` holds the tail (remaining elements as a list)

```erlang
head([X|_Xs]) -> X.
tail([_X|Xs]) -> Xs.

% demonstrating function composition
second(Xs) -> head(tail(Xs)).

% alternatively
second([_X1,X2|_Xs]) -> X2.
```

Different ways to concatenate

```erlang
> A=[1,2,3].
[1,2,3]
> [0]++A.
[0,1,2,3]

> [0|A].
[0,1,2,3]

% but the reverse is not the same!
> [A|0].
[[1,2,3],0]

% for ++, both operands are lists
> A++[0].
[1,2,3,0]
```

Built-ins

```erlang
> hd(A).  % head
1
> tl(A).  % tail
[2,3]
```

Going deeper with the "pipe" (cons?) operator

```erlang
% what is this???
> W=[3|4].
[3|4]
> hd(W).
3
> tl(W).
4

% seems it can only appear at the end
> X=[1,2,3,4|5].
[1,2,3,4|5]
> [5|4,3,2,1].
* 1: syntax error before: ','
> [5|[4,3,2,1]].
[5,4,3,2,1]

> [A|B] = [3|4].
[3|4]
> A.
3
> B.
4
> [X|Y] = [3,4].
[3,4]
> X.
3
> Y.
[4]
```

From the course

> It’s possible to put any values in [ | ], but in general these are called improper lists. To make use of lists, the argument on the right-hand side needs to be a (proper) list too.



### Spec

Introduced to the `spec` keyword. Does not enforce types, only meant to serve as reference and for documentation.

```erlang
-spec take(integer(), [T]) -> [T].
```

Simialar to type hints from Python? Erlang, after all, is dynamically typed.

#### Misc

Approach to solving problems — divide and conquer.

- Step 1. "If only I had a function to do X, I could define the answer"
- Step 2. Define X. GOTO Step 1

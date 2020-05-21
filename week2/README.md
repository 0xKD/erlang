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

## History

Erlang was built (at Ericsson) for running innumerable operations concurrently, being resistent to hardware failures, and enable insane amount of uptime.

## Properties

Functional (no side effects or shared state), immutability.

## Functional Programming Languages

Lazy v/s. strict evaluation (arguments evaluated only when necessary v/s. before entering function body) <br>
Purity refers to lack of side effects

---

## Code


```erlang
> % Period (.) terminates input.
> 3+4.
7
> % assignment (binding of variable)
> A = 1.
1
> % "forget" all assignments
> f().
> % quit
> q().
```

Refer to file `first.erl`

From a shell spawned in the same directory (using `erl`)

```erlang
> %compile the module.
> c(first)
{ok,first}
> % call the exported function.
> first:double(16).
32
```

`second.erl` uses function from `first.erl`

```erlang
> % both need to be compiled
> c(first),c(second)
> second:combined(3,4,5,7,8)
62.0
```


## Data Types

Numbers, Atoms, Booleans, Tuples and lists, Strings, Functions

### Numeric

Weakly typed

Integers are "bignums" (arbitrarily large with full precision - similar to Python?)

```erlang
> % represent numbers in a different base (base#number)
> 2#100.
4.
> 16#deadbeef.
3735928559
> % integer division (similar to Python's //)
> 12 div 5.
2
> 12 / 5.
2.4
> 12 rem 5
2
```

### Atoms

Is a piece of data, independent. `true`, `false` are special atoms


```erlang
> foo.
foo
> 'this is string'.
'this is string'
> foo == 'foo'.
true
```

### Tuples & Lists

Tuple represented as follows

```erlang
> {true, false}.
{true,false}
> {1,2.0,foobar}.
{1,2.0,foobar}
```

A commonn idiom is to use the first field to indicate what kind of data is stored in the tuple <br>

```erlang
{rectangles, {3,4}, {5,6}} % {x1, y1}, {x2, y2}
{circle, {1,2}, 5} % {x,y}, radius
```

Lists (_typically_ homogenous) - meant to be iterated over (unlike tuples)

```erlang
> [hello, world].
[hello,world]
> [1, foo].
[1,foo]
```

### Strings

A string is simply a list of characters

```erlang
> [97, 98, 99].
"abc"
> "foobar".
"foobar"
> $a.
97
> [16#41,16#41,16#41].
"AAA"
```

They are different from atoms

```erlang
> foobar == 'foobar'.
true
> "foobar" == 'foobar'.
false
> foobar == "foobar".
false
```


## Functions

```erlang
> F = fun (X) -> X*2 end.
#Fun<erl_eval.6.99386804>
> F(8).
16
> lists:map(F, [1,2,3]).
[2,4,6]
> Mult = fun(X,Y) -> X*Y end.
> lists:foldr(Mult, 1, [1,2,3]).
6
```

Quirk:

> Starting with a capital letter separates variables from atoms, which are used frequently in Erlang code. Atoms always start with a lowercase letter. Both module and function names are atoms.

```erlang
> % must begin with underscore or capital letter
> foo = fun () -> 1 end.
** exception error: no match of right hand side value #Fun<erl_eval.20.99386804>
> _foo = fun () -> 1 end.
#Fun<erl_eval.20.99386804>
> Foo = fun () -> 1 end. 
#Fun<erl_eval.20.99386804>
```

### Misc operations

Booleans

```erlang
% negation
> not true.
false

% combining booleans
> true and false.
false.

% shortcuiting is slightly different (and unintuitive I'd reckon)
> false and (4 div 0).
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  div/2
        called as 4 div 0
> false andalso (4 div 0).
false
> true or (4 div 0).
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  div/2
        called as 4 div 0
> true orelse (4 div 0).
true

% The values allowed on the right hand side also vary with "and" v/s "andalso"
> true and 3.
** exception error: bad argument
     in operator  and/2
        called as true and 3
> true andalso 3.
3
> false orelse 1.
1
```

Lists

```erlang
> [1]++[2].
[1,2]
> [1,2]--[2].
[1]
> [1]--[2].
[1]
```

Functions

```erlang
> fun(X)->X+2 end(40).
42
> (fun(X)->X+2 end)(40).
42
```

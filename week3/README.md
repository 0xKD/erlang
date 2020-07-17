## Approaches to unit testing

Apart from manually testing functions, or recording those tests as ad-hoc `test_func` functions (that need to be called manually), two main approaches are discussed.


### Built-in Unit Testing Framework

[EUnit](http://erlang.org/doc/apps/eunit/chapter.html) offers convenient way of declaring and running tests. (`?assertEqual` is a macro)

```erlang
-module(sample_testing).
-export([double/1]).
-include_lib("eunit/include/eunit.hrl").

double(N) -> N * 2.

% Functions to be run as tests must end with _test and take no arguments
double_a_test() -> 4 = double(2).
double_b_test() -> ?assertEqual(double(32), 64).
double_c_test() -> ?assertEqual(double(1), 3).
```

To run:

```erlang
> c(sample_testing).
{ok,sample_testing}
> sample_testing:test().  % test() is automatically exported by EUnit
sample_testing: double_c_test...*failed*
in function sample_testing:'-double_c_test/0-fun-0-'/0 (sample_testing.erl, line 9)
**error:{assertEqual,[{module,sample_testing},
              {line,9}, 
              {expression,"3"},
              {expected,2},
              {value,3}]}
  output:<<"">>

=======================================================
  Failed: 1.  Skipped: 0.  Passed: 2.
error
```

The `expected` and `value` seem counterintuitive. The "expected" value as defined in the test is 3, the actual value we got is 2, but the above error message seems to have it the other way round.

### Property based testing

Test general property (behaviour?) of the function, rather than fixed input and output values. Uses randomly generated input (like [hypothesis](https://hypothesis.readthedocs.io/en/latest/) for Python)

Libraries that do this:
- [QuickCheck](https://gist.github.com/efcasado/3df8f3f1e33eaa488019)
- [PropEr](https://github.com/proper-testing/proper)


## Types

The instructor refers to languages with compile time type checks as **strongly** typed. I have been referring to such languages as **statically** typed and that's what I will use here as well.

The reason for not using the former term is that Erlang (and similar languages) would be categorized as **weakly** typed, which is a term reserved for languages like JavaScript (which will automatically cast data types as it sees fit... during runtime).

So, in summary, C/C++/Haskell/OCaml are **statically** (and of course strongly) typed. Erlang,Python,Ruby are **dynamically** (but also strongly) typed. These don't have a way of enforcing type constraints at compile time, so some kind of errors can only be detected at runtime.


With the minutiae out the way, Erlang is similar to Python. In that it has no way of enforcing type constraints, but offers type **hints** (or **annotations**) using `-spec`. Some type errors can also be checked statically. Tools used here are [Typer](http://erlang.org/doc/man/typer.html "Typer") and [Dialyzer](http://erlang.org/doc/apps/dialyzer/dialyzer_chapter.html "Dialyzer").

### Types in Erlang

- **Numbers** - `integer()`, `float()`
- **Atoms** - `atom()`
- **Booleans** - `boolean()`
- **Tuples and Lists** - `list(T)`,`[T]`, `{T1,...,TN}`
- **Strings** - `string()`
- **Functions** - `fun()`, `fun((T1,...,TN) -> T)`

Using `-spec` does not restrict the usage of the function it is defined on. Typer will only throw an error if the spec is not in sync with what it has inferrred.


## Functional Patterns

Introduced to map, reduce, filter patterns. We've seen these before in `week2/listops.erl`.

- `map` - Transforming (applying a function) to each element in a list. `all_areas/1` in `listops.erl`
- `filter` - Select elements from a list based on some criteria. `circles/1` in `listops.erl`
- `reduce` - Combine (or aggregate) all elements in a list. `sum/1` in `listops.erl`

### Map

Defining `map` by yourself.

``` erlang
double(N) -> N*2.
double_all([X|Xs]) ->
  [double(X) | double_all(Xs)].

% Generic form of double_all
map(F, []) -> [];
map(F, [X|Xs]) -> [F(X) | map(Xs)].
```

Calling this is a bit different though.

``` erlang
double_all(Xs) -> map(fun double/1, Xs).
```

It makes sense that we simply can't just define it like `map(double, Xs)`, since `double` would be treated as an atom, and if there are multiple implementations of `double`, it would lead to ambiguity.
The `fun` syntax takes care of both issues.

Even though the original form of `double_all` isn't complicated, using `map` makes it more comprehensible since we don't have to read the code and understand the recursion.
It is immediately clear what is going on with just `map(fun double/1, Xs)`.


### Filter

Similary, implementing `filter`:

``` erlang
filter(P, []) -> [];
filter(P, [X|Xs]) ->
  case P(X) of
    true -> [X|filter(P, Xs)];
    false -> filter(P, Xs)
  end.
```

Where `P(X)` represents `X` satsifying a given property. We filter all elemenst of Xs that satisfy the property.

``` erlang
is_even(N) -> N rem 2 == 0.
filter_even(Xs) -> filter(fun is_even/1, Xs).
```

### Reduce

Finally, we look at `reduce`. This has two components. One is a combining function, and the other is a starting value.

``` erlang
reduce(Combine, Start, []) -> Start;
reduce(Combine, Start, [X|Xs]) ->
  Combine(X, reduce(Combine, Start, Xs)).
```


``` erlang
sum(Xs) -> reduce(fun plus/2, 0, Xs).
plus(X,Y) -> X+Y.
```

We can also use in-line lambda functions. This removes the need for defining the "combine" function (`plus`) separately.

``` erlang
sum(Xs) -> reduce(fun(X,Y) -> X+Y end, 0, Xs).
```

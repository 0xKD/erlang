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

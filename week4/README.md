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

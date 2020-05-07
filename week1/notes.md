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
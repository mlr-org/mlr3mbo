# Condition Classes for mlr3mbo

Condition classes for mlr3mbo.

## Usage

``` r
error_random_interleave(msg, ..., class = NULL, signal = TRUE, parent = NULL)

error_surrogate_update(msg, ..., class = NULL, signal = TRUE, parent = NULL)

error_acq_optimizer(msg, ..., class = NULL, signal = TRUE, parent = NULL)
```

## Arguments

- msg:

  (`character(1)`)  
  Error message.

- ...:

  (any)  
  Passed to [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

- class:

  (`character`)  
  Additional class(es).

- signal:

  (`logical(1)`)  
  If `FALSE`, the condition object is returned instead of being
  signaled.

- parent:

  (`condition`)  
  Parent condition.

## Errors

- `error_random_interleave()` for the `Mlr3ErrorMboRandomInterleave`
  class, signalling a random interleave error.

- `error_surrogate_update()` for the `Mlr3ErrorMboSurrogateUpdate`
  class, signalling a surrogate update error.

- `error_acq_optimizer()` for the `Mlr3ErrorMboAcqOptimizer` class,
  signalling an acquisition function optimizer error.

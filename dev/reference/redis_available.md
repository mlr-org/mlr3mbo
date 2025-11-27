# Check if Redis Server is Available

Attempts to establish a connection to a Redis server using the
[redux](https://CRAN.R-project.org/package=redux) package and sends a
`PING` command. Returns `TRUE` if the server is available and responds
appropriately, `FALSE` otherwise.

## Usage

``` r
redis_available()
```

## Value

(`logical(1)`)

## Examples

``` r
if (redis_available()) {
  # Proceed with code that requires Redis
  message("Redis server is available.")
} else {
  message("Redis server is not available.")
}
#> Redis server is not available.
```

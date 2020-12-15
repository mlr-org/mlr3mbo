format_bib = function(...) {
  str = vapply(list(...), function(entry) tools::toRd(bibentries[[entry]]), FUN.VALUE = "")
  paste0(str, collapse = "\n\n")
}

#' @importFrom utils bibentry
bibentries = c(
  jones_1998 = bibentry("article",
    title = "Efficient Global Optimization of Expensive Black-Box Functions",
    author = "Jones, Donald R. and Schonlau, Matthias and Welch, William J.",
    year = "1998",
    volume = "13",
    pages = "455--492",
    doi = "10.1023/A:1008306431147",
    journal = "Journal of Global optimization",
    number = "4"
  )
)

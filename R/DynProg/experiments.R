


`%with%` <- function(table_name, recursion) {
  table_name <- rlang::enexpr(table_name)
  recursion <- rlang::enquo(recursion)
  list(table_name = table_name, recursion = recursion)
}
`%where%` <- function(table_expr, ranges) {
  table_expr$ranges = rlang::enquo(ranges)
  table_expr
}

# evaluated left-to-right which means that it is %where% that sees an
# expression on its left (a lazy promise) *before* `%with` is evaluated.
fact <- fact %with% { 
  fact[n] <- n * fact[n-1] %when% n >= 1
  fact[n] <- 1             %when% n < 1
} %where% { n <- 1:4 }

fact

x <- "abccd"
y <- "abd"

edit <- edit %with% {
  edit[1,j] <- j
  edit[i,1] <- i
  edit[i,j] <- min(edit[i-1,j] + 1,
                   edit[i,j-1] + 1,
                   edit[i-1,j-1] + x[i] == y[j]) %when% i > 1 && j > 1
} %where% { i <- seq_along(x) ; j <- seq_along(y) }

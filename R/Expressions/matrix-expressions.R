A <- matrix(1:6, nrow = 20, ncol = 30)
B <- matrix(1:9, nrow = 30, ncol = 30)
C <- matrix(1:15, nrow = 30, ncol = 50)

library(microbenchmark)
microbenchmark((A %*% B) %*% C,
               A %*% (B %*% C))
20 * 30 * 30 + 20 * 30 * 50
20 * 30 * 50 + 30 * 30 * 50


m <- function(data, nrow, ncol) {
  structure(matrix(data, nrow = nrow, ncol = ncol),
            class = "matrix_expr")
}

A <- m(1:6, nrow = 2, ncol = 3)
B <- m(1:9, nrow = 3, ncol = 3)
C <- m(1:15, nrow = 3, ncol = 5)

`*.matrix_expr` <- function(A, B) {
  structure(list(left = A, right = B), 
            class = c("matrix_mult", "matrix_expression"))
}

as.double.matrix_mult <- function(x, ...) {
  x$left %*% x$right
}

as.double(A * B)

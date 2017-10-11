A <- matrix(1, nrow = 400, ncol = 300)
B <- matrix(1, nrow = 300, ncol = 30)
C <- matrix(1, nrow = 30, ncol = 500)
D <- matrix(1, nrow = 500, ncol = 400)

library(microbenchmark)
microbenchmark(A %*% B %*% C %*% D,
               ((A %*% B) %*% C) %*% D,
               (A %*% (B %*% C)) %*% D,
               (A %*% B) %*% (C %*% D),
               A %*% (B %*% (C %*% D)),
               A %*% ((B %*% C) %*% D))


m <- function(data) {
  name <- deparse(substitute(data))
  structure(data, 
            nrow = nrow(data),
            ncol = ncol(data),
            name = name, # FIXME
            class = c("matrix_expr", class(data)))
}
matrix_mult <- function(A, B) {
  structure(list(left = A, right = B),
            nrow = nrow(A),
            ncol = ncol(B),
            class = c("matrix_mult", "matrix_expr"))
}

dim.matrix_expr <- function(x) {
  c(attr(x, "nrow"), attr(x, "ncol"))
}

toString.matrix_expr <- function(x, ...) {
  attr(x, "name")
}
toString.matrix_mult <- function(x, ...) {
  paste0("(", toString(x$left), " * ", toString(x$right), ")")
}

print.matrix_expr <- function(x, ...) {
  print(toString(x))
}

`*.matrix_expr` <- function(A, B) {
  matrix_mult(A, B)
}




backtrack_matrix_mult <- function(i, j, dims, tbl, matrices) {
  if (i == j) {
    matrices[[i]]
  } else {
    k <- i:(j - 1)
    candidates <- dims[i,1]*dims[k,2]*dims[j,2] + tbl[i,k] + tbl[k + 1,j]
    split <- k[which(tbl[i,j] == candidates)][1]
    left <- backtrack_matrix_mult(i, split, dims, tbl, matrices)
    right <- backtrack_matrix_mult(split + 1, j, dims, tbl, matrices)
    matrix_mult(left, right)
  }
}

arrange_optimal_matrix_mult <- function(matrices) {
  n <- length(matrices)
  dims <- matrix(0, nrow = n, ncol = 2)
  for (i in seq_along(matrices)) {
    dims[i,] <- dim(matrices[[i]])
  }
  
  tbl <- matrix(0, nrow = n, ncol = 4)
  for (len in 2:n) {
    for (i in 1:(n - len + 1)) {
      j <- i + len - 1
      k <- i:(j - 1)
      tbl[i,j] <- min(dims[i,1]*dims[k,2]*dims[j,2] + tbl[i,k] + tbl[k + 1,j])
    }
  }
  
  backtrack_matrix_mult(1, n, dims, tbl, matrices)  
}



count_basic_matrices <- function(matrix_expr) {
  if (inherits(matrix_expr, "matrix_mult")) {
    count_basic_matrices(matrix_expr$left) + count_basic_matrices(matrix_expr$right)
  } else {
    1
  }
}
collect_basic_matrices <- function(matrix_expr) {
  n <- count_basic_matrices(matrix_expr)
  matrices <- vector("list", length = n)
  i <- 1
  collect <- function(matrix_expr) {
    if (inherits(matrix_expr, "matrix_mult")) {
      collect(matrix_expr$left)
      collect(matrix_expr$right)
    } else {
      matrices[[i]] <<- matrix_expr
      i <<- i + 1
    }  
  }
  collect(matrix_expr)
  matrices
}

rearrange_matrix_mult <- function(expr) {
  matrices <- collect_basic_matrices(expr)
  arrange_optimal_matrix_mult(matrices)
}

eval_matrix_mult <- function(expr) {
  if (inherits(expr, "matrix_mult")) {
    eval_matrix_mult(expr$left) %*% eval_matrix_mult(expr$right)
  } else {
    expr
  }
}

mA <- m(A)
mB <- m(B)
mC <- m(C)
mD <- m(D)

expr1 <- mA * mB * mC * mD
expr1
expr2 <- rearrange_matrix_mult(expr1)
expr2

microbenchmark(A %*% B %*% C %*% D,
               eval_matrix_mult(expr1),
               (A %*% B) %*% (C %*% D),
               eval_matrix_mult(expr2))

v <- function(expr) eval_matrix_mult(rearrange_matrix_mult(expr))

microbenchmark(A %*% B %*% C %*% D,
               v(m(A) * m(B) * m(C) * m(D)))


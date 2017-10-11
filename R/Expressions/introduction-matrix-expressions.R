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
  if (inherits(expr, "matrix_mult")) {
    matrices <- collect_basic_matrices(expr)
    arrange_optimal_matrix_mult(matrices)
  } else {
    expr
  }
}

eval_matrix_mult <- function(expr) {
  if (inherits(expr, "matrix_mult")) {
    eval_matrix_mult(expr$left) %*% eval_matrix_mult(expr$right)
  } else {
    expr
  }
}

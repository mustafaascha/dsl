
test_pattern_rec <- function(escape, expr, test_expr, eval_env, match_env) {

  # Is this a function-constructor?
  if (is_lang(test_expr)) {
    func <- get(as_string(test_expr[[1]]), eval_env)
    if ("constructor" %in% class(func)) {
      # This is a constructor.
      # Check if it is the right kind
      constructor <- as_string(test_expr[[1]])
      expr_constructor <- attr(expr, "constructor")
      if (is_null(expr_constructor) || constructor != expr_constructor)
        escape(NULL) # wrong type

      # Now check recursively
      for (i in seq_along(expr)) {
        test_pattern_rec(escape, expr[[i]], test_expr[[i+1]], eval_env, match_env)
      }

      # If we get here, the matching was successfull
      return(match_env)
    }
  }

  # Is this a constant-constructor?
  if (is_symbol(test_expr) && exists(as_string(test_expr), eval_env)) {
    constructor <- as_string(test_expr)
    val <- get(constructor, eval_env)
    val_constructor <- attr(val, "constructor_constant")
    if (!is_null(val_constructor)) {
      expr_constructor <- attr(expr, "constructor")
      if (is_null(expr) || constructor != expr_constructor)
        escape(NULL) # wrong type
      else
        return(match_env) # Successfull match
    }
  }

  # Not a constructor.
  # Must be a value to compare with or a variable to bind to
  if (is_symbol(test_expr)) {
    assign(as_string(test_expr), expr, match_env)
  } else {
    value <- eval_tidy(test_expr, eval_env)
    if (expr != value) escape(NULL)
  }

  match_env
}

test_pattern <- function(expr, test_expr, eval_env) {
  # Environment in which to store matched variables
  match_env <- env()

  if (test_expr == quote(otherwise))
    return(match_env)

  # Test pattern
  tester <- function(escape)
    test_pattern_rec(escape, expr, test_expr, eval_env, match_env)
  callCC(tester)
}

match <- function(expr, ...) {
  matchings <- quos(...)
  matchings[[1]]

  for (i in seq_along(matchings)) {
    eval_env <- get_env(matchings[[i]])
    match_expr <- quo_expr(matchings[[i]])
    stopifnot(match_expr[[1]] == "<-")

    test_expr <- match_expr[[3]]
    result_expr <- match_expr[[2]]

    match <- test_pattern(expr, test_expr, eval_env)
    if (!is_null(match))
      return(eval_tidy(result_expr, data = match, env = eval_env))
  }

  stop("No matching pattern!")
}


tree := T(left : tree, right : tree) | L(value : numeric)

match(L(1),
      L(2) -> 12,
      L(1) -> 11,
      otherwise -> 13)


match(L(1),
      L(v) -> v,
      T(L(v), L(w)) -> v + w,
      otherwise -> 5)

match(T(L(4), L(5)),
      L(v) -> v,
      T(L(v), L(w)) -> v + w,
      otherwise -> 5)

match(T(L(1), T(L(4), L(5))),
      L(v) -> v,
      T(L(v), L(w)) -> v + w,
      otherwise -> 5)


dfs <- function(tree) {
  match(tree,
        L(v) -> v,
        T(left, right) -> dfs(left) + dfs(right))
}

dfs(L(1))
dfs(T(L(1),L(2)))
dfs(T(T(L(1),L(2)),L(3)))

x <- 1
y <- 2
match(L(1),
      L(!!x) -> "x",
      L(!!y) -> "y")


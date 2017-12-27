# Examples: Lambda expressions and CTMCs

With the techniques we have seen so far, we are now able to implement some useful domain-specific languages. In this chapter, we examine two examples: lambda-expressions and continuous time Markov chains.

## Lambda expressions

```{r}
make_args_list <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  as.pairlist(res)
}
`:=` <- function(header, body) {
  header <- substitute(header)
  if (is.call(header)) header <- header[-1]
  body <- substitute(body)
  args <- make_args_list(as.character(header))
  new_function(args, body, caller_env())
} 
```

```{r}
sapply(1:4, x := x**2)
```

```{r}
mapply(lambda(x,y) := x*y, x = 1:6, y = 1:2)
```

```{r}
mapply(.(x,y) := x*y, x = 1:6, y = 1:2)
```




## Continuous time Markov chains
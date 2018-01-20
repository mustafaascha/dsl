# List comprehension

We will now use what we have learned to implement a very useful language construction that is not built in to R: *list comprehension*. List comprehensions provide a syntax for mapping and filtering sequences. In R we would use functions such as `Map` or `Filter`, or the `purrr` alternatives, for this, but in languages such as Haskell or Python there is syntactic sugar to make combinations of mapping and filtering easier to program.

Take an algorithm such as quick sort. Here, the idea is to sort a list by picking a random element in it, called the *pivot*, splitting the data into those elements smaller than the pivot, equal to the pivot and larger than the pivot. We then sort those smaller and larger elements recursively and concatenate the three lists to get the final sorted list. One way to implement this in R is using the `Filter` function:

```{r}

qsort <- function(lst) {
  n <- length(lst)
  if (n < 2) return(lst)
  
  pivot <- lst[[sample(n, size = 1)]]
  smaller <- Filter(function(x) x < pivot, lst)
  equal <- Filter(function(x) x == pivot, lst)
  larger <- Filter(function(x) x > pivot, lst)
  c(qsort(smaller), equal, qsort(larger))
}
(lst <- sample(1:10))
unlist(qsort(lst))
```

It is not exactly unreadable if you are familiar with functional programming, but it does take some decoding to work out the `Filter` expression and decode the predicate used in it. Compare this to a Python implementation that does exactly the same thing (except that the pivot isn’t chosen randomly because sampling requires a bit more in Python):

```python
def qsort(lst):
    if len(lst) < 2:
        return lst
    pivot = lst[0]
    return qsort([x for x in lst if x < pivot]) + \
                 [x for x in lst if x == pivot] + \
           qsort([x for x in lst if x > pivot])
```

Or consider a similar Haskell implementation:

```haskell
qsort lst = 
    if length lst < 2 then 
        lst
    else
        let pivot = lst !! 0
        in qsort([x | x <- lst, x < pivot]) ++ 
                 [x | x <- lst, x == pivot] ++ 
           qsort([x | x <- lst, x > pivot])
```

Expressions such as 
```python
	[x for x in lst if x < pivot]
```
in Python or
```Haskell
	[x | x <- lst, x < pivot]
```
in Haskell is what we call list comprehension. List comprehensions consist of three components, first an expression that will be evaluated for each element in the list (or lists if we use more than one), then one or more lists to map over, and finally zero or more predicates we use to filter over. It is thus a combination of `Map` and `Filter` calls in one expression.

Using non-standard evaluation, we can write an R function that provides a similar list comprehension syntax. We will write it such that its first argument must be an expression that we evaluate for all elements in the input list(s) and such that its remaining elements either identify lists or predicates. We will use named arguments to identify when an argument defines a list and unnamed arguments for predicates.



```{r}
library(rlang)
library(purrr)

lcomp <- function(expr, ...) {
  expr <- enquo(expr)
  rest <- quos(...)
  
  lists <- map(rest[names(rest) != ""], eval_tidy)
  predicates <- map(rest[names(rest) == ""], UQE)

  f <- new_function(lists, body = UQE(expr), env = get_env(expr))
  values <- pmap(lists, f)
  
  keep_index <- rep(TRUE, length(lists[[1]]))
  for (pred in predicates) {
    p <- new_function(lists, body = pred, env = get_env(expr))
    keep_index <- keep_index & unlist(pmap(lists, p))
  }
  
  values[keep_index]
}

qsort <- function(lst) {
  n <- length(lst)
  if (n < 2) return(lst)
  
  pivot <- lst[[sample(n, size = 1)]]
  smaller <- lcomp(x, x = lst, x < pivot)
  equal <- lcomp(x, x = lst, x == pivot)
  larger <- lcomp(x, x = lst, x > pivot)
  
  c(qsort(smaller), equal, qsort(larger))
}

(lst <- sample(1:10))
unlist(qsort(lst))
```


```{r}
qsort <- function(x) {
  n <- length(x)
  if (n < 2) return(x)

  pivot <- x[[sample(n, size = 1)]]
  smaller <- lcomp(y, y = x, y < pivot)
  equal <- lcomp(y, y = x, y == pivot)
  larger <- lcomp(y, y = x, y > pivot)

  c(qsort(smaller), equal, qsort(larger))
}
```
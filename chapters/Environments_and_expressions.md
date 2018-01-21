# Environments and expressions {#sec:env_and_expr}

We have already used environments in a couple of examples to evaluate expressions in a different context than they are usually evaluated, something known as *non-standard evaluation*. Many domain specific languages we could want to implement in R will need some sort of non-standard evaluation,  but getting evaluation to occur in the right context can be problematic. The rules for how expressions are evaluated are simple, but the way evaluation contexts—environments—are chained together can be complex.

## Scopes and environments

Whenever R evaluates an expression, it does so in a *scope*. A scope determines which values any given variable refers to. In standard evaluation, R uses what is known as *lexical scope*, which essentially means that variables in an expression refers to the variables defined in blocks around the expression. If you write an expression at the outermost level of an R script—the so-called *global environment*—then variable names in the expression refer to global variables. An expression inside a function, on the other hand, is evaluated in the scope of a function call, which means that variable symbols refer to local variables or function parameters, if these are defined, and only if they are not defined do they refer to global variables. A function defined inside another function will have a *nested scope*—variables in an expression there will be searched for first in the innermost function, then the surrounding function, and only if they are not found either place, in the global environment.

Consider this abstract example:

```{r}
x <- 1
f <- function(y) {
  z <- 3
  function() x + y + z
}
g <- f(2)
h <- f(3)
g()
h()
```

Here, we define four variables in the global environment, `x`, `f`, `g`, and `h`. In the function `f` we have one formal parameter, `y`, and one local variable `z`. Whenever we call `f`, a scope where `y` exists is created and the first statement in the function call is to also add `z` to this scope. The function returns another function that contains an expression that refers to variable `x`, `y`, and `z`. If we call this function, which we do when we call functions `g` and `h`, which are the results of two separate calls to `f`, this expression will be evaluated. When R evaluates the expression, it needs to find the three variables. They are neither formal arguments or local variables in the functions we call, `g` and `h`, but since the functions were created inside calls to `f`, they can see `y` and `z` in the surrounding scope, and both can find `x` in the global environment. Since `g` and `h` are the result of separate calls to `f`, the surrounding scope of calls to them are *different* instances of local scopes of `f`.

Scopes are implemented through *environments*, and the rules that guide environments and evaluation are very simple—even though you have to be careful if you start manipulating them. Environments are tables that map variables to values and all environments have a parent environment, a surrounding scope, that R will search in if a variable is not found when it searches the environment. The different environments you can evaluate expressions in have a tree structure with a root in the *empty environment*. This environment is the root of all environments. Packages you load are put on top of this environment, and on top of all loaded environments we have the *global environment*—which is why you can find variables define in packages if you search in the global environment. Strictly speaking there are a few other details in how packages and environments interact, but they are not important for the discussion here. If you are interested, you can find a description of this in my other book, *Meta-programming in R*. For the purpose of this book, we will simply assume that everything we define at the global level or any package is found in the global environment and consider this the root of the environment tree.

When we define new functions we do not create new environments, but we do associate the functions with one—the environment in which we define the function. When we defined function `f` in the example above, it got associated with the global environment, because that is where we defined it. We can get the environment a function is associated with using the `environment` function:

```{r}
environment(f)
```



```{r}
environment(g)
environment(h)
```



## Lazy evaluation and promises 

## Quotes and quosures


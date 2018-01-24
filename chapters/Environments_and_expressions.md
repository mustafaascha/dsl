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

Since `f` is defined at the global level, its environment is the global environment. When we call a function is when we create a new environment. This environment is where we store parameters and local variables. The environment associated with the function will be made the *parent environment* for this function-call environment. When we call function `f` we thus create an environment where we get a mapping from `y` and `z` to their values and with a parent environment that is the global environment, in which we can find the variable `x`. Inside the call to `f` we create a new (anonymous) function and return it. This function will also have an environment associated with it, but this time it is the local environment we created when we called `f`. Thus, the environments associated with `g` and `h` are two different environments as they are the result of two different calls to `f`.

```{r}
environment(g)
environment(h)
```

Functions defined inside other functions thus carry along with them environments that were created when the surrounding function was called and if we return them from the surrounding function they still carry this enclosing scope along with them—remembering local variable and parameters from the enclosing scope—and we call such functions *closures*.

In [@fig:calling-g-environments] I have drawn a slightly simplified graph of which environments exist and how they are wired together in the example at the point where we call function `g`. Environments are shown with a grey background, variables as circles with pointers to the values the variables refer to, and functions are shown as the three components that define a function: the formal parameters, the function body, and the environment associated with the function—the enclosing scope.

The enclosing environment for function `f` is, as we have already discussed, the global environment, while the enclosing scopes of `g` and `h` are two different instances of calls to `f`. These instance-environments have the global environment as their parents since that is the enclosing scope of `f`, the function they are instances of. Because they are two different instances of `f`, the variables in them can point to different values, as we see for variable `y`. For function `g`, variable `y` points to 3, while for function `h`, `y` points to 2. In a call to function `g` we create a local environment for the function call—shown at the bottom right in the figure. We do not have any local variables in `g` so this environment does not contain any variables, but it has a parent which is the `f` instance where we created `g`.

![Environment graph when calling `g`.](figures/calling-g-environments){#fig:calling-g-environments}

When we evaluate the expression `x + y + z` inside the call to `g` we need to map variables to values. The search will start in the local environment and then progress up the parent links until it finds a matching variable. For variables `y` and `z` we find values in the parent of the `g` call—the instance of the `f` call that created `g`—and for `x` we find the value in the grandparent, the global environment.

The rule for evaluating expressions is always the same: we look up variables by searching in environments, starting with the immediate environment where we evaluate the expression and searching up the chain of parent environments and check these in turn until we find the variable we are looking for. We get the standard evaluation rules of lexical scoping because functions get associated with the environment in which they are created and since this environment is set as the parent environment of instances of function calls. The only trick to understanding how expressions are evaluated in R is to understand which environments are used. For the body of functions it is as simple  as I have just explained, but for function parameters there are a few more rules to consider…

## Default parameters, lazy evaluation, and promises

When you pass primitive values such as numbers to a function parameter there is nothing we need to evaluate, so there are no complications. This is why we didn’t have to worry about the environment of the arguments in the previous example. If we pass expressions along as parameters, however, we need to know how these should be evaluated.

Most of the time, R behaves as if expressions are evaluated before a function is called, but this isn’t actually what happens. If we passed values along to functions instead of expressions, we would have been able to get the expressions using `substitute` as we have done in previous chapters. When we call a function in R, the parameters will refer to unevaluated expressions, so-called *promises*. These expressions are evaluated the first time we use a parameter variable but not before—an approach to parameter evaluation known as *lazy evaluation*. If we never refer to an argument, the corresponding expression will never be evaluated, so we can write code such as this without raising exceptions:

```{r}
f <- function(x, y) x
f(2, stop("error!"))
```

We never refer to the parameter `y` inside the body of `f`, so we never evaluate it. Consequently, we never call `stop` to raise the error.

So, since parameters can contain expressions, we need rule for how to evaluate these. Here, there is a difference between default parameters—specified when the function is created—and parameters provided when the function is called. The former is evaluated in the local scopes of function calls while the latter is evaluated in the environment where the function is called.

Consider this function:

```{r}
f <- function(y, z = 2 * y) y + z
```

The function takes two parameters, `y` and `z`

```{r}
f(2, 1)
```

but if we only provide `y`, then `z` will be set to `2 * y`:

```{r}
f(2)
```

When we evaluate the promise that `z` points to when the function is called—we do this in the expression where we use the variable—the promise-expression is evaluated. This means that R needs to find the variable `y`. If we tried to evaluate the expression `2 * y` in the scope where the function is defined—the global environment—then we would get an error; there is no `y` variable defined there. The semantics of default parameters *could* be such that we evaluated them in the scope where we define a function, but if so, we wouldn’t be able to make default parameters depend on other parameters, which is what we want here—we want `z` to depend on `y` if we do not explicitly provide a value to it. The actual semantics is that the promise is evaluated in the function-call environment. When we call `f`, before we evaluate the `y + z` expression, the situation is therefore as shown in [@fig:default-parameters-f]. Here, I have drawn the promise for `z` as the expression passed along as the function argument together with the environment in which it should be evaluated.

![Default parameter promise.](figures/default-parameters-f){#fig:default-parameters-f}

When we call `f` with a parameter that is an expression we do *not* want to evaluate this expression in function-call scope. Consider this:

```{r}
y <- 2
f(2 * y)
```

Here, clearly, the intent is to call `f` with `2 * y` which should be `4` since `y` is 2. If we tried to evaluate it inside the function call, however, we would have a circular dependency. Inside the function call, `y` is a variable and if it points to `2 * y` we cannot evaluate the expression without known what `y` is, which we cannot know until we have evaluated the expression, which we cannot…

When we call a function with an expression as argument, the corresponding promise will be evaluated in the environment where we call the function, so before we evaluate `y + z` inside `f`, the situation is as shown in [@fig:default-and-promise-parameters-f]. Inside the environment of the function call, both `y` and `z` refer to promises, but these promises are associated with different environments. To evaluate the expression `y + z` we need to evaluate both promises. To get the value for `y` we need to evaluate `2 * y` *in the global scope*, which gives us 4 and to get the value for `z` we need to evaluate `2 * y` *in the local scope*, which gives us 8.

![Calling `f` with an expression for `y`.](figures/default-and-promise-parameters-f){#fig:default-and-promise-parameters-f}

At the risk of taking the example a step too far, let us consider the situation where we call `f` from another function:

```{r}
g <- function(x) f(2 * x)
g(2 * y)
```

Just before we evaluate the expression `y + z` inside function `f`, the environment graph is connected as shown in [@fig:default-and-promise-parameters-g]. It takes a little effort to decode what happens when we want to evaluate `y + z`, but doing this exercise will go a long way towards understanding environments.

![Calling `g` with an expression for `x` that depend on variable `y`.](figures/default-and-promise-parameters-g){#fig:default-and-promise-parameters-g}

Both `y` and `z` are promises we haven’t evaluated yet. Since `z` depend on `y` we need to evaluate `y` first. To do this, we need to evaluate the expression `2 * x` in the scope of the call to `g`. Here, we need to evaluate `x` which is another promise, the expression `2 * y` which should be evaluated in the global scope (where `y` refers to a different variable than then local variable inside the `f` instance). In the global scope, `y` refers to the value 2, so we can evaluate `2 * y` directly and get the value 4. This value then replaces the promise in the scope of the call to `g`. Once we have evaluated a promise, the variable refers to the value and no longer the expression—more on that below. This now means that we can evaluate `2 * x` in the scope of the `g` call to get 8. So now `y` in the call to `f` refers to 8. This means we can evaluate `2 * y` to get 16 which we assign the variable `z`. Finally, we can evaluate `y + z` to get 8 + 16 = 24.

To summarise this section, parameters we pass to functions, if these are not primitive values, are considered expressions that must be evaluated at some point, and together with the expressions we have an associated scope in which to evaluate the expression. There is one more caveat, though, which I hinted to in the previous example: parameters are only considered expressions until the first time we evaluate them. After that, they are the result of this evaluation.

There are pros and cons with this semantics—although, to be honest, predominantly cons. Because promises are not evaluated until we refer to the variable that holds them we can avoid computing values we do not need and we can make default parameters that depend on some computation inside a function call as long as we do those computations before we use the variable that needs them. For example, we can define a default parameter in terms of a variable we set inside a function

```{r}
h <- function(x, y = 2 * w) {
  w <- 2
  x + y
}
h(1)
```

but this will fail if we refer to the promise that needs the variable before we compute it:

```{r}
h <- function(x, y = 2 * w) {
  res <- x + y
  w <- 2
  res
}
h(1)
```

If a promise depend on a variable that we update during a computation we also have to be careful. The promise is only ever evaluated once; after we have evaluated a promise, the variable that used to hold it now hold the result of the evaluation and no longer the promise expression. If we change variables that occurred in the promise, we do not update the value the variable now hold.

```{r}
h <- function(x, y = 2 * w) {
  w <- 1
  res <- x + y
  w <- 2
  res
}
h(1)
```

For promises held by default parameters, this usually does not cause any problems. It is simple to follow what local variables will change in the function and at which point the promise will be evaluated. Lazy evaluation of arguments, however, is a common source of problems when combined with closures. Consider this function:

```{r}
make_adder <- function(n) function(m) n + m
```

which returns a closure that will add `n` to its argument, `m`. We can use it like this:

```{r}
add_1 <- make_adder(1)
add_2 <- make_adder(2)
add_1(1)
add_2(1)
```

No problems here, but now consider this:

```{r}
adders <- vector("list", 3)
for (i in 1:3) adders[[i]] <- make_adder(i)
```

The intent here is to create three adder functions that add 1, 2, and 3, respectively, to their argument. When we call the first function, though, we get an unpleasant surprise:

```{r}
adders[[1]](1)
```

The expression `n + m` inside the closure is not evaluated until we call it. Just before we evaluate the body in the `adders[[1]](1)` call, the environment graph looks like [@fig:addders]. All three adders are closures that refer to different instances of `make_adders`, but all these instances have `n` refer to a promise that is the expression `i`. The variable `i` is found in the global environment and not in the closure environment, and after we have created all three closures, `i` refers to the number 3. To evaluate `n + m` inside the adder, we must first evaluate the promise that `n` refers to—we search for `n` and find it in the parent environment of the function call, which is the closure environment, and here `n` refers to `i` that should be evaluated in the global environment. We evaluate it and now `n` refers to 3, see [@fig:adders-2]. This is why the result of calling `adders[[1]]` with `m` set to one returns four and not two.

![Adders just before evaluating the body of the `adders[[1]](1)` call.](figures/adders){#fig:adders}

![Adders after evaluating the `n` promise in `adders[[1]]`.](figures/adders-2){#fig:adders-2}

After we have called this closure, the variable `n` no longer refers to a promise but to the value 3, so changing `i` at this point will not affect the closure:

```{r}
i <- 1
adders[[1]](1)
```

It will, however, affect the closures where we haven’t evaluated the promise yet, so if we call one of the other closures after changing `i` we will see result of the change:

```{r}
adders[[2]](1)
```

This is a problem that only occurs when you create closures, but every time you do, the risk is there. You can avoid the problem by explicitly evaluating promises before you return the closure; this is what the function `force` is for.

```{r}
make_adder <- function(n) {
  force(n)
  function(m) n + m
}
for (i in 1:3) adders[[i]] <- make_adder(i)
for (i in 1:3) print(adders[[i]](0))
```


## Quotes and non-standard evaluation

What we have seen so far in this chapter is the standard way to evaluate expressions, but as you can probably guess, the reason we call it the standard way is because there are alternatives to it—non-standard evaluation. That would be any other way we could evaluate expressions.

Actually, even non-standard evaluation follows the rules for looking up variable to value mappings that standard evaluation follows. We have a chain of environments and we search them in turn. With non-standard evaluation, we just chain together environments in alternative ways.

To implement non-standard evaluation we first need an expression to evaluate—rather than the value that is the result of evaluating one. We have already seen two ways of obtaining such an expression: we have used `quote` to get an expression from a literal expression or we can use `substitute` to translate a function argument into an expression. There are other ways to create quoted expressions—see e.g. functions `expression` and `bquote`—and `substitute` can be used for more than simply translating function arguments into expressions, but `quote` and `substitute` on arguments suffices for most uses of non-standard evaluation. They both give us a quoted expression with no environment associated with it.

```{r}
ex1 <- quote(2 * x + y)
ex1
f <- function(ex) substitute(ex)
ex2 <- f(2 * x + y)
ex2
```

When implementing lambda expressions we used such expressions to create new functions. 

```{r}
g <- rlang::new_function(alist(x=, y=), body = ex1)
g
g(1,3)
```

A more direct way to evaluate an expression is using `eval`:

```{r}
x <- 1
y <- 3
eval(ex1)
```

With `eval`, we will evaluate the expression in the environment where we call `eval` by default, so above we evaluated `ex1` in the global environment and in the example below we evaluate it in the local environment of calls to function `h`:

```{r}
h <- function(x, y) eval(ex1)
h
h(1,3)
```

If we use the default environment in calls to `eval`, we get standard evaluation, but we do not *have* to use the default environment. We can provide an environment to `eval` that we want it to evaluate the expression in. For example, we can make function `h` evaluate `ex1` in the calling environment instead of its own local environment:

```{r}
h <- function(x, y) eval(ex1, rlang::caller_env())
x <- y <- 1
h(4,4)
```

Here, we call `h` from the global environment where `x` and `y` are set to one. Even though the local variables in the call to `h` are four and four, `2 * x + y` evaluates to three because it is the values of `x` and `y` in the global environment that are used.

Similarly, we can use an alternative environment for functions we create. By default, `new_function` will use the environment where we create the function, so for example, we can create a function that creates a closure this way:

```{r}
f <- function(x) rlang::new_function(alist(y=), ex1)
f(2)
f(2)(2)
```

We can provide an environment to `new_function`, however, to change this behaviour. Consider, for example, this function:

```{r}
g <- function(x) {
  rlang::new_function(alist(y=), ex1, rlang::caller_env())
}
g(2)
g(2)(2)
```

When we call `g` we get a new function, but *this* function will be evaluated in the scope where we *call* `g`, not the scope *inside* the call to `g`. Thus, the argument `x` to `g` will not be used when evaluating `2 * x + y`. In this example, we instead use the global variable `x`, which we set to one above.

With `eval`, the environment parameter doesn’t actually have to be an environment. You can use a `list` or a `data.frame` (which is strictly speaking also a `list`) instead.

```{r}
eval(ex1, list(x = 4, y = 8))
df <- data.frame(x = 1:4, y = 1:4)
eval(ex1, df)
```

Evaluating expressions in the scope of lists and data frames is a powerful tool exploited in domain specific languages such as `dplyr`, but lists and data frames do not have the graph structure that environments have, which begs the question: if we do not find a variable in the list or data frame, where do we find it when we call `eval`? To determine this, `eval` takes a third argument that determines the enclosing scope. If variables are not found in the environment parameter, then `eval` will search in the enclosing scope parameter.

Consider the functions `f` and `g` defined below:
```{r}
f <- function(expr, data, y) eval(expr, data)
g <- function(expr, data, y) eval(expr, data, rlang::caller_env())
```

They both evaluate an expression in a context defined by `data` but `f` then uses the function call scope as the enclosing scope while `g` uses the calling scope as the enclosing environment in the call to `eval`. Both take the parameter `y` but if we use `y` in the expression we pass to the functions, only `f` will use the parameter; `g`, on the other hand, will look for `y` in the calling scope if it is not in `data`:

```{r}
df <- data.frame(x = 1:4)
y <- 1:4
f(quote(x + y), df, y = 5:8) == 1:4 + 5:8
g(quote(x + y), df, y = 5:8) == 1:4 + 1:4
```

The combination of quoted expressions and non-standard evaluation is indubitably a very powerful tool for creating domain-specific languages, but it is not without its pitfalls, of which there are mainly two: complications about who is responsible for quoting expressions and complications about stringing environments together correctly.

Let us consider these in turn. Some code must be responsible for turning an expression into a quoted expression. The simplest solution to this is to make it up to the user to always quote expressions that must be quoted. This would be the solution in a function like this:

```{r}
f <- function(expr, data) eval(expr, data, rlang::caller_env())
f(quote(u + v), data.frame(u = 1:4, v = 1:4))
```

It is, however, a bit cumbersome to explicitly quote every time you call such a function, and it goes against the spirit of domain-specific languages where we want to make new syntax to make it easer to write code. However, if we let the function quote the expression using substitute, as in this function

```{r}
fq <- function(expr, data) {
  eval(substitute(expr), data, rlang::caller_env())
}
fq(u + v, data.frame(u = 1:4, v = 1:4))
```

then we potentially run into problems if we want to call this function from another function. We can try just calling `fq` with an expression:

```{r}
g <- function(expr) fq(expr, data.frame(u = 1:4, v = 1:4))
g(u + v)
```

This doesn’t work because `expr` is now considered a promise that should be evaluated in the global scope, so inside `fq` we try to evaluate the expression, which we cannot do because `u` and `v` are not defined. We would be even worse off if we used an expression that we actually *can* evaluate, because it wouldn’t be obvious that we were evaluating it in the wrong scope and thus on the wrong data

```{r}
u <- v <- 5:8
g(u + v)
```

We could try to get the expression quoted using `substitute` inside `g`:

```{r}
g <- function(expr) {
  fq(substitute(expr), data.frame(u = 1:4, v = 1:4))
}
g(u + v)
```

This fails in a different way. The expression that we get inside `fq` when that function calls substitute is the expression the function was called with, which is `substitute(expr)`. So it evaluates `substitute(substitute(expr))` and get `expr`, not `u + v`. The same would happen if we used `quote`

```{r}
g <- function(expr) {
  fq(quote(expr), data.frame(u = 1:4, v = 1:4))
}
g(u + v)
```

in this case because `quote(expr)` doesn’t substitute the function argument into `expr`.

There isn’t any good way to resolve this problem. If you call a function that quotes an expression, you should give it a literal expression to quote. Such functions are essentially not useful for programming—they provide an interface to a user of your domain-specific language, but you cannot use them to implement the language by calling them from other functions.

The solution is to have functions that expect expressions to be quoted, like the function `f` we wrote before `fq`, and use those when you call one function from another:

```{r}
g <- function(expr) {
  f(substitute(expr), data.frame(u = 1:4, v = 1:4))
}
g(u + v)
```

If you want some functionality to be available for programming—i.e. calling a function from another function—and also as an operation in your language, then write one that expects expressions to be quoted and another that wraps it:

```{r}
f <- function(expr, data) eval(expr, data, rlang::caller_env())
fq <- function(expr, data) f(substitute(expr), data)
fq(u + v, data.frame(u = 1:4, v = 1:4))
```

This, however, brings us to the second pitfall—getting environments wired up correctly. Consider these two functions:

```{r}
g <- function(x, y, z) {
  w <- x + y + z
  f(quote(w + u + v), data.frame(u = 1:4, v = 1:4))
}
h <- function(x, y, z) {
  w <- x + y + z
  fq(w + u + v, data.frame(u = 1:4, v = 1:4))
}
```

Function `g` explicitly quotes the expression `w + u + v` and calls `f`; `h` instead calls `fq` that takes care of the quoting for it. The first function works, the second does not:

```{r}
g(1:4, 1:4, 1:4) == (1:4 + 1:4 + 1:4) + 1:4 + 1:4
h(1:4, 1:4, 1:4) == (1:4 + 1:4 + 1:4) + 1:4 + 1:4
```

This time, the problem is not quoting. Both functions attempt to evaluate the same expression, `w + u + v`, inside function `f`. The problem is that the variable `w` is only available to `f` when we call it from `g`. To see why, consider the environments in play. We do not define any nested functions, so all three functions, `f`, `fq`, `g`, and `h`, only have access to their local environment and the global environment. The expression that `f` gets as its argument, however, is not evaluated in `f`’s local environment; it is evaluated in its caller’s environment. When `f` is called directly from `g`, the caller environment is the local environment of the `g` call, where `w` is defined. When `f` is called from `h`, however, it is not called directly. Since `h` calls `fq` that then calls `f`, the caller of `f` in this case is `fq`. The variable `w` is defined in the local scope of `h` but this is not where `f` tries to evaluate the expression; `f` tries to evaluate the expression in the scope of `fq` where `w` is *not* defined.

It is less obvious how to resolve this issue. It is, of course, possible to pass environments along with expressions as separate function parameters, but this quickly becomes cumbersome if we have to work with more than one expression. What we want, ideally, is to associate expressions with the environment in which we want to look up variables we do not explicitly override, for example by getting them from a data frame.

Expressions do not carry along with them any environment, so we cannot get there directly. Formulas, however, do. Instead of using expressions, we can use one-sided formulas. Quoting would now involve making a formula out of an expression. If the formula is one-sided, we can get the expression as the second element in it, and the environment where the formula is defined is available using the `environment` function. We can rewrite the `f` and `fq` functions to be based on formulas:

```{r}
ff <- function(expr, data) {
  eval(expr[[2]], data, environment(expr))
}
ffq <- function(expr, data) {
  expr <- eval(substitute(~ expr))
  environment(expr) <- rlang::caller_env()
  ff(expr, data)
}
```

With `ff` you need to explicitly create the formula—similar to how you had to explicitly quote expressions in `f`—and this automatically gives you the environment associated with the formula. With `ffq` we translate an expression into a formula using `substitute` and explicitly set its environment to the caller environment. We can now define `g` and `h` similar to before, except that `g` uses a formula instead of `quote`:

```{r}
g <- function(x, y, z) {
  w <- x + y + z
  ff(~ w + u + v, data.frame(u = 1:4, v = 1:4))
}
h <- function(x, y, z) {
  w <- x + y + z
  ffq(w + u + v, data.frame(u = 1:4, v = 1:4))
}
```

This time, both functions will evaluate the expressions in the right scope:

```{r}
g(1:4, 1:4, 1:4) == (1:4 + 1:4 + 1:4) + 1:4 + 1:4
h(1:4, 1:4, 1:4) == (1:4 + 1:4 + 1:4) + 1:4 + 1:4
```

Associating environments to expressions is the idea behind *quosures* from the `rlang` package. The word is a portmanteau created from quotes and closures—similar to how closures are functions with associated environments, quosures are quoted expressions with associated environments. Quosures are based on formulas, and we could use formulas as in the example we just saw, but the `rlang` package provide functionality that makes it much simpler to program domain-specific languages using quosures. 

## Quosures

The `rlang` package provided functions to replace `quote` and `substitute` that create quosures instead of expressions. To create a quosure from an expression, you use `quo`:

```{r}
q <- rlang::quo(2 * x)
q
```

Inside a function call, the quosure analogue to `substitute` is `enquo`:

```{r}
f <- function(expr) rlang::enquo(expr)
q <- f(2 * x)
q
```

In both of these examples, the scope associated with the quosure is the global environment because this is the level at which the expression is written.

A quosure is just a special type of formula, so we can access one as we did in the previous section to get the environment and expression

```{r}
q[[2]]
environment(q)
```

but `rlang` provides functions for working with quosures that make the intent of our code clearer. To get the expression out of a quosure, we use the function `UQE`—unquote expression:

```{r}
rlang::UQE(q)
```

I honestly think the name is a bit unfortunate since the *un*-quote function returns a quoted expression, but it does strip away the quosure-ness and gives us a raw expression. There is another unquote function, `UQ`, that *does* unquote an expression in the sense of evaluating it, but it has a different purpose that we get to in the next section.

Getting the environment associated with a quosure can be done using `environment` as we saw above, but the `rlang` function for this is `get_env`:

```{r}
rlang::get_env(q)
```

With quosures you can no longer evaluate them with `eval`. A quosure is a formula, and the result of evaluating a formula is the formula itself.

```{r}
eval(q)
```

Instead, you need to use the function `eval_tidy`

```{r}
x <- 1
rlang::eval_tidy(q)
x <- 2
rlang::eval_tidy(q)
```

The quosure is evaluated inside the environment it is associated with. We created this quosure, `q`, inside function `f`, but its environment is the global environment, so when we modify this, by changing `x`, it affects the result of evaluating `q`.

If we create a quosure inside a local function scope, it will remember this context—just like a closure. For example, if we define function `f` as this

```{r}
f <- function(x, y) rlang::quo(x + y + z)
```

the quosure will know the function parameters `x` and `y` from when `f` is called, but will have to find `z` elsewhere. Consider the contrast between evaluating the quosure and the expression `x + y + z` directly:

```{r}
q <- f(1, 2)
x <- y <- z <- 3
rlang::eval_tidy(q) # 1 + 2 + 3
x + y + z # 3 + 3 + 3
```

Just as `eval`, `eval_tidy` lets you provide a list, data frame, or environment with bindings from variables to values. When you do this, the values you provide will overrule the variables in the quosure’s environment—an effect known as *over-scoping*. Consider this:

```{r}
x <- 1:4
y <- 1:4
q <- quo(x+y)
rlang::eval_tidy(q)
rlang::eval_tidy(q, list(x = 5:8))
```

The quosure `q` is bound to the global environment, so when we evaluate it, `x` and `y` are both `1:4`. However, when we provide the second argument to `eval_tidy`, we can override the value of `x` to `5:8`. You will recognise this feature from `dplyr` where you have access to columns in data frames in arguments you provide to the functions there and these columns over rule any global variable that might otherwise have been used.

This, of course, can also be used to override variables in a function call with function parameters. Consider these two functions:

```{r}
f <- function(expr,x) {
  q <- rlang::enquo(expr)
  rlang::eval_tidy(q)
}
g <- function(expr,x) {
  q <- rlang::enquo(expr)
  rlang::eval_tidy(q, environment())
}
f(x + y, x = 5:8)
g(x + y, x = 5:8)
```

The function `f` simply evaluates the quosure in its scope, which doesn’t contain the function parameter `x`, while the function `g` over-scopes with the function environment, making the variable `x` refer to the function parameter rather than the global parameter.

The expression you evaluate with `eval_tidy` doesn’t have to be a quosure. The function is equally happy to evaluate bare expressions, and then it behaves just like `eval`:

```{r}
rlang::eval_tidy(quote(x + y))
```

Just like `eval`, `eval_tidy` takes a third argument that will behave as the enclosing scope.  This is used for bare expressions—those created with `quote`

```{r}
rlang::eval_tidy(quote(xx), env = list2env(list(xx = 5:8)))
```

but not with quosures

```{r}
rlang::eval_tidy(quo(xx), env = list2env(list(xx = 5:8)))
```

If you want to create a closure with over-scoping, i.e. you want to create a function that evaluates a quosure where it first finds local variables and then look in the quosure’s environment, you cannot directly call `eval_tidy` when creating the function. This would ask R to attempt to evaluate the closure, but you do not yet have the variables you need—those are provided when you call the closure. Instead, you can separate the bare expression and the environment of the quosure using `UQE` and `get_env`, respectively. Consider the function `make_funciton` below:

```{r}
make_function <- function(args, body) {
  body <- rlang::enquo(body)
  rlang::new_function(args, rlang::UQE(body), rlang::get_env(body))
}
f <- function(z) make_function(alist(x=, y=), x + y + z)
g <- f(z = 1:4)
g
g(x = 1:4, y = 1:4)
```

Here, `make_function` takes two arguments, a pair list of arguments and an expression for the body of a function. It is a little more primitive than the lambda expressions we wrote in the previous chapter, but it is essentially doing the same thing, in this example I am just focusing on the closure we create rather than on language design issues. We translate the function body into a quosure, which guarantees that we have an environment associated with it. In the function we create using `new_function`, however, we strip the environment from the quosure to create the body of the new function, but we assign the function’s environment to be the quosure environment.

In the function `f` we have a local scope that knows the value of `z` and in this scope we create a new function. The quosure we get from this is associated with the local `f` scope, so it also knows about `z`. When call `g` we provide variables `x` and `y`, but the `z` value is taken from the local scope of `f`.

Of course, if called directly, there isn’t any difference between using the caller’s environment or the quosure’s environment 

```{r}
make_function_quo <- function(args, body) {
  body <- rlang::enquo(body)
  rlang::new_function(args, rlang::UQE(body), rlang::get_env(body))
}
make_function_quote <- function(args, body) {
  body <- substitute(body)
  rlang::new_function(args, body, rlang::caller_env())
}
g <- make_function_quo(alist(x=, y=), x + y)
h <- make_function_quote(alist(x=, y=), x + y)
g(x = 1:4, y = 1:4)
h(x = 1:4, y = 1:4)
```

However, consider a more involved example, where we collect expressions in a list and have a function for translating all the expressions into functions that we can then apply over values using the `invoke_map` function from the `purrr` package. We can construct the expressions like this, using the linked lists structure we have previously used:

```{r, echo=FALSE}
cons <- function(elm, lst) list(car=elm, cdr=lst)
lst_length <- function(lst) {
  len <- 0
  while (!is.null(lst)) {
    lst <- lst$cdr
    len <- len + 1
  }
  len
}
lst_to_list <- function(lst) {
  v <- vector(mode = "list", length = lst_length(lst))
  index <- 1
  while (!is.null(lst)) {
    v[[index]] <- lst$car
    lst <- lst$cdr
    index <- index + 1
  }
  v
}
```

```{r}
expressions <- function() list(ex = NULL)
add_expression <- function(ex, expr) {
  ex$ex <- cons(rlang::enquo(expr), ex$ex)
  ex
}
```

Translating the expressions into functions is straightforward, we just need to reverse the resulting list if we want the functions in the order we add them, since we prepend expressions when we use the linked lists:

```{r}
make_functions <- function(ex, args) {
  results <- vector("list", length = lst_length(ex$ex))
  i <- 1; lst <- ex$ex
  while (!is.null(lst)) {
    results[[i]] <- 
      rlang::new_function(args, rlang::UQE(lst$car), 
                           rlang::get_env(lst$car))
    i <- i + 1
    lst <- lst$cdr
  }
  rev(results)
}
```

With this small domain-specific language for collecting expressions, we can write a function that creates expressions for computing y-coordinates of a line given an intercept:

```{r}
make_line_expressions <- function(intercept) {
  expressions() %>% 
    add_expression(coef + intercept) %>%
    add_expression(2*coef + intercept) %>% 
    add_expression(3*coef + intercept) %>% 
    add_expression(4*coef + intercept)
}
```

The expressions know the intercept when we call `make_line_expressions`—that is the intent at least—but the coefficient should be added later in a function call. We can create the functions for the expressions using another function:

```{r}
eval_line <- function(ex, coef) {
  ex %>% make_functions(alist(coef=)) %>%
    purrr::invoke_map(coef = coef) %>% unlist
}
```

We can now pipe these functions together to get points on a line:

```{r}
make_line_expressions(intercept = 0) %>% eval_line(coef = 1)
make_line_expressions(intercept = 0) %>% eval_line(coef = 2)
make_line_expressions(intercept = 1) %>% eval_line(coef = 1)
```

All works as intended here, but what would happen if we used quotes instead? It is simple to write the corresponding functions:

```{r}
add_expression <- function(ex, expr) {
  ex$ex <- cons(substitute(expr), ex$ex)
  ex
}
make_functions <- function(ex, args) {
  results <- vector("list", length = lst_length(ex$ex))
  i <- 1; lst <- ex$ex
  while (!is.null(lst)) {
    results[[i]] <- rlang::new_function(args, lst$car, rlang::caller_env())
    i <- i + 1
    lst <- lst$cdr
  }
  rev(results)
}
```

We will get an error if we try to use them as before, however:

```{r}
make_line_expressions(intercept = 0) %>% eval_line(coef = 1)
```

The reason for this is easy to see once we consider which environments contain information about the intercept. This variable lives in the scope of calls to `make_line_expressions`, but when we create the functions we do so by calling `make_functions` from inside `eval_line`. The functions are created with `eval_line` local environments as their closures, and `intercept` is not found there.

In general, it is safer to use quosures than bare expressions for non-standard evaluation exactly because they carry their environment along with them, alleviating many of the problems we otherwise have with keeping track of which environment to evaluate expressions in.

## Quasi-quoting

```{r}
x <- y <- 1
quote(2 * x + !!y)
rlang::expr(2 * x + !!y)
rlang::quo(2 * x + !!y)
```

```{r}
x <- y <- 2
rlang::expr(!!x + y)
rlang::expr(!!x & y)
```

```{r}
rlang::expr(UQ(x) + y)
```


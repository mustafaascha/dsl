
# Functions, classes and operators

Everything you do in R, you do with functions, so if you want to implement a domain specific language, you must do so by writing functions. All the actions your new DSL should support, you must implement using functions, and should you want a special syntax for your DSL, you will have to write functions for parsing this syntax. When implementing an embedded DSL, as we shall see, much of the parsing can be outsourced to R’s parser. The price for this is some restrictions to the syntax for the DSL—the DSL syntactically valid R code. We cannot construct arbitrary syntaxes for embedded languages, but by using operator overloading and by defining new infix operators we do have some flexibility in designing our DSLs.

## Writing parsers for embedded DSLs

There isn’t necessarily a clear separation between parsing your DSL and evaluating expressions written in it. In many cases, embedded DSLs describe a series of operations to be done sequentially—this is, for example, the case with graphical transformations in `ggplot2` or data transformations in `magrittr` and `dplyr`. When this is the case, you wouldn’t necessarily split evaluations of DSL expressions into a parsing phase and an evaluation phase; you can perform transformations one at a time as they are seen by the R parser. Conceptually, though, there is still two steps involved—parsing a DSL statement and evaluating it—and with more complex DSL you have to be explicit about this. In this chapter, we first cover the parsing part and then the evaluation part, but in the running example, parts of the evaluation step will be done when parsing expressions and part at a later step.

We will develop a small language for drawing on a canvas. The language will allow us to create a *canvas* of a given size, and *graphical objects* that are either *points* or *line segments* or *groups* of these. Graphical objects can be modified by various operations that can *translate* (move) them, *scale* them, *rotate* them, or set graphical properties such as *line width* and *colour*. Graphical objects can be added to a canvas to create a drawing, and a canvas can be plotted to display a drawing.

We are developing a very simple language, but a language nonetheless. We have nouns—the canvases and graphical objects—and verbs—the transformations we can do on graphical objects. Adding a grammar for how to combine the nouns and verbs and we have our language syntax, and adding a semantics on top of this, and we know how to evaluate expressions in it.

### Text, tokens, grammars, and semantics

Before we dig into this drawing DSL, though, we need to define some terminology. Now, this book is not about language or parser theory, so I will stick with some informal working definitions, but there are a few terms we need in this chapter that we need to define.

When we look at a language, we can look at it at different levels of detail, from the most basic components to the meaning associated with sentences. For a spoken language, the most basic components are the *phonemes*—the distinct sounds used in the language. Strong together, phonemes become words, words combine to make sentences, and sentences have meaning. For a written language, the atomic elements are *glyphs*—the letters in languages written using alphabets, such as English. Sequences of letters can form words, but a written sentence contains more than just words—we have punctuation symbols as well. Together, we can call these *tokens*. A string of *tokens* form a sentence, and again, we can assign meaning to sentences.

For computer languages, we have the same levels of abstractions on strings of symbols. The most primitive level is just a stream of input characters, but we will have rules for translating such character sequences into sequences of *tokens*. This process is called *tokenisation*. The formal definition of a programming language will specify what the available tokens in the language are, and how a string of characters should be translated into a string of tokens.

Consider the following string of R code:

```r
foo(x, 2*x)
```

This is obviously a function call, but seen by the tokeniser it is a string of characters that it needs to translate into a sequence of tokens. What it will produce is this:

```
identifier["foo"] '(' identifier["x"], 
                      number[2], '*', identifier["x"] 
                   ')'
```

I'm using a home-brewed notation for this, but the idea is that a tokeniser will recognise that there are some identifiers---and it will identify those and what the identifiers are---and a number and then some verbatim tokens such as `'('`, `'*'`, and `')'`.

The tokeniser, however, will be equally happy to process a string such as

```r
foo x ( 2 ) x *
```

into the sequence

```
identifier["foo"] identifier["x"] '('
                  number[2] ')' identifer["x"] '*'
```

This is clearly not a valid piece of R code, but the tokeniser does not worry about this. It simply translate the string into a sequence of tokens (with some associated data, such as the strings "foo" and "x" for the identifiers and the number 2 for the number). It doesn't worry about higher levels of the language.

What a language consider a valid string of tokens is defined by its *grammar*.^[Technically, what I refer to as *grammar* is actually *syntax*. Linguists use *grammar* to refer to both *morphology* and *syntax*, where *syntax* is the rules for stringing words together. In computer science, though, the term *grammar* is used as I use it here, so I will use syntax and grammar interchangeably.] A *parser* is responsible for translating a sequence of tokens into an expression or a language statement. Usually, what a parser does is that it translates a string of tokens into an expression *tree*. **FIXME: more here**

Grammatical statements are those a parser will consider valid. It is, if we return to natural languages, those sentences that obey the grammatical rules. This is distinct from the set of sentences that have some associated *meaning*. It is quite possible to construct meaningless, but grammatically correct, sentences. The sentence "Colourless green ideas sleep furiously" is such a sentence, created by the linguist Noam Chomsky. It is completely correct grammatically and also completely meaningless. *Semantics* is the term we use to link grammatical sentences to their meaning. You will know this distinction in programming languages when you run into runtime exceptions. If you get an exception when you run a program, you will have constructed a grammatical sentence---otherwise the parser would have complained about syntactical errors---but a sentence that does not have a well defined meaning. Perhaps because you try to add a number to a string---this happens when the statements you have written violates type rules. Semantics, when it comes to programming languages, define what actual computations a statement describes. A compiler or an interpreter---the latter for R programs---give meaning to grammatical statements.^[Notice, however, that there is a distinction between giving a statement meaning and giving it the *correct* meaning. Just because your program computes *something* doesn't mean that it computes what you intended it to. When we construct a language, domain specific or general, we can give meaning to statements, but we cannot---this is theoretically impossible---guarantee that it is the *intended* meaning. That will always be the responsibility of the programmer.]

**FIXME: context of DSL embedded in R**

### Specifying a grammar

Now we get to defining the grammar for our drawing language. First, the syntax for creating a drawing:

```
DRAWING := CANVAS
        | DRAWING '+' GRAPHICAL_OBJECT
```

You should read this as “a drawing is either a canvas or a drawing we have added a graphical object to”. The definition is recursive—a drawing is defined in terms of another drawing—but we have a basis case, a canvas, that lets us create a basic drawing, and from such a drawing we can create more complex drawings by adding graphical objects.

The syntax I use here for specifying grammars is itself a grammar—a meta-grammar if you will. The way you should interpret it is thus: the grammatical object we are defining is to the left of the `:=` object. After that, we have a sequence of one or more ways to construct such an object, separated by `|`. These rules for constructing the object we define will be a sequence of other grammatical objects. These can either be objects we define by other rules—I will write those in all capitals and refer to them as meta-variables—or concrete lexical tokens—I write those in single quotes, as the `'+'` in the second rule for creating a `DRAWING`.

Meta-grammars like this are used to formally define languages, and there are many tools that will let you automatically create parsers from a grammar specification in a meta-grammar. I will use this home-made meta-grammar much less formally. I just use it as a concise way of describing the grammar of a DSL we create, and you can think of it as simply pseudo-code for a grammar.

To create a drawing we must follow the meta-grammar rules, so we must use one of the two alternatives provided: either create a canvas or create a drawing from another drawing. The second option is, of course, only possible if we can create a drawing to begin with, so we must always start with a canvas. We need another rule for creating a `CANVAS`, then.

```
CANVAS := 'canvas(<options>)'
```

This rule tells us that to create a `CANVAS` we need to use the verbatim `canvas(<options>)`, except that I will define `<options>` to generally mean whatever options an R function takes. This is partly laziness—I don’t want to specify the grammar for the various way you can call an R function for every time I define a grammar, and partly to allow functions to be extended without having to worry about updating a grammar. You should read this rule as such: to create a `CANVAS`, you should just call the R function `canvas`.

The `CANVAS` rule doesn’t refer to any meta-variables on the right, so it gives us a way to construct a canvas directly. We will call such rules *terminal* rules. In actual expressions in the DSL we must always write the expressions created from terminal rules, but the grammatical objects we create depend on how we get to those terminal rules from the meta-variables. We can, using just the rules we have now, create a concrete drawing by following these rules:

```
DRAWING > CANVAS > 'canvas(<options>)'
```

You should read this as: “a DRAWING is a CANVAS is a call to the `canvas` function. If we define a `canvas` function, we therefore  have a way to make a basic drawing.

If there are several different ways to go from meta-variables to the same sequence of terminal rules (so there are several rules that lead to the exact sequence of lexical tokens), then we have a problem with interpreting the language. The exact same sequence of tokens could be interpreted as specifying two different grammatical objects. This isn’t so unusual in natural languages— ”the hungry dog” can be both a subject and an object, for example—and can be resolved through context. “The hungry dog ate the sausage” (“the hungry dog” is a subject) versus “I saw the hungry dog” (“the hungry dog” is a direct object) versus “I gave the hungry dog a sausage” (“the hungry dog” is an indirect object). Ambiguous sequences of tokens can also be resolved into specific meta-grammar rules via context, but doing this complicates parsing: when parsing a sequence of tokens, we will need to know which context we are in.

If you are writing your own parser entirely, you can pass context along as you parse a sequence of tokens, but if you want to exploit R’s parser and create an embedded DSL, you are better off ensuring that all grammatically valid sequences of tokens unambiguously refer to one grammatical meta-variable. The meta-grammar does not guarantee this, but we will be careful to construct our DSLs such that this is true.

**FIXME: this is only possible because we have R to tokenize**

Getting back to the drawing DSL, we can make the following rule for constructing graphical objects:

```
GRAPHICAL_OBJECT := POINT 
                 | LINE_SEGMENT 
                 | GRAPHICAL_OBJECT TRANSFORMATION
```




**FIXME: example here**


### Classes and generic functions



### Operator overloading

## Evaluating DSL expressions

# Find Formula

Infers a formula, with the exact or approximate solution out of a input-response set.

In other words, it predicts *f*

```         
f(X, Y) = Z
```

provided X, Y, and Z are float-vectors.

## Installation

Make sure R, Maxima, and Python are installed.

## Generator

*Generator* outputs formulas out of a grammar capped at a given tree height.

Formulas are treated further in a separate environment in order to normalize them, and in most of the cases canonize them.

Practical results are that equivalent, non-identical formula (ie. normalized but non-canonized) seldom appear.

## Further developments

Generate a pool of grammars to solve more domain-specific problems

-   Discover expression
-   Discover 1-arg formula (various grammars)
-   Discover 2-args formula (various grammars)
-   Discover 3-args formula (various grammars)
-   Discover 4- to 20-args formula (simpler grammars)

Make *findformula* work for various domains.

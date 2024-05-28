# Find Formula

Infers a formula, with the exact or approximate solution out of a input-response set.

In other words, it predicts *f*

```         
f(X, Y) = Z
```

provided X, Y, and Z are float-vectors.

## Installation

Make sure R is installed. Maxima is required for `find_model`.

Additionally, for grammar expansion and development, Python is required.

## Usage

**find_formula**

```r
> x1 <- rnorm(500, mean = 10, sd = 2)
> x2 <- runif(500, -10, 50)
> y <- (x2/(x1 + x2*sqrt(x1)))^(-1)
> find_formula(y, x1, x2)
Found this one (exact): (sqrt(x1)*x2+x1)/x2
```

**find_model**

```r
> x1 <- rnorm(500, mean=10, sd=2)
> x2 <- runif(500, -10, 50)
> y <- (x2/(x1 + x2*sqrt(x1)))^(-1)
> find_model(y, x1, x2)
Best formula found was

   (1.0*(1.0*sqrt(1.0*x1)*x2+1.0*x1))/x2

RMSE = 3.19605423084161e-17
```

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

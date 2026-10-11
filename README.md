# transform

Automatically tests a numeric vector for normality, picks the best normalizing
transformation (log, sqrt, Box-Cox, Yeo-Johnson, rank-based inverse normal),
and lets you invert it back to the original scale.

## Installation

```r
# install.packages("remotes")
remotes::install_github("chapagainsujan/transform")
```

## Usage

```r
library(transform)
set.seed(1)
x <- rlnorm(200)

res <- auto_transform(x)   # summary + candidate comparison
res
plot(res)                  # 2x2 histogram / Q-Q diagnostics
inverse_transform(res)     # back to original scale
predict(res, newdata = c(0.5, 1, 2))
```

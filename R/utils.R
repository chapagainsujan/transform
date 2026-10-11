# Internal helpers (not exported) ---------------------------------------------

#' @importFrom stats approx lm optimize pchisq qnorm sd shapiro.test
NULL

.methods <- c("log", "sqrt", "boxcox", "yeojohnson", "int")

## Moments ---------------------------------------------------------------------

.skewness <- function(x) {
  m <- mean(x)
  mean((x - m)^3) / mean((x - m)^2)^1.5
}

## Normality tests -------------------------------------------------------------

# D'Agostino-Pearson K^2 omnibus test (valid for n >= 20, used for n > 5000).
.dagostino_pearson <- function(x) {
  n <- length(x)
  m <- x - mean(x)
  m2 <- mean(m^2); m3 <- mean(m^3); m4 <- mean(m^4)
  b1 <- m3 / m2^1.5
  b2 <- m4 / m2^2

  # skewness Z (D'Agostino 1970)
  Y <- b1 * sqrt((n + 1) * (n + 3) / (6 * (n - 2)))
  beta2 <- 3 * (n^2 + 27 * n - 70) * (n + 1) * (n + 3) /
    ((n - 2) * (n + 5) * (n + 7) * (n + 9))
  W2 <- -1 + sqrt(2 * (beta2 - 1))
  delta <- 1 / sqrt(0.5 * log(W2))
  a <- sqrt(2 / (W2 - 1))
  Z1 <- delta * log(Y / a + sqrt((Y / a)^2 + 1))

  # kurtosis Z (Anscombe & Glynn 1983)
  E <- 3 * (n - 1) / (n + 1)
  V <- 24 * n * (n - 2) * (n - 3) / ((n + 1)^2 * (n + 3) * (n + 5))
  xx <- (b2 - E) / sqrt(V)
  sb1 <- 6 * (n^2 - 5 * n + 2) / ((n + 7) * (n + 9)) *
    sqrt(6 * (n + 3) * (n + 5) / (n * (n - 2) * (n - 3)))
  A <- 6 + 8 / sb1 * (2 / sb1 + sqrt(1 + 4 / sb1^2))
  Z2 <- ((1 - 2 / (9 * A)) -
           ((1 - 2 / A) / (1 + xx * sqrt(2 / (A - 4))))^(1 / 3)) /
    sqrt(2 / (9 * A))

  K2 <- Z1^2 + Z2^2
  list(statistic = K2, p.value = pchisq(K2, df = 2, lower.tail = FALSE))
}

# Returns statistic, p.value, test name and a "score" (higher = more normal).
.normality_test <- function(x) {
  if (length(x) <= 5000L) {
    st <- shapiro.test(x)
    list(statistic = unname(st$statistic), p.value = st$p.value,
         test = "Shapiro-Wilk", score = unname(st$statistic))
  } else {
    dp <- .dagostino_pearson(x)
    list(statistic = dp$statistic, p.value = dp$p.value,
         test = "D'Agostino-Pearson", score = -dp$statistic)
  }
}

## Individual transformations --------------------------------------------------

.boxcox_fun <- function(x, lambda) {
  if (abs(lambda) < 1e-8) log(x) else (x^lambda - 1) / lambda
}
.boxcox_inv <- function(y, lambda) {
  if (abs(lambda) < 1e-8) exp(y) else (lambda * y + 1)^(1 / lambda)
}

.yj_fun <- function(x, lambda) {
  y <- numeric(length(x))
  pos <- !is.na(x) & x >= 0
  neg <- !is.na(x) & x < 0
  y[!pos & !neg] <- NA_real_
  y[pos] <- if (abs(lambda) < 1e-8) log1p(x[pos])
            else ((x[pos] + 1)^lambda - 1) / lambda
  y[neg] <- if (abs(lambda - 2) < 1e-8) -log1p(-x[neg])
            else -((-x[neg] + 1)^(2 - lambda) - 1) / (2 - lambda)
  y
}
.yj_inv <- function(y, lambda) {
  x <- numeric(length(y))
  pos <- !is.na(y) & y >= 0
  neg <- !is.na(y) & y < 0
  x[!pos & !neg] <- NA_real_
  x[pos] <- if (abs(lambda) < 1e-8) expm1(y[pos])
            else (y[pos] * lambda + 1)^(1 / lambda) - 1
  x[neg] <- if (abs(lambda - 2) < 1e-8) 1 - exp(-y[neg])
            else 1 - (1 - (2 - lambda) * y[neg])^(1 / (2 - lambda))
  x
}

# Maximum-likelihood lambda for Yeo-Johnson
.yj_lambda <- function(x) {
  n <- length(x)
  s <- sum(sign(x) * log1p(abs(x)))
  ll <- function(l) {
    y <- .yj_fun(x, l)
    v <- mean((y - mean(y))^2)
    if (!is.finite(v) || v <= 0) return(-Inf)
    -n / 2 * log(v) + (l - 1) * s
  }
  optimize(ll, interval = c(-5, 5), maximum = TRUE)$maximum
}

# Maximum-likelihood lambda for Box-Cox (via MASS::boxcox).
# A formula is used (not a data frame) so MASS can find `xx` in this frame.
.boxcox_lambda <- function(x) {
  xx <- x
  bc <- MASS::boxcox(xx ~ 1, lambda = seq(-3, 3, by = 0.01), plotit = FALSE)
  bc$x[which.max(bc$y)]
}

# Rank-based inverse normal transformation (Blom offsets)
.int_blom <- function(x) {
  n <- length(x)
  qnorm((rank(x, ties.method = "average") - 3 / 8) / (n + 1 - 2 * 3 / 8))
}

## Dispatchers -----------------------------------------------------------------

# Fit a transformation to x; returns list(y, lambda, shift).
.fit_transform <- function(x, method) {
  mn <- min(x)
  switch(
    method,
    log = {
      shift <- if (mn > 0) 0 else 1 - mn
      list(y = log(x + shift), lambda = NA_real_, shift = shift)
    },
    sqrt = {
      shift <- if (mn >= 0) 0 else -mn
      list(y = sqrt(x + shift), lambda = NA_real_, shift = shift)
    },
    boxcox = {
      if (mn <= 0) {
        stop("Box-Cox requires strictly positive data.", call. = FALSE)
      }
      lambda <- .boxcox_lambda(x)
      list(y = .boxcox_fun(x, lambda), lambda = lambda, shift = NA_real_)
    },
    yeojohnson = {
      lambda <- .yj_lambda(x)
      list(y = .yj_fun(x, lambda), lambda = lambda, shift = NA_real_)
    },
    int = list(y = .int_blom(x), lambda = NA_real_, shift = NA_real_),
    stop("Unknown method: ", method, call. = FALSE)
  )
}

# Apply a fitted transformation to (new) data.
.forward <- function(method, x, lambda, shift, ref_x, ref_y) {
  switch(
    method,
    none = x,
    log = log(x + shift),
    sqrt = sqrt(x + shift),
    boxcox = .boxcox_fun(x, lambda),
    yeojohnson = .yj_fun(x, lambda),
    int = approx(ref_x, ref_y, xout = x, rule = 2, ties = mean)$y,
    stop("Unknown method: ", method, call. = FALSE)
  )
}

# Invert a fitted transformation.
.backward <- function(method, y, lambda, shift, ref_x, ref_y) {
  switch(
    method,
    none = y,
    log = exp(y) - shift,
    sqrt = y^2 - shift,
    boxcox = .boxcox_inv(y, lambda),
    yeojohnson = .yj_inv(y, lambda),
    int = approx(ref_y, ref_x, xout = y, rule = 2, ties = mean)$y,
    stop("Unknown method: ", method, call. = FALSE)
  )
}

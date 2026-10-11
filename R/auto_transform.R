#' Automatically select a normalizing transformation
#'
#' Tests a numeric vector for normality. If the data are not normal, several
#' candidate transformations are fitted and the one that brings the data
#' closest to normality is returned.
#'
#' @section Normality tests:
#' For \eqn{n \le 5000} the Shapiro-Wilk test is used. For larger samples the
#' D'Agostino-Pearson omnibus (skewness + kurtosis) test is used, because
#' [stats::shapiro.test()] is limited to 5000 observations.
#'
#' @section Candidate transformations:
#' \describe{
#'   \item{`"log"`}{`log(x)`, or `log(x + c)` with `c = 1 - min(x)` when
#'     `min(x) <= 0`.}
#'   \item{`"sqrt"`}{`sqrt(x)`, or `sqrt(x + c)` with `c = -min(x)` when
#'     `min(x) < 0`.}
#'   \item{`"boxcox"`}{Box-Cox with the maximum-likelihood lambda from
#'     [MASS::boxcox()]. Requires strictly positive data.}
#'   \item{`"yeojohnson"`}{Yeo-Johnson with maximum-likelihood lambda;
#'     valid for positive, zero and negative values.}
#'   \item{`"int"`}{Rank-based inverse normal transformation (Blom offsets).}
#' }
#'
#' @section Selection criterion:
#' Candidates are ranked by the normality-test statistic after transformation
#' (Shapiro-Wilk W, or minus the D'Agostino-Pearson K-squared for
#' \eqn{n > 5000}). The W statistic is used rather than the p-value because
#' p-values saturate near 0 or 1 and cannot discriminate between good
#' candidates. If no candidate improves on the original data, the original
#' data are returned.
#'
#' @param x A numeric vector. `NA`/`NaN` values are removed for fitting (with a
#'   warning) and kept as `NA` in the output. Infinite values are an error.
#' @param alpha Significance level for the normality test, in (0, 1).
#'   Default `0.05`.
#' @param method One of `"auto"` (default), `"log"`, `"sqrt"`, `"boxcox"`,
#'   `"yeojohnson"` or `"int"`. With `"auto"`, data that already pass the
#'   normality test are returned unchanged. A specific method is always
#'   applied.
#'
#' @return An object of class `"auto_transform"`, a list with elements:
#' \describe{
#'   \item{transformed_data}{Transformed numeric vector (same length as `x`).}
#'   \item{method_used}{Name of the selected method (`"none"` if the data
#'     were already normal or no transformation helped).}
#'   \item{lambda}{Power parameter (Box-Cox / Yeo-Johnson), else `NA`.}
#'   \item{shift}{Additive constant (log / sqrt), else `NA`.}
#'   \item{original_p_value}{Normality-test p-value before transformation.}
#'   \item{final_p_value}{Normality-test p-value after transformation.}
#'   \item{original_data}{The original vector (needed for inversion).}
#'   \item{alpha, n, test, skewness_original, skewness_final}{Diagnostics.}
#'   \item{candidates}{Data frame comparing all candidates evaluated.}
#' }
#'
#' @seealso [inverse_transform()], [plot.auto_transform()]
#' @export
#'
#' @examples
#' set.seed(1)
#'
#' # Positively skewed data
#' x <- rlnorm(200)
#' res <- auto_transform(x)
#' res
#' plot(res)
#' all.equal(inverse_transform(res), x)
#'
#' # Data with zeros and negatives
#' y <- rexp(200) - 1
#' auto_transform(y)
#'
#' # Already normal: returned unchanged
#' auto_transform(rnorm(100))
#'
#' # Force a particular method
#' auto_transform(x, method = "yeojohnson")
auto_transform <- function(x, alpha = 0.05, method = "auto") {
  method <- match.arg(method, c("auto", .methods))
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }
  if (!is.numeric(x) || is.factor(x)) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }
  x_full <- as.numeric(x)
  if (any(is.infinite(x_full))) {
    stop("`x` contains infinite values.", call. = FALSE)
  }
  ok <- !is.na(x_full)
  if (any(!ok)) {
    warning(sum(!ok), " missing value(s) removed before fitting; ",
            "they are kept as NA in the output.", call. = FALSE)
  }
  xv <- x_full[ok]
  if (length(xv) < 3L) {
    stop("At least 3 non-missing observations are required.", call. = FALSE)
  }
  if (length(unique(xv)) < 2L || sd(xv) == 0) {
    stop("`x` is constant; a transformation cannot be evaluated.",
         call. = FALSE)
  }

  orig_test <- .normality_test(xv)
  skew_orig <- .skewness(xv)

  # Helper to build the return object ----------------------------------------
  build <- function(y, method_used, lambda, shift, final_test, cands) {
    y_full <- rep(NA_real_, length(x_full))
    y_full[ok] <- y
    structure(
      list(
        transformed_data = y_full,
        method_used = method_used,
        lambda = lambda,
        shift = shift,
        original_p_value = orig_test$p.value,
        final_p_value = final_test$p.value,
        original_data = x_full,
        alpha = alpha,
        n = length(xv),
        test = orig_test$test,
        skewness_original = skew_orig,
        skewness_final = .skewness(y),
        candidates = cands
      ),
      class = "auto_transform"
    )
  }

  empty_cands <- data.frame(method = character(), statistic = numeric(),
                            p_value = numeric(), lambda = numeric(),
                            shift = numeric(), stringsAsFactors = FALSE)

  # Already normal -----------------------------------------------------------
  if (method == "auto" && orig_test$p.value > alpha) {
    message("Data are already normally distributed (", orig_test$test,
            " p = ", format.pval(orig_test$p.value, digits = 3),
            " > alpha = ", alpha, "); returning the original data.")
    return(build(xv, "none", NA_real_, NA_real_, orig_test, empty_cands))
  }

  # Evaluate candidates ------------------------------------------------------
  to_try <- if (method == "auto") .methods else method
  fits <- list()
  rows <- list()
  for (m in to_try) {
    fit <- tryCatch(.fit_transform(xv, m), error = function(e) {
      if (method != "auto") stop(conditionMessage(e), call. = FALSE)
      NULL
    })
    if (is.null(fit) || any(!is.finite(fit$y)) ||
        length(unique(fit$y)) < 2L) next
    tst <- tryCatch(.normality_test(fit$y), error = function(e) NULL)
    if (is.null(tst)) next
    fits[[m]] <- list(fit = fit, test = tst)
    rows[[m]] <- data.frame(method = m, statistic = tst$statistic,
                            p_value = tst$p.value, lambda = fit$lambda,
                            shift = fit$shift, score = tst$score,
                            stringsAsFactors = FALSE)
  }
  if (length(fits) == 0L) {
    stop("No candidate transformation could be fitted to `x`.", call. = FALSE)
  }
  cands <- do.call(rbind, rows)
  cands <- cands[order(-cands$score), ]
  rownames(cands) <- NULL
  best <- cands$method[1L]
  cands$score <- NULL

  # Nothing improves on the original data (auto mode only) -------------------
  if (method == "auto" && fits[[best]]$test$score <= orig_test$score) {
    message("No candidate transformation improved normality; ",
            "returning the original data.")
    return(build(xv, "none", NA_real_, NA_real_, orig_test, cands))
  }

  sel <- fits[[best]]
  if (sel$test$p.value <= alpha) {
    warning("The best transformation (", best, ") did not achieve normality ",
            "at alpha = ", alpha, " (p = ",
            format.pval(sel$test$p.value, digits = 3), ").", call. = FALSE)
  }
  build(sel$fit$y, best, sel$fit$lambda, sel$fit$shift, sel$test, cands)
}

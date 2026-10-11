#' Print an `auto_transform` object
#'
#' @param x An object of class `"auto_transform"`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.auto_transform <- function(x, ...) {
  fmt_p <- function(p) format.pval(p, digits = 3, eps = 1e-16)
  cat("Automatic normalizing transformation\n")
  cat("------------------------------------\n")
  cat(sprintf("Observations : %d\n", x$n))
  cat(sprintf("Normality test: %s (alpha = %g)\n", x$test, x$alpha))
  cat(sprintf("Method used  : %s\n", x$method_used))
  if (!is.na(x$lambda)) cat(sprintf("Lambda       : %.4f\n", x$lambda))
  if (!is.na(x$shift))  cat(sprintf("Shift        : %.4f\n", x$shift))
  cat("\n")
  summ <- data.frame(
    p_value  = c(fmt_p(x$original_p_value), fmt_p(x$final_p_value)),
    skewness = round(c(x$skewness_original, x$skewness_final), 3),
    normal   = c(x$original_p_value > x$alpha, x$final_p_value > x$alpha),
    row.names = c("Original", "Transformed")
  )
  print(summ)
  if (nrow(x$candidates) > 0L) {
    cat("\nCandidates (best first):\n")
    cd <- x$candidates
    cd$statistic <- round(cd$statistic, 4)
    cd$p_value <- vapply(cd$p_value, fmt_p, character(1))
    cd$lambda <- round(cd$lambda, 4)
    cd$shift <- round(cd$shift, 4)
    print(cd, row.names = FALSE)
  }
  invisible(x)
}

#' Diagnostic plots for an `auto_transform` object
#'
#' Draws a 2 x 2 panel: histogram and normal Q-Q plot of the data before
#' (top row) and after (bottom row) transformation.
#'
#' @param x An object of class `"auto_transform"`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
#' @importFrom graphics hist lines par
#' @importFrom stats density qqnorm qqline
plot.auto_transform <- function(x, ...) {
  ok <- !is.na(x$original_data)
  before <- x$original_data[ok]
  after  <- x$transformed_data[ok]

  op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  on.exit(par(op))

  panel <- function(v, label, p) {
    hist(v, freq = FALSE, main = sprintf("%s: histogram", label),
         xlab = "Value", col = "grey85", border = "white")
    lines(density(v), lwd = 2)
    qqnorm(v, main = sprintf("%s: Q-Q plot (p = %s)", label,
                             format.pval(p, digits = 3, eps = 1e-16)),
           pch = 19, cex = 0.6)
    qqline(v, col = "red", lwd = 2)
  }
  panel(before, "Original", x$original_p_value)
  panel(after, sprintf("Transformed (%s)", x$method_used), x$final_p_value)
  invisible(x)
}

#' Reverse a transformation
#'
#' Maps values from the transformed scale back to the original scale.
#'
#' @param object An object of class `"auto_transform"`.
#' @param y Numeric vector on the transformed scale. Defaults to
#'   `object$transformed_data`.
#' @return A numeric vector on the original scale. For `method_used = "int"`
#'   the inverse is a monotone interpolation of the fitted rank mapping, so
#'   values outside the observed range are clamped to its ends.
#' @export
#' @examples
#' x <- rlnorm(100)
#' res <- auto_transform(x)
#' all.equal(inverse_transform(res), x)
inverse_transform <- function(object, y = NULL) {
  if (!inherits(object, "auto_transform")) {
    stop("`object` must be of class 'auto_transform'.", call. = FALSE)
  }
  if (is.null(y)) y <- object$transformed_data
  if (!is.numeric(y)) stop("`y` must be numeric.", call. = FALSE)
  ok <- !is.na(object$original_data)
  .backward(object$method_used, y, object$lambda, object$shift,
            ref_x = object$original_data[ok],
            ref_y = object$transformed_data[ok])
}

#' Predict method for `auto_transform` objects
#'
#' Applies the fitted transformation to new data, or inverts values.
#'
#' @param object An object of class `"auto_transform"`.
#' @param newdata Numeric vector. If `NULL`, the stored transformed data are
#'   returned (or inverted when `inverse = TRUE`).
#' @param inverse Logical. If `TRUE`, `newdata` is taken to be on the
#'   transformed scale and mapped back to the original scale.
#' @param ... Ignored.
#' @return A numeric vector.
#' @export
predict.auto_transform <- function(object, newdata = NULL, inverse = FALSE, ...) {
  if (isTRUE(inverse)) return(inverse_transform(object, newdata))
  if (is.null(newdata)) return(object$transformed_data)
  if (!is.numeric(newdata)) stop("`newdata` must be numeric.", call. = FALSE)
  ok <- !is.na(object$original_data)
  .forward(object$method_used, newdata, object$lambda, object$shift,
           ref_x = object$original_data[ok],
           ref_y = object$transformed_data[ok])
}

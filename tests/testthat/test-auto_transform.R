test_that("normal data are returned unchanged", {
  set.seed(42)
  x <- rnorm(200)
  expect_message(res <- auto_transform(x), "already normally distributed")
  expect_s3_class(res, "auto_transform")
  expect_equal(res$method_used, "none")
  expect_identical(res$transformed_data, x)
  expect_gt(res$original_p_value, 0.05)
  expect_equal(inverse_transform(res), x)
})

test_that("positively skewed data are normalized and invertible", {
  set.seed(1)
  x <- rlnorm(300)
  res <- auto_transform(x)
  expect_s3_class(res, "auto_transform")
  expect_lt(res$original_p_value, 0.05)
  expect_gt(res$final_p_value, res$original_p_value)
  expect_gt(res$final_p_value, 0.05)
  expect_true(res$method_used %in% c("log", "sqrt", "boxcox", "yeojohnson", "int"))
  expect_lt(abs(res$skewness_final), abs(res$skewness_original))
  expect_equal(inverse_transform(res), x, tolerance = 1e-6)
  expect_equal(predict(res, inverse = TRUE), x, tolerance = 1e-6)
  expect_equal(predict(res, newdata = x), res$transformed_data, tolerance = 1e-6)
})

test_that("data with negative values and zeros are handled", {
  set.seed(7)
  x <- rexp(300) - 1.5
  x[1:3] <- 0
  expect_true(min(x) < 0)
  res <- auto_transform(x)
  expect_gt(res$final_p_value, res$original_p_value)
  expect_false(res$method_used == "boxcox")
  expect_true(all(is.finite(res$transformed_data)))
  expect_equal(inverse_transform(res), x, tolerance = 1e-6)
})

test_that("shifted log and sqrt work for non-positive data", {
  set.seed(3)
  x <- rexp(100) - 1
  r_log <- suppressWarnings(auto_transform(x, method = "log"))
  expect_equal(r_log$method_used, "log")
  expect_equal(r_log$shift, 1 - min(x))
  expect_equal(inverse_transform(r_log), x, tolerance = 1e-8)

  r_sqrt <- suppressWarnings(auto_transform(x, method = "sqrt"))
  expect_equal(r_sqrt$shift, -min(x))
  expect_equal(inverse_transform(r_sqrt), x, tolerance = 1e-8)

  expect_error(auto_transform(x, method = "boxcox"), "strictly positive")
})

test_that("each explicit method round-trips", {
  set.seed(11)
  x <- rlnorm(150)
  for (m in c("log", "sqrt", "boxcox", "yeojohnson", "int")) {
    res <- suppressWarnings(auto_transform(x, method = m))
    expect_equal(res$method_used, m)
    expect_equal(inverse_transform(res), x, tolerance = 1e-6, info = m)
  }
})

test_that("missing values are removed with a warning and preserved as NA", {
  set.seed(5)
  x <- rlnorm(100)
  x[c(5, 50)] <- NA
  expect_warning(res <- auto_transform(x), "missing value")
  expect_length(res$transformed_data, 100)
  expect_true(all(is.na(res$transformed_data[c(5, 50)])))
  expect_equal(inverse_transform(res), x, tolerance = 1e-6)
})

test_that("invalid input raises informative errors", {
  expect_error(auto_transform("a"), "numeric")
  expect_error(auto_transform(factor(1:5)), "numeric")
  expect_error(auto_transform(rep(2, 20)), "constant")
  expect_error(auto_transform(c(1, 2)), "At least 3")
  expect_error(auto_transform(c(1, 2, Inf, 4)), "infinite")
  expect_error(auto_transform(rlnorm(20), alpha = 2), "alpha")
  expect_error(auto_transform(rlnorm(20), method = "nope"))
})

test_that("large samples use the D'Agostino-Pearson test", {
  set.seed(9)
  x <- rlnorm(6000)
  res <- auto_transform(x)
  expect_equal(res$test, "D'Agostino-Pearson")
  expect_lt(res$original_p_value, 0.05)
  expect_gt(res$final_p_value, res$original_p_value)
})

test_that("print and plot methods run", {
  set.seed(2)
  res <- auto_transform(rlnorm(100))
  expect_output(print(res), "Method used")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(plot(res))
})

test_that("t1kable gives units without escaped HTML (#143)", {
  dat <- data.frame(id=1, weight = 1)
  label(dat$weight) <- "Weight"
  units(dat$weight) <- "kg"
  x <- table1(~weight, data = dat)
  t1 <- t1kable(x)
  expect_false(
    grepl(x = t1, pattern = "&lt;", fixed = TRUE)
  )
  t1_latex <- t1kable(x, format = "latex")
  expect_false(
    grepl(x = t1_latex, pattern = "&lt;", fixed = TRUE)
  )
})

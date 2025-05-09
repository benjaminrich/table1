test_that("t1flex", {
  skip_if_not_installed("flextable")
  d <- data.frame(A = 1:10, B = 11:20)
  expect_error(
    t1flex(d, tablefn = "foo")
  )

  label(d$A) <- "Anything really long will illustrate the point here"
  t1 <- table1(~A+B, data = d)
  non_span <- t1flex(t1)
  spanning <- t1flex(t1, label_colspan = TRUE)

  expect_equal(non_span$body$spans$rows[1,1], 1)
  expect_equal(spanning$body$spans$rows[1,1], 2)
})

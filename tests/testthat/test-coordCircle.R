test_that("multiplication works", {
  expect_equal(
    coordCircle(c(0, pi), c(1, 2)), 
    data.frame(x = c(1, -2), y = c(0, 0))
  )
  expect_equal(coordCircle(NA), data.frame(x = NA_real_, y = NA_real_))
  expect_error(coordCircle(NULL))
})

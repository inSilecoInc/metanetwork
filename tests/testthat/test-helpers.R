test_that("basic plot", {
    expect_identical(choose_pal("viridis"), viridis::viridis)
    expect_identical(choose_pal("magma"), viridis::magma)
    expect_true(is.null(choose_pal("wrong")))
})

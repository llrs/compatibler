test_that("detect_version works", {
    expect_equal(detect_version("GPL-3.0"), "3.0")
    expect_equal(detect_version("GPL (>= 2) | BSD_2_clause + file LICENSE"), "3.0")
})
detect_version("GPL (>= 2) | BSD_2_clause + file LICENSE")

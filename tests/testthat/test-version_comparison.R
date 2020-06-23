test_that("equal_major works", {
    expect_equal(equal_major("MIT (>= 2)+ file LICENSE"), "2")
    expect_equal(equal_major("MIT (>=2)+ file LICENSE"), "2")
    expect_equal(equal_major("GLPv3(>=2)+ file LICENSE"), "2")
    expect_equal(equal_major("GLPv3(>=2.0.0)+ file LICENSE"), "2.0.0")
})

test_that("equal_equal works", {
    expect_equal(equal_equal("MIT (== 2)+ file LICENSE"), "2")
    expect_equal(equal_equal("MIT (==2)+ file LICENSE"), "2")
    expect_equal(equal_equal("GLPv3(==2)+ file LICENSE"), "2")
    expect_equal(equal_equal("GLPv3(==2.0.0)+ file LICENSE"), "2.0.0")
})

test_that("equal_minor works", {
    expect_equal(equal_minor("MIT (<= 2)+ file LICENSE"), "2")
    expect_equal(equal_minor("MIT (<=2)+ file LICENSE"), "2")
    expect_equal(equal_minor("GLPv3(<=2)+ file LICENSE"), "2")
    expect_equal(equal_minor("GLPv3(<=2.0.0)+ file LICENSE"), "2.0.0")
})

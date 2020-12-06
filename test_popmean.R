library("testthat")


#  test various intervention types, single or multi-year

test_that("Test popmean: year 2020, intervention none",{
  expected <- 0.07723264
  eps <- 1e-6
  actual <- popmean(dat=dat,intervention="none",years=2020)
# expect difference to correct result be within eps
    expect_lt(abs(expected - actual), eps)

})

test_that("Test popmean: years 2020:2025, intervention IRS.ITN",{
  expected <- 0.9076407
  eps <- 1e-6
  actual <- popmean(dat=dat,intervention="IRS.ITN",years=2020:2025)
  # expect difference to correct result be within eps
  expect_lt(abs(expected - actual), eps)
  
})

test_that("Test popmean: years 2005:2009, intervention IRS",{
  expected <- 6.514775
  eps <- 1e-6
  actual <- popmean(dat=dat,intervention="IRS",years=2005:2009)
  # expect difference to correct result be within eps
  expect_lt(abs(expected - actual), eps)
  
})
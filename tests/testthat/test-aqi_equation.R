# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that("there are no NAs from PM2.5 sub-equation",{
  expect_equal(sum(is.na(aqi_equation("pm2.5", seq(0,300,10)))), 0)
})

test_that("there are no NAs from Ozone sub-equation",{
  expect_equal(sum(is.na(aqi_equation("o3", seq(0,0.2,0.0075)))), 0)
})

test_that("PM2.5 sub-equation calculations are correct on the boundaries", {
  expect_equal(aqi_equation("pm2.5", c(0, 12.1, 35.5, 55.5, 150.5, 250.5,
                                       12.09, 35.49, 55.49,
                                         150.49, 250.49, 5000))$AQI,
                 c(0, 51, 101, 151, 201, 301, 50, 100, 150, 200, 300, 500))
})

test_that("Ozone sub-equation calculations are correct on the boundaries", {
  expect_equal(aqi_equation("o3", c(0, 0.055, 0.071, 0.086, 0.106,
                                    0.0549, 0.0709, 0.0859, 0.1059, 0.200))$AQI,
                 c(0, 51, 101, 151, 201, 50, 100, 150, 200, 300))
})



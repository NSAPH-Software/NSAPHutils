test_that("template_function works as expected", {

  val <- template_function(1,2,3)
  expect_equal(val , 6)

})

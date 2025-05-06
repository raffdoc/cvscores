test_that("score calculated corectly", {
  expect_equal(scr_clear(data.frame(age = c(50,50), weight = c(70, 80),scr = c(2,1), female = c(T, F))), data.frame(ccr = c(37, 100)))
  #expect_equal(, )
  #expect_equal(, )

})

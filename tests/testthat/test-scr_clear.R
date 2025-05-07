test_that("score calculated corectly", {
  expect_equal(scr_clear(data.frame(age = c(50,50), weight = c(70, 80),scr = c(2,1), female = c(T, F), dialysis = c(F, F))), data.frame(ccr = c(37, 100)))
  expect_equal(scr_clear(data.frame(age = c(50), weight = c(70),scr = c(2), female = c(T), dialysis = c(F))), data.frame(ccr = c(37)))
  expect_equal(scr_clear(data.frame(age = c(50), weight = c(70),scr = c(2), female = c(T), dialysis = c(T))), data.frame(ccr = c(999)))

})

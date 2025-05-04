test_that("score calculated corectly", {
  expect_equal(acef_score(age = 60, creatinine = 2, ejection_fraction = 50), 2.2)
  expect_equal(acef_score(age = 70, creatinine = 1.5, ejection_fraction = 60), 1.16666666666667)
  expect_equal(acef(data.frame(age = c(5,5), creatinine = c(2,1), ejection_fraction = c(55,55))), data.frame(acef = c(1.09090909, 0.09090909)))

})

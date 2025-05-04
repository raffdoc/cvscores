test_that("score calculated corectly", {
  expect_equal(acef_ii_score(), 1)
  expect_equal(acef_ii_score(age = 60, creatinine = 2, ejection_fraction = 60, emergency_operation = T, hct = 36), 4)
  expect_equal(acef_ii_score(age = 60, creatinine = 2.5, ejection_fraction = 60, emergency_operation = F, hct = 36), 3)
  expect_equal(acef_ii(data.frame(age = c(50,50), creatinine = c(2,1), ejection_fraction = c(55,55), emergency_operation = c(T, F), hct = c(36, 34))), data.frame(acef_ii = c(5.909, 1.309)))

})

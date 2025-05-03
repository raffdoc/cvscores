test_that("score calculated corectly", {
  expect_equal(acef_score(age = 60, creatinine = 2, ejection_fraction = 50), 2.2)
})

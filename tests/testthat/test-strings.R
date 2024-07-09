test_that("cleaning strings work", {
  expect_equal(clean_string("Austria [ref]"), "Austria")
  expect_equal(clean_string("RTBF / VRT"), "RTBF, VRT")
  expect_equal(clean_string("ARD (NDR)"), "ARD (NDR)")
})

test_that("cleaning character vector works", {
  expect_equal(clean_string(c("Bulgaria", "Czechia[b]")), c("Bulgaria", "Czechia"))
  expect_equal(clean_string(c("Albania", "Belarus ◇")), c("Albania", "Belarus"))
})

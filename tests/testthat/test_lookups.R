context("Lookup Functions")
library(ddptools)


test_that("lookup_tif accepts a vector input and returns a character vector of equal length", {
    expect_equal(length(lookup_tif(1:10)), 10)
    expect_equal(typeof(lookup_tif(1:10)), "character")
})

test_that("Invalid freq codes map to NA", {
    expect_equal(lookup_tif(0),     NA_character_)
    expect_equal(lookup_tif(0.5),   NA_character_)
    expect_equal(lookup_tif("999"), NA_character_)
})


test_that("lookup_obs_status accepts a vector input and returns a character vector of equal length", {
    expect_equal(length(lookup_obs_status(LETTERS)), length(LETTERS))
    expect_equal(typeof(lookup_obs_status(LETTERS)), "character")
})

test_that("Default for standard option is FALSE", {
    expect_equal(lookup_obs_status("ND", standard = FALSE), lookup_obs_status("ND"))
})

test_that("Nonstandard obs_status map to NA when standard option is FALSE", {
    expect_equal(is.na(lookup_obs_status("ND", standard = TRUE)), TRUE)
})

test_that("Unrecognized obs_staus always map to NA", {
    expect_equal(lookup_obs_status("C"), NA_character_)
    expect_equal(lookup_obs_status("a"), NA_character_)
})

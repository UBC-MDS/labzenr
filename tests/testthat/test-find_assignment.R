options(usethis.quiet = TRUE)

notebook <- system.file("extdata", "dummylab.Rmd", package = "labzenr")


test_that("find_assignment() must find the dummy lab", {
  fullpath <- find_assignment(notebook)
  expect_true(fs::is_file(fullpath))
})


test_that("find_assignment() must error if an invalid path is given", {
  fake <- "fakepath/fakelab.ipynd"
  expect_error(find_assignment(fake),
    regexp = "Could not find file"
  )
})


test_that("find_assignment() must messge if no file in the directory", {
  withr::with_options(
    list(usethis.quiet = FALSE),
    expect_error(find_assignment(), regexp = "in the right directory?")
  )
})


test_that("find_assignment() must find the dummy lab if no arguments passed", {

  # single file
  fakefile1 <- fs::file_temp(ext = "Rmd")
  fs::file_touch(fakefile1)
  fakedir <- fs::path_dir(fakefile1)
  withr::with_dir(
    fakedir,
    {
      expect_true(fs::is_file(find_assignment()))
    }
  )

  # multiple files
  fakefile2 <- fs::file_temp(ext = "Rmd")
  fs::file_touch(fakefile2)
  withr::with_dir(
    fakedir,
    {
      # mockery::stub(find_assignment, "utils::menu", 1)
      expect_warning(length(find_assignment()),
        regexp = "Multiple possible files found"
      )
    }
  )

  # cleanup
  fs::file_delete(c(fakefile1, fakefile2))
})

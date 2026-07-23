skip_if_not_installed("shiny")

test_that("gganimeOutput returns a shiny widget output tag", {
  out <- gganimeOutput("plot", width = "300px", height = "200px")
  expect_s3_class(out, "shiny.tag.list")
})

test_that("renderGganime returns a shiny render function", {
  r <- renderGganime(NULL)
  expect_type(r, "closure")
  expect_s3_class(r, "shiny.render.function")
})

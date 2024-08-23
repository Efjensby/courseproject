test_that("output is plot", {
  plot1 <- ppp_barplot(iris, 'Species', c('red','blue','green'))
  expect_s3_class(plot1, 'ggplot')
})

test_that("output is plot", {
  plot1 <- ppp_boxplot(iris, 'Species','Sepal.Length',
                       c('red','blue','green'),
                       list(c('setosa','versicolor'),
                            c('versicolor','virginica')), 
                       method = 'wilcox.test')
  expect_s3_class(plot1, 'ggplot')
})

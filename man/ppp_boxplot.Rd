% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppp.R
\name{ppp_boxplot}
\alias{ppp_boxplot}
\title{Build pretty box plots fast}
\usage{
ppp_boxplot(data, x_var, y_var, colors, comparisons, method)
}
\arguments{
\item{data}{The data frame containing the variables to be plotted and analyzed.}

\item{x_var}{The categorical variable in the data frame that will be used for grouping on the x-axis.}

\item{y_var}{The numeric variable in the data frame that will be plotted on the y-axis.}

\item{colors}{A character vector of color names, where the number of colors provided must match the number of unique categories in the x_var variable.}

\item{comparisons}{A list specifying the pairs of categories in x_var to be compared statistically.}

\item{method}{he statistical method used for comparing the groups specified in comparisons (e.g., "wilcox.test", "t.test").}
}
\value{
A visually appealing, colored box plot of y_var grouped by x_var, with statistical comparisons of the specified groups (comparisons) displayed on the plot, using the specified method. The plot is customized with colors from the provided colors vector
}
\description{
A function to produce box plots using ggplot2 with theme, colors and statistics.
}
\examples{
my_comparisons <- list(c("female", "hermaphroditic"), c("female", "male"), c("female", "none"))
my_colors <- c('#e09f3e', '#9e2a2b','#540b0e', '#335c67')
my_method = 'wilcox.test'
ppp_boxplot(data = dplyr::starwars,
           x_var = "sex",
           y_var = "height",
           comparisons = my_comparisons,
           colors = my_colors,
           method = my_method)
}

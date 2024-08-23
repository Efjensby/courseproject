
#' Build pretty box plots fast
#' 
#' A function to produce box plots using ggplot2 with theme, colors and statistics.
#'
#' @param data The data frame containing the variables to be plotted and analyzed.
#' @param x_var The categorical variable in the data frame that will be used for grouping on the x-axis.
#' @param y_var The numeric variable in the data frame that will be plotted on the y-axis.
#' @param comparisons A list specifying the pairs of categories in x_var to be compared statistically.
#' @param colors A character vector of color names, where the number of colors provided must match the number of unique categories in the x_var variable.
#' @param method he statistical method used for comparing the groups specified in comparisons (e.g., "wilcox.test", "t.test").
#'
#' @return A visually appealing, colored box plot of y_var grouped by x_var, with statistical comparisons of the specified groups (comparisons) displayed on the plot, using the specified method. The plot is customized with colors from the provided colors vector
#' 
#' @import ggplot2
#' @import ggpubr
#' @import dplyr
#'  
#' @export
#'
#' @examples
#' my_comparisons <- list(c("female", "hermaphroditic"), c("female", "male"), c("female", "none"))
#' my_colors <- c('#e09f3e', '#9e2a2b','#540b0e', '#335c67')
#' my_method = 'wilcox.test'
#' ppp_boxplot(data = dplyr::starwars,
#'            x_var = "sex",
#'            y_var = "height",
#'            comparisons = my_comparisons,
#'            colors = my_colors,
#'            method = my_method)

ppp_boxplot <- function(data, x_var, y_var, colors, comparisons, method) { 

  data_filtered <- subset(data, !is.na(data[[x_var]]))
  
ggplot(data = subset(data_filtered, !is.na(x_var)), mapping = aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(x_var)))+ 
  geom_boxplot(outlier.shape = NA, alpha = 0.8)+ 
  geom_jitter(width = 0.2, aes(color = !!sym(x_var)), alpha = 1)+
  labs(title = paste('Plot of', y_var, 'by', x_var),
       x = x_var,
       y = y_var,
       legend = x_var)+
  scale_fill_manual(values = colors)+ 
  scale_color_manual(values = colors)+
  stat_compare_means(method = method,
                     comparisons = comparisons)+
  theme_bw(base_size = 16)
}


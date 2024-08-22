
#' Build pretty boxplots fast
#' 
#' A function to produce boxplots using ggplot2 with theme, colors and statistics.
#'
#' @param data a data frame
#' @param x_var categorical variable in data
#' @param y_var numeric variable in data
#' @param comparisons list of groups to compare
#' @param colors character vector of color names for x_var
#' @param method statistical method 
#'
#' @return A pretty boxplot with stats printed on the plot. 
#' 
#' @import ggplot2
#' @import ggpubr
#' @import dplyr
#'  
#' @export
#'
#' @examples
#' my_comparisons <- list(c("female", "hermaphroditic"), c("female", "male"), c("female", "none"))
#' my_colors <- c('#47B2EC','#162F82', '#608D00', '#005115')
#' my_method = 'wilcox.test'
#' ppp_boxplot(data = dplyr::starwars,
#'            x_var = "sex",
#'            y_var = "height",
#'            comparisons = my_comparisons,
#'            colors = my_colors,
#'            method = my_method)

ppp_boxplot <- function(data, x_var, y_var, comparisons, colors, method) { 

  data_filtered <- subset(data, !is.na(data[[x_var]]))
  
ggplot(data = subset(data_filtered, !is.na(x_var)), mapping = aes_string(x = x_var, y = y_var, fill = x_var))+ 
  geom_boxplot(outlier.shape = NA, alpha = 0.5)+ 
  geom_jitter(width = 0.2, aes_string(color = x_var), alpha = 0.8)+
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
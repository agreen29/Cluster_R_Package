#' @title K-Means Clustering
#'
#' @description Perform k-means clustering on the desired dataset.
#'
#' @usage k_means(data_set, k,  perform_PCA = FALSE)
#'
#'
#' @param data_set The data set to perform k-means clustering on
#' @param k The number of clusters the user wants to perform
#' @param perform An optional argument specifying if a PCA transformation should be performed on the data before clustering
#'
#' @return A list containing clustering vector, clustering means, SSTotal, and a plot of the first two principal components
#'
#' @import tidyverse
#' @import tibble 
#' @import dplyr
#' @import ggplot2
#' @import ggforce
#'
#' @export
k_means <- function(data_set, k, perform_PCA = FALSE) {
  
  if (perform_PCA == TRUE){
    data_pca <- data_set %>%
      princomp()
    data_set <- data_pca$scores %>%
      as.data.frame() %>%
      select(Comp.1, Comp.2)
  }
  
  data_set <- data_set %>%
    as.matrix()
  n <- dim(data_set)[1]
  min_d <- rep(NA, n)
  cluster <- rep(0, n)
  k_sample <- data_set[sample(1:n, k),]
  
  for (i in 1:n){
    min_d[i] = sum((k_sample[1,] - data_set[i,])^2)
    cluster[i] = 1
    for (j in 2:nrow(k_sample)){
      distance = sum((k_sample[j,] - data_set[i,])^2)
      if (distance <= min_d){
        cluster[i] = j
        min_d[i] = distance
      }
    }
  }
  SST <- sum(min_d)
  for (i in 1:nrow(k_sample)){
    k_sample[i,] = apply(data_set[cluster == i,], 2, mean)
  }
  
  final_data <- data.frame(data_set, cluster)
  
  final_data_output <- cbind(rownames(data_set), cluster) %>% as.matrix() %>% t()
  
  #plotting the first two principle components 
  data_pca <- final_data %>%
    princomp()
  data_plot <- data_pca$scores %>%
    as.data.frame() %>%
    select(Comp.1, Comp.2)
  
  plot <- ggplot(data_plot, aes(x = Comp.1, y = Comp.2)) + 
    geom_point() + 
    geom_ellipse(data = data_plot, aes(x0 = 0, y0 = 0, a = 100, b = 200, angle = 200), geom = "circle",
                 position = "identity", n = 360, na.rm = FALSE, linetype = 2)
  
  return(list(Cluster_Means = as.matrix(k_sample),Clustering_Vector = final_data_output, Total_SS = SST, Plot = plot))
  
  
}
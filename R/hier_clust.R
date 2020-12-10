#' @title Hierarchical Clustering
#'
#' @description Perform hierarchical clustering on a dataset.
#'
#' @usage hier_clust(data_set)
#'
#'
#' @param data_set A data set for its variables to be merged into clusters
#' @param dist_method An optional argument specifying which method should be used for distance. Argument must be in quotes. Default is euclidean. 
#'
#' @return A vector displaying which clusters were created (the merge vector) and the plot of the dendrogram 
#'
#' @import tidyverse
#'
#' @export
hier_clust <- function(data_set, dist_method = "euclidean")
{
  data_set <- data_set %>% 
    dist(method = dist_method) %>% 
    as.matrix()
  
  rows <- nrow(data_set) - 1
  height <- rep(0, rows) 
  merge_matrix <- matrix(0,nrow= rows, ncol=2)
  group <- -(1:nrow(data_set))                       
  
  
  for(i in 1:(rows))
  {
    height[i] = min(data_set[data_set > 0 ])
    index = which(data_set == height[i], arr.ind=TRUE)
    index = index[1,,drop=FALSE]
    
    merge_pair = group[index]
    merge_matrix[i,] = merge_pair
    
    new_ag_group = c(index, which(group %in% group[index[1,group[index]>0]]))
    group[new_ag_group] = i
    
    new_distance = apply(data_set[index,],2, max)
    
    data_set[min(index),] = data_set[,min(index)] = new_distance
    data_set[max(index),] = data_set[,max(index)] = 0
    data_set[min(index),min(index)] = 0
    
  }
  
  #plotting dendrogram
  d_object <- list(height = height, merge = merge_matrix, labels = row.names(data_set))
  plot <- plot_dendrogram(d_object)
  
  return(list(MergeAndHeights = cbind(merge_matrix, height), Plot = plot))
}
#' @title Hierarchical Clustering Plotting Function
#'
#' @description Plots a dendrogram
#'
#' @usage plot_dendrogram(d_object, d_labels)
#'
#'
#' @param d_object A list of containing the merge matrix, height, and desired labels of the cluster 
#' 
#' @return A dendrogram plot
plot_dendrogram <- function(d_object){
  
  attr(d_object, "class") <- "hclust"
  
  d_object <- d_object %>% 
    as.dendrogram()
  
  op = par(bg = "#a8e6ce")
  
  plot <- plot(d_object, main = "Dendrogram Plot", type = "triangle", col.main = "#594f4f", col.axis = "#594f4f", lwd = 3, lty = 3, sub = '', cex = 0.2)
  
  return(plot)
}

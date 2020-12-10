#' @title Expectation-Maximization Algorithm 
#'
#' @description Implements the Expectation-Maximization algorithm.
#'
#' @usage em_clust(data_set, k)
#'
#'
#' @param data_set The data set to perform the Expectation-Maximization algorithm. All columns of data set must be quantitative. 
#' @param k Will choose k number of random observations in the data set as the starting points of the means of your respective normal distributions
#'
#' @return A list the parameters means, variances, the probabilities of cluster membership, and the final clusters each observation is assigned to
#'
#' @import tidyverse
#' @import mvtnorm
#'
#' @export
em_clust <- function(data_set, k) {
  
  n <- dim(data_set)[1]
  
  means <- data_set[sample(1:n, k, replace = FALSE),]
  
  cov_matrix <- cov(x = data_set)
  probs_vector <- rep(1/k, n)
  
  cluster_member = matrix(0, ncol=nrow(means), nrow=n)
 
  for (iterations in (0:100)) {
    #E-step
    for (i in 1:k){
      cluster_member[,i] = probs_vector[i] * dmvnorm(as.matrix(data_set), t(means[i,]), cov_matrix)
    }
    cluster_member = cluster_member/rowSums(cluster_member)
    
    #M step
    cluster_avg = colSums(cluster_member)                          
   
    mu = (t(data_set) %*% cluster_member) / cluster_avg                     
    var = (t(data_set^2) %*% cluster_member) / cluster_avg - mu^2

  }
  #cluster membership
  final_clust_member = which(round(cluster_member)==1, arr.ind=T)       
  final_clust_member = final_clust_member[order(final_clust_member[,1]), 2]     
 
  output_list = list(Mean=mu, Variance=var, Cluster=final_clust_member, ClusterMembershipProbability=cluster_member)
  return(output_list)
  
}
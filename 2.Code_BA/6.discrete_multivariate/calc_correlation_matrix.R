#function takes a covariance matrix and calculates the respective correlation matrix 


calculate_correlation_matrix <- function(covariance_matrix) {
  
  correlation_matrix <- covariance_matrix 
  
  sd_vector <- sqrt(diag(covariance_matrix))
  
  for(i in 1:nrow(correlation_matrix)) {
    
    for(j in 1:ncol(correlation_matrix)) {
      
      correlation_matrix[i,j] <- covariance_matrix[i,j] / (sd_vector[i] * sd_vector[j])
      
    }
    
  }
  
  return(correlation_matrix)
  
}
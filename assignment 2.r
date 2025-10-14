h <- rep(1:n, times = sample(1:5, n, replace = TRUE))[1:n]
get.net <- function(beta, h, nc = 15) {
  n <- length(beta)
  mean_beta <- mean(beta)
  contacts <- vector("list", n)
  prob_matrix <- matrix(0, n, n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      # 如果是同家庭成员，概率为0
      if (h[i] == h[j]) {
        prob_matrix[i, j] <- 0
        prob_matrix[j, i] <- 0
      } else {
        # 计算连接概率
        prob_ij <- (nc * beta[i] * beta[j]) / (mean_beta^2 * (n-1))
        prob_matrix[i, j] <- prob_ij
        prob_matrix[j, i] <- prob_ij
        
        # 根据概率决定是否建立连接
        if (runif(1) < prob_ij) {
          contacts[[i]] <- c(contacts[[i]], j)
          contacts[[j]] <- c(contacts[[j]], i)
        }
      }
    }
  }
  
  return(contacts)
}

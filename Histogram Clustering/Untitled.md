    ### Statistical Machine Learning\
    ### Homework-5; Multinomial Clustering

    rm(list = ls())
    H <- matrix(readBin("~/Documents/R data/histograms.bin", "double", 640000), 40000, 16)
    #H <- t(H)

    # ER Algorithm
    MultinomialEM <- function(H, K, thre){
      
      n <- nrow(H); d <- ncol(H)
      # You may encounter numerical problems due to empty histogram bins. 
      # If so, add a small constant (such as 0.01) to the input histograms.
      H <- H + 0.01
      c <- rep(1/K, K)                                             # 1 * k
      theta <- matrix(0, nrow = n, ncol = K)
      a <- matrix(0, nrow = n, ncol = K)                           # n * k
      
      # 1. Normalize Centroids
      centroids = sample(c(1:n), size=K)
      t <- t(apply(H[centroids, ], 1, 
                  function(row) { row / sum(row) }))               # k * j
      
      delta <- 50

      # 2. Iterate 
      while (delta > thre){
        for (k in 1:K){
          a_temp <- a
          # 2.1 E-Step
          for (i in 1:n){
            theta[i, k] <- exp(sum(H[i, ] * log(t[k, ])))
          }
          #print(head(theta))
          
          for (i in 1:n){
            value = c[k] * theta[i, k] / sum(c * theta[i,])
            if(is.na(value)) {
              a[i, k] <- 0
            } else {
              a[i, k] <- value
            }
          }
          #print(head(a))
          #print("after theta and a")
          
          # 2.2 M-Step
          c[k] <- sum(a[, k]) / n
          b <- a[, k] %*% H
          t[k, ] <- b / sum(b)
        }
        
        # 2.3 
        delta = norm(a_temp - a, "O")
        #print(delta)
      } 
      m = sapply(c(1:n), function(i) { which.max(a[i,])})
      return(m)
    }


    # Main function

    K = c(3, 4, 5)
    for (i in 1:length(K)){
      print("############################################################")
      ms <- MultinomialEM(H = H, K = K[i], thre = 0.1)
      matrix <- as.matrix(ms)
      
      m_matrix <- matrix(matrix[, 1], nrow = 200, ncol = 200)
      image(m_matrix, col = gray.colors(K[i]))
      
    }

    ## [1] "############################################################"

![](Untitled_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    ## [1] "############################################################"

![](Untitled_files/figure-markdown_strict/unnamed-chunk-1-2.png)

    ## [1] "############################################################"

![](Untitled_files/figure-markdown_strict/unnamed-chunk-1-3.png)

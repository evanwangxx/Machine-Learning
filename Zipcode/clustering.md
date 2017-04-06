    ## Clean Data
    library(MASS)
    library(dplyr)
    library(glmnet)
    library(leaps)
    Data3 <- read.table("/Users/Hongbo/Documents/R data/train_3.txt", header = FALSE, sep = ',')
    Data5 <- read.table("/Users/Hongbo/Documents/R data/train_5.txt", header = FALSE, sep = ',')
    Data8 <- read.table("/Users/Hongbo/Documents/R data/train_8.txt", header = FALSE, sep = ',')
    Data3$y <- 3; Data5$y <- 5; Data8$y <- 8;
    Train_Data <- rbind(Data3, Data5, Data8)
    colnames(Train_Data)[1:(ncol(Train_Data)-1)] <- c(1:(ncol(Train_Data)-1))

    Test_Data <- read.table("/Users/Hongbo/Documents/R data/zip_test.txt", header = FALSE)
    colnames(Test_Data)[2:ncol(Test_Data)] <- c(1:(ncol(Test_Data)-1))
    colnames(Test_Data)[1] = 'y'
    Test_Data <- Test_Data[which(Test_Data$y == 3 |
                                   Test_Data$y == 5 |
                                   Test_Data$y == 8), ]




    fit_1 <- lda(y ~., data=Train_Data)
    Train_error <- sum(predict(fit_1, data = Train_Data)$class != Train_Data$y)/length(Train_Data$y)
    Test_error <- sum(predict(fit_1, Test_Data)$class != Test_Data$y)/length(Test_Data$y)

    ## PCA
    prinComp <- princomp(Train_Data[, 1:(ncol(Train_Data) - 1)], cor = TRUE)

    Train_Data_PCA <- data.frame(Train_Data$y, prinComp$scores[,1:49])
    Test_Data_PCA <- data.frame(y = Test_Data$y, predict(prinComp, Test_Data[, -1]))[1:50]
    colnames(Train_Data_PCA)[1] <- 'y'

    fit_2 <- lda(y ~., data = Train_Data_PCA)
    Train_error_PCA <- sum(predict(fit_2, data = Train_Data_PCA)$class != Train_Data_PCA$y)/length(Train_Data_PCA$y)
    Test_error_PCA <- sum(predict(fit_2, Test_Data_PCA)$class != Test_Data_PCA$y)/length(Test_Data_PCA$y)

    ## 2*2 pixel Mean

    Train_Data_Pix <- matrix(NA, nrow = nrow(Train_Data), ncol = (ncol(Train_Data) - 1)/4)
    Test_Data_Pix <- matrix(NA, nrow = nrow(Test_Data), ncol = (ncol(Test_Data) - 1)/4)
    pixels <- c(seq(18, 32, by = 2) , seq(50, 64, by = 2), seq(82, 96, by = 2), seq(114, 128, by = 2),
                seq(146, 160, by = 2), seq(178, 192, by = 2), seq(210, 224, by = 2), seq(242, 256, by = 2))
    for(i in 1:nrow(Train_Data)){
      c = 1
      for(j in pixels){
        mean_output <- (Train_Data[i, j-17] + 
                          Train_Data[i, j-16] +
                          Train_Data[i, j-1] + 
                          Train_Data[i, j])
        Train_Data_Pix[i, c] <- mean_output/4
        c = c + 1
      }
      Temp <- matrix(Train_Data_Pix[i, ],8,8, byrow = TRUE)
      Train_Data_Pix[i,]=c(c(Temp))
    }

    Test_Data_Temp <- Test_Data[, 2:ncol(Test_Data)]
    for(i in 1:nrow(Test_Data_Temp)){
      c = 1
      for(j in pixels){
        mean_output <- (Test_Data_Temp[i, j-17] + 
                          Test_Data_Temp[i, j-16] +
                          Test_Data_Temp[i, j-1] + 
                          Test_Data_Temp[i, j])
        Test_Data_Pix[i, c] <- mean_output/4
        c = c + 1
      }
      Temp <- matrix(Test_Data_Pix[i, ],8,8, byrow = TRUE)
      Test_Data_Pix[i,]=c(c(Temp))
    }
    Train_Data_Pix <- as.data.frame(Train_Data_Pix)
    Train_Data_Pix$y <- Train_Data$y
    Test_Data_Pix <- as.data.frame(Test_Data_Pix)
    Test_Data_Pix$y <- Test_Data$y
    #--------------------

    #--------------------
    fit_3 <- lda(y~., data = Train_Data_Pix)
    Train_error_Pixel <- 1 - mean(predict(fit_3,Train_Data_Pix)$class == Train_Data$y)
    Test_error_Pixel <- 1 - mean(predict(fit_3,Test_Data_Pix)$class == Test_Data$y)

    ## Mutiple linear logistic regression

    Train_x <- as.matrix(Train_Data_Pix[, c(1:(ncol(Train_Data_Pix)-1))])
    Train_y <- factor(Train_Data_Pix$y)

    Test_x <- as.matrix(Test_Data_Pix[, c(1:(ncol(Test_Data_Pix))-1)])
    Test_y <- factor(Test_Data_Pix$y)

    fit_4 <- glmnet(x = Train_x, y = Train_y, family = 'multinomial')

    Train_Factor <- predict(fit_4, Train_x, type = 'response', s = 0.01)[,,1]
    Train_Predict_Factor <- rep(NA)
    for(i in 1:nrow(Train_Factor)){
      Train_Predict_Factor[i] <- levels(Train_y) [which(Train_Factor[i,] == max(Train_Factor[i,]))]
    }
    Train_Predict_Rate <- 1 - mean(Train_Predict_Factor == Train_y)

    Test_Factor <- predict(fit_4, Test_x, type = 'response', s = 0.01)[,,1]
    Test_Predict_Factor <- rep(NA)
    for(i in 1:nrow(Test_Factor)){
      Test_Predict_Factor[i] <- levels(Test_y) [which(Test_Factor[i,] == max(Test_Factor[i,]))]
    }
    Test_Predict_Rate <- 1 - mean(Test_Predict_Factor == Test_y)


    Train_error; Test_error

    ## [1] 0.01594533

    ## [1] 0.08739837

    Train_error_PCA; Test_error_PCA

    ## [1] 0.04612756

    ## [1] 0.09146341

    Train_error_Pixel; Test_error_Pixel

    ## [1] 0.03359909

    ## [1] 0.07520325

    Train_Predict_Rate; Test_Predict_Rate

    ## [1] 0.04441913

    ## [1] 0.08739837

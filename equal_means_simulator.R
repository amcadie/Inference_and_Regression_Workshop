sim <- function(mean1=0.996, sd1=0.0005, mean2=0.997, sd2=0.0005, n, runs=1000){
    pvals <- numeric()
    
    par(mfrow = c(3, 3), mar = c(2, 2, 1, 1))
    
    #build numeric vector of p-values from each run of random samples and t-test
    
    for(i in 1:runs){
        z1<- rnorm(n, mean1, sd1)
        z2<- rnorm(n, mean2, sd2)
        if(sd1 == sd2){
            test <- t.test(z1, z2, var.equal = TRUE)
        }
        else{
            test<- t.test(z1, z2)
        }
        
        if(i < 10){
            df <- data.frame(sample_1 = z1, sample_2 = z2)
            df_long <- stack(df)
            with(df_long, boxplot(values ~ ind, col = "cadetblue", 
                                  ylim = c(0.994, 1)))
            with(df_long, points(values ~ ind, col = "red"))
            #annotate with corresponding pval
            round_p <- round(as.numeric(test[3]), 2)
            text(paste("p = ", round_p), x = 1, y = 0.999)
        }
        
        pvals <- c(pvals, as.numeric(test[3]))
    }
    
    #calculate percentage of runs giving pval<0.05
    
    sig_ratio <- sum(pvals<0.05)/runs
    print(paste("Proportion of pvals<0.05:", sig_ratio, sep = " "))
    
    #reset par
    par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 1)
}
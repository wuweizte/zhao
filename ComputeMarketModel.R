ComputeMarketModel <- function(arg.object){
        
        # browser()
        
        time.attribute <- tsp(arg.object)
        
        internal.return.matrix <- arg.object

        for (i in 12:length(internal.return.matrix[,1])) {
                
                lm.fit <- lm(internal.return.matrix[(i-11) : i, 1] ~ internal.return.matrix[(i-11) : i, 2])
                
                internal.return.matrix[i,"alpha"] <- round(lm.fit$coefficients[1],digits = 2)
                internal.return.matrix[i,"beta"] <- round(lm.fit$coefficients[2],digits = 2)        
        }
        
        
        internal.return.matrix <- window(internal.return.matrix,start = time.attribute[1] + 11 / time.attribute[3])

        return(internal.return.matrix)        
        
}

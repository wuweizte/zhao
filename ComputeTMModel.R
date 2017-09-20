ComputeTMModel <- function(arg.object){
        
        # browser()
        
        time.attribute <- tsp(arg.object)
        
        internal.return.matrix <- arg.object

        for (i in 12:length(internal.return.matrix[,1])) {

                fund.data <- internal.return.matrix[((i-11) : i), 1]
                
                market.data <- internal.return.matrix[((i-11) : i), 2]
                market.data.square <- market.data^2
                
                lm.fit <- lm(fund.data ~ market.data + market.data.square)
                
                internal.return.matrix[i,"alpha"] <- round(lm.fit$coefficients[1],digits = 2)
                internal.return.matrix[i,"beta"] <- round(lm.fit$coefficients[2],digits = 2)        
                internal.return.matrix[i,"gamma"] <- round(lm.fit$coefficients[3],digits = 2)
        }
        
        
        internal.return.matrix <- window(internal.return.matrix,start = time.attribute[1] + 11 / time.attribute[3])

        return(internal.return.matrix)        
        
}


        



CompareObjectAccuracyArimaNolimitation <- function(arg.object, 
                                  arg.forecast.period,
                                  arg.training.set.endpoint, 
                                  arg.comparison.period,
                                  arg.maxorder){
        
        # browser()
        
        time.attribute <- tsp(arg.object)
        
        training.set.object.1 <- 
                window(arg.object,  
                       end = time.attribute[1] + (arg.training.set.endpoint - 1) / time.attribute[3])
        
        test.set.object.1 <- window(arg.object, 
                                    start = time.attribute[1] + arg.training.set.endpoint / time.attribute[3],
                                    end = time.attribute[1] + (arg.training.set.endpoint + arg.forecast.period - 1)/ time.attribute[3])

        

        fit.arima.object.1 <- auto.arima(training.set.object.1, seasonal = FALSE,
                                         max.order = arg.maxorder)                
        
        
        fc.arima.object.1 <- forecast(fit.arima.object.1, h = length(test.set.object.1))        

        pvalue <- Box.test(residuals(fit.arima.object.1), lag=10, 
                           fitdf=sum(fit.arima.object.1$arma[c(1,2)]))$p.value
        
        result <- t(array(c(arg.training.set.endpoint,
                            fit.arima.object.1$arma[c(1,6,2)],
                            ("drift" %in% names(fit.arima.object.1$coef)) * 1,
                            round(accuracy(fc.arima.object.1, test.set.object.1)[2,2], digits = 1),
                            round(pvalue, digits = 2)),
                          dim = c(7,1)))
        
        
        for(i in 1:arg.comparison.period){
                training.set.object.2 <- 
                        window(arg.object,  
                               end = time.attribute[1] + (arg.training.set.endpoint - 1 + i) / time.attribute[3])
                
                test.set.object.2 <- window(arg.object, 
                                            start = time.attribute[1] + (arg.training.set.endpoint + i) / time.attribute[3],
                                            end = time.attribute[1] + (arg.training.set.endpoint + i + arg.forecast.period - 1)/ time.attribute[3])
                
                
                fit.arima.object.2 <- auto.arima(training.set.object.2, seasonal = FALSE,
                                                 max.order = arg.maxorder)                          
                
                fc.arima.object.2 <- forecast(fit.arima.object.2, h = length(test.set.object.2))        
                
                pvalue <- Box.test(residuals(fit.arima.object.2), lag=10, 
                                   fitdf=sum(fit.arima.object.2$arma[c(1,2)]))$p.value
                
                
                result2 <- t(array(c(arg.training.set.endpoint + i,
                                     fit.arima.object.2$arma[c(1,6,2)],
                                     ("drift" %in% names(fit.arima.object.2$coef)) * 1,
                                     round(accuracy(fc.arima.object.2, test.set.object.2)[2,2], digits = 1),
                                     round(pvalue, digits = 2)),
                                   dim = c(7,1)))
                
                result <- rbind(result, result2)
                
        }
        
        df.result <- data.frame(result)
        colnames(df.result) <- c("seq", "p","d","q", "dr", "RMSE", "p.v")
        
        return(df.result)        
        
}

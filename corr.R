corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        files_list <- list.files(directory, full.names=TRUE)
        V <- c() #creates an empty vector
        for(i in 1:length(files_list)) { 
                #loops all the files,
                #total complete cases
                file<-read.csv(files_list[i])
                cc<-complete.cases(file)
                if (nrow(file[cc,])>threshold) {
                        x <- file[cc,][2]
                        y <- file[cc,][3]
                        V <- c(V, cor(x,y))
                }
                
        }
        V
}


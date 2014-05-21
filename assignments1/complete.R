complete <- function(directory, id=1:332) {
    dir <- paste("./", directory, sep="")
     answerDF <- data.frame()
    for( i in id) {
        fileid <- sprintf("%03s", i)
        file <- paste(dir, paste(fileid, ".csv", sep=""), sep="/")
        contents <- read.csv(file, header=TRUE)
        result <- !is.na(contents$sulfate) & !is.na(contents$nitrate)
        answerDF <- rbind(answerDF, c(i, length(result[result])))
    }
    colnames(answerDF) <- c("id", "nobs")
    answerDF
}


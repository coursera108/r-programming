corr <- function(directory, threshold=0) {
    compDF <- complete(directory)
    matchedDF <- compDF[compDF$nobs > threshold, ]
    ids <- matchedDF$id
    dir <- paste("./", directory, sep="")
    corrv <- numeric()
    for( i in ids) {
        file <- paste( dir, paste(sprintf("%03s", i), ".csv", sep=""), sep="/")
        contents <- read.csv(file, header=TRUE)
        comp.contents <- contents[!is.na(contents$sulfate) & !is.na(contents$nitrate),]
        corrv <- append(corrv, cor(comp.contents$sulfate, comp.contents$nitrate))

    }
    corrv
}

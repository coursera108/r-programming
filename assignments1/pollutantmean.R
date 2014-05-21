pollutantmean <- function(directory, pollutant, id=1:332) {
    dir <- paste("./", directory, sep="")
    pcolumn <- numeric()
    for( i in id) {
        fileid <- sprintf("%03s", i)
        file <- paste(dir, paste(fileid, ".csv", sep=""), sep="/")
        contents <- read.csv(file, header=TRUE)
        pcolumn <- append(pcolumn, with(contents, get(pollutant)))
    }
    round(mean(pcolumn, na.rm=TRUE), 3)
}


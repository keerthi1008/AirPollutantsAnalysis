#Calculating weightmedian using the data of pollutants.

weightmedian <- function(directory, day) {
        files_full <- list.files(directory, full.names=TRUE)
        dat <- data.frame()
    for (i in 1:5) {
        dat <- rbind(dat, read.csv(files_full[i]))
        }
        
        da <- dat[which(dat$Day == day), ]
        median(da$Weight, na.rm = TRUE)
}

#Calculating mean of pollutants.
pollutantmean <- function(directory, pollutant, id = 1:332) {
    directory <- "specdata"
    temp <- list.files(pattern = "*.csv")   
    vs <- numeric()

    for(i in id) {
        x <- read.csv(temp[i])
        y <- x[[pollutant]]
        z <- y[!is.na(y)]
        vs <- c(vs,z)
    }
 mean(vs)
}

complete <- function(directory, id = 1:332) {
    directory <- "specdata"
    temp <- list.files(pattern = "*.csv")   
    vs <- numeric()
    
for(i in id) {
        x <- read.csv(temp[i])
        good <- complete.cases(x[["sulfate"]],x[["nitrate"]])
        vs <- c(vs,nrow(x[good,]))
}

dataframe <- data.frame(id=id, nobs = vs)
print(dataframe)
}

#Calculating correlation between pollutants
corr <- function(directory, threshold = 0) {
    directory <- "specdata"
    temp <- list.files(pattern = "*.csv")   
    vs <- numeric()
    
for(i in 1:332) {
        x <- read.csv(temp[i])
        good <- complete.cases(x[["sulfate"]],x[["nitrate"]])
        k <- nrow(x[good,])
        if (k > threshold) {
        result <- cor(x[good,2],x[good,3]) 
        vs <- c(vs,result)
        } else {
        result <- 0  ##note here is result, not vs <-0.
        }
    }
return(vs)
}

############################################
## Data Loading                        ####
############################################
data <- read.csv("RealizedVol-2016-06-16.csv",
                 header=TRUE,
                 sep=",",
                 stringsAsFactors=FALSE)

# Manually assign the header names
column.name <- c("Column1","Column2","Column3")
names(data) <- column.name



############################################
## Volatility Smile                     ####
############################################



############################################
## Volatility Surfac                    ####
############################################
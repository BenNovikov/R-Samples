# Sys.setlocale('LC_ALL', 'ukrainian')  ## For Windows only, MacOS doesn't honour it
library(XML)
library(ggplot2)
library(grid)
library(dplyr)
library(tidyr)
# library(ggdendro)
# library(cluster)
library(reshape2)
library(scales)

## Initial settings and URIs ----
##

## Set working directory 
setwd("~/R-pet/downloads")

## Read or download to a temp file below
# fileURL <- "http://nais.gov.ua/download/open_data/15-UFOP.zip" ## 2.2GB data for ukrainian companies and entrepreneurs  
fileURL <- "https://www.w3schools.com/xml/cd_catalog.xml"

## Make a temporary file (tempFile) and a temporary folder (tempDir)
# tempFile <- tempfile(tmpdir = tempDir <- tempdir())
## or use an actual file (tempFile) at your working directory (tempDir)
tempFile <- paste0(tempDir <- getwd(), "/", tail(strsplit(fileURL,"[/]")[[1]],1))

## Data loading and unzipping ----
##

## Download zip file 
downloadUrl <- function(url, dest) {
  out <- tryCatch(
    {
      message("Processing started. This may take some time...")
      download.file(fileURL, dest, method="auto", mode = "wb")
    },
    error = function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning = function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NA)
    },
    finally = {
      message(paste("\nProcessed URL:", url))
      message("Processing finised.")
    }
  )    
  return(out)
}

## Unzip to the temp folder
unzipFile <- function(unFile, unDir) {
  out <- tryCatch(
    {
      message("Processing started. This may take some time...")
      unzip(unFile, exdir = unDir)
    },
    error = function(cond) {
      message(paste("File or directiry does not seem to exist:", unFile))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning = function(cond) {
      message(paste("A warning by:", unFile))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NA)
    },
    finally = {
      message(paste("\nProcessed File:", unFile))
      message("Processing finised.")
    }
  )
  return(out)
}

downFiles <- downloadUrl(fileURL, tempFile)
if (class(downFiles) == "try-error" || is.na(downFiles) || is.null(downFiles)) { 
  message("No data downloaded!")
  stop()
  } else {
    xmlFiles <- unzipFile(tempFile, tempDir)
    if (class(xmlFiles) == "try-error" || is.na(xmlFiles) || is.null(xmlFiles)) { 
      xmlFile <- tempFile
    } else {
      xmlFile <- xmlFiles[3]
    }
    xmlFile
}

## XML Parcer ---- 
##

## Parse the tree
xmlDoc <- xmlInternalTreeParse(xmlFile)
# head(xmlChildren(xmlDoc))

## Transform xmlDoc to Data Frame
message("Processing started. This may take some time...")
xmlDoc.df <- xmlToDataFrame(xmlDoc)
csvName <- paste0(substr(x = tempFile, start = 1, stop = nchar(tempFile) - 3), "csv")
write.csv(xmlDoc.df, file = csvName)
message("Processing finised.")

## Customized Plotting Functions ----
##

## Customized QQPlot
qqplot.data <- function (numericVector) 
{
  # following four lines from base R's qqline()
  numericVector <- na.omit(numericVector)
  y <- quantile(numericVector, c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  d <- data.frame(resids = numericVector)
  ggplot(d, aes(sample = resids)) + 
    stat_qq() + 
    geom_abline(slope = slope, intercept = int, col = 2) +
    labs(x = "Теоретичний нормальний розподіл", y = "Розподіл вибірки")
}

## Multiple plot function
## http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
## ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
## - cols:   Number of columns in layout
## - layout: A matrix specifying the layout. If present, 'cols' is ignored.
## 
## If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
## then plot 1 will go in the upper left, 2 will go in the upper right, and
## 3 will go all the way across the bottom.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) 
{
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

removeOutliers <- function(x, na.rm = TRUE, ...) 
{
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

## Data preparation ----

## Read initial data set
xmlDoc.df <- read.csv(csvName)
xmlDoc.df$PRICE <- as.numeric(as.vector(na.omit(xmlDoc.df$PRICE)))
meanPrice <- mean(xmlDoc.df$PRICE)
stdevPrice <- sd(xmlDoc.df$PRICE)

## Add an extreme value to dataset
xmlDoc.df$PRICE[1] <- 50
docPrice <- as.numeric(as.vector(na.omit(xmlDoc.df$PRICE)))

## Shapiro-Wilk normality test
shapiro.test(docPrice)
message("if p-value <.05, reject the hypothesis that the sample is from a population which has a normal distribution")
message("W has to be as close as possible to .99 for a normal distr. Obviously the problem is the outlier we added")

par(mfrow = c(1, 3))
q1 <- qqplot.data(docPrice)
b1 <- boxplot(docPrice)

## Data with the outlier get rid off 
docPrice <- removeOutliers(docPrice)
xmlDoc.df$PRICE <- docPrice
docPrice <- na.omit(docPrice)
q2 <- qqplot.data(docPrice)
b2 <- boxplot(docPrice)

## Normal data for comparison
set.seed(123)
normPrice <- rnorm(100, mean = meanPrice, sd = stdevPrice)
q3 <- qqplot.data(normPrice)
b3 <- boxplot(normPrice)

mtext(
  "Вхідні дані до і після усунення екстремальних значень порівняно з нормальним розподілом (справа)",
  side = 3, 
  line = -3, 
  outer = TRUE, 
  cex = 1
)

multiplot(q1 + ggtitle("До видалення екстр. значення"),  
          q2 + ggtitle("Після видалення екстр. значення"),  
          q3 + ggtitle("Нормальний розподіл"), 
          cols = 3)

shapiro.test(docPrice)

## Delete temporary files
# unlink(tempDir, T, T) ## This will wipe all the files out of working directory!
# closeAllConnections()

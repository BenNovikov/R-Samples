## Set working directory 
setwd("~/R-pet/downloads")

## Read a file from URL
loadFromURL <- function(path) {
  name <- tail(strsplit(path,"[/]")[[1]],1)
  tempfile <- paste0(getwd(), "/", name)
  download.file(path, name)
}
loadFromURL("http://www.soc.univ.kiev.ua/sites/default/files/course/materials/r1.pdf")
loadFromURL("https://www.mapr.com/sites/default/files/spark-certification-study-guide.pdf")
loadFromURL("http://ufpp.gov.ua/content/PDF/zakonodavstvo/konstitychiya.pdf")

## Select all pdf files in working directory
files <- list.files(pattern = "pdf$")

## Convert .pdf to text
# install.packages("pdftools")
library(pdftools)
textFile <- lapply(files, pdf_text)

## VectorSource -> interpret each element of the textFile object as a document
# install.packages("tm")
library(tm)
corp <- Corpus(VectorSource(textFile))

## SnowballC stemming words to a common root to aid comparison of vocabulary. 
## Doesn't support Ukrainian. Russian so-so. List of languages: getStemLanguages()
# install.packages("SnowballC")
library(SnowballC)
textFile.tdm <- TermDocumentMatrix(corp, control = list(removePunctuation = TRUE,
                                                        stopwords = TRUE,
                                                        tolower = TRUE,
                                                        stemming = TRUE,
                                                        removeNumbers = TRUE)) 
for(i in 1:ncol(textFile.tdm)) inspect(textFile.tdm[, i])

## Find words that occur at least 10 times
ft <- findFreqTerms(textFile.tdm, lowfreq = 10, highfreq = Inf)
ft.tdm <- inspect(textFile.tdm[ft, ])
apply(ft.tdm, 1, sum)
plot(factor(rownames(ft.tdm)), as.vector(ft.tdm[1:10, 2]))

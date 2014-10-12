library('tm')

library('parallel')
options('mc.cores' =  4)

### Set the source directory here
src.dir <- 'final/en_US_short/'  # testing purpose
#src.dir <- 'final/en_US/'

src <- DirSource(src.dir)
en.t <- Corpus(src, readerControl = list(reader = readPlain, language= "en",
                        load = TRUE)) # A collection of 4 text 

length(en.t)

### tm_Map(x, fn) apply f to all element of x
### fn can be  removeNumbers, removePunctuation, removeWords, stemDocument,
### stripWhitespace

### tmFilter and tmIndex for filtering.
### searchFullText function look for regular expresions inside the text
### doclevel = T, for usage to document level.

### We will perform white space elminiation and lowercase conversion with stopword removal.

### Tockenzation will be perform by the tm::TermDocumentMatrix function.
### /usr/share/dict/words for the dictionnary
RemovePunctuation <- function(plainTxtDoc){
  s <- plainTxtDoc$content
  s <- gsub("(\\w)-(\\w)", "\\1\1dd\\2", s)
  s <- gsub("(\\w)'(\\w)", "\\1\2dd\\2", s)
  s <- gsub("[[:punct:]]+", "", s)
  s <- gsub("\1dd", "-", s, fixed = TRUE)
  s <- gsub("\2dd", "'", s, fixed = TRUE)
  plainTxtDoc$content <- s
  return(plainTxtDoc)
}

CleanText <- function(corp.txt){
  transformations <- getTransformations()
  for (f in transformations){
    if (f == 'removeWords'){
      bw <- scan('bad_words.txt', character(), sep = '\n')
      corps.txt <- tm_map(corp.txt, get(f), bw)
      next
    }

    if (f == 'removePunctuation'){
      corp.txt <- tm_map(corp.txt, function(txt) RemovePunctuation(txt))
      next
    }

    if (f == 'stemDocument') next
    
    corps.txt <- tm_map(corp.txt, get(f))
  }
  return(corp.txt)
}


txt.clean <- CleanText(en.t)
# save(txt.clean, file='final/en_US/en_US_corpus_clean.RData')
save(txt.clean, file='final/en_US_short/en_US_corpus_clean_short.RData')

# load('final/en_US/en_US_corpus_clean.RData') # load txt.clean as a corpus 
### List of words from  https://gist.github.com/jamiew/1112488
### Rewrite them in a easy format to read
## bw <- scan('bad_words.txt', character(), sep = '\n')
## bwc <- unlist(lapply(strsplit(bw, ':'), '[[', 1))
## bwc <- gsub('\\"', '', bwc)
## writeLines(y.c, 'bad_words.txt', sep='\n')

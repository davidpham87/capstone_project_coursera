# load('final/en_US_short/en_US_corpus_clean.RData') # load txt.clean as a corpus
load('final/en_US_short/en_US_corpus_clean_short.RData') # load txt.clean as a corpus

library('tau')
library('tm')
library('Matrix')
library("RWeka")
library('ggplot2')
library('data.table')
library('parallel')
library('reshape2')

# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# tdm <- TermDocumentMatrix(txt.clean, control = list(tokenize = BigramTokenizer))

dtm <- TermDocumentMatrix(txt.clean)
ft <- findFreqTerms(dtm, 200, Inf)


dtm.to.sm <-
  sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
               dims=c(dtm$nrow, dtm$ncol))

### Coupling to see the distrbution of the word in function of the number of
### characters in the word

umat <- as.matrix(dtm)
u <- data.table(umat, ID = rownames(umat))
u[, nchar.word := nchar(ID)]
u[, gr:= cut(nchar.word, breaks = c(1, 2, 4, 6, 10, Inf))]
u.m <- melt(u, id.vars = c('ID', 'nchar.word', 'gr'))

p <- ggplot(u.m) + facet_grid(gr~variable) +
    geom_histogram(aes(x=value)) + scale_x_log10(limits=c(10, 10000)) +
      labs(title = 'Histogram of number of words depending their number of character', x = 'Number of Character of the word')
print(p)
ggsave('hist_num_char.pdf', p, width=8, height=8)

u.m[order(value, decreasing = T)][1:100]
u.r <- u.m[, list(value=sum(value)), by='ID']
paste(u.r[order(value, decreasing=T)][1:200]$ID, collapse=', ')























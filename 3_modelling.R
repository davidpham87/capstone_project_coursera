# setwd('./Dropbox/coursera_capstone_project')

library('tau')
library('tm')
library('Matrix')
library("RWeka")
library('ggplot2')
library('data.table')
library('reshape2')
library('parallel')


load('final/en_US/en_US_corpus_clean.RData') # load txt.clean as a corpus
# load('final/en_US_short/en_US_corpus_clean_short.RData') # load txt.clean as a corpus


##' Count n-gram by skipping one word
##'
##' Function to take any text taking lower case and counting the number
##' of sequence of words in the text by skpping one word a the time
##' @title Count skipped ngram
##' @param s, character vector
##' @param n the n-gram to consider
##' @return 
##' @author David
CountNgramSkip <- function(s, n=3, skip=1){

  cl <- makeCluster(4)
  
  clusterEvalQ(cl, {
    library('data.table')
    library('parallel')
  })
  env <- new.env()
  assign('n',  n, env)
  assign('skip',  skip, env)
  clusterExport(cl, c('n', 'skip'), envir=env)
  
  res <- unlist(parLapply(cl, s, function(txt){
    txt <- tolower(txt) 
    t.l <- strsplit(txt, ' ')[[1]]

    if (length(t.l) < 5) {return(data.table())}
    
    res <- vapply(1:(length(t.l)-(2*n-1)), function(idx){
      paste(t.l[idx+((skip+1)*(1:n)-(skip+1))], collapse=' ')
    }, character(1))
    
    return(res)
  }))

  stopCluster(cl)
  print('End of cluster')
  print(paste('n:', n, 'skip:', skip))  
  res <- data.table(s=res)
  res.DT <- res[, .N, by='s'][order(-N)][N > 1]
  setnames(res.DT, 'N', 'count')
  return(res.DT)
}


FindNgram <- function(x, n=3){
  content <- iconv(x$content, "utf-8", "ASCII", sub="")
  print('Compute n-gram')
  res.ngram <- CountNgramSkip(content, n, 0)
  if (n==1) return(res.ngram)
  print('Compute skip n-gram')
  res.ngram.skip <- CountNgramSkip(content, n, 1)
  res.ngram.all <- rbind(res.ngram, res.ngram.skip)  
  return(res.ngram.all)
}

### A Lot of the small word are used really often
CreateNGramTable <- function(corp, n, v.size=0){
  
  res.l <- lapply(corp, FindNgram, n=n)
  
  res.dt <- rbindlist(res.l)[, list(count=sum(count)), by=s]
  res.dt <- res.dt[res.dt$count>2]
  res.dt <- CleanNGram(res.dt)
  res.dt <- res.dt[, list(count=sum(count)), by=s]

  if(n>1 & v.size > 0){ 
    res.adj.count <- GoodTuringSmoothing(FreqNGramVector(res.dt, n, v.size),
                                         v.size) # Compute the smoothing count
    setkey(res.dt, count) # Join the two datasets
    setkey(res.adj.count, count)
    res.dt <- res.adj.count[J(res.dt)]
  }
  setkey(res.dt, s)
  return(res.dt)
}

CleanNGram <- function(n.gram.table){
  ngt <- n.gram.table
  ngt <- ngt[!grepl('- ', ngt[, s]),]
  ngt[, 's':= gsub('^-','', s)]
  return(ngt)
}

### Function to compute the "frequency of frequencies vector"
# http://en.wikipedia.org/wiki/Good%E2%80%93Turing_frequency_estimation
FreqNGramVector <- function(n.gram.table, n=1, v.size=9e5){
  res <- n.gram.table[, list(inv.freq=.N), by=count][order(count)]
  res <- rbind(data.table(count=0, inv.freq=v.size^2-sum(res$inv.freq)), res)
  return(res)
}

##' Implements a heuristic version of the Good-Turing Smoothing 
##'
##' When there are no n-gram with r+1, one uses the the alpha smoothing version
##' @title Good-Turing Smoothing for adjusted count of n-grams
##' @param DT, result of the function FreqNGramVector
##' @param v.size, vocabulary size (number of row of the unigrams)
##' @param alpha smoothing parameter, default 0.00017
##' @return a data.table with column count, inv.freq and adj.count
##' @author david
GoodTuringSmoothing <- function(DT, v.size, alpha = 0.00017, ngram=3){
  n <- DT[, sum(inv.freq)]
  DT[, adj.count:=0.0]
  DT[1, adj.count:=inv.freq/n]

  for (i in seq_along(DT$count)[-1]){
    if(DT[c(i-1, i), diff(count)!=1]) {
      DT[i, adj.count:=(count+alpha)*n/(n+alpha*v.size^ngram)] # alpha adjustement
    } else {
      DT[i, adj.count:=(DT[i, count])*(DT[i, inv.freq]/DT[i-1, inv.freq])]
    }
  }
  return(DT)
}

##' Compute the Conditional probability of trigram
##'
##' Use 3- and 2-grams to compute the conditional probability of a sentence
##' @title Conditional Probability of Trigram computations
##' @param nb a data.table with the with column "s" and "count" where s is a three words strings (nb = ngram big) (usually the trigram)
##' @param ns  idem as trig but for bigrams ("s" contains two words sentence) n(gram-small) , usually the bigram
##' @param n, the dimension of the n-gram of in nb
##' @return a data.table with the conditional probability for each trigrams
##' @author david
ConditionalProbNgram <- function(nb, ns, n){

  if (n == 1){
   res <-  nb[, list(s=s, count=count, adj.count=count,
                        cond.prob=count/sum(count))]
   return(res)
  }
  
  setkey(ns, s)
  
  nb[, key.b:= vapply(strsplit(nb$s, ' '), function(x){    
      paste0(x[-length(x)], collapse=' ')
    }, character(1))]
  
  setkey(nb, key.b)
  ### Join and do the division on the adjusted count
  res <- ns[J(nb)][,list(key.w=s, s=i.s, adj.count=adj.count,
                         cond.prop=i.count/count)]
  setkey(res, s)
  return(res)
}

##' Discount Factor computation
##'
##' Use d as the sum of 1 - \sum_{w_2} \alpha (w_2, w_1)
##' @title Dicount Factor Computation for Back Off model
##' @param DT , result of function CondtionalProbNgram
##' @return a data.table with the discount factor compute as 1 - \sum_{w_2} \alpha(w_2, w_1)
##' @author david
ComputeDiscountFactor <- function(DT){
  res <- DT[, list(disc.f= 1 - sum(cond.prop)), by = key.w]
  setnames(res, 'key.w', 's')
  setkey(res, s)
  return(res)
}


GenerateAdjustedProbTables <- function(txt.clean){

  unigram <- CreateNGramTable(txt.clean, 1L)
  v.size <- nrow(unigram)
  print('End of unigram')
  # uprob <- ConditionalProbNgram(unigram, unigram, 1)
  gc()
  trigram <- CreateNGramTable(txt.clean, 3L, v.size)
  print('End of trigram computation.')
  
  bigram <- CreateNGramTable(txt.clean, 2L, v.size)
  print('End of bigram computation.')
  
  bprob <- ConditionalProbNgram(bigram, unigram, 2)
  setkey(bprob, 'key.w')
  ## bdisc <- ComputeDiscountFactor(bprob)
  
  gc()
  tprob <- ConditionalProbNgram(trigram, bigram, 3)
  setkey(tprob, 'key.w')
  
  ## tdisc <- ComputeDiscountFactor(tprob)

  print('End of conditional probability computation')
  
  ## ngrams <- list(n1=unigram, n2=bigram, n3=trigram)

  prob.adj <- lapply(list(n2=bprob, n3=tprob),
                     function(DT) {
                       res <- DT[(DT[c('', ' '), -.I])]
                       res <- res[!grep('\\d', res$s),]
                       res[, adj.count:=NULL]
                       setkey(res, 'key.w')
                     })
  gc()                     
  ## disc.fac <- list(n2=bdisc, n3=tdisc) # Discount Factors

  return(prob.adj)
}

prob.adj <- GenerateAdjustedProbTables(txt.clean)
print('I finished to compute the probabilities. Now saving them.')
# save(prob.adj, file='prob.adj.Rdata')
save(prob.adj, file='prob.adj.all.Rdata')

### TODO Do a jump a one words 2 and 3 grams
### Create function split by spaces, than having the right index, and then table function or text count function

### TODO Write that words does not exists

### Not interesting in giving the probability, but giving the best prediction
### So no need of discounting factor or back-off algorithm: just itereate
### with the n2 and n1 if necessary

### Justify with computing power
### Right method: trigram with skip one word,

### If bigrams not found, go in either keys
### Words clouds

## if .tl less than 5 words skip

x <- txt.clean[[1]]

## res <- FindNgram(x)
content <- iconv(x$content, "utf-8", "ASCII", sub="")
n <- 3

## library('microbenchmark')
## cl <- makeCluster(4)

## clusterEvalQ(cl, {
##     library('data.table')
##     library('parallel')
##     })
## n <- 3
## skip <- 1

## clusterExport(cl, c('n', 'skip'))

## CountNgramSkip(content[1:2000])
    
## res2 <- parLapply(cl, content[1:108], CountNgramSkip, n=3, skip=1))

## s <- content

## system.time({
## res <- unlist(parLapply(cl, s, function(txt){
##     txt <- tolower(txt) 
##     t.l <- strsplit(txt, ' ')[[1]]

##     if (length(t.l) < 5) {return(data.table())}
  
##     res <- vapply(1:(length(t.l)-(2*n-1)), function(idx){
##       paste(t.l[idx+((skip+1)*(1:n)-(skip+1))], collapse=' ')
##     }, character(1))
    
##     return(res)
##   }))
## }
## )

## stopCluster(cl)

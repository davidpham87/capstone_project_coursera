library(data.table)
library(parallel)
library(xtable)
### Load Data

x <- scan('final/en_US/en_US.news.txt', character(0), sep = '\n')
y <- scan('final/en_US/en_US.blogs.txt', character(0), sep = '\n')
z <- scan('final/en_US/en_US.twitter.txt', character(0), sep = '\n')

# Exo 3
max(nchar(x))
max(nchar(y))

# Exor 4
n <- round(length(z)/1000)
z.s <- z[sample(n)]

length(grep('love', z.s))/length(grep('hate', z.s))

# Exo 5
z[grep('biostats', z)]

# Exo 6
idx <- z == "A computer once beat me at chess, but it was no match for me at kickboxing"
sum(idx)


### Basic statistics
dat <- list(news=x, blogs=y, twitter=z)



basicStat <- function(txt){
    messages <- length(txt)
    txt <- sample(txt, 20000) # sampling as it useless to have more
    lt <- strsplit(txt, ' ') # split by spaces

    ## num.w.u <- vapply(lt, function(x) length(unique(x)), numeric(1))
    ## med.words.unique <- median(num.w.u)
    ## med.words.unique <- mad(num.w.u)

    num.words <- vapply(lt, length, numeric(1))
    med.words <- median(num.words)
    mad.words <- mad(num.words)
    
    num.chars <- nchar(txt) # vectorized version    
    med.chars <- median(num.chars)
    mad.chars <- mad(num.chars)
       
    res <- data.table(messages, med.words, mad.words, med.chars, mad.chars,
                      med.words.unique, mad.words.unique)
    
    return(res)
}

sum.tab <- rbindlist(mclapply(dat, basicStat, mc.cores=4))
sum.tab[, colnames(sum.tab):=lapply(.SD, round, 2)]
sum.tab[, dataset:=names(dat)]

xtable(sum.tab) # LaTeX output

## Certainly several clusters of observations: some observation have really little words a other have a lot more.










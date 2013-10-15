#install.packages("rjson")
library(rjson)

rd <- read.table("intensifiers.results", sep="\t", quote='"', header=TRUE)

all.subjects <- rd$workerid
questions <- lapply(as.character(rd$Answer.questions), fromJSON)

#get rid of first and last quotes.
cutQuotes <- function(quoteyString) {
  startLetter <- 2
  stopLetter <- nchar(quoteyString) - 1
  return(substr(quoteyString, startLetter, stopLetter))
}

#function to get mturk responses, long form
getProp <- function(id) {
  c(sapply(questions, function(question.set){
    return(c(sapply(question.set, function(q) {
      return(q[[id]])
    })))
  }))
}

#get long-form data for different variables
intensifier <- getProp("intensifier")
item <- as.character(getProp("item"))
price <- as.numeric(getProp("price"))
rt <- getProp("rt")
name <- getProp("name")
color <- getProp("color")

#copy subjects times n.qs to make long-form data.frame
n.qs <- length(questions[[1]]) #how many times to copy Ss
subj <- c(sapply(all.subjects, function(x){return(rep(x,n.qs))}))

raw.data <- data.frame(subj, intensifier, item, price, rt, name, color, z=numeric(length(subj)),
                       scaled=numeric(length(subj)), subj.scaled=numeric(length(subj)))
#four answers were many degrees of magnitude off from the others, these are excluded
no.outliers.data <- raw.data[raw.data$price < 100000 | !(raw.data$intensifier == "horribly" |
                                               raw.data$intensifier == "terribly" |
                                               raw.data$intensifier == "wildly" |
                                               raw.data$intensifier == "excessively"),]
#one person gave no difference for different intensifiers (e.g. said all "__ expensive laptop"s
#cost $1200)
data <- no.outliers.data[no.outliers.data$subj != "A21XQA7O8MV44U",]

items <- as.character(unique(data$item))
subjects <- as.character(unique(data$subj))
nsubj <- length(subjects)

syllables <- c(0, 2, 4, 4, 4, 3, 3, 2, 3, 1, 2, 2, 3, 4, 2, 2, 3)
names(syllables) <- sort(unique(data$intensifier))

#z-score data by subject and item
for (s in unique(data$subj)) {
  for (i in items) {
    indices <- data$subj==s & data$item==i
    df <- subset(data, indices)
    data$z[indices] <- scale(df$price)
    if (is.na(data$z[indices][[1]])) {
      print(indices)
      print(df)
    }
  }
}

#divide by base "expensive" case for given subject and item
for (s in unique(data$subj)) {
  for (i in items) {
    indices <- data$subj==s & data$item==i
    df <- subset(data, indices)
    #some people made the base judgment tiwce, which is why i'm averaging
    data$subj.scaled[indices] <- df$price / mean(df$price[df$intensifier == ""])
  }
}

#divide by base "expensive" case for given item, averaged across Ss
for (s in unique(data$subj)) {
  for (i in items) {
    indices <- data$item == i
    df <- subset(data, indices)
    data$scaled[indices] <- df$price / mean(df$price[df$intensifier == ""])
  }
}

#subtract base "expensive" case for given item, averaged across Ss
for (s in unique(data$subj)) {
  for (i in items) {
    indices <- data$item == i
    df <- subset(data, indices)
    data$subtract[indices] <- df$price - mean(df$price[df$intensifier == ""])
  }
}

#prices in standard deviations above the mean (per item) using justine's data
filename <- 'humanPriors.json'
human.priors <- fromJSON(readChar(filename, file.info(filename)$size))
for (i in items) {
  indices <- data$item == i
  item.sd <- sd(human.priors[[i]])
  df <- subset(data, indices)
  data$sd[indices] <- df$price / item.sd
}

conf <- function(v) {
  v <- v[is.na(v) == F]
  sample.means <- replicate(100, mean(sample(v, nsubj, replace=TRUE)))
  return(quantile(sample.means, c(0.025, 0.975)))
}
lower.conf <- function(v) {
  conf(v)[["2.5%"]]
}
higher.conf <- function(v) {
  conf(v)[["97.5%"]]
}
error.bar <- function(x, y, upper, lower=upper, lw=2, col="black", length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  #arrows(x, upper, x, lower, angle=90, code=3, lwd=2, col=col, length=length, ...)
  segments(x, upper, x, lower, col=col, lw=lw, ...)
}
getUnigrams <- function() {
  cnames <- c("word", "year", "count")
  cclass <- c("character", "numeric", "numeric")
  unigrams.data <- read.table("intensifier1grams.sep", sep=" ", header=F, col.names=cnames, colClasses=cclass)
  #bigrams.data <- read.table("intensifier2grams.sep", sep="\t", header=F, col.names=cn, colClasses=cc)
  
  ages <- as.numeric(sapply(as.character(rd$Answer.age), cutQuotes))
  avg.age <- mean(ages)
  min.age <- min(ages)
  
  expteriment.year = 2013
  year.since <- expteriment.year - avg.age
  
  subs <- subset(unigrams.data, unigrams.data$year > year.since)
  unigram.avg.data <- aggregate(count ~ word, data=subs, FUN=sum)
  
  o <- order(unigram.avg.data$word)
  unigram <- unigram.avg.data$count[o]
  names(unigram) <- unigram.avg.data$word[o]
  
  return(unigram)
}

unigram <- getUnigrams()
unigram <- unigram[names(unigram) != "super"]
unigram <- unigram[names(unigram) != "crazy"]
unigram <- unigram[names(unigram) != "expensive"]
unigram <- unigram[names(unigram) != "total"]

analyze <- function(version) {
  avg.data <- aggregate(x=data[[version]],
                        by=list(intensifier=data$intensifier, item=data$item),
                        FUN=function(x){return(mean(x, na.rm=T))})
  avg.data$lower <- aggregate(x=data[[version]],
                              by=list(intensifier=data$intensifier, item=data$item),
                              FUN=lower.conf)$x
  avg.data$higher <- aggregate(x=data[[version]],
                               by=list(intensifier=data$intensifier, item=data$item),
                               FUN=higher.conf)$x
  
  #draw plots
  colors <- c('red', 'green2', 'blue')
  lws <- c(3, 2, 1)
  all.expt <- c()
  col.names <- c()
  for (i in 1:length(items)) {
  #for (i in 2:3) {
    item <- items[i]
    col.names <- c(col.names, item)
    color <- colors[i]
    lw <- lws[i]
    expt <- avg.data[avg.data$item == item, ]
    base.expensive <- expt$x[expt$intensifier == ""]
    expt <- expt[!(expt$intensifier %in% c("super", "crazy", "")),]
    all.expt <- c(all.expt, expt$x)
    if (version == "price") {
      ylab="Ss price"
      main="intensifier responses by item"
      ylim=c(200, 3800)
    } else if (version == "scaled") {
      ylim=c(1, 10)
      ylab="Ss multiple of 'expensive' price (avged accross Ss)"
      main="intensifier by item - multiple of base 'expensive' judgement"
    } else if (version == "subj.scaled") {
      ylim=c(1, 14)
      ylab="Ss multiple of their own 'expensive' price"
      main="intensifier by item - multiple of base 'expensive' judgement"
    } else if (version == "z") {
      ylim=c(-1.5, 1.5)
      ylab="Ss prices z-scored"
      main="intensifier by item - z-scores"
    } else if (version == "subtract") {
      main <- "intensifier by item - subtract off base 'expensive'"
      ylab <- "Ss prices minus base 'expensive'"
      ylim <- c(0, 3500)
    } else if (version == "sd") {
      ylim=c(0, 300)
      #ylim=c(3,12)
      ylab="Ss responses in standard deviations"
      main="intensifier by item - standard deviations (from justine's data)"
    }
    plot(log(unigram), expt$x, ylim=ylim, ylab=ylab,
         xlab="log freq", col=color, main=main)
    print(paste(version, item, cor(log(unigram), expt$x)))
    error.bar(log(unigram), expt$x, expt$lower, col=color, lw=lw, expt$higher)
    #abline(h=base.expensive)
    par(new=T)
  }
  legend("topright", items, cex=0.8, col=colors, lwd=2, bty="n");
  par(new=F)
  all.expt <- matrix(data=all.expt, ncol=3)
  colnames(all.expt) <- col.names
  expt.means <- sapply(1:nrow(all.expt), function(i) {return(mean(all.expt[i,]))})
  print(paste(version, "mean", cor(log(unigram), expt.means)))
}

# for (version in c("price", "scaled", "subj.scaled", "z", "sd")) {
#   analyze(version)
# }

# # hist(data$price[data$item == "laptop"], breaks=20)
# # hist(human.priors$laptop, breaks=20)
# watch <- sort(data$price[data$item == "watch"])
# hist(watch[1: (length(watch)-3)], breaks=100)
# hist(human.priors$watch, breaks=20)
# coffee <- sort(data$price[data$item == "coffee maker"])
# hist(x=coffee[1:(length(coffee)-1)], breaks=20)
# # hist(human.priors[["coffee maker"]], breaks=20)
# # 
# hist(human.priors[["sweater"]], breaks=20)
# hist(human.priors[["headphones"]], breaks=100)
# hist(human.priors[["electric kettle"]], breaks=20)
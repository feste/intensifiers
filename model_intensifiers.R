##need to figure out how thetas work. is there one for every utterance?

#install.packages("rjson")
library(stats)
require(logspline)
library(rjson)

surprisal.weight = 1
length.weight = 0

#for discretization:
grid.steps = 64
grid = seq(0,1,length.out=grid.steps)
cache.index = function(v) {
  return(1+round(v*(grid.steps-1)))
}

#load human priors:
examples <- fromJSON(readLines("human-priors.JSON")[[1]])
#scale to max 1:
examples.scale <- lapply(examples, max)
examples <- lapply(examples, function(exs){
  return(exs/max(exs))
})

#intensifiers from experiment, 15 each distribution
possible.utterances = c('none', 'expensive', 'enormously', 'exceedingly',
                        'excessively', 'extremely', 'horribly', 'hugely',
                        'insanely', 'quite', 'really', 'terribly', 'uncommonly',
                        'vastly', 'very', 'wildly')
#ridiculously, outrageously, awfully, ...

getUnigrams <- function() {
  cnames <- c("word", "year", "count")
  cclass <- c("character", "numeric", "numeric")
  unigrams.data <- read.table("intensifier1grams.sep", sep=" ", header=F, col.names=cnames, colClasses=cclass)
  #bigrams.data <- read.table("intensifier2grams.sep", sep="\t", header=F, col.names=cn, colClasses=cc)
  
  ages <- c(31, 51, 33, 54, 31, 27, 45, 31, 29, 44, 24, 28, 35, 25, 52,
            43, 32, 37, 41, 53, 37, 31, 44, 25, 42, 47, 24, 41, 59, 60,
            22, 40, 43, 37, 21, 38, 35, 28, 30, 20)
  avg.age <- mean(ages)
  min.age <- min(ages)
  
  expteriment.year = 2013
  year.since <- expteriment.year - avg.age
  
  subs <- subset(unigrams.data, unigrams.data$year > year.since)
  unigram.avg.data <- aggregate(count ~ word, data=subs, FUN=sum)
  
  o <- order(unigram.avg.data$word)
  unigram <- unigram.avg.data$count[o]
  names(unigram) <- unigram.avg.data$word[o]
  unigram <- unigram[names(unigram) != "super"]
  unigram <- unigram[names(unigram) != "crazy"]
  
  return(unigram)
}
unigrams <- getUnigrams() #frequency
surprisal <- -log(unigrams/unigrams["total"])
surprisal[names(surprisal) != "expensive"] = surprisal[names(surprisal) != "expensive"] *
                                             surprisal["expensive"]
surprisal["none"] = 0
utterance.surprisal = surprisal[possible.utterances]

syllables <- c(none=0, expensive=3, enormously=7, exceedingly=7, excessively=7,
               extremely=6, horribly=6, hugely=5, insanely=6, quite=4, really=5,
               terribly=6, uncommonly=7, vastly=5, very=5, wildly=5.5)

utterance.length = syllables[possible.utterances]
utterance.polarities = c(replicate(length(utterance.length), 1))

#using r function density to find kernal density, so it's not actually continuous
# kernel.granularity <- grid.steps #2^12 #how many points are calculated for the kernel density estimate
# est.kernel <- function(dist, bw) {
#   return(density(examples[[dist]], from=0, to=1, n=kernel.granularity,
#                  kernel="gaussian", bw=bw, adjust=1))
# }
est.kernel <- function(dist,bw) {
  e <- examples[[dist]]
  es <- examples.scale[[dist]]
  k <- list()
  k$y <- dlogspline(grid*es, logspline(es*e, lbound=0))#,ubound=1)) #do smoothing in original space
  k$x <- grid
    return(k)
}

#norms the kernel density
#takes in all the points where kernel density is estimated
make.pdf.cache <- function(kernel.est) {
  k = kernel.est$y + 0.00001
  area <- sum(k) 
  normed.dens <- k/area
  return(normed.dens)
}

#creates fn that approximates percentage of area before x
#takes in all the points where kernel density is estimated
make.cdf.cache <- function(kernel.est) {
  cumulants <- cumsum(make.pdf.cache(kernel.est))
  return(cumulants)
}


##caching. (R has strange purity on global assignments, so must use <<- to set cache)
L0.cache <- array(NA,dim = c(grid.steps,grid.steps,length(possible.utterances)))
S1.cache <- array(NA,dim = c(grid.steps,grid.steps,length(possible.utterances)))
cache.misses=0 #to track whether caching is working right.

clear.cache = function(){
  cache.misses<<-0
  L0.cache <<- array(NA,dim = c(grid.steps,grid.steps,length(possible.utterances)))
  S1.cache <<- array(NA,dim = c(grid.steps,grid.steps,length(possible.utterances)))
}


listener0 = function(utterance.idx, thetas.idx, degree.idx, pdf, cdf) {
  
  if(is.na(L0.cache[degree.idx,thetas.idx[1],utterance.idx])) {
    cache.misses <<- cache.misses + 1
    if (utterance.idx == 1) { #assume the null utterance
      L0.cache[degree.idx,thetas.idx[1],utterance.idx] <<- pdf[degree.idx]
    }  else if(utterance.polarities[utterance.idx] == +1) {
      theta.idx = thetas.idx[utterance.idx-1]
      utt.true = grid[degree.idx] >= grid[theta.idx]
      true.norm = if(theta.idx==1){1} else {1-cdf[theta.idx-1]}
      L0.cache[degree.idx,thetas.idx[1],utterance.idx] <<- utt.true * pdf[degree.idx] / true.norm
    } else {
      theta.idx = thetas.idx[utterance.idx-1]
      utt.true = grid[degree.idx] <= grid[theta.idx] 
      true.norm = cdf[theta.idx]
      L0.cache[degree.idx,thetas.idx[1],utterance.idx] <<- utt.true * pdf[degree.idx] / true.norm
    }
  }
  return(L0.cache[degree.idx,thetas.idx[1],utterance.idx])
}

speaker1 = function(thetas.idx, degree.idx, utterance.idx, alpha, pdf, cdf) {
  
  if(is.na(S1.cache[degree.idx,thetas.idx[1],utterance.idx])) {
    cache.misses <<- cache.misses + 1
    utt.probs = array(0,dim=c(length(possible.utterances)))
    for(i in 1:length(possible.utterances)) {
      l0 = listener0(i, thetas.idx, degree.idx, pdf, cdf)
      ##########HERE###################FIXTHIS##############
      utt.cost = surprisal.weight*utterance.surprisal[utterance.idx] +
                 #or is there an interaction?
                 length.weight * utterance.length[utterance.idx]
      utt.probs[i] <- (l0^alpha) * exp(-alpha * utt.cost)
    }
    S1.cache[degree.idx,thetas.idx[1],] <<- utt.probs/sum(utt.probs)
  }
  
  return(S1.cache[degree.idx,thetas.idx[1],utterance.idx])
}

listener1 = function(utterance, alpha, n.samples, step.size,
                     dist, band.width) {
  
  utt.idx = which(possible.utterances == utterance)
  
  kernel.est <- est.kernel(dist, band.width)
  pdf <- make.pdf.cache(kernel.est)
  cdf <- make.cdf.cache(kernel.est)
  
  dim1 <- paste('samp', 1:n.samples, sep='')
  dim2 <- c('degree', paste('theta.', possible.utterances[-1], sep=''))
  dimnames <- list(dim1, dim2)
  samples = matrix(NA, nrow=n.samples, ncol=length(possible.utterances), dimnames=dimnames)
  
  
  #scoring function, to compute (unormalized) probability of state. (should be in log domain?)
  prob.unnormed = function(state) {
    #check bounds:
    if (any(state < 0) || any(state > 1)) {return(0)}
    degree.idx = cache.index(state[1])
    thetas.idx = c(cache.index(state[2]))#sapply(thetas,cache.index)
    #prior for degree (thetas have unif prior):
    prior = pdf[degree.idx]
    #probbaility speaker would have said this (given state):
    likelihood = speaker1(thetas.idx, degree.idx, utt.idx, alpha, pdf, cdf)
    return(prior*likelihood)
  }
  
  #initialize chain by rejection:
  print("initializing")
  state.prob=0
  state = runif(length(possible.utterances), 0, 1) #a degree val, and a theta for all but "no-utt"
  while(state.prob==0) {
    state = runif(length(possible.utterances), 0, 1) #a degree val, and a theta for all but "no-utt"
    state.prob = prob.unnormed(state)
    print(state.prob)
  }
  samples[1,] = state
  
  #make an MH proposal, spherical gaussian on degree and thetas. 
  make.proposal = function(v) {
    perturbations = rnorm(length(v), mean = 0, sd = step.size)
    return(v + perturbations)
  }
  
  #run mcmc chain:
  print("running mcmc")
  n.proposals.accepted = 0
  for (i in 2:n.samples) {
    proposal = make.proposal(state)
    proposal.prob = prob.unnormed(proposal)
    #MH acceptance, assumes proposal is symmetric:
    if(runif(1,0,1) <= min(1, proposal.prob/state.prob)) {
      n.proposals.accepted = n.proposals.accepted + 1
      state = proposal
      state.prob = proposal.prob
    }
    samples[i,] = state
  }
  
  print("acceptance rate:")
  print(n.proposals.accepted/(n.samples-1))
  #print("misses since last cache clear:")
  #print(cache.misses)
  
  return(list(samples=samples, prop.accepted=n.proposals.accepted/(n.samples-1)))
}

model.intensifiers <- function(cat) {
  n.true.samples <- 30000 #number of samples to keep
  lag <- 5 #number of samples to skip over
  burn.in <- 10
  n.samples <- n.true.samples * lag + burn.in
  step.size <- 0.03 #note this may not be appropriate for all conditions.
  alpha<-5
  
  clear.cache()
  samples = listener1('none', alpha=alpha, n.samples=n.samples,
                      step.size=step.size, dist=cat, band.width="SJ")
}

model.intensifiers("watch")
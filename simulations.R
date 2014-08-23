nosim <- 1000

lambda <- .2
n <- 40
cfunc <- function(x, n) sqrt(n) * (x - (1/lambda)) / (1/lambda)
AllMeans= NULL
NormalizedMean= NULL
AllSD = NULL
for (i in seq(1,nosim,1)) {
    x <- rexp(n, lambda)
    sample_mean <- mean(x)
    normalized_mean <- cfunc(sample_mean,n)
    standard_deviation <- sd(x)
    AllMeans=rbind(AllMeans,sample_mean)
    NormalizedMean=rbind(NormalizedMean, normalized_mean)
    #AllSD = rbind(AllSD,standard_deviation)
    
}
rownames(NormalizedMean) <- NULL
#par(mfrow=c(2,2))
#dseq=seq(-5,5,length=1000)
#y=dnorm(dseq,mean=0,sd=1)
#plot(dseq,y,type="l",lwd=2,col="red")
#data <- data.frame(AllMeans,AllOther,AllSD)
data <- data.frame(NormalizedMean)

#hist(AllMeans)
#hist(AllOther)
#hist(AllSD)
theory_sample_mean_sd <- 1/sqrt(n) * (1/lambda)
real_sample_mean_sd <- sd(AllMeans)
sample_mean_mean <- mean(AllMeans)
ul <- mean(AllMeans) +  1.96 * real_sample_mean_sd
ll <- mean(AllMeans) -  1.96 * real_sample_mean_sd
inside <- 0
for (i in AllMeans) {
    if (i > ll & i < ul){
        inside <- inside + 1
    }
}
coverage <- inside/length(AllMeans)
g <- ggplot(data, aes(x = NormalizedMean)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)
print(g)

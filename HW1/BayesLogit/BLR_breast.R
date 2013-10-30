# BLR_functions.R contains the Metropolis Hastings implementation
source("BLR_functions.R") 

#~~~~~ Let's do it ~~~~~~
dat=read.csv("breast_cancer.txt", sep="")
n=nrow(dat)-1 # the last row is garbage
dat=dat[1:n,] # remove that last row
m=rep(1,n) # vector of all 1's
response=dat$diagnosis
y=ifelse(response=="M", 1, 0) # response vector, with M=1, B=0
p=ncol(dat)
# keep track of which variables are in what column
variables=c("intercept", colnames(dat)[1:(p-1)])
X=dat[,1:(p-1)]
X=as.matrix(sapply(X, as.numeric)) # convert everything to numbers
# Rescale the matrix because posterior can't be calculated with too large numbers
# Also adds the 1's column to X.
X=cbind(rep(1,n), scale(X))
beta.0=rep(0,p)
Sigma.0.inv=1000*diag(p)

res=bayes.logreg(m,y,X,beta.0,Sigma.0.inv, verbose=FALSE)
thetas=res$thetas

autocorrs=autocorr(thetas,lags=1)[1,,]
write.table(autocorrs, file="breast/autcorrelation.csv",
						row.names=FALSE, col.names=FALSE, sep=",", quote=FALSE)

# save quantiles of estimates
qq=matrix(NA, ncol=p, nrow=99)
for(i in 1:p)
{
	quant=quantile(thetas[,i], seq(.01,.99,.01))
	qq[,i]=quant
}
write.table(qq, file=paste0("results/blr_res_breast.csv"),
						row.names=FALSE, col.names=FALSE, sep=",", quote=FALSE)

for(i in 1:p)
{
	jpeg(paste0("breast/breast_mean_", variables[i], ".jpg"))
	hist(thetas[,i], xlab=variables[i])
	dev.off()
	jpeg(paste0("breast/breast_mean_", variables[i], ".jpg"))
	hist(thetas[,i], xlab=variables[i])
	dev.off()
}

# Do posterior predictive checking. Generate a dataset per starting theta,
# then calculate mean and median.
# Probably wrong, 'cause I suck.
means=colMeans(thetas)
B=nrow(thetas) # number of repetitions, just made it 30000 for fun
PC_greater=matrix(NA, ncol=ncol(thetas), nrow=B)
PC_lesser=matrix(NA, ncol=ncol(thetas), nrow=B)
samp=sample(1:nrow(thetas), size=B, replace=TRUE)

# Count how many data points > sample point, and conversely.
for(i in 1:B)
{
	for(j in 1:ncol(thetas))
	{
		samplepoint=thetas[samp[i],j]
		ProbGreater = PC_greater[i,j] = sum(samplepoint>thetas[,j])
		ProbLesser = PC_lesser[i,j] = sum(samplepoint<thetas[,j])
	}
}

# Use the counts from above to determine P(mean(sample)>mean(actual)) and
# converse.
# vector p contains the "p-value" 2*min(P(x>y),P(y>x)) for each variable
p_g=colMeans(PC_greater/nrow(thetas))
p_l=colMeans(PC_lesser/nrow(thetas))
p=numeric(ncol(thetas))
for(i in 1:ncol(thetas))
{
	p[i]=2*min(p_g, p_l)
	jpeg(paste0("breast/blr_breast_meean_", variables[i], ".jpg"))
	hist(cbind(PC_greater[,i]/B, PC_lesser[,i]/B),
				xlab=paste0("P(",variables[i],">true mean)"),
				main=paste("Histogram for", variables[i]))
	abline(v=.5)
	dev.off()
}


source("BLR_functions.R") # BLR_functions.R contains the Metropolis Hastings implementation

#~~~~~ Let's do it ~~~~~~
dat=read.csv("breast_cancer.txt", sep="")
n=nrow(dat)-1 # the last row is garbage
dat=dat[1:n,] # remove that last row
m=rep(1,n) # vector of all 1's
response=dat$diagnosis
y=ifelse(response=="M", 1, 0) # response vector, with M=1, B=0
p=ncol(dat)
variables=c("intercept", colnames(dat)[1:(p-1)]) # keep track of which variables are in what column
X=dat[,1:(p-1)]
X=as.matrix(sapply(X, as.numeric)) # convert everything to numbers
X=cbind(rep(1,n), scale(X))#sweep(X,2,colSums(X), FUN="/") # rescale the matrix because posterior can't be calculated with too large numbers
#attributes(X)<-NULL
beta.0=rep(0,p)
Sigma.0.inv=1000*diag(p)

res=bayes.logreg(m,y,X,beta.0,Sigma.0.inv, verbose=FALSE)
thetas=res$thetas

autocorrs=autocorr(thetas,lags=1)[1,,]
write.table(autocorrs, file="breast/autcorrelation.csv", row.names=FALSE, col.names=FALSE, sep=",", quote=FALSE)

# save sample set in file, with column names, but no row index
#write.table(thetas, file="thetas.txt", row.names=FALSE, quote=FALSE)
#write.table(res$acceptance, file=paste("acceptance_", datasetNum, ".txt", sep=''), row.names=FALSE, col.names=1:ncol(res$acceptance), quote=FALSE)

# save quantiles of estimates
qq=matrix(NA, ncol=p, nrow=99)
for(i in 1:p)
{
	quant=quantile(thetas[,i], seq(.01,.99,.01))
	qq[,i]=quant
}
write.table(qq, file=paste0("results/blr_res_breast.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote=FALSE)

for(i in 1:p)
{
	jpeg(paste0("breast/breast_mean_", variables[i], ".jpg"))
	hist(thetas[,i], xlab=variables[i])
	dev.off()
	jpeg(paste0("breast/breast_mean_", variables[i], ".jpg"))
	hist(thetas[,i], xlab=variables[i])
	dev.off()
}

# Do posterior predictive checking. Generate a dataset per starting theta, then calculate mean and median.
numOfCheck=10
pc_mean=matrix(NA, ncol=p, nrow=numOfCheck+1)
pc_mean[1,]=colMeans(thetas)
pc_median=matrix(NA, ncol=p, nrow=numOfCheck+1)
pc_median[1,]=apply(thetas, 2, median)
#for(i in 1:nrow(thetas))
#{
#	beta.0=thetas[i,]
#
#	res_check=bayes.logreg(m,y,X,beta.0,Sigma.0.inv, niter=10000, print.every=9000)
#	pc_mean[i+1,]=colMeans(res_check$thetas)
#	pc_median[i+1,]=apply(res_check$thetas, 2, median)
#}
for(i in 1:numOfCheck)
{
	print(i)
	beta.0=rnorm(p) # sample initial point from prior (standard Normal)
	res_check=bayes.logreg(m,y,X,beta.0,Sigma.0.inv, verbose=FALSE)
	pc_mean[i+1,]=colMeans(res_check$thetas)
	pc_median[i+1,]=apply(res_check$thetas, 2, median)
}

for(i in 1:ncol(thetas))
{
	jpeg(paste0("breast/breast_pcMean_", variables[i], ".jpg"))
	hist(pc_mean[2:nrow(pc_mean),i])
	abline(v=pc_mean[1,i], lwd=5, col="red")
	dev.off()

	jpeg(paste0("breast/breast_pcMedian_", variables[i], ".jpg"))
	hist(pc_median[2:nrow(pc_median),i])
	abline(v=pc_median[1,i], lwd=5, col="red")
	dev.off()
}

write.table(pc_mean, file=paste0("breast/blr_breast_meanAll.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote=FALSE)
write.table(pc_median, file=paste0("results/blr_breast_medianAll.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote=FALSE)

means=colMeans(thetas)
var=1
B=nrow(thetas) # number of repetitions
PC_greater=matrix(NA, ncol=ncol(thetas), nrow=B)
PC_lesser=matrix(NA, ncol=ncol(thetas), nrow=B)
samp=sample(1:nrow(thetas), size=B, replace=TRUE)
for(i in 1:B)
{
	for(j in 1:ncol(thetas))
	{
		samplepoint=thetas[samp[i],j]
		ProbGreater = PC_greater[i,j] = sum(samplepoint>thetas[,j])#/sum(means[j]>thetas[,j])
		ProbLesser = PC_lesser[i,j] = sum(samplepoint<thetas[,j])#/sum(means[j]<thetas[,j])
	}
}

p_g=colMeans(PC_greater/nrow(thetas))
p_l=colMeans(PC_lesser/nrow(thetas))
p=numeric(ncol(thetas))
for(i in 1:ncol(thetas))
{
	p[i]=2*min(p_g, p_l)
	jpeg(paste0("breast/blr_breast_meean_", variables[i], ".jpg"))
	hist(cbind(PC_greater[,i]/B, PC_lesser[,i]/B), xlab=paste0("P(",variables[i],">true mean)"), main=paste("Histogram for", variables[i]))
	abline(v=.5)
	dev.off()
}

#greater=colMeans(ifelse(thetas[samp,]>means, 1, 0))
#lesser=colMeans(ifelse(thetas[samp,]<means, 1, 0))
#for(i in 1:B)
#{
#	for(j in 1:ncol(thetas))
#	{
#		sampletheta=thetas[sampl[i],j]
#		PC[i,j]=sum(sampletheta>means[j])

# m: Vector containing the number of trials for each observation (of length n)
#	y: Vector containing the number of successes for each observation (of length n)
#	X: Design matrix (of dimension n×p)
#	beta.0: Prior mean for β (of length p)
# Sigma.0.inv: Prior precision (inverse covariance) matrix for β (of dimension p×p)
#	niter: Number of iterations to run the MCMC after the burnin period
# burnin: Number of iterations for the burnin period (draws will not be saved)
#	print.every: Print an update to the user after every period of this many iterations
#	retune: Retune the proposal parameters every return iterations. No tuning should be done after the burnin period is completed
#	verbose: If TRUE then print lots of debugging output, else be silent

args<-commandArgs(TRUE)

sim_start <- 1000
length.datasets <- 200
#######################

if (length(args)==0){ 
	  sinkit <- FALSE
		datasetNum <- sim_start + 1  
		set.seed(1330931)
} else {
	  # Sink output to file?
		sinkit <- TRUE
		# Decide on the job number, usually start at 1000:
		datasetNum <- sim_start + as.numeric(args[1])
		# Set a different random seed for every job number!!!
		set.seed(762*sim_num + 1330931)
} 

#datasetNum=1000+as.numeric(args[1])
#print(datasetNum)

dat=read.csv(paste("data/blr_data_", datasetNum, ".csv", sep=''))
m=dat$n
y=dat$y
p=ncol(dat)
p=2
beta.0=c(0,0)
Sigma.0.inv=diag(p)
X=as.matrix(dat[,c('X1','X2')])

source("blr_functions.R")
res=bayes.logreg(m,y,X,beta.0,Sigma.0.inv)
thetas=res$thetas

# save sample set in file, with column names, but no row index
#write.table(thetas, file="thetas.txt", row.names=FALSE, quote=FALSE)
#write.table(res$acceptance, file=paste("acceptance_", datasetNum, ".txt", sep=''), row.names=FALSE, col.names=1:ncol(res$acceptance), quote=FALSE)

# save quantiles of estimates
quantiles1=quantile(thetas[,1],seq(0,.99,.01)) 
quantiles2=quantile(thetas[,2],seq(0,.99,.01)) 
asColumns=cbind(quantiles1, quantiles2)
write.table(asColumns, file=paste("blr_res__", datasetNum, ".csv"), row.names=FALSE, col.names=FALSE, sep=",", quote=FALSE)


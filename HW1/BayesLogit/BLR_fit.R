# BLR_functions.R contains the Metropolis Hastings implementation
source("BLR_functions.R")

#~~~~~~ Determine which dataset to test ~~~~~~
args<-commandArgs(TRUE)

sim_start <- 1000
length.datasets <- 200

if (length(args)==0){ 
	  sinkit <- FALSE
		sim_num <- sim_start + 1  
		set.seed(1330931)
} else {
	  # Sink output to file?
		sinkit <- TRUE
		# Decide on the job number, usually start at 1000:
		sim_num <- sim_start + as.numeric(args[1])
		# Set a different random seed for every job number!!!
		set.seed(762*sim_num + 1330931)
} 
datasetNum=sim_num
print(datasetNum)

#~~~~~ Let's do it ~~~~~~
dat=read.csv(paste0("data/blr_data_", datasetNum, ".csv"))
m=dat$n
y=dat$y
p=ncol(dat)-2
beta.0=rep(0,p)
Sigma.0.inv=diag(p)
X=as.matrix(dat[,3:ncol(dat)])

res=bayes.logreg(m,y,X,beta.0,Sigma.0.inv)
thetas=res$thetas

# save quantiles of estimates
quantiles1=quantile(thetas[,1],seq(.01,.99,.01)) 
quantiles2=quantile(thetas[,2],seq(.01,.99,.01)) 
asColumns=cbind(quantiles1, quantiles2)
write.table(asColumns, file=paste0("results/blr_res_", datasetNum, ".csv"), row.names=FALSE, col.names=FALSE, sep=",", quote=FALSE)


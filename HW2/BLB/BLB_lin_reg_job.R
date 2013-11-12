# Hello
library(BH)
library(bigmemory.sri)
library(bigmemory)
library(biganalytics)

#
# ~~==| Preset values |==~~
#
path <- "/home/pdbaines/data"
outpath <- "output/"

mini <- FALSE
# mini or full? n and number of parameters is determined based on this
# Of course, general code would determine these by reading the file,
# which is unnecessary here.
if (mini){
	  rootfilename <- "blb_lin_reg_mini"
		n<-10000
		numParameters<-41
} else {
	  rootfilename <- "blb_lin_reg_data"
		n<-1000000
		numParameters<-1001
}

s=5
r=50
gamma=0.7
datafile=paste0(path, "/", rootfilename, ".txt")

#
# ~~==| Setup for running on Gauss |==~~
#
args <- commandArgs(TRUE)
cat("Command-line arguments:\n")
print(args)
sim_start <- 1000
if (length(args)==0){
	sim_num <- sim_start + 1 
	set.seed(121231)
} else {
	sim_num <- sim_start + as.numeric(args[1])
	sim_seed <- (762*(sim_num-1) + 121231)
}
cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))
job=sim_num-sim_start

#
# ~~==| Preset Values |==~~
# 
# n = countLines(datafile) # Get total observations. Already known.
n.subset = floor(n^gamma) # Size of the subset data.

dat<-attach.big.matrix(dget(paste0(path, "/blb_lin_reg_data.desc"))
											,backingpath=path) # only for full data

#
# ~~==| Preliminary Steps 1(a): Creating subset sample. |==~~
#
# A function that samples from data to form the subsample.
getSampleFrom<-function(datafile, n, n.subset, numParameters)
{
	# Now, sample n^gamma indexes
	sampledRows = sample(n, size=n.subset)
  # Use those indexes to get the subsample.
	sample=dat[sampledRows,]

	invisible(sample) # Same as a return() statement, but faster.
} # end getSampleFrom()

# Determine s and r based on job number
r_index=((job-1) %% r) + 1 # This is mod(job-1, r)+1
s_index=((job-1) %/% r) + 1 # This is div(job-1, r)+1
set.seed(s_index) # subsample will be the same for each s_index
sample=getSampleFrom(datafile, n, n.subset, numParameters)

#
# ~~==| Bootstrap Sampling 1(b) |==~~
#
set.seed(s_index*r_index) # bootstrap will be different for each s_index
# Determine how many times an observation from subsample appears in the bootstrap.
bs = rmultinom(1, n, prob=rep(1/n.subset, n.subset))

y=sample[,ncol(sample)] # This is the response vector.
obs=sample[,-ncol(sample)] # This is the data minus the response vector.
bs.fit = lm(y ~ obs, weights=bs)$coefficients # Size: small. Hope it fits.

# Write the results from the fit to file 'coef_s_r.txt'
outfile = paste0("output/", "coef_", sprintf("%02d",s_index),
								 "_", sprintf("%02d",r_index), ".txt")
write.table(bs.fit, file=outfile,
						row.names=FALSE, col.names=FALSE, quote=FALSE, sep=", ")


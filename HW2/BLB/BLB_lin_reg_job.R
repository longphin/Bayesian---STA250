#install.packages("R.utils", repos="http://cran.us.r-project.org")
library(R.utils) # for countLines()
library(BH)
library(bigmemory.sri)
library(bigmemory)
library(biganalytics)

path <- "/home/pdbaines/data"
outpath <- "output/"

mini <- FALSE

# mini or full?
if (mini){
	  rootfilename <- "blb_lin_reg_mini"
		n<-10000
		numParameters<-41
} else {
	  rootfilename <- "blb_lin_reg_data"
		n<-1000000
		numParameters<-1001
}

#============================== Setup for running on Gauss... ==============================#

args <- commandArgs(TRUE)

cat("Command-line arguments:\n")
print(args)

####
# sim_start ==> Lowest possible dataset number
###

###################
sim_start <- 1000
###################

if (length(args)==0){
	sim_num <- sim_start + 1 
	set.seed(121231)
} else {
	# SLURM can use either 0- or 1-indexing...
	# Lets use 1-indexing here...
	sim_num <- sim_start + as.numeric(args[1])
	sim_seed <- (762*(sim_num-1) + 121231)
}

cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))


job=sim_num-sim_start

# ~~==| Preset Values |==~~
s=5
r=50
gamma=0.7
datafile=paste0(path, "/", rootfilename, ".txt")

#n = countLines(datafile) # Get total observations.
n.subset = floor(n^gamma)

dat<-attach.big.matrix(dget(paste0(path, "/blb_lin_reg_data.desc")), backingpath=path) # only for ull data

# ~~==| Preliminary Steps 1(a): Creating subset sample. |==~~
getSampleFrom<-function(datafile, n, n.subset, numParameters)
{
	# Now, sample n^gamma indexes (sorted, but not needed).												 # Remove sorting
	sampledRows = sort(sample(n, size=n.subset))

	#sample=as.matrix(readTable(datafile, rows=sampledRows, sep=",", colClasses="numeric",col.names=as.character(1:numParameters), comment.char="")) # http://www.biostat.jhsph.edu/~rpeng/docs/R-large-tables.html

	sample=dat[sampledRows,]
	# Read first sample line to obtain number of predictors, to preallocate a
	# matrix.
	# Note: skipping sampledRows[1]-1 observations since they are not sampled.
#	rr = scan(datafile, nlines=1, sep=",", skip=sampledRows[1]-1)
#	sample = matrix(NA, nrow=n.subset, ncol=length(rr))
#	sample[1,] = rr # We already read the first sample, so might as well store it

#	for(linesRead in 2:n.subset)
#	{
#		rr = scan(datafile, nlines=1, sep=",", skip=sampledRows[linesRead]-1)
#
#		sample[linesRead,] = rr
#	}

	invisible(sample)
} # end getSampleFrom()

# >>>
r_index=((job-1) %% r) + 1 #mod(job-1, r)+1
s_index=((job-1) %/% r) + 1#idivide(job-1, r)+1
set.seed(s_index) # subsample will be the same for each s_index
sample=getSampleFrom(datafile, n, n.subset, numParameters)

# ~~==| Bootstrap Sampling 1(b) |==~~

# Columns of bs contain how many times sample i appears in bootstrap j
set.seed(s_index*r_index) # bootstrap will be different for each s_index
bs = rmultinom(1, n, prob=rep(1/n.subset, n.subset))

y=sample[,ncol(sample)]
obs=sample[,-ncol(sample)]
bs.fit = lm(y ~ obs, weights=bs)$coefficients

outfile = paste0("output/", "coef_", sprintf("%02d",s_index),
								 "_", sprintf("%02d",r_index), ".txt")
write.table(bs.fit, file=outfile,
						row.names=FALSE, col.names=FALSE, quote=FALSE, sep=", ")


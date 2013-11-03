#install.packages("R.utils", repos="http://cran.us.r-project.org")
library(R.utils) # for countLines()

## Handle batch job arguments:
# 1-indexed version is used now.
args <- commandArgs(TRUE)

cat(paste0("Command-line arguments:\n"))
print(args)

####
# sim_start ==> Lowest simulation number to be analyzed by this particular batch job
###

#######################
sim_start <- 0
length.datasets <- 250 
#######################

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

job=sum_num

# ~~==| Preset Values |==~~
s=5
r=50
gamma=0.7
datafile="test.txt"

n = countLines(datafile) # Get total observations.
n.subset = floor(n^gamma)

# ~~==| Preliminary Steps 1(a): Creating subset sample. |==~~
getSampleFrom<-function(datafile, n, n.subset)
{
	# Now, sample n^gamma indexes (sorted, but not needed).												 # Remove sorting
	sampledRows = sort(sample(n, size=n.subset))

	# Read first sample line to obtain number of predictors, to preallocate a
	# matrix.
	# Note: skipping sampledRows[1]-1 observations since they are not sampled.
	rr = scan(datafile, nlines=1, sep=",", skip=sampledRows[1]-1)
	sample = matrix(NA, nrow=n.subset, ncol=length(rr))
	sample[1,] = rr # We already read the first sample, so might as well store it

	for(linesRead in 2:n.subset)
	{
		rr = scan(datafile, nlines=1, sep=",", skip=sampledRows[linesRead]-1)

		sample[linesRead,] = rr
	}

	invisible(sample)
} # end getSampleFrom()

# >>>
r_index=mod(job-1, r)+1
s_index=idivide(job-1, r)+1
set.seed(s_index) # subsample will be the same for each s_index
sample=getSampleFrom(datafile, n, n.subset)

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


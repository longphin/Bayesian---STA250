library(RCUDA)

	source("utility.R") # utility() is used to determine number of threads

	cat("Setting cuGetContext(TRUE)...\n")
cuGetContext(TRUE)
	cat("done. Profiling CUDA code...\n")

	cat("Loading module...\n")
	m = loadModule(system.file("tn.ptx", package = "RCUDA"))
	cat("done. Extracting kernel...\n")
	k = m$rtruncnormal_kernel
	cat("done. Setting up miscellaneous stuff...\n")
	N = 1e6L
	x = numeric(N) # A vector of 0's, length N
	cat("done. Setting mu and sigma...\n")
	mu = rep(0.3, N)
	sigma = rep(1.5, N)

bg<-computed_grid(N)
	grid_dims<-bg$grid_dims
	block_dims<-bg$block_dims
nthreads <- prod(grid_dims)*prod(block_dims)
	cat("Total number of threads to launch = ",nthreads,"\n")
	if (nthreads < N){
		stop("Grid is not large enough...!")
	}

cu_time <- system.time({
		mem = copyToDevice(x)
		.cuda(k, mem, N, mu, sigma, gridDim = grid_dims, blockDim = block_dims)
		cat("Copying result back from device...\n")
		cu_ret = copyFromDevice(obj=mem,nels=mem@nels,type="float")
		})


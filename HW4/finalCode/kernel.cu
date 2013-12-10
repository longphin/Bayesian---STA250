#include <stdio.h>
#include <stdlib.h>
#include <cuda.h>
#include <curand_kernel.h>
#include <math_constants.h>

extern "C"
{

__global__ void
rtruncnorm_kernel(float *x, int n,
		  float *mu, float *sigma,
	   	  float *a, float *b,
		  int len_mu, int len_sigma,
		  int len_a, int len_b,
		  int maxRejections,
		  int rng_c)
{
	int rng_a=1; // These can easily be made as argument, but I'm lazy.
	int rng_b=2;

	// Usual block/thread indexing...
	int myblock = blockIdx.x + blockIdx.y * gridDim.x;
	int blocksize = blockDim.x * blockDim.y * blockDim.z;
	int subthread = threadIdx.z*(blockDim.x * blockDim.y) + threadIdx.y*blockDim.x + threadIdx.x;
	int idx = myblock * blocksize + subthread;

	// Determine indexes for vectors if using recycling.
	// Note: A good programmer would avoid the code repitions.
	int ind_mu = ((len_mu<2) ? 0 : (idx % len_mu));
	int ind_sigma = ((len_sigma<2) ? 0 : (idx % len_sigma));
	int ind_a = ((len_a<2) ? 0 : (idx % len_a));
	int ind_b = ((len_b<2) ? 0 : (idx % len_b));

	if (idx < n){ 
		// Set up RNG
		curandState rng;
		curand_init(rng_a+idx*rng_b, rng_c, 0, &rng);

		// Sample truncated normal, doing rejection-sampling
		for(int i=0; i<maxRejections; i++)
		{
			float samp=mu[ind_mu]+sigma[ind_sigma]*curand_normal(&rng);
			if(a[ind_a]<=samp && samp<=b[ind_b])
			{
				x[idx]=samp;
				return;
			}
		}
		// Could not sample using rejection-sampling.
		// Simply sample from Uniform(a,b).
		x[idx]=curand_uniform(&rng)*(a[ind_a]-b[ind_b])+a[ind_a];
	}
	return;
}

} // END extern "C"


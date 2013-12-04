#include <stdio.h>
#include <stdlib.h>
#include <cuda.h>
#include <curand_kernel.h>
#include <math_constants.h>

extern "C"
{

	__global__ void
		rtruncnorm_kernel(float *vals, int n,
				float *mu, float *sigma,
				float *lo, float *hi,
				int mu_len, int sigma_len,
				int lo_len, int hi_len,
				int maxRejections,
				int rng_a,
				int rng_b,
				int rng_c)
		{
			// Usual block/thread indexing...
			int myblock = blockIdx.x + blockIdx.y * gridDim.x;
			int blocksize = blockDim.x * blockDim.y * blockDim.z;
			int subthread = threadIdx.z*(blockDim.x * blockDim.y) + threadIdx.y*blockDim.x + threadIdx.x;
			int idx = myblock * blocksize + subthread;
			if (idx < N){ 
				// Set up RNG
				curandState rng;
				curand_init(rng_a+idx*rng_b, rng_c, 0, &rng);

				// Sample truncated normal, doing rejection-sampling
				accepted=0 // 0 indicates no acceptance yet
					for(int i=0; i<maxRejections; i++)
					{
						float samp=mu[idx]+sigma[idx]*curand_normal(&rng);
						if(a[idx]<=samp && samp<=b[idx])
						{
							x[idx]=samp;
							break;
						}else{
							if(i==maxRejections-1)
							{
								// Could not sample using rejection-sampling. Let's cheat.
								x[idx]=curand_uniform(&rng)*(a[idx]-b[idx])+a[idx]; // sample x~Uniform(a,b) 
							}
						}
					}

				return;
			}

		} // END extern "C"


library(coda) # for mcmc object

# % Sample new theta using normal distribution centered around each element
propose<-function(theta.prop, v)
{
	# Note for non-R users: "invisible" can be used like a "return" statement if
	# it's the last line of the function
	invisible(rnorm(length(theta.prop), mean=theta.prop, sd=v))
}

# % Calculate posterior
posterior<-function(theta.post, m, y, mu=0, sigma=1)
{
	# Calculate logit^-1. The if statements are error checks
	# (e.g. R gives exp(*)=NA if it is too large)
	try(z<-exp(X%*%theta.post)) # returns error if something bad happened
	if(exists("z")==FALSE || class(z)=="try-error" || is.infinite(z) || is.na(z))
		return(NA)

	ex=z/(1+z) # the logit^-1
	if(exists("ex")==FALSE || class(ex)=="try-error" || is.infinite(ex) || is.na(ex))
		return(NA)

	# and take the log of the posterior
	invisible(sum(log(ex^y * (1-ex)^(m-y)))+sum(log(dnorm(theta.post, mu, sigma))))
	# Below is proportionally the same, but avoid having to fully calculate dbinom.
	# In particular, the factorials, so the above is slightly faster.
	#	return(sum(log(dbinom(y, m, ex)))+sum(log(dnorm(theta.post, mu, sigma))))
}

# % Calculate proposal distribution (a normal distr. in this case)
proposal<-function(theta1, theta2)
{
	invisible(dnorm(theta2, mean=theta1, sd=v))
}

# % Only useful for weird reasons.
thetat<-function(theta, j)
{
	temp=theta[1,]
	temp[j]=theta[2,j]
	invisible(temp)
}

# % The meaty stuff.
"bayes.logreg" <- function(m, y, X, beta.0, Sigma.0.inv,
			niter=30000, burnin=10000,
			print.every=500, retune=500,
			verbose=TRUE)
{
	# --==| Initialization |==--
	p=length(beta.0)
	v=sqrt(diag(Sigma.0.inv)) # vector of standard deviation for parameter j

	# initialize matrix of sample at iteration i for parameter j
	theta=matrix(NA, ncol=p, nrow=niter+burnin)
	theta[1,]=beta.0

	# initialize matrix of acceptance rate at iteration i for parameter j
	acceptance=matrix(NA, ncol=p, nrow=niter+burnin)
	acceptance[1,]=rep(.5,length(beta.0))
	printIt=print.every # iteration counter; prints when == 0
	retuneIt=retune # iteration counter; retunes when == 0


	# --==| Start Metropolis Hastings |==--
	for(i in 2:(burnin+niter))
	{
		posteriorBeta = posterior(theta[i-1,], m, y)
		theta[i,] = theta.star = propose(theta[i-1,], v)

		for(j in 1:p)
		{
			# Determine acceptance
			# Note: Since the proposal is symmetric, I don't bother calculating it,
			#				so it is commented out.
			alpha=min(log(1), posterior(thetat(theta[(i-1):i,],j), m, y) - posteriorBeta) # * proposal(theta.star, theta[[i]][j]) / proposal(theta[[i]][j], theta.star))
			if(is.na(alpha)) alpha<-log(1) # never accept if there was an error
			acceptance[i,j]=exp(alpha) # alpha is in log scale, so acceptance is exp(alpha)
			if(log(runif(1)) < alpha)
			{
				theta[i,j] = theta.star[j] # accepted!
				if(verbose==TRUE)
					print(sprintf("[%i, %i, %.2f] accepted with probability %.2f",
												 i, j, theta.star[j], acceptance[i,j]))
			}else{
				theta[i,j] = theta[i-1,j] # rejected!
			}
		}


		# --==| Retuning if still in the burnin period |==--
		if(i<=burnin && retuneIt==0)
		{
			beta.0=sapply(1:p, function(k) mean(theta[(i-retune+1):i,k]))
			for(k in 1:p)
			{
				avgAcceptance = mean(acceptance[i-retune+1, k])
				# variances are adjusted based on how far the acceptance rate is,
				# targeting the [.3, .5] range. The adjustments are randomized a little.
				if(avgAcceptance<.1) v[k]=v[k]*runif(1, min=.1, max=.2)
					else
					if(avgAcceptance<.3) v[k]=v[k]*runif(1, min=.4, max=.6)
						else
						if(avgAcceptance>.8) v[k]=v[k]*runif(1, 1.7, 1.8)
							else
							if(avgAcceptance>.5) v[k]=v[k]*runif(1, 1.3, 1.6)

			}
			retuneIt=retune
		}


		# --==| Printing stuff |==--
		printIt=printIt-1
		retuneIt=retuneIt-1
		if(printIt==0)
		{
			print(sprintf("[estimate] = %s",
				paste(round(sapply(1:p, function(k) mean(theta[1:i,k])), 2),
							collapse=", ")))
			print(sprintf("[variance] = %s", paste(round(v,2), collapse=", ")))
			print(sprintf("[acceptRate] = %s",
				paste(round(sapply(1:p, function(k) mean(acceptance[(i-retune+1):i, k])), 2),
							collapse=", ")))
			printIt=print.every
		}
	}

	invisible(list(burned=theta[1:burnin,],
								thetas=mcmc(theta[(burnin+1):nrow(theta),]),
								acceptance=acceptance))
}


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

library(coda)

propose<-function(theta.prop, v)
{
	return(rnorm(length(theta.prop), mean=theta.prop, sd=v))
}

posterior<-function(theta.post, m, y, mu=0,sig=1)#rep(0, length(theta.post)), sig=diag(2))
{
	z=exp(X%*%theta.post)
	ex=z/(1+z)
	return(sum(log(ex^y * (1-ex)^(m-y)))+sum(log(dnorm(theta.post, mu, sig))))
# below is proportionally the same, but avoid having to fully calculate dbinom. In particular, the factorials, so the above is slightly faster.
#	return(sum(log(dbinom(y, m, ex)))+sum(log(dnorm(theta.post, mu, sig))))
}

proposal<-function(theta1, theta2)
{
	return(dnorm(theta2, mean=theta1, sd=v))
}

thetat<-function(theta, j)
{
	temp=theta[1,]
	temp[j]=theta[2,j]
	return(temp)
}

"bayes.logreg" <- function(m, y, X, beta.0, Sigma.0.inv,
			niter=100000, burnin=5000,
			print.every=500, retune=500,
			verbose=TRUE)
{
	# initialization
	beta.0=c(0,0) # temporary initial value
	v=c(1,1) # temporary initial value

	p=length(beta.0)
	theta=matrix(NA, ncol=p, nrow=niter+burnin)
	theta[1,]=beta.0
	acceptance=matrix(NA, ncol=p, nrow=niter+burnin)
	acceptance[1,]=rep(.5,length(beta.0))
	printIt=print.every
	retuneIt=retune

	# Start Metropolis Hastings
	for(i in 2:(burnin+niter))
	{
		posteriorBeta = posterior(theta[i-1,], m, y)
		theta[i,] = theta.star = propose(theta[i-1,], v)

		for(j in 1:p)
		{
			# acceptance
			alpha=min(log(1), posterior(thetat(theta[(i-1):i,],j), m, y) - posteriorBeta) # * proposal(theta.star, theta[[i]][j]) / proposal(theta[[i]][j], theta.star)) # proposal cancels for symmetric proposal
			acceptance[i,j]=exp(alpha)
			if(log(runif(1)) < alpha)
			{
#				print(sprintf("[%i, %i, %.2f] accepted with probability %.2f", i, j, theta.star[j], acceptance[i,j]))
				theta[i,j] = theta.star[j]
			}else{
				theta[i,j] = theta[i-1,j]
			}
		}

		# extra stuff (i.e. printing)
		printIt=printIt-1
		retuneIt=retuneIt-1
		if(printIt==0)
		{
			print(sprintf("[estimate] = %s", paste(round(sapply(1:p, function(k) mean(theta[1:i,k])), 2), collapse=", ")))
			print(sprintf("[variance] = %s", paste(round(v,2), collapse=", ")))
			print(sprintf("[acceptRate] = %s", paste(round(sapply(1:p, function(k) mean(acceptance[(i-retune+1):i, k])), 2), collapse=", ")))
#			print(sprintf("avg. [est., variance, acceptRate] = [%.2f, %.2f, %.2f, %.2f, %s]", mean(theta[1:i,1]), mean(theta[1:i, 2]), mean(acceptance[(i-retune+1):i, 1]), mean(acceptance[(i-retune+1):i, 2]), paste(round(v, digits=2), collapse=", ")))#v[1], v[2]))
			printIt=print.every
		}
		if(i<=burnin && retuneIt==0)
		{
			beta.0=sapply(1:p, function(k) mean(theta[(i-retune+1):i,k]))
			for(k in 1:p)
			{
				avgAcceptance = mean(acceptance[i-retune+1, k])
				if(avgAcceptance<.1) v[k]=v[k]*runif(1, min=.1, max=.2)
					else
					if(avgAcceptance<.3) v[k]=v[k]*runif(1, min=.4, max=.6)
						else
						if(avgAcceptance>.6) v[k]=v[k]*runif(1, 1.3, 1.6)
							else
							if(avgAcceptance>.8) v[k]=v[k]*runif(1, 1.7, 1.8)

			}
			#v=sapply(1:p, function(k) exp(.10*(mean(acceptance[i-retune+1,k])-.2)))
			retuneIt=retune
		}

	}

	invisible(list(burned=theta[1:burnin,], thetas=mcmc(theta[(burnin+1):nrow(theta),]), acceptance=acceptance)) # function returns all theta(t) and acceptance rate
}


#!/usr/bin/Rscript

k=200 # number of simulations
n=1000 # number of process iterations per simulation

# preallocation
results=matrix(data=NA, n, k)
y0=0
rho=rep(.9,n) # a vector is used rather than a constant, just to keep things more general
y=numeric(n)

for(simulation in 1:k)
{
	epsilon=rnorm(n)
	y[1]=y0*rho[1]+epsilon[1]

		for(t in 2:n){
			y[t]=rho[t]*y[t-1]+epsilon[t]
		}

	results[,simulation]=y # store the results in a matrix
}

jpeg('plot_q6_1.jpg')
plot(1:k, sort(colMeans(results)), ylab="mean", xlab="Index", main="Mean across all times") # part c)
dev.off()

jpeg('plot_q6_2.jpg')
plot(1:k, sort(apply(results, 2, var)), ylab="variance", xlab="Index", main="Var across all times") # part d)
dev.off()

jpeg('plot_q6_3.jpg')
plot(1:k, sort(colMeans(results[,1:200])), ylab="mean", xlab="Index", main="Mean across 200 times") # part e)
dev.off()

jpeg('plot_q6_4.jpg')
plot(1:k, sort(apply(results[,1:200], 2, var)), ylab="variance", xlab="Index", main="Var across 200 times") # part f)
dev.off()


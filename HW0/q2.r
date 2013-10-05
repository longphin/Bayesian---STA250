#!/usr/bin/Rscript

x=runif(1000, 0, 2*pi)
y=runif(1000, 0, 1)
u=y*cos(x)
v=y*sin(x)

plot(u,v)

# answer to what is distribution of r=sqrt(u^2+v^2):
# r = sqrt(u^2+v^2) = y*sqrt(cos(x)^2+sin(x)^2) = y ~ Uniform(0,1)
# => r ~ Uniform(0,1)


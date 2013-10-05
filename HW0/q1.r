#!/usr/bin/Rscript

for(i in 1:100)
{
	a=(i%%3==0)
	b=(i%%5==0)
	if(a && b)
	{
		cat("FizzBuzz\n")
	}else{
		if(a)
		{
			cat("Fizz\n")
		}else{
			if(b)
			{
				cat("Buzz\n")
			}else{
				cat(i)
				cat('\n')
			}
		}
	}
}


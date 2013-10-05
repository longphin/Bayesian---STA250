#!/usr/bin/Rscript

str="Hello, my name is Bob. I am a statistician. I like statistics very much."

split=unlist(strsplit(str, '')) # divide string into single characters

# PART a)
files=NULL # a listing of the files -- to be combined in part b)
for(i in 1:length(split))
{
	if(i<10) num=paste("0", i, sep='') else num=as.character(i) # index number for file name
	filename=paste("out_", num, ".txt", sep='')
	files=c(files, filename) # keep track of files

  write(split[i], file=filename)
}

# PART b)
ultimatePhrase=NULL
for(i in 1:length(files))
{
	ultimatePhrase=paste0(ultimatePhrase, paste0(readLines(files[i]))) # read entire file and put it together
}
write(ultimatePhrase, file="master.txt")


SE=read.table("final/blb_lin_reg_data_s5_r50_SE.txt", header=TRUE)[,1]

jpeg("scatterplot.jpg")
plot(1:length(SE), SE, main="Scatterplot of SE's", xlab="index", ylab="SE")
dev.off()


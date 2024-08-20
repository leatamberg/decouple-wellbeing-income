library("stringr")

p_values <- function(model, vcov){
  (1-pnorm(abs(summary(model)$coefficients[,"Estimate"]/sqrt(diag(vcov)))))*2
}

# https://stackoverflow.com/questions/26684072/add-columns-to-an-empty-data-frame-in-r
cbind.all <- function (...) 
{
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function(x) rbind(x, matrix(, n - 
        nrow(x), ncol(x)))))
}

df1=data.frame()
df2=data.frame()
for (p in 1:11*5) {
	rm(model,vcov1,vcov2,data1,data2)
	gc(reset=TRUE)

	print(paste0("Processing results.H1c_f/model_level",str_pad(p,width=2,pad="0"),".rds"))

	model<-readRDS(paste0("results.H1c_f/model_level",str_pad(p,width=2,pad="0"),".rds"))
	vcov1<-readRDS(paste0("results.H1c_f/vcov_CR1_level",str_pad(p,width=2,pad="0"),".rds"))
	vcov2<-readRDS(paste0("results.H1c_f/vcov_CR2_level",str_pad(p,width=2,pad="0"),".rds"))

	data1<-as.data.frame(p_values(model,vcov1))
	data2<-as.data.frame(p_values(model,vcov2))
	colnames(data1) <- paste0("p=",str_pad(p,width=2,pad="0"))
	colnames(data2) <- paste0("p=",str_pad(p,width=2,pad="0"))

	df1<-cbind.all(df1,data1)
	df2<-cbind.all(df2,data2)
}

write.csv(df1,"p_values/p_values_CR1.csv")
write.csv(df2,"p_values/p_values_CR2.csv")

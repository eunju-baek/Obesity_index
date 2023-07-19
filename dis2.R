
library(data.table)
library(dplyr)
library(stringr)


save1 = str_split(ICD10,'/')[[1]][10]
print(save1)
save2 = str_split(save1,'_')[[1]][3]
print(save2)


pheno <- fread("../01_BMI_ENV/pheno_PGS000027.txt")
icd10 <- read.table(ICD10,header=F)
icd10$case <- 1
names(icd10) <- c("eid",name)


pheno <- left_join(pheno,icd10)
pheno[is.na(pheno$name),]$name <- 0
pheno[is.na(pheno)] <- 0

#print(table(pheno$SAVE))

f <- pheno[ pheno$X31.0.0==0, ]
m <- pheno[ pheno$X31.0.0==1, ]

fper10 <- f[ f$per10 != 'none',]
mper10 <- m[ m$per10 != 'none',]


fmat <- matrix(ncol=4,nrow=4)
for1 <- paste(name,"~ X21001.0.0 + X21022.0.0") %>% as.formula()
for2 <- paste(name," ~ st_score + X21022.0.0+X22001.0.0+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+array") %>% as.formula()

for3 <- paste(name ,"~ ob + X21022.0.0") %>% as.formula()
for4 <- paste(name ,"~ per10 + X21022.0.0") %>% as.formula()

print(1)
f1 <- summary(glm(for1,data=f,family = binomial))$coefficients
print(2)
print(3)
f3 <- summary(glm(for3,data=f,family = binomial))$coefficients
print(4)
f4 <- summary(glm(for4,data=fper10,family = binomial))$coefficients

print(f1)

print('hi')
fmat[1,] <- f1["X21001.0.0",]
fmat[2,] <- f2["st_score",]
fmat[3,] <- f3["ob",]
fmat[4,] <- f4[2,1:4]
fmat[4,1] <- abs(fmat[4,1])
print(f1)
print(f2)
print(f3)
print(f4)
print(fmat)

fmat <- as.data.frame(fmat)
colnames(fmat) <- c("Beta","SE","Z","P")
rownames(fmat) <- c("DS_BMI","DS_BMIPGS","DS_OB","BMIPGS_HR")
fmat$OR <- exp(abs(fmat$Beta))
fmat <- fmat[,c("OR","Beta","SE","Z","P")]
print(fmat)

fwrite(fmat,paste("F_",save2,sep=""),quote=F,sep='\t',row.names=T,col.names=T)


#=====================================================================



mmat <- matrix(ncol=4,nrow=4)


print(1)
m1 <- summary(glm(for1,data=m,family=binomial))$coefficients
print(2)
m2 <- summary(glm(for2,data=m,family = binomial))$coefficients
print(3)
m3 <- summary(glm(for3,data=m,family=binomial))$coefficients
print(4)

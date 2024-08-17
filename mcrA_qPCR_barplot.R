library(readxl)
library(ggplot2)

copy_numbers <- read_excel("240306_mcrA qPCR data by copy number_HB.xlsx",
                           range = "A8:B14", col_names = FALSE)
paras <- copy_numbers$...2
names(paras) <- copy_numbers$...1
dilution <- c(1e-1,1e-10,1e-2,1e-3,1e-4,1e-5,1e-6,1e-7,1e-8,1e-9)

#1
qPCR <- read_excel("qPCR.xlsx",sheet = "Sheet2")
#2
qPCR <- read_excel("qPCR_0626.xlsx")

#set up standard data
qPCR_std <- qPCR[qPCR$`Sample Type`=='Standard',c(3,5)]
qPCR_std$molecules <- paras["No. molecules (neat, molecules)"]*rep(dilution,each=3)
qPCR_std$log_mole <- log10(qPCR_std$molecules)
qPCR_std$Cq <- as.numeric(qPCR_std$Cq)
ggplot(qPCR_std,aes(x=log_mole,y=Cq))+
  geom_point(size=2,alpha=0.7,shape=20,col='gray30')

#1 remove the outlier and check again
qPCR_std <- qPCR_std[-30,]
ggplot(qPCR_std,aes(x=log_mole,y=Cq))+
  geom_point(size=2,alpha=0.7,shape=20,col='gray30')

qPCR_model <- lm(log_mole~Cq,data=qPCR_std)
summary(qPCR_model)
ggplot(qPCR_std,aes(x=log_mole,y=Cq))+
  geom_point(size=2,alpha=0.7,shape=20,col='gray30')+
  geom_smooth(method = 'lm')

qPCR_sample <- qPCR[qPCR$`Sample Type`=='Unknown',c(3,5)]
qPCR_sample$Cq <- as.numeric(qPCR_sample$Cq)
qPCR_sample$log_mole <- predict(qPCR_model,qPCR_sample)
qPCR_sample$molecules <- 10^qPCR_sample$log_mole

#1
concentration <- c(12.9,76.8,80.6,85.6,1.51)

#2
qPCR_sample$molecules[1:3] <- 0 #set NA as 0
concentration <- c(0.362,1.95,1,1.64,1.52,3.44)

mean_mole <- aggregate(molecules ~ `Sample Name`, data = qPCR_sample, FUN = mean)
min_mole <- aggregate(molecules ~ `Sample Name`, data = qPCR_sample, FUN = min)
max_mole <- aggregate(molecules ~ `Sample Name`, data = qPCR_sample, FUN = max)

data_mole <- data.frame(Group=mean_mole$`Sample Name`,
                         Mean=mean_mole$molecules/concentration,
                         Min=min_mole$molecules/concentration,
                         Max=max_mole$molecules/concentration)

data <- rbind(data_mole,data_mole2)
data$Sample <- c("BB DNA","BP01 DNA","BP02 DNA","BP03 DNA","LB DNA","BP01 RNA","BP01 cDNA","BP02 RNA","BP02 cDNA","BP03 RNA","BP03 cDNA" )
data$Group <- c("BB", "BP01","BP02","BP03","LB",rep(c("BP01","BP02","BP03"),each=2))

ggplot(data,aes(x=Sample,y=Mean,fill=Group))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=Min,ymax=Max),width=0.2,color='darkblue')+
  labs(title = "mcrA Reverse Transcription qPCR",y="copy numbers")

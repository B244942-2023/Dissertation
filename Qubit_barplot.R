library(readxl)
library(ggplot2)
qubit <- read_excel("qubit.xlsx")

DNA <- qubit[c(27,30,33,42,43),]
RNA <- qubit[-c(27,30,33,42,43),]
cleanup <- RNA[c(3,4,13:38),]

ggplot(cleanup, aes(x=sample,y=concentration,fill=type))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette = "Set1")+
  labs(title = "Qubit")

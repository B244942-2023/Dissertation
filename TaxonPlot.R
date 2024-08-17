library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(cowplot)
library(pheatmap)
library(RColorBrewer)
library(ggalluvial)
library(reshape2)

nano <- read_table("5_normalized.tsv", comment = "#")
nano <- nano[,-1]
illu <- read_table("sediment-analysis/sediment-analysis/sediment-seq-filtered-table-rel-5.tsv",
                   comment = "#")
illu <- illu[,-1]
meta <- read_delim("BP_kaiju/BP_Family.tsv", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE)
meta <- meta[1:760,c(2,5)]
meta[,1] <- meta[,1]/100
colnames(meta) <- c("ISED_meta","taxa")

dat <- full_join(nano,illu,by="taxa")
dat <- full_join(dat,meta,by="taxa")
dat[is.na(dat)] <- 0
taxa <- dat$taxa
rownames(dat) <- taxa
dat <- dat[,-c(6:9)]
dat <- dat[rowSums(dat) > 0,]

write.table(dat, file = paste( "relative.table", sep = "." ), quote = F, sep = "\t",row.names = F)

#sort & Top N
dat <- dat[,c(1:4,9:13)]
dat <- data.frame( dat, total = apply(dat, 1, sum),taxa=taxa)
datsort <-arrange(dat , desc(total))
datTop <- datsort
N <- 20
if (nrow(dat) > N) {
    datTop1 <- datsort[c(1:N),]
    Othersum <- apply(datsort[-c(1:N), ], 2, sum)
    datTop <- rbind(datTop1 ,  Others = Othersum)
}
datTop <- subset(datTop, select = -total)

#Normalize
datNorm <- apply(datTop, 2, function(x){x/sum(x)})
datNorm <- rownames_to_column(as.data.frame(t(datNorm)), var = "Taxa")
tax <- colnames(datNorm)
tax <- tax[-1]

#alluvial plot
datBar <- gather(datNorm, key="sample", value="count", -Taxa)
Colors <- c()
for ( color in c("Set1", "Set2", "Set3","Paired", "Dark2")) {
   Colors <- c( Colors, brewer.pal(8, color))
}

datBar <- mutate(datBar, sample = factor(rep(tax,each=9), levels = tax))
p<- ggplot(datBar, aes(x = Taxa, y = count, alluvium = sample ,stratum = sample)) +
  geom_bar(stat = 'identity',aes(fill = sample),position = 'fill', width = 0.6) +
  geom_alluvium(aes(fill = sample), alpha = .5) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = Colors[1:ncol(datNorm)]) +
  theme_half_open()+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 60,vjust = 0.97,hjust = 0.95))+
  labs(x = 'Group',y = 'Sequence Percent',fill = 'Family')


#mock community analysis
mc <- dat[,c(6:9)]
mc$total <- apply(mc[,-4],1,sum)
mc <- mc[(mc$total!=0),-5]
rownames(mc) <- mc$taxa
mc_long <- melt(mc, varnames = c("taxa","Group"), value.name = "Proportion")
ggplot(mc_long, aes(x=Group, y = Proportion, fill = taxa)) +
  geom_bar(stat = "identity") +
  labs(title = "Mock Community", y = "Percentage", fill = "Taxonomy") +
  scale_fill_manual(values = Colors) +
  theme_minimal()


library(phyloseq)
library(vegan)

library(RColorBrewer)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(tibble)
library(ggpubr)
library(patchwork)

otu_table_file <- read_delim("5_normalized.tsv",
                             delim = "\t", escape_double = FALSE,
                             comment = "#", trim_ws = TRUE)
otu_table_file <- read_delim("sediment-analysis/sediment-analysis/sediment-seq-filtered-table-rel-5.tsv", 
                  delim = "\t", escape_double = FALSE, 
                  comment = "#", trim_ws = TRUE)
otu_table_data <- otu_table_file[,-1]
rownames(otu_table_data) <- otu_table_file$`OTU ID`
otu_table_data <- round(otu_table_data*10000)

otu_table_obj <- otu_table(as.matrix(otu_table_data), taxa_are_rows = T)
phyloseq_obj <- phyloseq(otu_table_obj)

Colors <- c()
for ( color in c("Accent","Pastel1","Pastel2","Set1", "Set2", "Set3","Dark2","Paired")) {
  Colors <- c( Colors, brewer.pal(8, color))
}

options(scipen=200)

#####  Alpha Diversity
 datAlpha <- estimate_richness(phyloseq_obj, measures = c("Observed", "Shannon", "Simpson"))
 datAlpha$Pielou <- datAlpha$Shannon/datAlpha$Observed
 datAlpha <- datAlpha[1:5,]
 datAlpha$group <- c("BB",rep("BP",3),"LB","MC","MC","N","BB","BP","BP","LB")
 datAlpha <- datAlpha[-c(5,8),]

 write.table(datAlpha, 
            file=paste(outpre, "alpha-diversity.table" , sep="."), 
            sep = "\t", row.names = T, quote = F)

alpha_ob <- ggplot(datAlpha,aes(group,Observed))+
  stat_boxplot(geom = "errorbar",width=0.3)+
  geom_boxplot(width=0.6,fill=Colors[10:13])+
  xlab("group")+ylab("Observed Species")+
  theme_bw()+
  theme(axis.title = element_text(size = 9))
alpha_sh <- ggplot(datAlpha,aes(group,Shannon))+
  stat_boxplot(geom = "errorbar",width=0.3)+
  geom_boxplot(width=0.6,fill=Colors[10:13])+
  xlab("group")+ylab("Shannon-Wiener")+
  theme_bw()+
  theme(axis.title = element_text(size = 9))
alpha_si <- ggplot(datAlpha,aes(group,Simpson))+
  stat_boxplot(geom = "errorbar",width=0.3)+
  geom_boxplot(width=0.6,fill=Colors[10:13])+
  xlab("group")+ylab("Simpson index")+
  theme_bw()+
  theme(axis.title = element_text(size = 9))
alpha_pi <- ggplot(datAlpha,aes(group,Pielou))+
  stat_boxplot(geom = "errorbar",width=0.3)+
  geom_boxplot(width=0.6,fill=Colors[10:13])+
  xlab("group")+ylab("Pielou's Evenness")+
  theme_bw()+
  theme(axis.title = element_text(size = 9))

plot_grid(alpha_ob,alpha_sh,alpha_si,alpha_pi,
          labels = c("A","B","C","D"),
          label_size = 10,
          ncol=2,nrow=2)


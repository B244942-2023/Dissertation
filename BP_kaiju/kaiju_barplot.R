library("ggplot2")
library("ggsci")

BPF <- read.delim("E:/CFQ/UoE/MSc Project/BP_kaiju/BP_Family.tsv", 
                  stringsAsFactors=TRUE)
BPG <- read.delim("E:/CFQ/UoE/MSc Project/BP_kaiju/BP_Genus.tsv", 
                  stringsAsFactors=TRUE)

BPF1 <- BPF[1:760,c(2,5)]
BPG1 <- BPG[1:4061,c(2,5)]
BPF1$new_per <- BPF1$percent/sum(BPF1$percent)*100
BPG1$new_per <- BPG1$percent/sum(BPG1$percent)*100

BPF2 <- BPF1[BPF1$new_per>1,-1]
BPG2 <- BPG1[BPG1$new_per>1,-1]
levels(BPF2$taxon_name) <- c(levels(BPF2$taxon_name),"others")
levels(BPG2$taxon_name) <- c(levels(BPG2$taxon_name),"others")
BPF2[nrow(BPF2)+1,1] <- as.factor("others")
BPF2[nrow(BPF2),2] <- sum(BPF1$new_per[BPF1$new_per<1])
BPG2[nrow(BPG2)+1,1] <- as.factor("others")
BPG2[nrow(BPG2),2] <- sum(BPG1$new_per[BPG1$new_per<1])

BP_taxa <- rbind(BPF2,BPG2)
BP_taxa$taxa_level <- c(rep("Family",nrow(BPF2)),rep("Genus",nrow(BPG2)))

colors <- c("#C1CDCD","#EEEE00","#FFA07A","#9370DB","#FF7F24",
            "#3CB371","#FFD700","#CD3333","#4169E1","#CD853F",
            "#228B22","#DAA520","#EEB4B4","#DDA0DD","#FFFFAE",
            "#76EEC6","#8B6914","#FF8247","#0000CD","#8B008B",
            "#90EE90","#CD3700","#EEC591")

ggplot(BPF2, aes(x="", y = new_per, fill = taxon_name)) +
  geom_bar(stat = "identity") +
  labs(title = "BP Metagenome kaiju classification", x = "Family level", y = "Percentage") +
  scale_fill_manual(values = colors) +
  theme_minimal()

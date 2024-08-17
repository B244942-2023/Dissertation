library(phyloseq)
library(vegan)
library(PCAtools)

library(RColorBrewer)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(tibble)

Colors <- c()
for ( color in c("Set1", "Set2", "Set3", "Dark2", "Paired")) {
  Colors <- c( Colors, brewer.pal(8, color))
}

options(scipen=200)

##### Beta Diversity

dat0 <- otu_table_file
dat1 <- t(dat0)
## bray curtis distance
datDist <- vegdist(dat1, method = "bray")
datDist <- as.matrix(datDist)
write.table(datDist, 
            file=paste(outpre, "dist-braycurtis.table" , sep=".") , 
            sep = "\t", row.names = T, quote = F)

## adonis
dat1T <- as.data.frame(t(otu_table(dat0,taxa_are_rows = T)))
cond <- c("BB",rep("BP",3),"MC","MC","BB","BP","BP","LB")

adonis_out <- adonis2(dat1T ~ cond, permutations = 999, method="bray")
adonis_out <- adonis_out[1,]

## PCoA analysis
PCoA <- cmdscale(datDist, eig = TRUE, k = 2)
pcoa_points <- as.data.frame(PCoA$points)
colnames(pcoa_points) <- c("PC1", "PC2")
pcoa_points$group <- rownames(pcoa_points)

ggplot(pcoa_points, aes(x = PC1, y = PC2,
                        color = group)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "longdash", 
             linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "longdash", 
             linewidth = 0.4) + 
  scale_color_manual(values = Colors[7:20] )+
  labs(x =  paste("PC1"," (", round(PCoA$eig[1] / sum(PCoA$eig) * 100, 2), "%",")", sep = "") ,
       y =  paste("PC2"," (", round(PCoA$eig[2] / sum(PCoA$eig) * 100, 2), "%",")", sep = "") ,
       color = NULL,
       title = paste("adonis R^2:",round(adonis_out$R2,2), "  P-value:", adonis_out$`Pr(>F)`))+
  theme_minimal()

write.table(PCoA , 
            file=paste(outpre, "PCoA.table" , sep="."), 
            sep = "\t", 
            row.names = T, quote = F)

library(pheatmap)
library(tidyverse)
sediment <- read.delim("sediment-analysis/sediment-analysis/sediment-seq-filtered-table-rel-5.tsv", 
                                                row.names=1, comment.char="#")
rn <- sub(".*f__", "", rownames(sediment))
unc <- which(rn=="uncultured")
sediment <- sediment[-unc,]
rownames(sediment) <- sub(".*f__", "", rownames(sediment))
filtered_sediment <- sediment[rowSums(sediment != 0) > 1, ]

sed1 <- dat[rowSums(dat!=0)>1,]
rownames(sed1) <- t1
sed1f <- sed1 %>%
  filter(rowSums(sed1 != 0) > 1)
sed2 <- sed1f[,c(1,6,5,9,2:4,7:8,10)]
rownames(sed2) <- rownames(sed1f)

pheatmap(sed2, 
         scale="row",         
         clustering_distance_rows="euclidean", 
         cluster_cols = F,
         clustering_method="complete",
         color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
         fontsize_row = 4,
         angle_col = 45)

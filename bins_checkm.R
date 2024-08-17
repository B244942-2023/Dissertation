library(ggplot2)
library(ggrepel)

bins <- read.table('bins_checkm.txt', sep='\t', header=TRUE)

ggplot(bins, aes(x=Completeness, y=Contamination)) +
  geom_point(size=3, aes(color=Bin.Id)) +
  geom_text(aes(label=ifelse((Completeness > 60 & Contamination < 20), Bin.Id, '')), 
            size=3, vjust=1, hjust=0.5) +
  geom_text(aes(label=ifelse((Completeness > 60 & Contamination < 20), Marker.Lineage, '')), 
            size=3, vjust=1, hjust=-0.35)+
  labs(title='CheckM Results: Completeness vs Contamination', x='Completeness (%)', y='Contamination (%)') +
  theme_minimal()+
  theme(legend.position="none")+
  scale_x_continuous(limits = c(0, 100),
                     breaks = seq(0,100,20))

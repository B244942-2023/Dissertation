library("ggplot2")
library("tidyverse")

kegg <- read_csv("pivoted_bin_data.csv")
kegg[is.na(kegg)] <- 0
kegg <- data.frame( kegg, max = apply(kegg[,3:7], 1, max))
ksort <-arrange(kegg , desc(max))
N <- 26
func <- ksort$name
ksort <- ksort[,3:8]
rownames(ksort) <- func

if (nrow(kegg) > N) {
  kTop1 <- ksort[c(1:N),]
  Othersum <- apply(ksort[-c(1:N), ], 2, sum)
  kTop <- rbind(kTop1 ,  Others = Othersum)
}
kTop <- subset(kTop, select = -max)

kNorm <- apply(kTop, 2, function(x){x/sum(x)})
kNorm1 <- rownames_to_column(as.data.frame(t(kNorm)), var = "bin")
funct <- rownames(kNorm)
kBar <- gather(kNorm1, key="functions", value="count", -bin)
kBar <- mutate(kBar, functions = factor(rep(funct,each=5), levels = funct))

ggplot(kBar, aes(x = bin, y = count, alluvium = functions ,stratum = functions)) +
  geom_bar(stat = 'identity',aes(fill = functions),position = 'fill',width=0.6) +
  geom_alluvium(aes(fill = functions), alpha = .5) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.5,1))+
  scale_fill_manual(values = Colors[1:ncol(kNorm1)]) +
  theme_half_open()+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 60,vjust = 0.97,hjust = 0.95))+
  labs(x = 'Bin',y = 'Functions Percent',fill = 'Function')

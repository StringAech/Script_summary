pkgs <- c("e1071",
          "parallel",
          "preprocessCore",
          "matrixStats", 
          "pheatmap", 
          "RColorBrewer", 
          "tidyverse", 
          "cowplot",
          "ggpubr",
          "bslib",
          "ggthemes",
          "dplyr",
          "tidyr","reshape2",
          "RColorBrewer")
lapply(pkgs, library, character.only = T)
source("E:/yuceguohao/code/CIBERSORT Rscript/CIBERSORT.R")#load Rscript and Custom Functions.
getwd()
#CIBERSORT DATA 
results=CIBERSORT("LM22.csv", "YTP0006_28case_log2exp.csv", 
                  perm=100, 
                  QN=TRUE,
                  filetype = "csv")
row.names(results) <- str_extract(row.names(results),"(?<=\\_)[^\\_]+?(?=\\_[^\\_]*$)")
write.csv(results,"YTP0006_28case_log2exp_CIBERSORT.csv")
#可视化结果
cibersort_raw <-results
dd1 <- cibersort_raw %>%
  as.data.frame() %>%
  rownames_to_column("sample") %>%
  pivot_longer(cols = 2:23,
               names_to = "CellType",
               values_to = "Composition")
#dd1$sample <- str_extract(dd1$sample,"(?<=\\_)[^\\_]+?(?=\\_[^\\_]*$)")

plot.info <- dd1[,c(5,1,6)]  
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
ggboxplot(
  plot.info,
  x = "CellType",
  y = "Composition",
  color = "black",
  fill = "CellType",
  xlab = "",
  ylab = "Cell composition",
  main = "TME Cell composition") +
  theme_base() +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1,
    vjust = 1
  ))  
ggbarplot(
  plot.info,
  x = "sample",
  y = "Composition",
  size = 0,
  fill = "CellType",
  #color = "CellType",
  
) +
  scale_fill_manual(values = col_vector)+
  theme_base() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 1,
      size = 1
    ),
    legend.position = "right"
  )
+scale_fill_manual(values = colors)
#对cibersort结果 分组注释 并进行可视化
data <- read.csv("YTP0006_28case_log2exp_CIBERSORT.csv",header = T,sep = ",",)
data <- P_value_filtration(a,pvalue_filtration = F)
dt_boxplot <- melt(data, id.vars=c("X","group" ), 
                   variable.name="CellType", value.name = "log2_Expression")
p <- ggplot(dt_boxplot,aes(x = CellType,
                           y=log2_Expression,
                           fill = group)) + 
  geom_boxplot(position = 'dodge',width = 0.5) +
  labs(fill = "CellType",x = "",y = "Infiltration") + 
  # ylim(0,max(dt_boxplot$log2_Expression) + 4.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,color = "black"),
        axis.ticks.x = element_blank(),legend.title= element_blank(),
        legend.position = "bottom")
p
ggsave(filename = "./CIBERSORT_group_boxplot.pdf",plot = p, width = 8, height = 7)

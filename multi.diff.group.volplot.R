source("E:/CRO项目/loading.package.R")
#### load packages ####
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(openxlsx)
library(ggsci)
library(ggrepel)
# Create color parameters
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#### read in data ####
deg <- read.csv("./Differentially_Expressed_Markers_Each_Cluster.csv", header = T)
deg$cluster <- as.factor(deg$cluster)
head(deg)
deg <- deg %>% dplyr::filter(p_val_adj < 0.05) %>% 
  dplyr::filter(abs(avg_log2FC) > 0.75) %>% 
  dplyr::select(avg_log2FC, p_val_adj, cluster, gene)  # filter and tidy the matrix
#### add label ####
deg <- deg %>% 
  mutate(label = ifelse(p_val_adj < 0.01, "adjusted P-val < 0.01", "adjusted P-val >= 0.01")) %>% 
  mutate(Change = ifelse(avg_log2FC > 0.75, "UP", "DOWN"))

bardata <- deg %>% dplyr::select(cluster, avg_log2FC ) %>% 
  group_by(cluster) %>% 
  summarise_all(list(tail = min, top = max))

tagedgene <- deg %>% group_by(cluster) %>% 
  slice_max(abs(avg_log2FC), n = 3)

ggplot(deg, aes(x = cluster, y = avg_log2FC ))+
  geom_col(data = bardata, mapping = aes(x = cluster, y = tail),
           fill = "grey", width = 0.8) +
  geom_col(data = bardata, mapping = aes(x = cluster, y = top),
           fill = "grey", width = 0.8) +
  geom_jitter(aes(color = label), size = 1,
              position = position_jitter(seed = 0328)) +
  scale_color_manual(values = c("#db5a6b", "black")) +
  geom_tile(aes(y = 0, fill = cluster), show.legend = F, 
            color = "black", width = 1) +
  scale_fill_manual(values = col_vector) +
  geom_text(aes(y = 0, label = cluster)) +
  geom_text_repel(data = deg %>% filter(gene %in% unique(tagedgene$gene)),
                  aes(label = gene), position = position_jitter(seed = 0328),
                  arrow = arrow(angle = 30, length = unit(0.05, "inches"),
                                ends = "last", type = "open")) +
  theme_minimal() +
  theme(axis.line.y = element_line(color = "black", linewidth = 1),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank())
   
  
  

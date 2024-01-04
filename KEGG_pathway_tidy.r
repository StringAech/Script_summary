library(rjson)
library(jsonlite)
library(tidyverse)
library(dplyr)
KO <- fromJSON("../../guoha/Desktop/hsa00001.json")#下载并解析JSON文件
KO$name <- NULL
KO <- as.data.frame(KO) %>% 
  unnest(cols = c("children.name","children.children"),names_repair = tidyr_legacy) %>%#重要函数
  unnest(cols = c("children.name","name","children"),names_repair = tidyr_legacy) %>%
  unnest(cols = c("children.name","name","name1","children"),names_repair = tidyr_legacy)
colnames(KO) <- c("L1","L2","L3","KO") 
KO %<>% #整理KEGG ORTHOLOGY
  select(last_col(),everything()) %>%
  separate(col = "KO",sep = ";",into = c("KO","Description")) %>%
  separate(col = "L1",sep = " ",into = c("L1_ID","L1"),extra = "merge") %>%
  filter(!L1_ID %in% c("09180","09190")) %>% #去除BRITE hierarchies和Not Included in Pathway or Brite两大类
  separate(col = "L2",sep = " ",into = c("L2_ID","L2"),extra = "merge") %>%
  separate(col = "L3",sep = " ",into = c("L3_ID","L3"),extra = "merge") %>%
  separate(col = "L3",sep = " \\[PATH:",into = c("L3","PathwayID")) %>%
  mutate(PathwayID=str_remove(PathwayID,pattern = "\\]")) %>%
  separate(col = "KO",sep = " ",into = c("GeneID","GeneSymobol")) %>% 
  drop_na()#KEGG ORTHOLOGY等级有缺失的删掉
head(KO)
# A tibble: 6 x 9
# KO     Description                                                L1_ID L1         L2_ID L2                      L3_ID L3                     PathwayID
# <chr>  <chr>                                                      <chr> <chr>      <chr> <chr>                   <chr> <chr>                  <chr>    
# 1 K00844 HK; hexokinase [EC:2.7.1.1]                                09100 Metabolism 09101 Carbohydrate metabolism 00010 Glycolysis / Gluconeo~ ko00010  
# 2 K12407 GCK; glucokinase [EC:2.7.1.2]                              09100 Metabolism 09101 Carbohydrate metabolism 00010 Glycolysis / Gluconeo~ ko00010  
# 3 K00845 glk; glucokinase [EC:2.7.1.2]                              09100 Metabolism 09101 Carbohydrate metabolism 00010 Glycolysis / Gluconeo~ ko00010  
# 4 K25026 glk; glucokinase [EC:2.7.1.2]                              09100 Metabolism 09101 Carbohydrate metabolism 00010 Glycolysis / Gluconeo~ ko00010  
# 5 K01810 GPI, pgi; glucose-6-phosphate isomerase [EC:5.3.1.9]       09100 Metabolism 09101 Carbohydrate metabolism 00010 Glycolysis / Gluconeo~ ko00010  
# 6 K06859 pgi1; glucose-6-phosphate isomerase, archaeal [EC:5.3.1.9] 09100 Metabolism 09101 Carbohydrate metabolism 00010 Glycolysis / Gluconeo~ ko00010  

## select lipld metabolism

head(KO)
lipid_pathway <- dplyr::filter(KO,L2 == "Lipid metabolism")
L3_pathway <- split(lipid_pathway,lipid_pathway$L3)
L3_pathway %<>% lapply(., function(.x){
  .x <- list(annotation_matrix = .x,genelist = .x$GeneSymbol)
  return(.x)
})
saveRDS(L3_pathway,file = "../../guoha/Desktop/lipid.rds")


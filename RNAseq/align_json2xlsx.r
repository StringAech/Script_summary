library(tidyr)
library(stringr)
library(rjson)
library(openxlsx)
setwd("C:/Users/guoha/Desktop/新建文件夹 (2)")
data_dir <- "C:/Users/guoha/Desktop/新建文件夹 (2)/qc/"
bad_sample <- c("GJS.rna.align.json","YYQ*")
sample.id <- list.files(data_dir) %>% grep(pattern = "rna.align.json",value = T) %>% .[-c(2,3,4,10,12)]
dir <- file.path(data_dir,sample.id)
# dir
# result <- fromJSON(file = dir[1]) %>% as.data.frame()
# result <- fromJSON(file = dir[1]) %>% as.data.frame()

result <- lapply(dir,function(x){
  a <- fromJSON(file = x) %>% as.data.frame()
  return(a)
}) %>% Reduce(rbind,.)
rownames(result) <- result$sample_id
write.xlsx(result,"./summary_qc_Normal_sample_results.xlsx")

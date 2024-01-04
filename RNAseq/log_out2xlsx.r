library(tidyr)
library(stringr)
library(openxlsx)


data_dir <- "C:/Users/guoha/Desktop/新建文件夹 (2)/qc/"
sample.id <- list.files(data_dir)
sample.id
sample.id <- list.files(data_dir) %>% grep(pattern = "rna.align.json")
library(tidyr)
library(stringr)
setwd("C:/Users/guoha/Desktop/新建文件夹 (2)")
data_dir <- "C:/Users/guoha/Desktop/新建文件夹 (2)/新建文件夹/"
sample.id <- list.files(data_dir) #%>% grep(pattern = "rna.align.json",value = T) #%>% .[c(2,3,4,10,12)]
sample.id
dir <- file.path(data_dir,sample.id)
dir
library(rjson)
result <- lapply(dir, function(x) {
  a <- fromJSON(file = x) %>% as.data.frame()
  return(a)
}) %>% Reduce(rbind, .)
rownames(result) <- result$sample_id
library(openxlsx)
write.xlsx(result, "./summary_qc_results.xlsx")
sample.id <-
  list.files(data_dir) %>% grep(pattern = "rna.align.json", value = T) %>% .[-c(2, 3, 4, 10, 12)]
dir <- file.path(data_dir, sample.id)
result <- lapply(dir, function(x) {
  a <- fromJSON(file = x) %>% as.data.frame()
  return(a)
}) %>% Reduce(rbind, .)
rownames(result) <- result$sample_id
write.xlsx(result, "./summary_qc_Normal_sample_results.xlsx")
# read .out file

dir <- file.path(data_dir,sample.id)
results <- lapply(dir, function(.x){
  a <- read.csv(.x,sep = "|",row.names = 1) %>% t()
  rownames(a) <- .x %>% str_extract(.,pattern = "(?<=\\/\\/).*?(?=.Log)")
  return(a)
}) %>% Reduce(rbind,.) %>% as.data.frame()
colnames(results) <- colnames(results) %>% gsub(.,pattern = "\\s{2,}",replacement = " ")
write.xlsx(results,file = "summary_log_out_file.xlsx",rowNames =T)

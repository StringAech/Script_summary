library(openxlsx)
library(OlinkAnalyze)
library(dplyr)
library(stringr)
library(stringi)
tidy.olink.rawdata<- function(sample.table){
  a <- paste0(rownames(sample.table),"1")
  path <- cbind(a,sample.table$`1`)
  for (i in 2:ncol(sample.table)) {
    a <- paste0(rownames(sample.table),i)
    b <- cbind(a,sample.table[,i])
    path <- rbind(path,b)}
  return(path)
}

npx_readin <- function(path=NPX.path,wide=TRUE){
## read in NPX resutls 
## path:: NPX file path
## wide:: results file format is long or wide
  ### long: wide=F
  ### wide: wide=T(default)
  if(wide){NPX <- read.xlsx(NPX.path) %>% .[(which(.[,1]=="OlinkID")+1):(which(.[,1]=="LOD")-1),]}else{NPX <- read.xlsx(NPX.path,
                                                                                                                        skipEmptyRows = F,
                                                                                                                        colNames = F)}
}
sample.table.path <- "./results/20230601-YuKM-R-20-001 1.0 Olink Signature Q100 血浆血清样本 Target 96多重蛋白组学检测实验记录-SZYC202212-T01170045.xlsx"
NPX.path <- "../NPX_results/SZYC202212_T01170045_上海浦东医院_NPX.xlsx"
sample.table <- read.xlsx(sample.table.path,
                          sheet = "Sheet2",
                          rowNames = T
                          )

#sample.table <- sample.table[,1:9]
sample.arrangement <- tidy.olink.rawdata(sample.table)


sample.demo <- read.xlsx("./Olink_samplerank_demo.xlsx",
                         sheet = 1,
                         rowNames = T)
demo.arrangement <- tidy.olink.rawdata(sample.demo)

sample.hole <- merge(demo.arrangement,
                     sample.arrangement,
                     by.x="a",
                     by.y="a",
                     all=FALSE)%>%select(.,holenum="V2.x",sample="V2.y")
sample.hole$sample <- gsub(sample.hole[,2],replacement = "",pattern = " ")#去除可能存在的空格
#根据结果文件格式的不同选择wide参数                       
NPX <- npx_readin(path = NPX.path,wide = F)

NPX[[1]] <- stri_replace_all_regex(NPX[[1]],replacement = sample.hole$sample,sample.hole$holenum,vectorize_all = F)
write.xlsx(NPX,
           file = paste0(dirname(NPX.path),"/2sampleid_",basename(NPX.path)),
           colNames = F)
rm(list = ls())

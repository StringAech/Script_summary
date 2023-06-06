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
  if(wide){NPX <- read.xlsx(NPX.path) %>% .[(which(.[,1]=="OlinkID")+1):(which(.[,1]=="LOD")-1),]}else{NPX <- read.xlsx(NPX.path)}
}
sample.table.path <- "./2/20230323-YuKM-R-20-001 1.0 Olink Signature Q100 血浆血清样本 Target 96多重蛋白组学检测实验记录.xlsx"
NPX.path <- "./2/YKKY0091_IO_NPX.xlsx"
sample.table <- read.xlsx(sample.table.path,
                          sheet = "Sheet1",
                          rowNames = T
                          )

#sample.table <- sample.table[,1:9]
sample.arrangement <- tidy.olink.rawdata(sample.table)


sample.demo <- read.xlsx("./Olink样本排布demo.xlsx",
                         sheet = 1,
                         rowNames = T
)
demo.arrangement <- tidy.olink.rawdata(sample.demo)

sample.hole <- merge(demo.arrangement,
                     sample.arrangement,
                     by.x="a",
                     by.y="a",
                     all=FALSE)%>%select(.,holenum="V2.x",sample="V2.y")
sample.hole$sample <- gsub(sample.hole[,2],replacement = "",pattern = " ")#去除可能存在的空格
                       
# #NPX <- read.xlsx(NPX.path) %>% .[(which(.[,1]=="OlinkID")+1):(which(.[,1]=="LOD")-1),]
NPX <- npx_readin(path = NPX.path,wide = F)
# NPX.new <- merge(sample.hole,NPX,by.y = colnames(NPX)[1],by.x = "holenum")

# with(NPX,expr = {
#   SampleID <- stri_replace_all_regex(SampleID,replacement = sample.hole$sample,sample.hole$holenum,vectorize_all = F)
#   NPX$SampleID <<-  SampleID
# })
NPX[[1]] <- stri_replace_all_regex(NPX[[1]],replacement = sample.hole$sample,sample.hole$holenum,vectorize_all = F)
write.xlsx(NPX,file = paste0(dirname(NPX.path),"/sampleid_",basename(NPX.path)))
rm(list = ls())



# sample.list <- read.xlsx("YuKM-R-20-001 1.0 Olink Signature Q100 血浆血清样本 Target 96多重蛋白组学检测实验记录.xlsx",
#                          sheet = 1)
# sample.list2 <- sample.list[3:90,3]
# #sample.list2[87] <- "CT2111516XYZA"
# #which(sample.table==a,sample.table)
# 
# a <- "22R5457XBZA"
# b <- str_pad(which(sample.table==a,sample.table),2,side = "left",pad = "0")#缺位自动补0
# sample.loc <- c(a,paste0("S",b))
# for (i in sample.list2) {
#   b <- str_pad(which(sample.table==i,sample.table),2,side = "left",pad = "0")
#   c <- c(i,paste0("S",b))
#   sample.loc <- rbind(sample.loc,c)
# }
# #sample.loc %>% as.data.frame()
# write.table(sample.loc,file = "./result/sample_loc.txt",sep="\t",row.names = F,col.names = F)
# #对应回npx
# NPX <- read.xlsx("YKKY0056-杭州联川生物Olink&P4测试&P5张同梅主任项目-多项目拼单_NPX-2.xlsx")
# NPX <- NPX[6:93,]
# colnames(NPX) <- NULL
# colnames(NPX) <- colnames(NPX,do.NULL = F)
# NPX.new <- merge(sample.loc,
#                  NPX, 
#                  by.x = "V2",
#                  by.y = "col1")
# #删除重复值
# NPX.new <- unique(NPX.new)
# NPX.new[2,2]
# NPX.new$V2 <- NULL
# NPX.new$V1 <- gsub(" ","",NPX.new$V1)
# write.table(NPX.new,file = "./result/sample_NPX.txt",sep = "\t",row.names = F,col.names = F)
#按照不同项目进行拆分
# #YKKY0056 联川生物
# ykky0056 <- read.xlsx("下单表-YKKY0056-杭州联川生物Olink-免疫癌症20221208(1).xlsx")
# ykky0056 <- ykky0056[9:62,c(2,19)] %>% as.data.frame()#提取sample id 列;##提取批次号,按照批次进行拆分,不一定是每个项目都有
# ykky0056.data <- merge(ykky0056,NPX.new,
#                        by.x = ".",
#                        by.y = "V1") %>% select(.,sample_id=`.`,everything())
# 
# ykky0056.data[1,]%>%as.numeric()%>%is.na()
# ykky0056.data[,ykky0056.data[1,]%>%as.numeric()%>%{!is.na(.)}]=ykky0056.data[,ykky0056.data[1,]%>%as.numeric()%>%{!is.na(.)}]%>%apply(2,as.numeric)
# writexl::write_xlsx(ykky0056.data%>%tibble(),path  = "./result/ykky0056_联川生物_拆分.xlsx")
# #str(ykky0056.data)
# #ykky0054 中南大学湘雅三院 6sample
# ykky0054 <- read.xlsx("下单表-YKKY0054-中南大学湘雅三院Olink循环蛋白检测1208.xlsx")
# ykky0054 <- ykky0054[9:13,2] %>% as.data.frame()
# ykky0054.data <- merge(ykky0054,NPX.new,
#                        by.x = ".",
#                        by.y = "V1") %>% select(.,sample_id=`.`,everything())
# write.table(ykky0054.data,file = "./result/ykky0054_中南大学_拆分.csv",sep = ",",row.names = F,col.names = F)
# #YKKY0063 内部研发项目
# ykky0063 <- read.xlsx("下单表P5-YKKY0063-SZYC201912-T02050014张同梅主任Olink-免疫癌症20221208.xlsx")
# ykky0063 <- ykky0063[9:36,2] %>% as.data.frame()
# ykky0063$.[27] <- "CT2111516XYZA"
# ykky0063.data <- merge(ykky0063,NPX.new,
#                        by.x = ".",
#                        by.y = "V1") %>% select(.,sample_id=`.`,everything())
# write.table(ykky0063.data,file = "./result/ykky0063_拆分.csv",row.names = F,col.names = F,sep = ",")

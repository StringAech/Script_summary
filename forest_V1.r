Immune_celltype <- data.frame(row.names = colnames(data@exp))
# 从储存的信息中调取细胞类型对应的marker队列，放置在celltype_list中
celltype_file <- paste0(system.file("extdata", package = "EPARS"), "/","Signature_List.xlsx")
tbl <- read.xlsx(celltype_file, sheet = "nCounter_CellType")
colnames(tbl) <- NULL
celltype_list <- sapply(tbl[,1],function(x) NULL)
for (i in 1:nrow(tbl)){
  celltype_list[[i]] <- append(celltype_list[[i]], tbl[i,-1][!is.na(tbl[i,-1])])
}
# 根据对应marker计算样本的细胞类型表达
for (i in names(celltype_list)){
  dataset_i <- as.matrix((as.data.frame(t(data@exp)))[, colnames((as.data.frame(t(data@exp)))) %in% celltype_list[[i]],drop = F])
  if (ncol(dataset_i) == 1){Immune_celltype[[i]] <- log2(dataset_i)}
  if (ncol(dataset_i) >= 2){Immune_celltype[[i]] <- rowSums(log2(dataset_i))/ncol(dataset_i)}
}
#write.table(Immune_celltype, file = paste0(CachePath, "TME_celltype.csv"), sep = ",", row.names = T, col.names = NA)
for (i in colnames(data@anno)) {
  if(length(unique(data@anno[[i]])) == 2){
    Immune_celltype_df_ann = cbind(Immune_celltype,ann = data@anno[[i]])
    var1 = data.frame(unique(Immune_celltype_df_ann$ann))[1,]#unique(Immune_celltype_df_ann$ann)  提取变量种类,
    var2 = data.frame(unique(Immune_celltype_df_ann$ann))[2,] 
    var1_data <- subset(Immune_celltype_df_ann,ann == var1)#提取变量1(var1)组变量2(var2)组
    var1_data$ann = NULL
    var2_data <- subset(Immune_celltype_df_ann,ann == var2)
    var2_data$ann = NULL
    pd_data <- var1_data
    sd_data <- var2_data
    mean_sd_data <- data.frame(colMeans(sd_data))#SD组求平均
    pd_data <- t(pd_data)#转置，配合mean_sd_data数据格式。
    dat_unand <- pd_data-mean_sd_data[,1]#var1组减去var2组的均值，因为这里的var1和var2都是经过log2之后的数值，因此，log2（var1/var2_mean）等同于
    #log2PD-log2SD，mean_sd_data为第一列，**应该可以改个写法。**
    dat_unand <- apply(dat_unand,1,t.test)#对结果进行t检验
    dat_test <- dat_unand#连接俩模块，**后面再改**
    mean <- lapply(dat_test,\(x){x[["estimate"]]})%>%unlist%>% data.frame()#提取均值
    lcl95 <- lapply(dat_test,\(x){x[["conf.int"]][[1]]})%>%unlist%>% as.data.frame()#提取95%置信区间的下限
    ucl95 <- lapply(dat_test,\(x){x[["conf.int"]][[2]]})%>%unlist%>% as.data.frame()#提取95%置信区间的上限
    p_value <- lapply(dat_test,\(x){x[["p.value"]]})%>%unlist%>% as.data.frame()#提取p值，但是在后面的图里没有展示
    merge_data <- cbind(mean$.,lcl95,ucl95,p_value)#合并，mean$. 是因为如果直接使用mean行名会变得复杂，好像只有mean这个元素的行名与其他不同。
    merge_data <- cbind(row.names(merge_data),merge_data)#删除行名，把行名当做列
    row.names(merge_data) <- NULL
    colnames(merge_data) <- c("name","logFC","lcl95","ucl95","p_value") #重命名列名。 
    #绘制森林图
    forest_data <- merge_data %>%   #提取数据列
      transmute(
        name,
        mean = logFC,
        upper = ucl95,
        lower = lcl95
      )
    group <- ifelse(
      forest_data$lower > 0, "up",
      ifelse(
        forest_data$upper < 0, "down",
        "not"
      )
      
    ) 
    forest_data <- data.frame(forest_data,Significance=group) #合并
    
    #绘制森林图
    cols <- c( "down" = "black", "not" = "white","up" = "black")
    sp <- c("down" = 25, "not" = 21,"up" = 24)
    p <- ggplot(forest_data, aes(mean, 
                                 y = reorder(name,mean),
                                 #col=Significance,
                                 shape = Significance,
                                 fill = Significance
    )
    ) # 不同形状shape= Factor
    forest_groupinfo <- paste0("Down from ",var1,"/",var2," <--- Log2 FC ---> " ,"Up from ",var1,"/",var2)
    p + 
      geom_errorbarh(aes(xmax =upper, xmin = lower), 
                     height = 0.4,
                     colour="green",
                     size = 1.2
      ) +
      geom_point(size=2,
                 color = "green", 
                 stroke = 1.5
      )+
      geom_vline(aes(xintercept = 0),
                 linetype = "dashed",
                 size = 1.2
      ) +
      xlab(forest_groupinfo) + 
      ylab(' ')+
      #scale_colour_manual(values = cols)+
      scale_shape_manual(values = sp)+
      scale_fill_manual(values = cols)+
      #scale_fill_discrete(guide = "none")
      #theme(legend.position = 'none')
      
      theme(legend.position="bottom",#左边left,右边 right, 底部bottom
            panel.border = element_rect(linetype = "solid", fill = NA),#设置边框线式样
            panel.background = element_blank(),#去掉背景的灰底
            legend.key  = element_blank(),#去掉图例的灰底
            #panel.grid.major.y= element_line(size = 3,color = "grey"),
            #panel.grid.minor.y= element_blank(),
            panel.grid.major.x=element_blank(),
            panel.grid.minor.x=element_blank()
      )
    ggsave(paste0(PlotsPath_forest,"Immune_CellType_DE_foresetplot_",i, ".pdf"))
  }
}
library(bladderbatch)
data(bladderdata)
dat <- bladderEset[1:50,]

pheno = pData(dat)
edata = exprs(dat)
mod = model.matrix(~as.factor(cancer), data=pheno)
mod0 = model.matrix(~1,data=pheno)
batch = pheno$batch
library(batchtma)
library(sva)
library(tidyverse)

data=edata[1:50,]; batch=batch; mod=mod;group=pheno$cancer;
scale_batch =function(data,batch,mod =c("Simple_means"),group,confounders =NA ){
get_data=  list(batch=list(),data=data,batch_data=batch,group=group)
  if("Simple_means"%in%mod){
    Simple_means_fun= function(.x,batch=batch){
      ret=data.frame(.x=unlist(.x),batch=batch)%>%
        adjust_batch(markers = .x, batch = batch, 
                     method = simple) %>%.[,3]
      return(ret)
    }
    Simple_means= apply(data, 1,Simple_means_fun,batch=batch)%>%t%>%as.data.frame()%>%{a=.;colnames(a)=colnames(data);a}
    get_data$batch[["Simple_means"]]=Simple_means
  }
  if("standardize"%in%mod){
  if(!is.na(confounders )){#nedd confounders line
  Simple_means_fun= function(.x,batch=batch){
    ret=data.frame(.x=unlist(.x),batch=batch)%>%
      adjust_batch(markers = .x, batch = batch, 
                   method = standardize ) %>%.[,3]
    return(ret)
  }
  standardize = apply(data, 1,Simple_means_fun,batch=batch)%>%t%>%as.data.frame()%>%{a=.;colnames(a)=colnames(data);a}
  get_data$batch[["standardize"]]=standardize 
  }
  }

  if("quantnorm"%in%mod){
  if(!is.na(confounders )){#nedd confounders line
    Simple_means_fun= function(.x,batch=batch){
      ret=data.frame(.x=unlist(.x),batch=batch)%>%
        adjust_batch(markers = .x, batch = batch, 
                     method = quantnorm  ) %>%.[,3]
      return(ret)
    }
    quantnorm  = apply(data, 1,Simple_means_fun,batch=batch)%>%t%>%as.data.frame()%>%{a=.;colnames(a)=colnames(data);a}
    get_data$batch[["quantnorm"]]=quantnorm  
  }
  }

  if("ComBat"%in%mod){
    model= data.frame(row.names =colnames(data), batch,group)
    mod = model.matrix(~as.factor(group), data=model)
    mod0 = model.matrix(~1,data=model)
    combat_edata = ComBat(edata, batch=batch, mod=mod)
    
    get_data$batch[["ComBat"]]=combat_edata  
  }
if("ComBat_seq"%in%mod){
  model= data.frame(row.names =colnames(data), batch,group)
  mod = model.matrix(~as.factor(group), data=model)
  mod0 = model.matrix(~1,data=model)
  combat_edata = ComBat_seq(round(edata), batch=batch#, group=group
                            )
  
  get_data$batch[["ComBat_seq"]]=combat_edata  
}
  if("ARSyN"%in%mod){
    data2=data%>%t%>%data.frame(batch=batch,group=group,check.names = F)%>%split(batch)
    names_batch= map(data2, function(.x) {
      .x$batch%>%unlist%>%unique()
    })%>%unlist%>%c
    inputOmics=map(data2,function(.x) { 
      .x=data.frame(.x,check.names = F)
      return(.x[,!colnames(.x)%in%c("batch","group")]%>%t)}
    )
    experimentalDesign =data.frame(batch=batch,group=group%>%as.character,check.names = F)%>%group_by(batch)%>%
      group_split(.keep = F)%>%{a=.;names(a)=names_batch;a}%>%map(unlist)
    #data.frame(batch=batch,group=group,check.names = F)%>%group_by(batch)%>%group_split(.keep = F)%>%{a=.;names(a)=names_batch;a}
    # data_RNA<- createMbac(inputOmics =list(`2`=inputOmics[[2]],`5`=inputOmics[[5]]),batchFactor =names_batch[c(2,5)],
    #                         experimentalDesign =list(`2`=experimentalDesign[[2]],`5`=experimentalDesign[[5]]),
    #                         omicNames = "RNA")
    data_RNA<- createMbac(inputOmics=inputOmics,batchFactor =names_batch[c(2,5)],
                          experimentalDesign =experimentalDesign,
                          omicNames = "RNA")
    ARSyN=ARSyNbac(data_RNA, batchEstimation = F,# filterNoise = TRUE,
              #Interaction=FALSE, Variability = 0.90, beta = 3, modelName = "Model 1"
             )
  get_data$batch[["ARSyN"]]=
    ARSyN[["CorrectedData"]]%>%map(~.x@ExperimentList@listData[["RNA"]])%>%do.call(cbind,.) 
  }

  if("fsva"%in%mod){
  
  model= data.frame(row.names =colnames(data), batch,group)
  mod = model.matrix(~as.factor(group), data=model)
  mod0 = model.matrix(~1,data=model)
  trainSv = sva(data,mod,mod0)
  fsvaobj = fsva(data,mod,trainSv,data)
  get_data$batch[["fsva"]]=fsvaobj$new
  }
  if("removeBatchEffect"%in%mod){
  get_data$batch[["removeBatchEffect"]]=limma::removeBatchEffect(data, batch)
  }
if("batch_correction"%in%mod){
  batch_correction <- function(data, batches) {
    # 检查输入数据的格式是否正确
    if (!is.matrix(data)) {
      stop("请提供一个矩阵作为输入数据。")
    }
    if (!is.factor(batches)) {
      stop("请提供一个因子向量用于表示数据的批次。")
    }
    if (nrow(data) != length(batches)) {
      stop("输入数据和批次向量的长度不匹配。")
    }
    
    # 计算每个变量的均值和标准差
    means <- colMeans(data)
    stds <- apply(data, 2, sd)
    
    # 对于每个批次，计算批次均值和标准差
    batch_means <- tapply(data, batches, colMeans)
    batch_stds <- tapply(data, batches, function(x) apply(x, 2, sd))
    
    # 计算校正因子
    correction_factors <- mapply(function(batch_mean, batch_std, mean, std) {
      (batch_mean - mean) / batch_std * std
    }, batch_means, batch_stds, means, stds)
    
    # 对每个变量，对每个批次应用校正因子
    corrected_data <- data
    for (i in 1:ncol(data)) {
      for (j in levels(batches)) {
        corrected_data[batches == j, i] <- corrected_data[batches == j, i] + correction_factors[[j]][i]
      }
    }
    
    # 返回校正后的数据
    corrected_data
  }
  
  get_data$batch[["batch_correction"]]=batch_correction(data, batch)
}

 return(get_data)
}

batch_data= scale_batch(data=edata[1:50,], batch=batch,group=pheno$cancer)


batch_plot=function(batch_data){
  batch_plot_data=list("plot",data=list("PCA","tSNE","UMAP"))
  batch_plot=map(batch_data$batch,function(.x){
    scale_batch= .x%>%t%>%scale%>%t
    
    #PCA
    pca_fit=prcomp(scale_batch)
    # 查看成分重要性
    batch_plot_data$data[["PCA"]]=data.frame(pca_fit$rotation,batch=as.factor(batch_data$batch_data),group=as.factor(batch_data$group))
    batch_plot_data$plot[["PCA"]]= batch_plot_data$data[["PCA"]]%>%
      ggplot(aes(x=PC1, 
                 y=PC2,
                 color=batch,shape=group))+
      geom_point()+theme_bw()+ggsci::scale_color_aaas()
    
    #tSNE
    batch_plot_data$data[["tsne"]]=   Rtsne::Rtsne(scale_batch%>%t, perplexity=10, check_duplicates = FALSE)$Y %>%
      as.data.frame()%>%
      rename(tsne1="V1",
             tsne2="V2")  %>% data.frame(batch=as.factor(batch_data$batch_data),group=as.factor(batch_data$group))
    batch_plot_data$plot[["tsne"]]= batch_plot_data$data[["tsne"]]%>%
      ggplot(aes(x=tsne1, 
                 y=tsne2,
                 color=batch,shape=group))+
      geom_point()+theme_bw()+ggsci::scale_color_aaas()
    #UMAP
    
    batch_plot_data$data[["UMAP"]]= umap::umap(scale_batch%>%t)$layout %>%
      as.data.frame()%>%
      rename(UMAP1="V1",
             UMAP2="V2")  %>% data.frame(batch=as.factor(batch_data$batch_data),group=as.factor(batch_data$group))
    batch_plot_data$plot[["UMAP"]]= batch_plot_data$data[["UMAP"]]%>%
      ggplot(aes(x=UMAP1, 
                 y=UMAP2,
                 color=batch,shape=group))+
      geom_point()+theme_bw()+ggsci::scale_color_aaas()
    return(batch_plot_data)
    
  })
  
  
  
  return(batch_plot)
 
}
ge=batch_plot(batch_data)

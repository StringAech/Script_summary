package = c("ggplot2","EPARS","ComplexHeatmap","openxlsx","RColorBrewer",
            "corrplot","data.table","ggpubr","limma","ggplot2","ggthemes","ggsci",
            "dplyr","pryr","tidyHeatmap","circlize","kableExtra","DT","base64enc","stringr",
            "R.utils","patchwork")#要安装的包名
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}#自定义安装函数
ipak(package)#安装未安装的R包
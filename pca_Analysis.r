setwd("C:\\Users\\guoha\\Desktop\\Rdemo")
library("ggplot2")
install.packages("xlsx", repos = "https://mirrors.ustc.edu.cn/CRAN/")
Pca_file <- read.csv("PCA.csv",sep = "," , header = T , row.names = 1)

df <- iris[c(1,2,3,4)]
head(df)
df_pca <- prcomp(df)
capture.output(df_pca, file = "df_pca.txt")
df_pcs = data.frame(df_pca$x,Species = iris$Species)
#b = rep(c("normal","tumor"),each=75)
#df_pcf_2 = data.frame(df_pca$x,Species = b)
head(df_pcs,3) 
#绘制pca图
plot(df_pca$x[,1],df_pca$x[,2])
#ggplot绘制pca图
ggplot(df_pcs,aes(x=PC1,y=PC2,color=Species))+geom_point()#geom_point()散点图的意思
#去掉背景及网格线
ggplot(df_pcs,aes(x=PC1,y=PC2,color=Species))+geom_point()+theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"))
#添加PC1 PC2的百分比
percentage <- round(df_pca$sdev/sum(df_pca$sdev)*100,4)
#round()可以将value四舍五入 round（x，digits= 0）digits是你希望保留的小数点位。也可以不写digits
percentage <- round(df_pca$sdev/sum(df_pca$sdev)*100,2)
percentage
percentage <- round(df_pca$sdev/sum(df_pca$sdev)*100,4)
percentage
percentage <- paste(colnames(df_pcs),"(",
                    paste(as.character(percentage),"%",")",sep = ""))
ggplot(df_pcs,aes(x=PC1,y=PC2,color=Species))+
  geom_point()+
  xlab(percentage[1])+
  ylab(percentage[2])
#添加置信椭圆
ggplot(df_pcs,aes(x=PC1,y=PC2,color=Species))+
  geom_point()+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"))+
  xlab(percentage[1])+
  ylab(percentage[2])+
  stat_ellipse(level = 0.95, show.legend = F) + 
  annotate('text', label = 'setosa', x = -2, y = -1.25, 
           size = 5, colour = '#f8766d') +
  annotate('text', label = 'versicolor', x = 0, y = - 0.5, 
           size = 5, colour = '#00ba38') +
  annotate('text', label = 'virginica', x = 3, y = 0.5,
           size = 5, colour = '#619cff')
#查看各个变量对PCA贡献
df_r <- as.data.frame(df_pca$rotation)
df_r$feature <- row.names(df_r)
df_r
#贡献度绘图
ggplot(df_r,aes(x=PC1,y=PC2,label=feature,color=feature )) + 
  geom_point()+ geom_text(size=3)
#绘制总图
ggplot(df_pcs,aes(x=PC1,y=PC2,color=Species )) +
  geom_point()+xlab(percentage[1]) +
  ylab(percentage[2]) + 
  stat_ellipse(level = 0.95, show.legend = F) +
  annotate('text', label = 'setosa', x = -2, y = -1.25,
           size = 5, colour = '#f8766d') +
  annotate('text', label = 'versicolor', x = 0, y = - 0.5, 
           size = 5, colour = '#00ba38') +
  annotate('text', label = 'virginica', x = 3, y = 0.5, 
           size = 5, colour = '#619cff') +
  labs(title="Iris PCA Clustering", 
       subtitle=" PC1 and PC2 principal components ",    
       caption="Source: Iris") + theme_classic()
#20220728更新
#### data is processed using by R package Magriitr 
library(magrittr)
iris[,1:2] %>% set_colnames(c("A","B"))
colnames(iris)
iris %>% extract(1) %T>% {
  head(.) %>% set_colnames("not add 5") %>% print()
} %>% add(5) %>% head()
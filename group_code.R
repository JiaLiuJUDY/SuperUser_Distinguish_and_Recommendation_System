library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(stringr)
library(glmnet)
library(MASS)
library(klaR)

# ******************** Load data ********************
setwd("/Users/jialiu/Desktop/Spring/479/final project/dataset")
business <- fread("business.csv")
dim(business)

# ******************** focus on America and restaurant **************
USAbusiness <- business %>% filter(business$state %in% state.abb)
ind <- grep("'Restaurants'", USAbusiness$categories)
business <- USAbusiness[ind, ]
dim(business)

# ********************* generate category variables *****************
categories = unlist(str_split(business$categories,","))
allCate = sub(pattern = ".*'(.+)'.*", replacement = "\\1", x=cate)
distinctCate <- unique(allCate) #608
categories = data.frame(allCate)
colnames(categories) = c("Name")
considerCate <- categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  filter(Count > 32 & Name != 'Restaurants') %>% # appear prob > 0.1%
  select(Name)
considerCate <- unlist(as.character(considerCate$Name))

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  filter(Count > 1000 & Name != 'Restaurants') %>%
  ggplot(aes(x = Name,y = Count)) +
  geom_bar(stat='identity',fill = 'bisque', color = 'black') +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count', 
       title = 'Categories with 1000 more Business') +
  coord_flip() + 
  theme(panel.grid = element_blank())

i = 1
cateDf <- data.frame(matrix(ncol = 152, nrow =  32472))
colnames(cateDf) <- considerCate
for (ele in considerCate){
  index = grep(ele, business$categories, fixed = T)
  cateDf[,i] <- as.factor(sapply(seq(length(business$categories)), 
                                 function(x){ifelse(x %in% index, TRUE, FALSE)}))
  i = i+1
  print(i)
}

business <- cbind(business, cateDf)
cateVars <- business[, c(seq(102,253))]

### Try PCA
pca = PCA((sapply(cateVars, function(x){as.numeric(x)})))
#pca$eig ### very bad result

### Try MCA
library(FactoMineR)
library(factoextra)
mca = MCA(cateVars, ncp = 50, graph = TRUE)
#mca$eig ### still bad

### Try k-modes ckuster
save(cateVars, file = 'cateVars.Rdata')

# ******* find optimal k for k-modes cluster of category variables ********
# ******* run this on linux
# load('cateVars.Rdata')
# seedList <- rnorm(1000, 2, 1000)
# i = 1
# wssDf = data.frame(matrix(rep(0, 25000), nrow = 1000, ncol = 25)) 
# for (seed in seedList){
#   set.seed(seed)
#   k.max <- 25
#   wss <- sapply(2:k.max, 
#                 function(k){sum(kmodes(cateVars, k)$withindiff)})
#   wssDf[i,] <- wss
#   print(i)
# }
# write.table(wssDf, "wssDf.txt", row.names = FALSE, col.names = TRUE, quote = FALSE)

# ******* find optimal seed for 14-modes cluster of category variables ********
# ******* run this on linux
# load('cateVars.Rdata')
# seedList <- rnorm(1000, 2, 1000)
# wssSeed = data.frame(matrix(rep(0, 2000), nrow = 1000, ncol = 2)) 
# names(wssSeed) = c('seed', 'wss')
# i = 1
# for (seed in seedList){
#   set.seed(seed)
#   wss <- sum(kmodes(cateVars, k)$withindiff)
#   wssSeed$seed[i] = seed
#   wssSeed$wss[i] = wss
# }
# write.table(wssSeed, "wssSeed.txt", row.names = FALSE, col.names = TRUE, quote = FALSE)

wssSeed <- fread("wssSeed.txt", colClasses=c(seed="float",wss ="float"))
set.seed(which[wssSeed$wss = min(wssSeed$wss), 1])
kmodesRe = kmodes(cateVars, 14)
clprofiles(kmodesRe, cateVars)
categoryGroup <- kmodesRe$cluster
business$categoryGroup <- factor(categoryGroup )

# *********** Main Attribute Cluster **********************
mainVar <- business[,c(seq(1,3), seq(6,9), seq(12,14), seq(16, 20), seq(22,26), seq(28,30), seq(32,37), seq(39,42), 44,seq(46, 49), seq(51,54), 56, 58, 59, 61, 62,seq(63,72), seq(74, 77), seq(79, 101), 254)]

# encode missing values, convert to factor
for (i in 1:length(names(mainVar))){
  mainVar[,i] = ifelse(is.na(mainVar[,i]) | mainVar[,i] == '', 'NULL', mainVar[,i])
  if (!is.numeric(mainVar[,i])){
    mainVar[,i] = as.factor(mainVar[,i])
  }
}

mainVar$categoryGroup <- factor(mainVar$categoryGroup )

mainVar %>% 
  group_by(categoryGroup) %>% 
  mutate(count = n()) %>% 
  select(categoryGroup, count) %>%
  ggplot(aes(categoryGroup), stat = "count") +
  geom_bar(aes(y=..count..))

# ******** find optimal k and seed ***************

summary(mainVar$categoryGroup)
mainVar$review_count <- (mainVar$review_count -min(mainVar$review_count ))/(max(mainVar$review_count )-min(mainVar$review_count )) * 10
library("factoextra")
seed = 10.38
set.seed(10.38)
clusterRe = kproto(mainVar,10)
clprofiles(clusterRe, clusterData)

### location cluster
locationVar <- data.frame(latitude = business$latitude, longtitude = business$longitude)
locationVar <- locationVar %>% filter(longtitude < -50) # some wrong record
### tried 1000 times respectively to found the best k and seed
seed = -69.12361
set.seed(seed)
kmeansRe <- kmeans(locationVar, 7, algorithm = "MacQueen", iter.max = 5000)
locationVar$cluster = factor(kmeansRe$cluster)

# ********************* get map distribution ************************
require(maps)
require(mapdata)
library(ggrepel)

usa <- map_data("state") 
ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = 'bisque', color = 'black', size = 0.05) +
  coord_fixed(1.3) +
  geom_point(aes(x = longtitude, y = latitude, color = cluster), data = locationVar)+
  labs(x = '', y = '', 
       title = '') +
  theme(panel.grid = element_blank())


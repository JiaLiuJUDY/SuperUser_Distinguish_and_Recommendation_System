# ****************** cluster based on category variables ***********
### Try PCA
pca = PCA((sapply(cateVars, function(x){as.numeric(x)})))
#pca$eig ### very bad result

### Try MCA
mca = MCA(cateVars, ncp = 50, graph = TRUE)
#mca$eig ### still bad

### Try k-modes ckuster
save(cateVars, file = 'cateVars.Rdata')

# ******* find optimal k for k-modes cluster of category variables ********
### run this on linux
load('cateVars.Rdata')
seedList <- rnorm(1000, 2, 1000)
wssDf = data.frame(matrix(rep(0, 25000), nrow = 1000, ncol = 25))
i = 1
for (seed in seedList){
  set.seed(seed); k.max <- 25
  wss <- sapply(2:k.max, function(k){sum(kmodes(cateVars, k)$withindiff)})
  wssDf[i,] <- wss; print(i)
}
write.table(wssDf, "wssDf.txt", row.names = FALSE, col.names = TRUE, quote = FALSE)
####
## use similiar way to find optimal seed
wssSeed <- fread("wssSeed.txt", colClasses=c(seed="float",wss ="float"))
set.seed(which[wssSeed$wss = min(wssSeed$wss), 1]); kmodesRe = kmodes(cateVars, 14)

# *********** cluster based on attribute variables **********************
library("factoextra")
mainVar$review_count <- (mainVar$review_count -min(mainVar$review_count ))/(max(mainVar$review_count )-min(mainVar$review_count )) * 10
seed = 10.38; set.seed(seed)
clusterRe = kproto(mainVar,10)

# ************ cluster based on location variables **********************
locationVar <- data.frame(latitude = business$latitude, longtitude = business$longitude)
locationVar <- locationVar %>% filter(longtitude < -50) # some wrong record
### tried 1000 times respectively to found the best k and seed
seed = -69.12361; set.seed(seed)
kmeansRe <- kmeans(locationVar, 7, algorithm = "MacQueen", iter.max = 5000)
locationVar$cluster = factor(kmeansRe$cluster)

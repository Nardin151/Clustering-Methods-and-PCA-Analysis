library(jpeg)
library(image)
cat <- readJPEG("C:/Users/samer/Downloads/photo-1568605117036-5fe5e7bab0b7.jpeg")
ncol(cat)
nrow(cat)
r <- cat[,,1]
g <- cat[,,2]
b <- cat[,,3]
cat.r.pca=prcomp(r,center=F)
cat.g.pca <- prcomp(g, center = FALSE)
cat.b.pca <- prcomp(b, center = FALSE)
rgb.pca=list(cat.r.pca,cat.g.pca,cat.b.pca)


for (i in seq.int(3, round(nrow(cat) - 10), length.out = 10)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste("C:/Users/samer/Downloads/", round(i,0), '_components.jpg', sep = ''))
}

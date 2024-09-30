dataMDS <- read_excel("Downloads/Table11_9.xlsx")
dataMDS

#calc distance
dist = dist(dataMDS[,3:11])
dist

#Count MDS
fit = cmdscale(dist, eig = TRUE, k=2)
fit

#plot MDS
X = fit$points[,1]
Y = fit$points[,2]
X
Y

plot(X,Y, xlab = 'Dim1', ylab = 'Dim2', main='MDS Result', col='red',pch=4)
abline(h=0, v=0, col='blue', lty=1)

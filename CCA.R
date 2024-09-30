library(readxl)
library(dplyr)
library(expm) #for sqrtm()
library(car)
library(mvShapiroTest) 
library(CCP)
datamvar <- read_excel("Downloads/Dataset_Mulvar_K4.xlsx")
View(datamvar) 
data = datamvar

###Canonical Correlations###
#Bikin matrix korelasi
data[1] = NULL
s = cor(data)
s
rho11 = s[5:7, 5:7]
rho22 = s[1:4, 1:4]
rho12 = s[5:7, 1:4]
rho21 = s[1:4, 5:7]

rho11
rho22
rho12
rho21

#solve in r is inverse matrix
invrho11 = solve(rho11)
invrho11
invsqrtrho11 = solve(sqrtm(rho11))
invsqrtrho11

invrho22 = solve(rho22)
invrho22
invsqrtrho22 = solve(sqrtm(rho22))
invsqrtrho22

#find A % B
a = invsqrtrho11%*%rho12%*%invrho22%*%rho21%*%invsqrtrho11
b = invsqrtrho22%*%rho21%*%invrho11%*%rho12%*%invsqrtrho22
a
b

#eigen of A
eigenA = eigen(a)
eigenA
eA = eigenA$vectors #Prepare e to find functions of U 
eA

#eigen of B
eigenB = eigen(b)
eigenB
eB = eigenB$vectors #Prepare e to find functions  V

#r-squared adalah $values
#r adalah sqrt(r-squared)
r2 = eigenA$values
r = sqrt(r2)
r2
r
#Terbentuk 3 fungsi kanonik, 
#dengan fungsi pertama r=0.98
#fungsi kedua r=0.77
#fungsi ketiga r=0.45

#Find U
u1 = eA[,1] %*% invsqrtrho11
u2 = eA[,2] %*% invsqrtrho11
u3 = eA[,3] %*% invsqrtrho11

u1
u2
u3

#Find V
v1 = eB[,1] %*% invsqrtrho22
v2 = eB[,2] %*% invsqrtrho22
v3 = eB[,3] %*% invsqrtrho22

v1
v2
v3

###ASUMSI###
#Split data into x and y
X = data[1:4]
Y = data[5:7]
X
Y
#Make linear model to check VIF
X$dummy = seq(1)
model_X = lm(dummy~., data=X)
Y$dummy = seq(1)
model_Y = lm(dummy~., data=Y)

##Signifikansi
#Serentak
#H0: SigmaYX = 0 (Tidak ada hubungan linear yang signifikan antara X dan Y)
#H1: SigmaYX /= 0 (Ada hubungan linear yang signifikan antara X dan Y)
#A krit = A(p;q;n-1-q)
#Reject H0 if A stat < A krit
detR = det(s)
detYY = det(rho11)
detXX = det(rho22)
#Stat uji
detR/(detYY*detXX)
#Nilai kritis = LAMBDA(p;q;n-1-q) = LAMBDA(4;3;5)
#Stat < Kritis, reject H0
#Reject H0, sehingga ada hubungan yang signifikan antara kelompok x dan y
#Atau terdapat setidaknya satu fungsi kanonik yang berpengaruh signifikan terhadap variabel kanonik x dan y

#Karena berpengaruh signifikan pada serentak, harus uji parsial
#H0:rho1 = rho2 = rho3 = 0
#H1: at least rho1 /= 0
#H0: rho2 = rho3 = 0
#H1: at least rho2 /= 0
#H0: rho3 = 0
#H1: rho3 /= 0

#lambda 1-3
lambda1 = (1-0.95)*(1-0.59)*(1-0.20)
lambda2 = (1-0.59)*(1-0.20)
lambda3 = (1-0.20)
  
lambda1
lambda2
lambda3

#Find F
n <- nrow(data)
p <- 3
q <- 4
#w = n-1/2(p+q+3)
#p<q
w = n-(0.5*(p+q+3))
w

#Uji partial pakai fungsi
cannonical_corr = cancor(data[1:4], data[5:7])$cor
cannonical_corr
wilks_result <- p.asym(cannonical_corr, n, p, q, tstat = "Wilks")
wilks_result


#Dist. Normal, use shapiro test
#H0 = variable pada Community terdistribusi multivariate normal 
#H1 = variable pada Community Tidak terdistribusi normal multivariate
mvShapiro.Test(as.matrix(X))
mvShapiro.Test(as.matrix(Y))
#pada kasus ini kurang data jadi test ga jalan

#No multicolinearity in one canonical var
vif(model_X)
vif(model_Y)
#Y terjadi multikolinearitas harusnya ga bisa bikin lanjut CCA, tp buat contoh, lanjut aja


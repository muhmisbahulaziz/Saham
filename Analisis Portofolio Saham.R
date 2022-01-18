#Mengunduh Data Saham
library(quantmod)
getSymbols("ADRO.JK",src="yahoo",from="2016-01-15",to="2021-01-15")
head(ADRO.JK)
getSymbols("AKRA.JK",src="yahoo",from="2016-01-15",to="2021-01-15")
head(AKRA.JK)
getSymbols("ICBP.JK",src="yahoo",from="2016-01-15",to="2021-01-15")
head(ICBP.JK)
getSymbols("INCO.JK",src="yahoo",from="2016-01-15",to="2021-01-15")
head(INCO.JK)
getSymbols("INDF.JK",src="yahoo",from="2016-01-15",to="2021-01-15")
head(INDF.JK)
getSymbols("KLBF.JK",src="yahoo",from="2016-01-15",to="2021-01-15")
head(KLBF.JK)
getSymbols("TLKM.JK",src="yahoo",from="2016-01-15",to="2021-01-15")
head(TLKM.JK)
getSymbols("UNTR.JK",src="yahoo",from="2016-01-15",to="2021-01-15")
head(UNTR.JK)
getSymbols("UNVR.JK",src="yahoo",from="2016-01-15",to="2021-01-15")
head(UNVR.JK)
getSymbols("WIKA.JK",src="yahoo",from="2016-01-15",to="2021-01-15")
head(WIKA.JK)

#Menghitung Return Saham
ADRO=monthlyReturn(ADRO.JK, type="arithmetic")
head(ADRO)
AKRA=monthlyReturn(AKRA.JK, type="arithmetic")
head(AKRA)
ICBP=monthlyReturn(ICBP.JK, type="arithmetic")
head(ICBP)
INCO=monthlyReturn(INCO.JK, type="arithmetic")
head(INCO)
INDF=monthlyReturn(INDF.JK, type="arithmetic")
head(INDF)
KLBF=monthlyReturn(KLBF.JK, type="arithmetic")
head(KLBF)
TLKM=monthlyReturn(TLKM.JK, type="arithmetic")
head(TLKM)
UNTR=monthlyReturn(UNTR.JK, type="arithmetic")
head(UNTR)
UNVR=monthlyReturn(UNVR.JK, type="arithmetic")
head(UNVR)
WIKA=monthlyReturn(WIKA.JK, type="arithmetic")
head(WIKA)

#Uji Normalitas
library(nortest)
lillie.test(as.vector(ADRO))
lillie.test(as.vector(AKRA))
lillie.test(as.vector(ICBP))
lillie.test(as.vector(INCO))
lillie.test(as.vector(INDF))
lillie.test(as.vector(KLBF))
lillie.test(as.vector(TLKM))
lillie.test(as.vector(UNTR))
lillie.test(as.vector(UNVR))
lillie.test(as.vector(WIKA))

#Expeted Return dan Standar Deviasi
ER <- c(ER1=mean(ADRO),ER2=mean(AKRA),ER3=mean(ICBP),ER4=mean(INCO),
        ER5=mean(INDF),ER6=mean(KLBF),ER7=mean(TLKM),ER8=mean(UNTR),
        ER9=mean(UNVR),ER10=mean(WIKA))
ER
std <- c(std1=sd(ADRO),std2=sd(AKRA),std3=sd(ICBP),std4=sd(INCO),
         std5=sd(INDF),std6=sd(KLBF),std7=sd(TLKM),std8=sd(UNTR),
         std9=sd(UNVR),std10=sd(WIKA))
std
VR <- c(VR1=var(ADRO),VR2=var(AKRA),VR3=var(ICBP),VR4=var(INCO),
        VR5=var(INDF),VR6=var(KLBF),VR7=var(TLKM),VR8=var(UNTR),
        VR9=var(UNVR),VR10=var(WIKA))
VR

#Koefisien Variasi
cv <- c(cv1=std1/ER1,cv3=std3/ER3,cv4=std4/ER4,cv5=std5/ER5,
        cv8=std8/ER8,cv10=std10/ER10)
cv

#VaR Simulasi Monte Carlo Aset Tunggal
for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim <- rnorm(1000,mu,sigma)
  cat(VaR <- quantile(sim, c(.05)), "\n", sep = "")
}
for(n in c(1:25)) {
  mu <- ER3
  sigma <- std3
  sim <- rnorm(1000,mu,sigma)
  cat(VaR <- quantile(sim, c(.05)), "\n", sep = "")
}
for(n in c(1:25)) {
  mu <- ER4
  sigma <- std4
  sim <- rnorm(1000,mu,sigma)
  cat(VaR <- quantile(sim, c(.05)), "\n", sep = "")
}
for(n in c(1:25)) {
  mu <- ER8
  sigma <- std8
  sim <- rnorm(1000,mu,sigma)
  cat(VaR <- quantile(sim, c(.05)), "\n", sep = "")
}

#Menghitung Proporsi Saham dengan Metode LGP
library(goalprog)
#Portofolio 1
coefficients <- matrix( 
  c(0.025697, 0.006442, 0.035362, 0.011609, 
    0.168142, 0.089116, 0.204989, 0.134317,
    1, 1, 1, 1, 
    1, 0, 0, 0, 
    0, 1, 0, 0, 
    0, 0, 1, 0, 
    0, 0, 0, 1,
    1, 0, 0, 0, 
    0, 1, 0, 0, 
    0, 0, 1, 0, 
    0, 0, 0, 1 ), nrow=11, byrow=TRUE )
targets1 <- c( 0.019778, 0.149141, 1, 0.1, 0.1, 0.1, 0.1, 0.3, 0.3, 0.3, 0.3 )
achievements <- data.frame( matrix( 
  c( 1, 1, 1, 0, 
     2, 2, 1, 0, 
     3, 3, 1, 1, 
     4, 4, 1, 0,
     5, 4, 1, 0,
     6, 4, 1, 0,
     7, 4, 1, 0,
     8, 4, 0, 1,
     9, 4, 0, 1,
     10, 4, 0, 1,
     11, 4, 0, 1), nrow=11, byrow=TRUE ) )
names( achievements ) <- c( "objective", "priority", "p", "n" )
sol1 <- llgp( coefficients, targets1, achievements )
sol1
#Portofolio 2
targets2 <- c( 0.019778, 0.149141, 1, 0.1, 0.1, 0.1, 0.1, 0.32, 0.32, 0.32, 0.32 )
sol2 <- llgp( coefficients, targets2, achievements )
sol2
#Portofolio 3
targets3 <- c( 0.019778, 0.149141, 1, 0.1, 0.1, 0.1, 0.1, 0.34, 0.34, 0.34, 0.34 )
sol3 <- llgp( coefficients, targets3, achievements )
sol3
#Portofolio 4
targets4 <- c( 0.019778, 0.149141, 1, 0.1, 0.1, 0.1, 0.1, 0.36, 0.36, 0.36, 0.36 )
sol4 <- llgp( coefficients, targets4, achievements )
sol4
#Portofolio 5
targets5 <- c( 0.019778, 0.149141, 1, 0.1, 0.1, 0.1, 0.1, 0.38, 0.38, 0.38, 0.38 )
sol5 <- llgp( coefficients, targets5, achievements )
sol5
#Portofolio 6
targets6 <- c( 0.019778, 0.149141, 1, 0.1, 0.1, 0.1, 0.1, 0.4, 0.4, 0.4, 0.4 )
sol6 <- llgp( coefficients, targets6, achievements )
sol6
#Portofolio 7
targets7 <- c( 0.019778, 0.149141, 1, 0.1, 0.1, 0.1, 0.1, 0.42, 0.42, 0.42, 0.42 )
sol7 <- llgp( coefficients, targets7, achievements )
sol7
#Portofolio 8
targets8 <- c( 0.019778, 0.149141, 1, 0.1, 0.1, 0.1, 0.1, 0.44, 0.44, 0.44, 0.44 )
sol8 <- llgp( coefficients, targets8, achievements )
sol8
#Portofolio 9
targets9 <- c( 0.019778, 0.149141, 1, 0.1, 0.1, 0.1, 0.1, 0.46, 0.46, 0.46, 0.46 )
sol9 <- llgp( coefficients, targets9, achievements )
sol9
#Portofolio 10
targets10 <- c( 0.019778, 0.149141, 1, 0.1, 0.1, 0.1, 0.1, 0.48, 0.48, 0.48, 0.48 )
sol10 <- llgp( coefficients, targets10, achievements )
sol10
#Portofolio 11
targets11 <- c( 0.019778, 0.149141, 1, 0.1, 0.1, 0.1, 0.1, 0.5, 0.5, 0.5, 0.5 )
sol11 <- llgp( coefficients, targets11, achievements )
sol11

#Expeted Return Portofolio
ERp1 <- (0.3*ER1)+(0.3*ER3)+(0.2312424*ER4)+(0.1687576*ER8)
ERp1
ERp2 <- (0.32*ER1)+(0.32*ER3)+(0.2237309*ER4)+(0.1362691*ER8)
ERp2
ERp3 <- (0.34*ER1)+(0.34*ER3)+(0.2162194*ER4)+(0.1037806*ER8)
ERp3
ERp4 <- (0.36*ER1)+(0.36*ER3)+(0.18*ER4)+(0.1*ER8)
ERp4
ERp5 <- (0.38*ER1)+(0.38*ER3)+(0.14*ER4)+(0.1*ER8)
ERp5
ERp6 <- (0.4*ER1)+(0.4*ER3)+(0.1*ER4)+(0.1*ER8)
ERp6
ERp7 <- (0.42*ER1)+(0.38*ER3)+(0.1*ER4)+(0.1*ER8)
ERp7
ERp8 <- (0.44*ER1)+(0.36*ER3)+(0.1*ER4)+(0.1*ER8)
ERp8
ERp9 <- (0.46*ER1)+(0.34*ER3)+(0.1*ER4)+(0.1*ER8)
ERp9
ERp10 <- (0.48*ER1)+(0.32*ER3)+(0.1*ER4)+(0.1*ER8)
ERp10
ERp11 <- (0.5*ER1)+(0.3*ER3)+(0.1*ER4)+(0.1*ER8)
ERp11

ERp <- c(ERp1,ERp2,ERp3,ERp4,ERp5,ERp6,ERp7,ERp8,ERp9,ERp10,ERp11)
sprintf("%.10f",ERp)

#VaR Simulasi Monte Carlo pada Portofolio
for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim1 <- rnorm(1000,mu,sigma)
  mu <- ER3
  sigma <- std3
  sim2 <- rnorm(1000,mu,sigma)
  mu <- ER4
  sigma <- std4
  sim3 <- rnorm(1000,mu,sigma)
  mu <- ER8
  sigma <- std8
  sim4 <- rnorm(1000,mu,sigma)
  
  RP1 <- (sim1*0.3)+(sim2*0.3)+(sim3*0.2312424)+(sim4*0.1687576)
  cat(VaR <- quantile(RP1, c(.05)), "\n", sep = "")
}

for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim1 <- rnorm(1000,mu,sigma)
  mu <- ER3
  sigma <- std3
  sim2 <- rnorm(1000,mu,sigma)
  mu <- ER4
  sigma <- std4
  sim3 <- rnorm(1000,mu,sigma)
  mu <- ER8
  sigma <- std8
  sim4 <- rnorm(1000,mu,sigma)
  
  RP2 <- (sim1*0.32)+(sim2*0.32)+(sim3*0.2237309)+(sim4*0.1362691)
  cat(VaR <- quantile(RP2, c(.05)), "\n", sep = "")
}

for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim1 <- rnorm(1000,mu,sigma)
  mu <- ER3
  sigma <- std3
  sim2 <- rnorm(1000,mu,sigma)
  mu <- ER4
  sigma <- std4
  sim3 <- rnorm(1000,mu,sigma)
  mu <- ER8
  sigma <- std8
  sim4 <- rnorm(1000,mu,sigma)
  
  RP3 <- (sim1*0.34)+(sim2*0.34)+(sim3*0.2162194)+(sim4*0.1037806)
  cat(VaR <- quantile(RP3, c(.05)), "\n", sep = "")
}

for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim1 <- rnorm(1000,mu,sigma)
  mu <- ER3
  sigma <- std3
  sim2 <- rnorm(1000,mu,sigma)
  mu <- ER4
  sigma <- std4
  sim3 <- rnorm(1000,mu,sigma)
  mu <- ER8
  sigma <- std8
  sim4 <- rnorm(1000,mu,sigma)
  
  RP4 <- (sim1*0.36)+(sim2*0.36)+(sim3*0.18)+(sim4*0.1)
  cat(VaR <- quantile(RP4, c(.05)), "\n", sep = "")
}

for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim1 <- rnorm(1000,mu,sigma)
  mu <- ER3
  sigma <- std3
  sim2 <- rnorm(1000,mu,sigma)
  mu <- ER4
  sigma <- std4
  sim3 <- rnorm(1000,mu,sigma)
  mu <- ER8
  sigma <- std8
  sim4 <- rnorm(1000,mu,sigma)
  
  RP5 <- (sim1*0.38)+(sim2*0.38)+(sim3*0.14)+(sim4*0.1)
  cat(VaR <- quantile(RP5, c(.05)), "\n", sep = "")
}
for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim1 <- rnorm(1000,mu,sigma)
  mu <- ER3
  sigma <- std3
  sim2 <- rnorm(1000,mu,sigma)
  mu <- ER4
  sigma <- std4
  sim3 <- rnorm(1000,mu,sigma)
  mu <- ER8
  sigma <- std8
  sim4 <- rnorm(1000,mu,sigma)
  
  RP6 <- (sim1*0.4)+(sim2*0.4)+(sim3*0.1)+(sim4*0.1)
  cat(VaR <- quantile(RP6, c(.05)), "\n", sep = "")
}
for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim1 <- rnorm(1000,mu,sigma)
  mu <- ER3
  sigma <- std3
  sim2 <- rnorm(1000,mu,sigma)
  mu <- ER4
  sigma <- std4
  sim3 <- rnorm(1000,mu,sigma)
  mu <- ER8
  sigma <- std8
  sim4 <- rnorm(1000,mu,sigma)
  
  RP7 <- (sim1*0.42)+(sim2*0.38)+(sim3*0.1)+(sim4*0.1)
  cat(VaR <- quantile(RP7, c(.05)), "\n", sep = "")
}
for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim1 <- rnorm(1000,mu,sigma)
  mu <- ER3
  sigma <- std3
  sim2 <- rnorm(1000,mu,sigma)
  mu <- ER4
  sigma <- std4
  sim3 <- rnorm(1000,mu,sigma)
  mu <- ER8
  sigma <- std8
  sim4 <- rnorm(1000,mu,sigma)
  
  RP8 <- (sim1*0.44)+(sim2*0.36)+(sim3*0.1)+(sim4*0.1)
  cat(VaR <- quantile(RP8, c(.05)), "\n", sep = "")
}
for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim1 <- rnorm(1000,mu,sigma)
  mu <- ER3
  sigma <- std3
  sim2 <- rnorm(1000,mu,sigma)
  mu <- ER4
  sigma <- std4
  sim3 <- rnorm(1000,mu,sigma)
  mu <- ER8
  sigma <- std8
  sim4 <- rnorm(1000,mu,sigma)
  
  RP9 <- (sim1*0.46)+(sim2*0.34)+(sim3*0.1)+(sim4*0.1)
  cat(VaR <- quantile(RP9, c(.05)), "\n", sep = "")
}
for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim1 <- rnorm(1000,mu,sigma)
  mu <- ER3
  sigma <- std3
  sim2 <- rnorm(1000,mu,sigma)
  mu <- ER4
  sigma <- std4
  sim3 <- rnorm(1000,mu,sigma)
  mu <- ER8
  sigma <- std8
  sim4 <- rnorm(1000,mu,sigma)
  
  RP10 <- (sim1*0.48)+(sim2*0.32)+(sim3*0.1)+(sim4*0.1)
  cat(VaR <- quantile(RP10, c(.05)), "\n", sep = "")
}
for(n in c(1:25)) {
  mu <- ER1
  sigma <- std1
  sim1 <- rnorm(1000,mu,sigma)
  mu <- ER3
  sigma <- std3
  sim2 <- rnorm(1000,mu,sigma)
  mu <- ER4
  sigma <- std4
  sim3 <- rnorm(1000,mu,sigma)
  mu <- ER8
  sigma <- std8
  sim4 <- rnorm(1000,mu,sigma)
  
  RP11 <- (sim1*0.5)+(sim2*0.3)+(sim3*0.1)+(sim4*0.1)
  cat(VaR <- quantile(RP11, c(.05)), "\n", sep = "")
}

#Indeks Sharpe dengan Rf = SBIS
sr1 <- (ERp1-0.004420533)/0.070424629
sr2 <- (ERp2-0.004420533)/0.069339004
sr3 <- (ERp3-0.004420533)/0.070821121
sr4 <- (ERp4-0.004420533)/0.070240642
sr5 <- (ERp5-0.004420533)/0.070831384
sr6 <- (ERp6-0.004420533)/0.072341876
sr7 <- (ERp7-0.004420533)/0.074598582
sr8 <- (ERp8-0.004420533)/0.078371068
sr9 <- (ERp9-0.004420533)/0.079491714
sr10 <- (ERp10-0.004420533)/0.081126468
sr11 <- (ERp11-0.004420533)/0.083651548

SR <- c(sr1,sr2,sr3,sr4,sr5,sr6,sr7,sr8,sr9,sr10,sr11)
SR

#Korelasi
cor.test(ADRO,ICBP)
cor.test(ADRO,INCO)
cor.test(ADRO,UNTR)
cor.test(ICBP,INCO)
cor.test(ICBP,UNTR)
cor.test(INCO,UNTR)

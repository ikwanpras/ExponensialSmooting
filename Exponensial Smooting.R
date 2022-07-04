install.packages("forecast")
library(forecast)
library(TTR)
library(graphics)

hujan <- read.csv("D:/HARIS/UNIMUS/GANJIL 2019_2020/KULIAH PERBAIKAN ARW/hujan.CSV",header = TRUE,sep = ";")
hujan
hujan.ts <- ts(hujan, start=c(1813))
plot(hujan.ts)


#Single Moving Average
hujan.sma <- SMA(hujan.ts,3)
cbind(hujan.ts,hujan.sma)
hujan.sma


#Double Moving Average

#Membuat Fungsi DMA
DMA <- function(data, orde) {
  
  #Mendefinisikan vektor MA Pertama
  s1=c()
  
  #Menghitung MA pertama
  for (i in orde:length(data)) {
    s1[i] = mean(data[(i-orde+1):i])
  }
  
  ##Mendefinisikan vektor MA Kedua
  s2=c()
  
  #Menghitung MA kedua
  for (j in (2*orde-1):length(data)) {
    s2[j] = mean(s1[(j-orde+1):j])
  }
  
  #Mendefinisikan Konstanta  dan KOefisien Slope
  a= c()
  b= c()
  
  
  #Menghitung Konstanta dan Koefisien Slope
  for (k in (2*orde-1):length(data)) {
    a[k] = s1[k] + (s1[k]-s2[k])
    b[k] = 2/(orde-1)*(s1[k]-s2[k])
  }
  
  #Mendefinisikan Ventor Peramalan
  f =c()
  
  #Menghitung Peramalan data
  f[2*orde-1] = a[2*orde-1] 
  for (l in (2*orde):(length(data)+1)) {
    f[l] = a[l-1]+b[l-1]
  }
  
  #Mendefinisikan Ventor Precentage Error
  PE = c()
  
  #menghitung PE
  for (m in (2*orde-1):length(data)){
    PE[m] = abs(data[m]-f[m])/data[m]*100
  }
  
  #Menghitung MAPE
  MAPE = mean(PE, na.rm = TRUE)
  
  Hasil_Perhitungan = data.frame(Data = data, S1 = s1, S2 = s2, a = a, b = b, Ft= f[-length(f)], PE = PE)
  list (Hasil_perhitungan = Hasil_Perhitungan, MAPE = MAPE, Peramalan_1_Periode_kedepan = f[length(f)])
}

hujan.dma<-DMA(hujan.ts,3)
hujan.dma

#Plot
plot(hujan.ts,xlab="Tahun",ylab="Curah Hujan",lty=1,col="black")
points(hujan.ts)
lines(hujan.sma, col = "red")

#Prediksi
phujan.sma <- lag(hujan.sma,-1)
phujan.sma
sma <- cbind(hujan.ts,hujan.sma,phujan.sma)
sma

#Evaluasi
SSE <- sum((phujan.sma-hujan.ts)^2,na.rm=T)
SSE
MSE <- mean((phujan.sma-hujan.ts)^2,na.rm=T)
MAPE <- mean(abs((hujan.ts-phujan.sma)/hujan.ts),na.rm=T)
MAPE

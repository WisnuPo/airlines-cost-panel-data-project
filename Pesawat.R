# PROJECT ANALISIS DATA PANEL DENGAN MENGGUNAKAN R

# project ini akan menganalisis biaya yang dikeluarkan 6 maskapai dalam waktu 15 tahun

# import library
library(plm)
library(dplyr)
library(car)
library(lmtest)
library(stargazer)
options(scipen = 999)

# import dataset
data <- read.csv('PanelData.csv')

# statistik deskriptif variabel
summary(data)

# transformasi variabel menjadi bentuk Ln (Logaritma Natural)
data$LnC <- log(data$C)
data$LnQ <- log(data$Q)
data$LnPF <- log(data$PF)
data$LnLF <- log(data$LF)

# MEMBANGUN MODEL REGRESI DATA PANEL

# Model PLS
PLS <- plm(data$LnC ~ data$LnQ + data$LnPF + data$LnLF, data = data, model = 'pooling')
summary(PLS)

# Model FEM
FEM <- plm(data$LnC ~ data$LnQ + data$LnPF + data$LnLF, data = data, model = 'within')
summary(FEM)

# Model REM
REM <- plm(data$LnC ~ data$LnQ + data$LnPF + data$LnLF, data = data, model = 'random')
summary(REM)

# PEMILIHAN MODEL TERBAIK

# Uji Chow --> PLS vs FEM
pooltest(PLS, FEM) # jika p-value kurang dari 0.05, maka terima FEM

# uji hausman --> FEM vs REM
phtest(FEM, REM) # jika p-value kurang dari 0.05, maka terima FEM

# uji LM --> PLS vs REM
plmtest(PLS, type = 'bp') # jika p-value kurang dari 0.05, maka terima REM

# DARI ANALISIS INI, MODEL TERBAIK ADALAH = REM

# UJI ASUMSI KLASIK

# Multikolinearitas
multikol <- lm(data$LnC ~ data$LnQ + data$LnPF + data$LnLF, data = data)
vif(multikol) # hasilnya tidak ada multikolinearitas

# jika yang terpilih adalah REM, banyak sumber mengatakan bahwa kita tidak perlu melakukan
# uji heteroskedastisitas dan autokorelasi. Namun karena ini adalah sebagai bahan pembelajaran, 
# maka akan tetap dilakukan uji heteroskedastisitas dana autokorelasi.

# Heteroskedastisitas
bptest(REM) # jika p-value kurang dari 0.05, maka ada masalah heteroskedastisitas

# Autokorelasi
pbgtest(REM) # jika p-value kurang dari 0.05, maka ada masalah autokorelasi

# BAGAIMANA JIKA TERDAPAT MASALAH ASUMSI KLASIK? 
# Salah satu cara mengatasinya adalah mengoreksi standar error dengan menggunakan fungsi vcoHC.

clustered.se <- vcovHC(REM, type = 'HC1', cluster = 'group') # mengoreksi se dengan vcoHC untuk mengatasi hetero dan autokol
rem1 <- coeftest(REM, vcov. = clustered.se) # menampilkan hasil regresi yang sudah dikoreksi
stargazer(rem1, type = 'text') # menampilkan tabel hasil regresi

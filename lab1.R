# ZADANIE 1:
rm(list=ls())
install.packages("faraway")
library(faraway)
data(pima)
pima$diastolic[pima$diastolic==0]<-NaN
pima
pima$test[pima$test==1]<-"Prawda"
pima$test[pima$test==0]<-"Falsz"
head(pima)
summ<-summary(pima$diastolic)
summ
x<-pima$diastolic
cat("Mean: ", mean(x,na.rm=T))
cat(" Median: ",median(x,na.rm=T))
cat("Mediana,dolny,g?rny kwantyl 1-szy decyl:",quantile(x,c(0.5,0.25,0.75,0.1),na.rm=T))
cat("IQR: ", quantile(x,0.75,na.rm=T)-quantile(x,0.25,na.rm=T))
cat("Rostep: ",max(x,na.rm=T)-min(x,na.rm=T))
cat("STDEV: ", sqrt(var(x,na.rm=T)))
#(E)
cat(mean(pima$diastolic[pima$test=="Prawda"],na.rm=T),sqrt(var(pima$diastolic[pima$test=="Prawda"],na.rm=T)))
boxplot(pima$pregnant,horizontal=FALSE,ylab="Number of pregnancies")
#wiekszosc kobiet byla mniej razy w ciazy
par(mfrow=c(1,2))
barplot(table(pima$test),col=c("green","blue"))
pie(table(pima$test),col=c("green","blue"))
par(mfrow=c(1,1))
hist(pima$diastolic,freq=F,xlab="diastolic")    
lines(density(pima$diastolic,na.rm=T),col="red",xlab="diastolic")

# ZADANIE 2:
wyspy <- read.table(file.choose(),header=T)
attach(wyspy)
wyspy
zm <- c(wyspy$Area)
species <- c(wyspy$Species)
hist(zm[zm<=quantile(zm,0.75)],breaks=3,xlab="Area",main="Histogram")
boxplot(species[zm>1 & zm<25], main="Wykres pudełkowy", ylab="Wartość", col="red")

# ZADANIE 3:
#a:
waga <- c(60, 72, 57, 90, 95, 72)
wzrost <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)

bmi <- c(waga/wzrost^2)
bmi

#b:
# srednia
mean(bmi)
# mediana
quantile(bmi,0.5)
# dolny kwartyl
quantile(bmi,0.25)
# gorny kwartyl
quantile(bmi,0.75)

#c:
boxplot(bmi, main="Wykres pudełkowy", ylab="Wartość", col="red")

#d:
dane <- data.frame(waga,wzrost,bmi)
wynik <- dane$wzrost[dane&bmi >= 20.7 & dane&bmi < 26.5]
wynik

print(wzrost[20.7<=bmi& bmi<26.5])

# ZADANIE 4:
library(MASS)
data(Cars93)
head(Cars93)

kurs_USD_to_PLN <- 3.80  # przyjmijmy przykładowy kurs

# Przeliczenie jednostek i tworzenie nowych zmiennych
Cars93$MPG_city_liter <- Cars93$MPG.city / 1.6 * 3.8  # Zużycie paliwa w mieście w litrach na 100 km
Cars93$MPG_highway_liter <- Cars93$MPG.highway / 1.6 * 3.8  # Zużycie paliwa na autostradzie w litrach na 100 km
Cars93$Weight_kg <- Cars93$Weight * 0.4536  # Waga samochodu w kg
Cars93$Min.Price_PLN <- Cars93$Min.Price * kurs_USD_to_PLN  # Cena wersji podstawowej w tys. PLN

# Podstawowe statystyki próbkowe dla danych opisujących cenę w tys. PLN wersji podstawowej samochodu
summary(Cars93$Min.Price_PLN)

# Obliczenie kwantyla rzędu 0,95 dla cen wersji podstawowej
quantile(Cars93$Min.Price_PLN, 0.95)

# Ceny wersji podstawowej samochodów wyższe od kwantyla
ceny_wyzsze <- Cars93$Min.Price_PLN[Cars93$Min.Price_PLN > quantile(Cars93$Min.Price_PLN, 0.95)]
modeli <- Cars93$Model[Cars93$Min.Price_PLN > quantile(Cars93$Min.Price_PLN, 0.95)]
cbind(modeli, ceny_wyzsze)

# Wykresy skrzynkowe dla zużycia benzyny osobno dla samochodów amerykańskich i nieamerykańskich
boxplot(Cars93$MPG_city_liter ~ Cars93$Origin, main="Zużycie benzyny w mieście", xlab="Pochodzenie", ylab="MPG (l/100km)")

# Histogram częstości dla danych dotyczących wagi samochodu z jądrowym estymatorem gęstości
hist(Cars93$Weight_kg, freq=TRUE, main="Histogram wagi samochodu", xlab="Waga (kg)")
lines(density(Cars93$Weight_kg), col="red")

# Wykres słupkowy dla zmiennej Type
barplot(table(Cars93$Type), main="Wykres słupkowy dla zmiennej Type", xlab="Typ samochodu", ylab="Liczba")

# Wykres kołowy dla zmiennej Type
pie(table(Cars93$Type), main="Wykres kołowy dla zmiennej Type")

# ZADANIE 5:
x<-c(5,1,0,-2,3,0,-1,1,2,4)
quantile(x,c(0.25,0.5,0.75))
#b
x<-c(-1,5,1,0,-2,3,0,-1,1,2,4)
quantile(x,c(0.25,0.5,0.75))
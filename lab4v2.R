# Zadanie 3.6 abe
# n=10
# Rozkład normalny
waga <- c(5.21, 5.15, 5.20, 5.48, 5.19, 5.25, 5.09, 5.17, 4.94, 5.11)

# podpunkt a
# Model II - t.test
# hipoteza zerowa H0: mu = 5.20
# hipoteza alternatywna: H1: mu < 5.20
# poziom istotności: alpha=0.05
alpha <- 0.05
mu0 <- 5.20
T <- (mean(waga) - mu0)*sqrt(length(waga))/sd(waga)
T
zbior <- qt(1-alpha, length(waga)-1)
zbior

if(T< -zbior) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy')

# sposób 2 - t.test
test <- t.test(x=waga, mu=5.2, alternative='less')
if(test[3]<alpha) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy')

# wynik -> nie ma podstaw do odrzucenia H0

# Odp: Przy poziomie istotności 0.05, nie można stweirdzić, że
# średnia waga ptaków jest mniejsza niż 5.2kg

##################################################
# podpunkt b
power <- power.t.test(n=length(waga), delta=(5.20-5.15), 
  sd=sd(waga), sig.level=0.05, type='one.sample', alternative='one.sided')
power[5]

# Odp: Test przeprowadzony w pkt. (a), przyjmie na pozimoie 
# istotniści 0.05 hipotezę, że średnia waga ptaków badanego
# gatunku jest mniejsza niż 5.20kg, w sytuacji, gdy
# w rzeczywistości ta średnia waga wynosi 5.15kg z 
# p-stwem P=0.283

###############################
# podpunkt c
power <- power.t.test(power=0.8, n=length(waga), sd=sd(waga), sig.level=0.05, type='one.sample', alternative='one.sided')
weight <- mu0 - power$delta
weight

# Odp: Srednia waga ptakow musialaby wynosic 5.083513kg
# przy zadanych warunkach

#####################
# podpunkt d
power <- power.t.test(power=0.8, delta=(5.20-5.15), 
  sd=sd(waga), sig.level=0.05, type='one.sample', alternative='one.sided')
power
print(paste('Minimalna liczność próby:', ceiling(power$n)))

# Odp: Minimalna liczność próby, by spełnione były warunki zadania
# wynosi n=48

#####################
# podpunkt e
# hipoteza zerowa: H0: sigma=0.2
# hipoteza alternatywna: H1: sigma<0.2   
# (bo sd(waga) = 0.14 czyli mniej niż 0.2)

sigma0=0.2
chisq <- (length(waga)-1)*var(waga)/sigma0^2
chisq
qchisq(0.95, length(waga)-1)

if(chisq > 0 & chisq <= qchisq(0.95, length(waga)-1)) print('Odrzucamy hipotezę zerową') else 
  print('Nie ma podstaw do odrzucenia hipotezy zeorwej')

# Odp: Na poziomie istotności 0.05 nie można stwierdzić, że 
# odchylenie standardowe wagi ptaków badanego gatunku 
# wynosi 0.20kg

###########################################################################################################################################

# Zadanie 3.7
n <- 150
k <- 118
p <- 0.8
alpha <- 0.01

# Model IV
# X = 1, gdy losowo wybrany pracownik pracuje krócej niż 5 lat
# X = 0, gdy losowo wybrany pracownik pracuje dłużej niż 5 lat

if(k>=5 & (n-k)>=5) print('Mozna uzyc modelu prop test') else
  print('Nie mozna uzyc modelu prop test')
#Mozna użyć prop.test

# Hipoteza zerowa: H0: p=0.80
# Hipoteza alternatywna: H1: p<0.80, bo k/n=0.79

test <- prop.test(x=k, n=n, p=0.8, alternative='less')

p.value <- test[3]
p.value

if(p.value <= alpha) print('Odrzucamy hipotezę zerową') else 
  print('Nie ma podstaw do odrzucenia hipotezy zerowej')

# Odp: Przy poziomie istotności można stwierdzić, że 80% 
# pracowników legitymuje się stażem pracy mnijeszym niż 5 lat.
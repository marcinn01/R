# Zadanie 2.5
set.seed(1)   # tutaj ustawiamy DOWOLNĄ liczbę, u nas padło na 1
dane <- rgamma(n=100,shape=3,rate=2)
library(MASS)
fitdistr(dane, 'gamma', lower=c(0,0))

dane2 <- rgamma(n=1000,shape=3,rate=2)
fitdistr(dane2, 'gamma', lower=c(0,0))


# ZADANIE 3.1
curve(dnorm(x), from=qnorm(0.001), to=qnorm(0.999), main="N(0,1)")
curve(dt(x, df=5), add=T, col="pink")
curve(dt(x, df=10), add=T, col="red")
curve(dt(x, df=25), add=T, col="green")
curve(dt(x, df=50), add=T, col="blue")
curve(dt(x, df=100), add=T, col="yellow")
text(x=2, y=0.38, labels="t[5]", col="pink")
text(x=2, y=0.36, labels="t[10]", col="red")
text(x=2, y=0.34, labels="t[25]", col="green")
text(x=2, y=0.32, labels="t[50]", col="blue")
text(x=2, y=0.30, labels="t[100]", col="yellow")

###########################################################################################################################################3
# Zadanie 3.2
# podpunkt a 
dane <- read.table(file.choose(),header=T)
attach(dane)
dane
masy <- dane$WeightInitial
masy 

alpha <- 0.05
len <- length(masy)
#hipoteza zerowa: H0: mu=23
#hipoteza alternatywna: H1: mu>23
mu0 <- 23
test <- t.test(x=masy, mu=mu0, alternative='greater')
test
p_val <- test[3]
if(p_val < alpha) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy')

# podpunkt b
# H0: mu=mu0
# Szukamy: P(odrzucimy H0 | mu1=23)
# moc testu (theta) = P(odrzucenia H0 | theta)
n <- length(masy)
mu0 <- 24
mu1 <- 23

test2 <- power.t.test(power = NULL, n = n, delta = mu0 - mu1, sd = sd(masy), sig.level = 0.05, type = 'one.sample', alternative = 'one.sided')
1-test2$power

#podpunkt c
# szukmay n takiego, by P(odrzucenie H0 | mu=24) >= 0.8 przy poziomie 
# istotności 0.05

# szukamy n takiego, by moc_testu(24) >= 0.8 przy poziomie istotności 0.05
test3 <- power.t.test(power = 0.8, n = NULL, delta = 1, sd = sd(masy), sig.level = 0.05, type = 'one.sample', alternative = 'one.sided')
ceiling(test3$n)


###########################################################################################################################################3
# Zadanie 3.3
# podpunkt a 
# hipoteza zerowa: H0: var = 20
# hipoteza alternatywna: H0: var != 20
var0 <- 20
alpha <- 0.1
len <- length(masy)
stat_test <- (n-1)*var(masy)/var0
w_low <- qchisq(alpha/2, n-1)
w_high <- qchisq(1-alpha/2, n-1)
stat_test
w_low
w_high
# statystyka testowa = 23.755
# zbiór krytyczny W = (-inf, 25.7) U (54.6, inf)
# statystyka testowa należy do zbioru krytycznego, więc 
# odrzucamy hipotezę zerową. Nie można przyjąć, że wariancja wagi 
# młodych kóz wynosi 20 kg^2

# b
dane <- read.table(file.choose(),header=T)
attach(dane)
dane

# sprawdzić, czy odchylenie standardowe wagi młodych kóz hodowanych w Australii przekracza 3kg

masy <- dane$WeightInitial
masy 
odchylenie <- sd(masy)
odchylenie

# mu, sigma nieznane - Model II
# hipoteza zerowa: H0: sigma = 3
# H1: sigma > 3
# poziom istosności: 0.1
# używamy t.test:
odchylenie0 <- 3
stat_test <- ((n-1)*sd(masy)^2)/odchylenie0^2
stat_test
w_low <- qchisq(0.9, n-1)
w_low
# Zbiór krytyczny: W=(50.66, +inf)
# statystyka testowa = 52.8, więc należy do zbioru krytycznego, 
# więc należy odrzucić hipotezę zerową 









###########################################################################################################################################
# Zadanie 3.4
install.packages("HSAUR2")
library(HSAUR2)
data("Forbes2000")
# podpunkt a
# Na poziomie istotnosci alfa=0.05 przetestowac hipoteze, ze sredni roczny zysk duzych 
# spolek zajmujacych sie uslugami bankowymi w 2004 roku byl mniejszy niz 425 mln USD
# H0: mu=0.425 -> sredni roczny zysk duzych spolek zajmujacych sie uslugami bankowymi w 2004r. 
# wynosil 425mln usd
# H1: mu<0.425 -> sredni roczny zysk duzych spolek zajmujacych sie uslugami bankowymi w 2004r. 
# mniejszy niz 425mln usd
mean(dane)
dane <- Forbes2000[Forbes2000$category == 'Banking', 'profits']
dane
test <- t.test(dane, alternative = 'less', mu = 0.425)
test$p.value
test

# podpunkt b
# Zakladajac, ze sredni roczny zysk duzych spolek zajmujacych sie uslugami bankowymi wynosil 
# w 2004 roku 422 mln USD, obliczyc prawdopodobienstwo, ze test z pkt. (a) da nam bledna odpowiedz
# mu0=0.425
# mu1=0.422
# H0: mu=mu0
# Szukamy: P(odrzucimy H0 | mu1=0.422)
# moc testu (theta) = P(odrzucenia H0 | theta)
n <- length(dane)
mu0 <- 0.425
mu1 <- 0.422
test2 <- power.t.test(n = n, delta = mu0 - mu1, sd = sd(dane), sig.level = 0.05, type = 'one.sample', alternative = 'one.sided')
test2$power
# P(odrzucimy H0 | mu1=0.422) = 0.0527811

# podpunkt c
# Szukamy takiego mu<0.425, ze H(odrzucimy H0 | mu) = 0.7
test3 <- power.t.test(power = 0.7, n = n, sd = sd(dane), sig.level = 0.05, type = 'one.sample', alternative = 'one.sided')
test3
mu_kryt = mu0 - test3$delta
mu_kryt
# mu_kryt = 0.1783644
# sredni roczny zysk duzych spolek musialby wynosic mu=0.1783644 (okolo 178 mln USD)


###########################################################################################################################################
# Zadanie 3.5 
# Podpunkt a 

# minimum: 35% = 0.35 
# Czy na poziomie istotności 0.05 można uznać, że udział mężczyzn jest mniejszy niż minimum, jeśli 
# wśród losowo zbadanych 400 pracowników przedszkoli było 128 mężczyzn?

# n=400
# k=128
# poziom istoności: 0.05

# rozkład dwupunktowy - albo mężczyzna, albo kobieta

#    {1, gdy wylosowany pracownik jest mężczyzną 
#X = {
#    {0, gdy wylosowany pracownik jest kobietą

# hipoteza zerowa: H0:p=0.35
# H1:p<0.35, bo wynik p=128/400<0.35

# p=P(X=1), ^p=128/400, ^q=1-p

# sprawdzamy warunki:
# 1) np>=5 & 2)nq>=5
k <- 128
n <- 400
if(k>=5 & n-k>=5) print('Warunki spełnione') else print('Warunki niespełnione')

test <- prop.test(x=128, n=400, p=0.35, alternative='less', conf.level=0.05)
p_value <- test[3]
p_value

poziom_istotnosci <- 0.05

if(p_value <= poziom_istotnosci) print('Odrzucamy H0') else print('Nie ma podstaw do odrzucenia H0') 

# podpunkt b
# 1) np>=5 & 2)nq>=5
k <- 3
n <- 10
if(k>=5 & n-k>=5) print('Warunki prop.test spełnione') else print('Warunki prop.test niespełnione')

# Używamy binom.test

test2 <- binom.test(x=3, n=10, p=0.35, alternative='less')
p_value2 <- test2[3]
p_value2
if(p_value <= poziom_istotnosci) print('Odrzucamy H0') else print('Nie ma podstaw do odrzucenia H0') 

###########################################################################################################################################
# Zadanie 3.6 abe
# n=10

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

# podpunkt b
power <- power.t.test(n=length(waga), delta=(mu0-5.15), sd=sd(waga), sig.level=0.05, type='one.sample', alternative='one.sided')
power[5]

# podpunkt c
power <- power.t.test(power=0.8, n=length(waga), sd=sd(waga), sig.level=0.05, type='one.sample', alternative='one.sided')
weight <- mu0 - power$delta
weight

# podpunkt d
power <- power.t.test(power=0.8, delta=(5.20-5.15), sd=sd(waga), sig.level=0.05, type='one.sample', alternative='one.sided')
print(paste('Minimalna liczność próby:', ceiling(power$n)))

# podpunkt e
# hipoteza zerowa: H0: sigma=0.2
# hipoteza alternatywna: H1: sigma<0.2   
# (bo sd(waga) = 0.14 czyli mniej niż 0.2)

sigma0=0.2
chisq <- (length(waga)-1)*var(waga)/sigma0^2
chisq
qchisq(0.95, length(waga)-1)
if(chisq > 0 & chisq <= qchisq(0.95, length(waga)-1)) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy zeorwej')


###########################################################################################################################################
# Zadanie 3.7
n <- 150
k <- 118
p <- 0.8
alpha <- 0.01

# Model IV
# X = 1, gdy losowo wybrany pracownik pracuje krócej niż 5 lat
# X = 0, gdy losowo wybrany pracownik pracuje dłużej niż 5 lat

if(k>=5 & (n-k)>=5) print('Mozna uzyc modelu prop test') else print('Nie mozna uzyc modelu prop test')

# Hipoteza zerowa: H0: p=0.80
# Hipoteza alternatywna: H1: p<0.80, bo k/n=0.79

test <- prop.test(x=k, n=n, p=0.8, alternative='less')

p.value <- test[3]
p.value

if(p.value <= alpha) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy zerowej')
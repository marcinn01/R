# Zadanie 3.3
# b
# kolumna WeightInitial - masy kóz 
# rozkład jest normlany

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
test <- t.test(x=masy, mu=3, alternative='greater')
test
p_val <- test[3]

if(p_val < 0.1) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy')


T <-(mean(masy)-3)*sqrt(length(masy))/sd(masy)
T
# T \in (qt(0.95, length(masy)-1); inf)
kwartyl <- qt(0.9, length(masy)-1)
# T in <1.3; inf)

if (T>kwartyl) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy')

#----------------------------------------------------------------------------------------------------------
# Zadanie 3.4
library(HSAUR2)
data("Forbes2000")

# podpunkt a
# Na poziomie istotnosci alfa=0.05 przetestowac hipoteze, ze sredni roczny zysk duzych 
# spolek zajmujacych sie uslugami bankowymi w 2004 roku byl mniejszy niz 425 mln USD
# H0: mu=0.425 -> sredni roczny zysk duzych spolek zajmujacych sie uslugami bankowymi w 2004r. 
# wynosil 425mln usd
# H1: mu<0.425 -> sredni roczny zysk duzych spolek zajmujacych sie uslugami bankowymi w 2004r. 
# mniejszy niz 425mln usd

dane <- Forbes2000[Forbes2000$category == 'Banking', 'profits']
test <- t.test(dane, alternative = 'less', mu = 0.425)
test$p.value

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

#----------------------------------------------------------------------------------------------------------
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

test2 <- binom.test(x=3, n=10, p=0.35, alternative='less', conf.level=0.05)
p_value2 <- test2[3]
p_value2
if(p_value <= poziom_istotnosci) print('Odrzucamy H0') else print('Nie ma podstaw do odrzucenia H0') 


# Zadanie 3.6
# n=10
waga <- c(5.21, 5.15, 5.20, 5.48, 5.19, 5.25, 5.09, 5.17, 4.94, 5.11)

# podpunkt a
# Model II - t.test
# hipoteza zerowa H0: mu = 5.20
# hipoteza alternatywna: H1: mu < 5.20
# poziom istotności: alpha=0.05

alpha <- 0.05
mu0 <- 5.20
T <- (mean(waga) - mu0)*length(waga)/sd(waga)
T
zbior <- qt(1-alpha, length(waga)-1)
zbior

if(T<-zbior) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy')

# sposób 2 - t.test
test <- t.test(x=waga, mu=5.2, alternative='less')
if(test[3]<alpha) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy')

# podpunkt b
power <- power.t.test(n=length(waga), delta=(mu0-5.15), sd=sd(waga), sig.level=0.05, type='one.sample', alternative='one.sided')
power[3]

# podpunkt c
power <- power.t.test(power=0.8, n=length(waga), sd=sd(waga), sig.level=0.05, type='one.sample', alternative='one.sided')
weight <- mu0 - power$delta
weight

# podpunkt d
power <- power.t.test(power=0.8, delta=(5.20-5.15), sd=sd(waga), sig.level=0.05, type='one.sample', alternative='one.sided')
print(paste('Minimalna liczność próby:', ceiling(power$n)))

# podpunkt e
# hipoteza zerowa: H0: sigma=0.2
# hipoteza alternatywna: H1: sigma<0.2   (bo sd(waga) = 0.14 czyli mniej niż 0.2)

sigma0=0.2
chisq <- (length(waga)-1)*var(waga)/sigma0
chisq
qchisq(0.95, length(waga)-1)

if(chisq > 0 & chisq <= qchisq(0.95, length(waga)-1)) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy zeorwej')


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

if(p.value <= alpha) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy zerowej')
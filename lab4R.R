# Zadanie 3.4 
# podpunkt a
library(HSAUR2)
data("Forbes2000")
dane <- Forbes2000[Forbes2000$category == 'Banking', 'profits']

# X - przychód dużych spółek zajmujących się usługami bankowymi
# hipoteza zerowa: H0: mu = 0.425
# hipoteza alternatywna: H1: mu < 0.425

test <- t.test(dane, alternative = 'less', mu = 0.425)
test

if(test[3] < 0.05) print("Odrzucamy hipotezę zerową") else
  print(paste("Nie ma podstaw do odrzucenia H0, bo p_value = ",
              test[3], " > 0.05"))
# Można stwierdzić, że przy poziomie istotności 0.05, że średni zysk spółek 
# nie był mniejszy niż 425 mln

# podpunkt b
# szukamy P(przyjmujemy H0 | mu=422mln) = 1-P(odrzucamy H0 | mu=422mln) 
# = 1-moc.testu(mu=422mln), delta=0.425-0.422=0.003

1-power.t.test(n=length(dane), delta=0.003, sd=sd(dane), sig.level=0.05, 
                        type="one.sample", alternative="one.sided")$power

# P(przyjmujemy H0 | mu=422mln) = 0.9472189

# podpunkt c
# szukmay mu takiego, aby P(odrzucimy H0 | mu=?) = 0.7

delta <- power.t.test(power=0.7, n=length(dane), sd=sd(dane), sig.level=0.05,
              type="one.sample", alternative="one.sided")$delta
delta
0.425 - delta
# sredni roczny zysk musialby wynosic mu0-delta=0.178mld

##################################################
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

# Hipoteza zerowa: H0: p=0.80
# Hipoteza alternatywna: H1: p<0.80, bo k/n=0.79

test <- prop.test(x=k, n=n, p=0.8, alternative='less')

p.value <- test[3]
p.value

if(p.value <= alpha) print('Odrzucamy hipotezę zerową') else print('Nie ma podstaw do odrzucenia hipotezy zerowej')

# Odp: Można stwierdzić, że 80% pracowników pracuje mniej 
# niż 5 lat, przy poziomie istotności 0.01

###################################################
# Zadanie 4.1
# podpunkt a 

# WWA 233 -> 40
# KRK 220 -> 31

alpha <- 0.05

# X = 1, gdy zapytany Warszawiak kupuje w Żuczku 
# X = 0, gdy zapytany Warszawiak nie kupuje w Żuczku 

# Y = 1, gdy zapytany Krakowiak kupuje w Żuczku 
# Y = 0, gdy zapytany Krakowiak nie kupuje w Żuczku 

# hipoteza zerowa: H0: p1=p2  => p1-p2=0
# hipoteza alternatywna: H1: p1>p2  => p1-p2>0

n1 <- 233
k1 <- 40
n2 <- 220
k2 <- 31

# sprawdzamy, czy warunki prop.test są spełnione:

if(k1>=5 & n1-k1>=5 & k2>=5 & n2-k2>=5) print("Warunki prop.test są spełnione") else 
  print("Warunki prop.test nie są spełnione")

m <- matrix(c(k1, k2, n1-k1, n2-k2), nrow=2, byrow=FALSE)

#test <- prop.test(x=c(k1, k2), n=c(n1, n2), p=NULL, alternative="greater")
test <- prop.test(m, alternative="greater")

test
p.value <- test[3]
p.value
if(p.value > alpha) print("Brak podstaw do odrzucenia H0") else 
  print("Odrzucamy H0")

# podpunkt b
p1 <- 0.17
p2 <- 0.14

# P(odrzucenie H0 | p1=0.17 i p2=0.14) = 
# = moc.testu(p1=0.17 i p2=0.14)

p <- power.prop.test(n = c(n1, n2), p1 = p1, p2 = p2, sig.level = 0.05, alternative = "one.sided")$power
p
p[1]
p[2]

# P-stwo, że test potwierdzi, że odsetek regularnych
# klientów Żuczka jest większy w Warszawie niż w
# Krakowie w sytuacji, przy założeniu, że p1=0.17 i p2=0.14
# mieści się w przedziale (21.88%, 22.64%)

# podpunkt c
# Szukamy n=n1=n2 takiego, by P(przyjmiemy H1 | p1=0.17 i p2=0.14) >= 0.8
# P(przyjmiemy H1 | p1=0.17 i p2=0.14) = P(odrzucamy H0 | p1=0.17 i p2=0.14) =
# moc.testu(p1=0.17, p2=0.14)
n <- power.prop.test(power=0.8, p1=0.17, p2=0.14, sig.level=0.05, alternative="one.sided")$n
n
n <- ceiling(n)
n

# Odp: Trzeba wylosować po 1799 mieszkańców z KRK i WWA

########################################################
# Zadanie 4.2

# X - wielkość plonów uzyskanych z gatunku I
# Y - wielkość plonów uzyskanych z gatunku II

# X ma rozkład normalny N(mu1, sigma1^2)
# Y ma rozkład normalny N(mu2, sigma2^2)
# gdzie mu1, mu2, sigma1, sigma2 nieznane

# Dysponujemy niezależnymi próbami losowymi z tych populacji
# Możemy zastosować unpaired.t.test()
# Musimy sprawdzić, czy sigma1=sigma2
# H0: sigma1^2 = sigma2^2
# H1: sigma1^2 != sigma2^2

dane <- read.table(file.choose(), header=T)
dane
gatunek1 <- subset(dane, variety == "v1")$yield
gatunek2 <- subset(dane, variety == "v2")$yield

var.test(x=gatunek1, y=gatunek2, alternative="two.sided")
# p_value = 0.08542
# p_value = 0.08542 < 0.1, czyli odrzucamy H0,
# więc stwierdzamy, że wariancje nie są równe

# H0: mu1 = mu2
# H1: mu1 != mu2

t.test(x=gatunek1, y=gatunek2, alternative="two.sided", 
       paired=FALSE, var.equal=FALSE)

# p_value = 0.4883

# p_value = 0.4883 > 0.05
# Nie ma podstaw do odrzucenia H0

# Odp: 2 gatunki pszenicy dają takie same plony
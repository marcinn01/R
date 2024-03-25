# Zadanie 4.3

df<-read.table(file="domy.txt",header=T)
# podpunkt a

# X - pomiar w domu energooszczednym
# Y - pomiar w domu tradycyjnym

# pomiary byly wykonywane niezaleznie

# X, Y mają rozkłady normalne, mu1, mu2, sigma1^2, sigma2^2 - nieznane
# X ~ N(mu1, sigma1^2)
# Y ~ N(mu2, sigma2^2)

# Przeprowadzamy test F
# Hipoteza zerowa: H0: sigma1 = sigma2 
# alternatywna H1: sigma1 != sigma2

# Przyjmuje się taki poziom istotności przy teście F
alpha <- 0.1
p_val <- var.test(df$domE,df$domS,alternative="two.sided")$p.value

if(p_val > alpha) print("Nie ma podstaw do odrzucenia H0") else
  print("Odrzucamy H0")

# Zakładamy że wariancje sigma1^2 = sigma2^2
# Możemy teraz zastosować unpaired-t-test z równymi wariancjami 

# Hipoteza zerowa: H0: mu1 = mu2
# Hipoteza alternatywna: H1: mu1 > mu2
alpha_2 <- 0.05
p_val_2 <- t.test(df$domE,df$domS,alternative="greater",var.equal=T,paired=F)$p.value

if(p_val_2 > alpha_2) print("Nie ma podstaw do odrzucenia H0") else
  print("Odrzucamy H0")

# podpunkt b

# zakladamy ze: mu1 - mu2 = 50
# szukamy mocy.testu(50)

delta <- 50
sd <- sqrt((var(df$domE)+var(df$domS))/2)
power <- power.t.test(n=length(df$domE), sd=sd, sig.level=alpha_2, delta=delta, 
    alternative="one.sided", type="two.sample")$power

cat("moc test=",power)
# pstwo odrzucenia H0 takiego, ze stezenie dwutlenku wegla w domu E i S sa rowne wtedy gdy stezenie
# dwutlenku wegla jest srednio o 50 ppm wieksze wynosi 0.26


# Zadanie 4.4
# podpunkt a 
alpha <- 0.01
prz_A <- c(144, 165, 125, 149, 128, 159)
prz_B <- c(147, 167, 124, 152, 127, 160)

# Oznaczamy:
# X - pomiar ciśnienia przez urządzenie A
# Y - pomiar ciśnienia przez urządzenie B

# Pomiary dotyczą tego samego pajenta, co sugeruje, że można zastosować paired.t.test

# pomiary mają rozkłady normalne
# X~N(mu1, sigma1^2)
# Y~N(mu2, sigma2^2)
# gdzie mu1, mu2, sigma1, sigma2 - nieznane  

print(mean(prz_A)>mean(prz_B))

# Hipoteza zerowa: H0: mu1 = mu2
# Hipoteza alternatywna: H1: mu1 < mu2

?t.test
test2 <- t.test(prz_A, prz_B, alternative = "less", paired = TRUE, conf.level = 1-alpha)
test2$p.value
if(test2$p.value > 0.01) print("Nie ma podstaw do odrzucenia H0") else 
  print("Odrzucamy H0")

# Odp: Na poziomie istotności 0.01 można stwierdzić, że nie istnieje istotna 
# różnica między pomiarami z dwóch przyrządów


# Podpunkt b
# szukamy delta takiej, by 
# P(odrzucamy H0 | delta) = moc.testu(delta)=0.8

?power.t.test
alpha <- 0.1
#sd <- sqrt((var(prz_A) + var(prz_B))/2)
sd <- sd(prz_A-prz_B)
test3 <- power.t.test(n=length(prz_A), delta=NULL, sd=sd, power=0.8, sig.level = alpha,
                      type="paired", alternative = "one.sided")
test3$delta
print(paste("Test z punktu a) jest w stanie wykryć różnicę: ", ceiling(test3$delta*100)/100))


# statystyka testowa:
x <- prz_A - prz_B
z <- mean(x)
T <- z*sqrt(length(prz_A))/sd
q <- qchisq(0.9, length(prz_A)-1)
if(T< -q) print("Odrzucamy H0") else 
  print("Brak podstaw do odrzucenia H0")

# Podpunkt c
# Szukamy P(odrzucamy H0 | mu1-mu2=1.2) = moc.testu(1.2) = 0.8

sd <- sd(prz_A-prz_B)
test4 <- power.t.test(n=NULL, delta=1.2, sd=sd, power=0.8, sig.level = 0.01,
                      type="paired", alternative = "one.sided")
ceiling(test4$n)


##################################################
# Zadanie 4.5
# podpunkt a

gat_A <- c( 26.4, 22.5, 24.9, 23.7, 21.5)
gat_B <- c(25.1, 29.0, 23.4, 27.6, 22.3)

# X - pomiar zawartosci nikotyny w gatunku A
# Y - pomiar zawartosci nikotyny w gatunku B

# pomiary wykonywane były niezależnie 
# pomiary obu gatunków mają rozkład normalny 
# X~N(mu1, sigma1^2)
# Y~N(mu2, sigma2^2)
# gdzie mu1, mu2, sigma1, sigma2 - nieznane  

# Sprawdzamy, czy sigma1==sigma2, w tym celu stosujemy test F:

# Hipoteza zerowa: H0: sigma1 = sigma2 
# Hipoteza alternatywna: H1: sigma1 != sigma2
# poziom istotności dla tesu F: alpha=0.1

alpha <- 0.1
p_val <- var.test(gat_A, gat_b, alternative="two.sided")$p.value
if(p_val > alpha) print("Nie ma podstaw do odrzucenia H0") else
  print("Odrzucamy H0")

# Zakladamy rowność wariancji - możemy zastosowac unpaired.t.test - Model II
mean(gat_A)
mean(gat_B)

# Hipoteza zerowa: H0: mu1 = mu2
# Hipoteza alternatywna: H1: mu1 < mu2 (bo mean(gat_A) < mean(gat_B))

alpha_2 <- 0.05
p_val_2 <- t.test(gat_A, gat_B, alternative="less", var.equal=T, paired=F)$p.value

if(p_val_2 > alpha_2) print("Nie ma podstaw do odrzucenia H0") else
  print("Odrzucamy H0")

# Odp: Na pozimie istotności nie 0.05 nie można stwierdzić, że gatunek B ma zyższą zawartość nikotyny


# Podpunkt b

# Zakladamy ze hipoteza alternatywna jest prawdziwa, bo zakladamy ze mu2 = mu1 + 2
# Błędna odpowiedź to przyjęcie H0

delta <- 2
sd <- sqrt((var(gat_A) + var(gat_B))/2)
power <- power.t.test(n=length(gat_A), sd=sd, sig.level=alpha_2, delta=delta,
    alternative="one.sided", type="two.sample")$power

cat("P(przyjecia H0 | mu1-mu2=2) = ", 1-power)

# Odp: Pstwo, że test z punktu (a) da błędną odpowiedź wynosi: 0.33


# Podpunkt c

# moc.testu(Beta) = P(odrzucenia H0 | mu0 = beta)
# szukamy P(odrzucenia H0 | mu0=beta) bo mean(gat_B) > mean(gat_A)

power <- 0.75
delta <- 2
sd <- sqrt((var(gat_A)+var(gat_b))/2)
num <- power.t.test(power=power, sd=sd, sig.level=alpha_2, delta=delta,
                    alternative = "one.sided", type="two.sample")$n
cat("Wymagana licznosc grupy n =", ceiling(num))


#################################
# Zadanie 4.6

library(MASS)
data("nlschools")
nlschools

mediana <- median(nlschools$SES)

reach <- nlschools$IQ[nlschools$SES >= mediana]
poor <- nlschools$IQ[nlschools$SES < mediana]

mean(reach)
mean(poor)

# Oznaczamy:
# X - IQ uczniów z bogatszych domów
# Y - IQ uczniów z biedniejszych domów

# rozklady zmiennych sa nieznane, licznosci obu prob >100
# mu1, mu2, sigma1, sigma2 - nieznane  
# Sugeruje to zastosowanie modelu IV

# Hipoteza zerowa: H0: mu1 = mu2
# Hipoteza alternatywna: H1: mu1 > mu2  (bo mean(reach) > mean(poor))

alpha <- 0.05

# Statystyka testowa:
stat <- (mean(reach) - mean(poor))/sqrt((var(reach)/length(reach) + var(poor)/length(poor)))
stat

qnorm(1-alpha)

# Odp: Na poziomie istotności 0.05 nie można stwierdzić, że uczniowie z domów bogatszych 
# mają wyższy iloraz inteligencji werbalnej


#################################
# Zadanie 4.7
# Podpunkt a

# Rozklad dwupunktowy - losowa spytana osoba popiera partię BETA lub nie
#X = 1 gdy na wsi BETA jest popierana
#X = 0 gdy na wsi BETA nie jest popierana

#Y = 1 gdy w miescie BETA jest popierana
#Y = 0 gdy w miescie BETA nie jest popierana

# p_w = P(X=1) pstwo trafienia na mieszkanca wsi ktory popiera bete
# p_m = P(Y=1) pstwo trafienia na mieszkanca miasta ktory popiera bete

# Model V

# Hipoteza zerowa: H0 : p_w = p_m
# Hipoteza alternatywna: H1 : p_w > p_m
n_w <- 2000
n_m <- 1000
n_ws <- 450
n_ms <- 0.2*n_m
p_w <- n_ws/n_w
p_m <- n_ms/n_m

# Sprawdzamy, czy warunki prop.test są spełnione:
if(n_w*p_w >= 5 && n_w*(1-p_w) >= 5 && 
   n_m*p_m >= 5 && n_m*(1-p_m) >= 5) print("warunki spełnione") else 
     print("warunki niespełnione")

# Mozna uzyc prop.test

alpha <- 0.05
test <- prop.test(x=c(n_ws,n_ms), n=c(n_w,n_m), alternative="greater")
if(test$p.value > alpha) print("Nie ma podstaw do odrzucenia H0") else
  print("Odrzucamy H0")

# Odp: Na poziomie istotności 0.05 nie można stwierdzić, że partia "BETA"
# cieszy się mniejszym poparciem w mieście niż na wsi 


# Podpunkt b
# Chcemy przyjąć hipotezę alternatywną, czyli odrzucić H0: p_w=p_m, gdy p_m=0.2 i p_m=0.23
# P(odrzucamy H0 | p_m=0.2 i p_m=0.23) = moc.testu(p_m=0.2 i p_m=0.23) = 0.8

p_m <- 0.2
p_w <- 0.23

power <- 0.8
n <- power.prop.test(power=power, p1=p_m, p2=p_w, sig.level=alpha,
                     alternative="one.sided")$n
cat("Wymagana licznosc grupy n =",ceiling(n))

#################################
# Zadanie 4.8

library(MASS)
data(Cushings)
Cushings
Cushings$Tetrahydrocortisone

tetra_A <- Cushings$Tetrahydrocortisone[Cushings$Type== "a"]
tetra_A

tetra_B <- Cushings$Tetrahydrocortisone[Cushings$Type== "b"]
tetra_B

# Oznaczamy:
# X - tempo wydalania tetrahydrokortyzonu u pacjentów z postacią zespołu a
# Y - tempo wydalania tetrahydrokortyzonu u pacjentów z postacią zespołu b

# pomiary mają rozkłady normalne, można zapisać:
# X~N(mu1, sigma1^2)
# Y~N(mu2, sigma2^2)
# gdzie mu1, mu2, sigma1, sigma2 - nieznane  

# Hipoteza zerowa: H0: sigma1 = sigma2
# Hipoteza alternatywna: H1: sigma1 != sigma2

# pomiary są niezależne - wyniki pochodzą od różnych pacjentów
# Można zastosować test F

alpha <- 0.1
test <- var.test(tetra_A, tetra_B, alternativ="two.sided", conf.level = 1-alpha)
test$p.value

# Odp: Na poziomie istotności 0.1 nie można przyjąć że wariancje badanych prób są równe
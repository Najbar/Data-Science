################################################################################################################################
# Marcin Najbar
# DataScience  - data mining 2021L Sprawozdanie 1
# dataset: AdultUCI
################################################################################################################################
### Sposób wyszukiwania i filtrowania reguł interesujących:
#   Główną miarą selekcji reguł asocjacyjnych jest współcznnik podniesienia (lift).
#   Przy wyższych współczynnikach wsparcia i zaufania lift jest niższy,
#   natomiast przy niższych wartościach tych współczynników poziom liftu rośnie.
#   Poziom liftu dobrany jest w ten sposób, aby znalezionych zostało od kilku 
#   do kilkunastu najbardziej interesujących reguł.
#   Dodatkowo reguły asocjacyjne zostały przefiltrowane ze względu na miarę 
#   "improvement" z przedziału (0.1, 1> oraz wybrane zostały zbiory maksymalne.
#
### Struktura raportu:
# 1. Przygotowanie danych
# 2. Wykrywanie reguł asocjacyjnych przy wykorzystaniu funkcji apriori
#    a) Wsparcie > 0.2, zaufanie > 0.2, lift > 1.1 (aRules111)
#    b) Wsparcie > 0.1, zaufanie > 0.1, lift > 1.2 (aRules222)
#    c) Wsparcie > 0.01, zaufanie > 0.1, lift > 5  (aRules333)
# 3. Reguły z wybranym następnikiem - wartościami atrybutu income
#    Wsparcie > 0.1, zaufanie > 0.1, lift > 1.1 (aRules_income)
# 4. Ocena praktycznej użyteczności uzyskanych reguł
#
### Uwagi dotyczące sprawozdania:
# 1. Pisząc częściej rozmiem, że dany poprzednik zwiększa prawdopodobieństwo wystąpnia następnika (lift > 1)
# 2. Reguły interesujące wybieram w oparciu o wartość lift-u, filtrując zbiory maksymalne i nie opisując reguł trywialnych jak np.:
#    a) osoba młoda jest częściej niezamężna
#    b) osoba starsza jest częściej wdową/wdowcem
#    c) osoba zamężna jest kobietą
################################################################################################################################

library(arules)
library(arulesViz)

################################################################################################################################

### 1. Przygotowanie danych ###

data("AdultUCI")

dim(AdultUCI)
str(AdultUCI)
# View(AdultUCI)
head(AdultUCI)

# a) Wybór atrybutów. Usunięcie zbędnych zmiennych "fnlwgt" i "education-num"

table(AdultUCI$education,AdultUCI$`education-num`)

# Jak widać w tabeli zmienne "education" i "education-num" zawierają dokładnie
# tą samą informację wobec tego usuwam jedną z nich

AdultUCI[["education-num"]] <- NULL
AdultUCI[["fnlwgt"]] <- NULL

table(AdultUCI$relationship,AdultUCI$`marital-status`)

# Informacja zawarta w zmiennych "relationship" i "martial-status" jest podobna,
# ale nie identyczna. Wobec tego decyduję się pozostawić obie zmienne. Jednak
# reguły asocjacyjne tworzone w oparciu o obie te zmienne mogą być trywialne

# AdultUCI[["relationship"]] <- NULL

# b) dyskretyzacja atrybutów ciągłych

AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)), labels = c("Young", "Middle-aged", "Senior", "Old"))

AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]],c(0,25,40,60,168)),
                                         labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))

AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)), labels = c("None", "Low", "High"))

AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
                                           c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)),
                                       labels = c("none", "low", "high"))

# c) problem braków danych (NA)

summary(AdultUCI)

# Braki danych dotyczą zmiennych "workclass", "occupation", "native-country", "income". 
# Jedną z możliwości byłoby zastąpienie braków danych dominantą. Drugą możliwością
# jest zastąpienie braków danych wartością "Other". Jednak obie te możliwości spowodowałyby
# zniekształcenie stworzonych reguł asocjacyjnych. Wobec tego decyduję się pozostawić
# wartości NA.

# d) Problem duplikatów. Decyduję się usunąć duplikaty.

dim(AdultUCI)

which(duplicated(AdultUCI) == TRUE)
length(which(duplicated(AdultUCI) == TRUE))

AdultUCI <- unique(AdultUCI)
dim(AdultUCI)
summary(AdultUCI)

# e) Konwersja danych z postaci relacyjnej na transakcyjną

adultTR <- as(AdultUCI, "transactions")

inspect(head(adultTR))
str(adultTR)
class(adultTR)
summary(adultTR)
################################################################################################################################

### 2. Wykrywanie reguł asocjacyjnych przy wykorzystaniu funkcji apriori ###
# Eksperymenty

# Ustawienie parametrów do funkcji apriori
# a) Wsparcie > 0.2, zaufanie > 0.2, lift > 1.1 (suffix = 111)

aParam  = new("APparameter", "confidence" = 0.2, "support" = 0.2, "minlen"= 2, maxtime = 20, target ="rules")
# print(aParam)

aRules111 <- apriori(adultTR, aParam)
length(aRules111) # 1004

aRules111 <- aRules111[aRules111@quality$lift > 1.1]

### Inny sposób na uzyskanie identycznych reguł asocjacyjnych 
# resTbl <- interestMeasure(aRules111,"lift", asets)
# summary(resTbl)
# aRules111 <- which(sapply(resTbl, function(x) {x > 1.1})==TRUE)

### Jeszcze inny sposób na uzyskanie identycznych reguł asocjacyjnych 
# aRules111 <- subset(aRules111, subset =  lift > 1.1)

length(aRules111) # 117
# size(aRules111)
# summary(aRules111)

# Większość reguł opiera się na silnej korelacji wartości "relationship=Husband", "sex=Male" oraz marital-status=Married-civ-spouse
# wobec czego zdecydowałem usunąć dwie spośród tych wartości

# wykrycie reguł, które nie zawierają elementów "relationship=Husband", "marital-status=Married-civ-spouse"
aRules111 <- apriori(adultTR, aParam,
  appearance = list(none = c("relationship=Husband", "marital-status=Married-civ-spouse")))

length(aRules111) # 769

aRules111 <- aRules111[aRules111@quality$lift > 1.1]

length(aRules111) # 12
inspect (aRules111)

# Spośród uzyskanych reguł wybieram te które mają współczynnik poprawy
# z przedziału (0.1, 1>
resTbl <- interestMeasure(aRules111,"improvement", asets)
intres <- which(sapply(resTbl, function(x) {x > 0.1  && x <= 1 })==TRUE)

aRules111 <- aRules111[intres]
length(aRules111) # 8

# Wybieram reguły oparte o częste zbiory maksymalne
aRules111 <- aRules111[is.maximal(aRules111) == TRUE]

length(aRules111) # 8
size(aRules111)
summary(aRules111)
inspect(aRules111)

# Najważniejsze wnioski:
# 1. Osoby niezamęzne częściej pracują na własny rachunek (i vice versa)
# 2. Kobiety częściej mają niskie dochody (i vice versa)
# 3. Osoby pracujące na cały etat i nie mające dochodóW kapitałowych częściej mają niskie dochody
# 4. Osoby pracujące na cały etat i nie mające dochodóW kapitałowych częściej są kobietami
# 5. Osoby pracujące na własny rachunek i nie mające dochodóW kapitałowych częściej mają niskie dochody

# Można zauważyć pozytywną zależność pomiędzy grupą wartości:
# 1. płeć = kobieta; dochód = niski; długość czasu pracy = pełen etat; praca = na własny rachunek, dochody lub straty kapitałowe = brak
# 2. stan cywilny = nieżonaty/niezamężna; dochód=mały
################################################################################################################################

# Ustawienie parametrów do funkcji apriori
# b) Wsparcie > 0.1, zaufanie > 0.1, lift > 1.2 (suffix = 222)

aParam  = new("APparameter", "confidence" = 0.1, "support" = 0.1, "minlen"= 2, maxtime = 20, target ="rules")
# print(aParam)

aRules222 <- apriori(adultTR, aParam)
length(aRules222) # 4815

aRules222 <- aRules222[aRules222@quality$lift > 1.2]

length(aRules222) # 595
inspect (aRules222)

# Większość reguł opiera się na silnej korelacji wartości "relationship=Husband", "sex=Male" oraz marital-status=Married-civ-spouse
# wobec czego zdecydowałem usunąć dwie spośród tych wartości

# wykrycie reguł, które nie zawierają elementów "relationship=Husband", "marital-status=Married-civ-spouse", "marital-status=Never-married"
aRules222 <- apriori(adultTR, aParam,
                              appearance = list(none = c("relationship=Husband", "marital-status=Married-civ-spouse",
                                                         "marital-status=Never-married")))
length(aRules222) # 3023

aRules222 <- aRules222[aRules222@quality$lift > 1.2]

length(aRules222) # 48
# size(aRules222)
# summary(aRules222)
inspect (aRules222)

# Spośród uzyskanych reguł wybieram te które mają współczynnik poprawy
# z przedziału (0.1, 1>
resTbl <- interestMeasure(aRules222,"improvement", asets)
intres <- which(sapply(resTbl, function(x) {x > 0.1  && x <= 1 })==TRUE)

aRules222 <- aRules222[intres]
length(aRules222) # 24
size(aRules222)
inspect(aRules222)

#reguły oparte o częste zbiory maksymalne
aRules222 <- aRules222[is.maximal(aRules222) == TRUE]
length(aRules222) # 24
inspect(aRules222)

# Najważniejsze wnioski (nietrywialne i które nie zostały wyszczególnione w poprzednim przykładzie)
# 1. Biali mężczyźni częściej osiągają wysokie dochody
# 2. Biali mężczyźni, pochodzący ze Stanów Zjednoczonych częściej osiągają wysokie dochody
# 3. Młode osoby (nieosiągające dochodów lub kapitałowych) częściej mają niskie dochody
# 4. Mężczyźni w średnim wieku częściej pracują w nadgodzinach
# 5. Białe kobiety (pochodzące ze Stanów Zjednoczonych) częściej nie zakładają rodzin
# 6. Osoby, które pracują na własny rachunek i nie osiągają dochodów oraz strat kapitałowych częściej są młode
# 7. Biali mężczyźni pochodzący z USA częściej pracują w nadgodzinach
# 8. Osoby pochodzące z USA, pracujące na własny rachunek częściej są płci żeńskiej
################################################################################################################################

# Ustawienie parametrów do funkcji apriori
# c) Wsparcie > 0.01, zaufanie > 0.1, lift > 5 (suffix = 333)

aParam  = new("APparameter", "confidence" = 0.1, "support" = 0.01, "minlen"= 2, maxtime = 20, target ="rules")
# print(aParam)

aRules333 <- apriori(adultTR, aParam)
length(aRules333) # 263308

# Korzystając z doświadczeń poprzednich przykłądów od razu usunę elementy prowadzące do trywialnych wniosków
# Wykrycie reguł, które nie zawierają elementów "relationship=Husband", "marital-status=Married-civ-spouse", "marital-status=Never-married"
aRules333 <- apriori(adultTR, aParam,
                              appearance = list(none = c("relationship=Husband", "marital-status=Married-civ-spouse",
                                                         "marital-status=Never-married")))
length(aRules333) # 147300

aRules333 <- aRules333[aRules333@quality$lift > 5]

length(aRules333) # 93
# inspect (aRules333)

# Spośród uzyskanych reguł wybieram te które mają współczynnik poprawy
# z przedziału (0.1, 1>
resTbl <- interestMeasure(aRules333,"improvement", asets)
intres <- which(sapply(resTbl, function(x) {x > 0.1  && x <= 1 })==TRUE)

aRules333 <- aRules333[intres]
length(aRules333) # 17

#reguły oparte o częste zbiory maksymalne
aRules333 <- aRules333[is.maximal(aRules333) == TRUE]
length(aRules333) # 15
size(aRules333)
summary(aRules333)
inspect(aRules333)

# Najważniejsze wnioski (nietrywialne i które nie zostały wyszczególnione w poprzednich przykładzach)
# 1. Kobiety osiągające wysokie dochody są częściej zamężne
# 2. Osoby pracujące w nadgodzinach, o dużych dochodach, nie osiągające strat kapitałowych częściej osiągają wysokie dochody kapitałowe
# 3. Osoby o dużych dochodach pochodzące z USA, nie osiągające strat kapitałowych częściej osiągają wysokie dochody kapitałowe
# 4. Osoby rasy białej, pracujące w sektorze prywatnym o dużych dochodach, nie osiągające strat kapitałowych częściej osiągają wysokie dochody kapitałowe
################################################################################################################################

### 3. Reguły z wybranym następnikiem ###
# Następnik to wartości zmiennej income

# Ustawienie parametrów do funkcji apriori
# Wsparcie > 0.1, zaufanie > 0.1, lift > 1.1

aRules_income <- apriori(adultTR, parameter = list(support=0.1, confidence = 0.1, minlen = 2),
                        appearance = list(rhs = c("income=small", "income=large")))
length(aRules_income) # 254
aRules_income <- aRules_income[aRules_income@quality$lift > 1.1]

length(aRules_income) # 76

# Spośród uzyskanych reguł wybieram te które mają współczynnik poprawy
# z przedziału (0.1, 1>
resTbl <- interestMeasure(aRules_income,"improvement", asets)
intres <- which(sapply(resTbl, function(x) {x > 0.1  && x <= 1 })==TRUE)

aRules_income <- aRules_income[intres]
length(aRules_income) # 11
size(aRules_income)
inspect(aRules_income)

#reguły oparte o częste zbiory maksymalne
aRules_income <- aRules_income[is.maximal(aRules_income) == TRUE]
length(aRules_income) # 11
inspect(aRules_income)

# Najważniejsze wnioski:
# 1. Osoby w związku małżeńskim osiągają częściej wysokie dochody
# 2. Mężczyźni osiągają częściej wysokie dochody
# 3. Młode osoby osiągają częściej niskie dochody
# 4. Osoby nie posiadające rodzin osiągają częściej niskie dochody
# 5. Osoby nie będące w związku małżeńskim osiągają częściej niskie dochody
# 6. Kobiety osiągają częściej niskie dochody
# 7. Osoby rasy białej pochodzące z USA osiągają częściej wysokie dochody
# 8. Osoby, które ukończyły liceum i nie posiadają dochodów kapitałowych osiągają częściej niskie dochody
# 9. Osoby pracujące na cały etat w sektorze prywatnym, nie posiadające dochodów kapitałowych osiągają częściej niskie dochody
# 10. Osoby pracujące na cały etat, nie posiadające dochodów ani strat kapitałowych osiągają częściej niskie dochody
# 11. Osoby pracujące w sektorze prywatnym, nie posiadające dochodów ani strat kapitałowych osiągają częściej niskie dochody
################################################################################################################################

### 4. Ocena praktycznej użyteczności uzyskanych reguł ###

# Przykładowe zastosowania praktyczne znalezionych reguł:
# a) działania marketingowo - sprzedażowe (np. produkty inwestycyjne, produkty luksusowe)
# b) polityka społeczna (np. pomoc społeczna, polityka rodzinna, polityka zatrudnienia)

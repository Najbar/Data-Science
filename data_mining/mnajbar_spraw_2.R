################################################################################################################################
# Marcin Najbar
# DataScience  - data mining 2021L Sprawozdanie 2
# dataset: Uber
################################################################################################################################

library(dbscan)
library(fpc)
library(cluster)
library(factoextra)
library(corrplot)
library(ggplot2)
################################################################################################################################
# 1. Przygotowanie danych

# opis: https://www.kaggle.com/fivethirtyeight/uber-pickups-in-new-york-city/data
# download.file('http://staff.ii.pw.edu.pl/~rbembeni/dane/uber-raw-data-may14.csv','UberMay14.csv')
uber <- read.csv("UberMay14.csv")
# View(uber)
# head(uber)

# Ustawienie ziarna losowego
set.seed(7777)

################################################################################################################################
# Sprawdzenie jakiego rodzaju są atrybuty i czy występują braki danych

# Sprawdzenie czy występują braki danych --> Nie występują
summary(uber)
str(uber)

any(is.na(uber))
anyNA(uber)
sum(is.na(uber))

# Sprawdzenie czy zmienne użyte do grupowania są skończone --> Są skończone
sum(is.finite(uber$Lat))
sum(is.finite(uber$Lon))

################################################################################################################################
# Uzyskanie dodatkowych atrybutów na podstawie zmiennej Date.Time
# (nie wykorzystywałem tych danych później)

#Zmiana formatu daty
uber$Date.Time <- strptime(uber$Date.Time, format = "%m/%d/%Y %H:%M:%S")
typeof(uber$Date.Time)

# Stworzenie kolumn dla miesiąca, dnia, dnia tygodnia, godziny, minuty, sekundy
# Dane dotyczą maja 2014, a więc miesiąc przyjmuje tylko jedną wartość
# Z kolei dane oznaczające minutę lub sekundę są zbyt szczegółowe i przedstawiają wartości w dalszej analizie

# uber$Month <- factor(month(uber$Date.Time))
# uber$Day <- as.integer(format(uber$Date.Time, "%d"))
uber$Week_day <- as.integer(format(uber$Date.Time, "%w"))
uber$Hour <- as.integer(format(uber$Date.Time, "%H"))
# uber$Minute <- as.integer(format(uber$Date.Time, "%M"))
# uber$Second <- as.integer(format(uber$Date.Time, "%S"))
summary(uber)

################################################################################################################################
# Dane do grupowania.
# Usunięcie atrybutów dotyczących daty i bazy.
uber2 <- uber[c(2, 3)]
summary(uber2)
dim(uber2)

################################################################################################################################
# 2. Grupowanie algorytmem partycjonującym k-means

# Znajdowanie optymalnej liczby grup - metoda 'łokcia'
nrow(uber2)
sample_size <- nrow(uber2) * 0.25
sample_size # sample_size = 163108.8

# Uwaga: Wykonanie poniższej pętli może trwać do kilku minut
for (i in 1:4) {
  sample <- sample.int(nrow(uber2), sample_size)
  uber2_sample <- (uber2[sample,])
  wss <- 0
  for (i in 1:15) 
  {
    km.out <- kmeans(uber2_sample, centers = i, nstart=20)
    wss[i] <- km.out$tot.withinss
  }
  plot(1:15, wss, type = "l",  xlab = "Liczba grup", ylab = "Suma bledu kwadratowego wewnatrz grup")
}

################################################################################################################################
# Na podstawie metody łokcia ustalenie optymalnej liczby grup nie jest jednoznaczne.
# Można zauważyć, że jest to liczba z przedziału od 2 do 7.
# Analizę będę prowadził dla 5 grup i dla 4 różnych algorytmów


algs = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")

par(mfrow = c(2, 2))
for(i in 1:4)
{
  uber2.kmeans = kmeans(uber2, 5, nstart = 1,
                           iter.max = 30, algorithm = algs[i])
  plot(uber2, col = uber2.kmeans$cluster, 
       main = paste(algs[i], round(uber2.kmeans$tot.withinss)), 
       xlab = "Latitude", ylab = "Longitude")
  points(uber2.kmeans$centers, col = 'white', pch = 10, cex=2)
  #  print(uber2.kmeans)
}

################################################################################################################################
# Zastosowanie wybranego algorytmu k-means ("Hartigan-Wong")
# oraz przypisanie poszczególnych rekordów do grup
dim(uber2)
uber2.kmeans = kmeans(uber2, 5, nstart = 1, iter.max = 20)

#pozyskanie informacji nt. grupowania
print(uber2.kmeans)
print(uber2.kmeans$iter)
print(uber2.kmeans$centers)
print(uber2.kmeans$cluster)

### Przypisanie poszczególnych rekordów do grup
uber2$cluster <- uber2.kmeans$cluster
uber$cluster <- uber2.kmeans$cluster
head(uber2)

################################################################################################################################
# Wskaźnik silhouette

si_kmeans <- silhouette(uber2$cluster, dist(uber2))
# Error: cannot allocate vector of size 1585.7 Gb

#sample
sample <- sample.int(nrow(uber2), 0.01 * nrow(uber2))
uber2_sample <- (uber2[sample,])
dim(uber2_sample)

# pierwszy sposób przez silhouette

si_kmeans <- silhouette(uber2_sample$cluster, dist(uber2_sample))

# class(si_kmeans)
# typeof(si_kmeans)
# dim(si_kmeans)

# Wskaźniki Silhouette dla poszczególnych rekordów
print(si_kmeans)

# Wskaźnik GSilhouette
mean(si_kmeans[, 3]) # ok. 97%

# drugi sposób przez cluster.stats
cs <- cluster.stats(dist(uber2_sample), uber2_sample$cluster)
cs$avg.silwidth # ok. 97% (identyczny wynik jak w poprzednim sposobie) 

# Inne współczynniki:
# cs$average.between
# cs$average.within
# cs$within.cluster.ss
# (cs$average.between - cs$average.within) / cs$average.between
# cs[c("within.cluster.ss","avg.silwidth")]

################################################################################################################################
# wizualizacja

# plot(uber2[1:2])
plot(uber2[1:2], col=uber2$cluster)

# liczebność grup
table(uber2[3])
################################################################################################################################
### Znalezienie charakterystycznych elementów grup
print(uber2.kmeans$centers)

plot(uber2[1:2], col=uber2$cluster)
points(uber2.kmeans$centers, col = 'white', pch = 10, cex=2)

################################################################################################################################
# Mapa Nowego Jorku
# https://en.wikipedia.org/wiki/Boroughs_of_New_York_City#/media/File:5_Boroughs_Labels_New_York_City_Map.svg

################################################################################################################################
## 3. Grupowanie gęstościowe: algorytm dbscan

# Próbka 10% populacji
sample <- sample.int(nrow(uber2), 0.1 * nrow(uber2))
uber2_sample <- (uber2[sample,])

# Wyznaczenie parametru eps dla algorytmu DBSCAN
# metodą szukania punktu przegięcie z wykorzystaniem
# 10% losowo wybranych danych – sprawdzenie dla kilku wartości K

par(mfrow = c(1, 1))

for (i in 1:5) {
  dbscan::kNNdistplot(uber2_sample, k=i)
}

# Z wykresów niewiele można odczytać (niezależnie od wielkości próby) 
# więc skorzystam z funkcji kNNdist i przeanalizuję dane wynikowe

for (i in 1:10) {
  dist <- dbscan::kNNdist(uber2_sample, k=i)
  print(c(i, mean(dist)))
}

for (i in 1:10) {
  dist <- dbscan::kNNdist(uber2, k=i)
  print(c(i, mean(dist)))
}

# [1] 1.000000e+00 5.724381e-05
# [1] 2.000000e+00 9.785187e-05
# [1] 3.0000000000 0.0001323879
# [1] 4.0000000000 0.0001638688
# [1] 5.0000000000 0.0001934747
# [1] 6.000000000 0.000218465
# [1] 7.0000000000 0.0002412419
# [1] 8.00000000 0.00026305
# [1] 9.0000000000 0.0002835992
# [1] 1.000000e+01 3.030114e-04

# wykonanie algorytmu dbscan - dane oryginalne
# Uwaga: Wykonanie poniższej linii może trwać bardzo długo
# uber2.dbscan_eps0.005 <- dbscan::dbscan(uber2_sample, eps=0.005, minPts=5)

# Ze względu na ograniczenia sprzętowe zdecydowałem się przeprowadzić analizę
# na 10% próbie danych (epsilon = 0.005, liczba sąsiadów = 5)
uber2.dbscan_eps0.005 <- dbscan::dbscan(uber2_sample, eps=0.005, minPts=5)
uber2.dbscan_eps0.005 # zbyt mały epsilon --> aż 63 grupy

# 10% próba danych (epsilon = 0.05, liczba sąsiadów = 5)
uber2.dbscan_eps0.05 <- dbscan::dbscan(uber2_sample, eps=0.05, minPts=5)
uber2.dbscan_eps0.05 # 5 grup, ale duża dysproporcja w liczebności

# 10% próba danych (epsilon = 0.1, liczba sąsiadów = 10)
uber2.dbscan_eps0.1 <- dbscan::dbscan(uber2_sample, eps=0.1, minPts=10)
uber2.dbscan_eps0.1 # tylko 2 grupy i duża dysproporcja w ich liczebności

# 10% próba danych (epsilon = 0.1, liczba sąsiadów = 100)
uber2.dbscan_eps0.1 <- dbscan::dbscan(uber2_sample, eps=0.01, minPts=100)
uber2.dbscan_eps0.1 # 7 grup i spora dysproporcja w ich liczebności

# Przypisanie poszczególnych rekordów do grup
uber2_sample$cluster_dbscan <- uber2.dbscan_eps0.1$cluster
summary(uber2_sample)
plot(uber2_sample[1:2], col = uber2_sample$cluster_dbscan)

#Zmniejszam próbkę w celu policzenia indeksu silhouette
sample <- sample.int(nrow(uber2_sample), 0.1 * nrow(uber2_sample))
uber2_sample <- (uber2_sample[sample,])
dim(uber2_sample)
uber2_sample
# Ocena jakości grupowanie przy użyciu indeksu Silhouette.
cs <- cluster.stats(dist(uber2_sample[1:2]), uber2_sample$cluster_dbscan)
# cs
cs$avg.silwidth # współczynnik sillhouette wynosi tylko ok 42%

################################################################################################################################
# Znalezienie charakterystycznych elementów grup

summary(uber2_sample)
table(uber2_sample$cluster_dbscan)

plot(uber2_sample[1:2], col = uber2_sample$cluster_dbscan)

summary(uber2_sample[uber2_sample$cluster_dbscan == 1,]) 
summary(uber2_sample[uber2_sample$cluster_dbscan == 2,]) 
summary(uber2_sample[uber2_sample$cluster_dbscan == 3,]) 
summary(uber2_sample[uber2_sample$cluster_dbscan == 4,])
summary(uber2_sample[uber2_sample$cluster_dbscan == 5,]) 

# Grupa są różnej wielkości. Większe grupy są zróżnicowane,
# małe grupy są skupione blisko średnich współrzędnych dla grupy

################################################################################################################################
# 4. Porównanie wyników uzyskanych dwoma metodami grupowania

# Wyniki grupowania obu algorytmów są podobne:
table(uber2_sample$cluster, uber2_sample$cluster_dbscan)

# Grupowanie algorytmem k-means przyniosło lepsze rezultaty niż grupowanie
# gęstościowe (dbscan)

# Współczynnik silhoutte wyniósł:
# - w przypadku k-means 97%
# - w przypadku dbcan 42%

# Grupowanie algorytmem k-means przypomina nieco podział na 5 dzielnic
# Nowego Jorku

# Wady grupowania algorytmem dbcan:
# - W grupowaniu gęstościowym występuje znaczna dysproporcja liczebności grup
# - Ze względu na ograniczenia obliczenia przeprowadziłem obliczenia na próbce
# 10% lub 1% danych
# - Sporo obserwacji nie zostało przypisanych do żadnej grupy


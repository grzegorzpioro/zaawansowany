# 1. Ile kolumn kategorycznych
# 2. Ile kolumn liczbowych
# 3. Ile jest kolumn time stampowych
# 4. Jaki jest target zbioru danych (y)
# 5. Jaki typ ma target
# 6.Ilosc obserwacji posiadajacych conajmniej jeden brak w danych
# 7.Ilosc obserwacji gdzie brakuje targetu
# 8.Ile i które kolumny sa statyczne
# 9.Ile i które kolumny sa duplikatami
# 10.Jak mocno skorelowane sa zmienne liczbowe (Pearsona) i kategoryczne (Crammera V)
# 11.Ile i które kolumny sa ID-like (maja id w nazwie / sa takim kluczem glównym, sanumeracja pewnego rodzaju)
# 12.Ile obserwacji zawiera zbiór danych
# 13.Czy dane sa rzeczywiste czy sztucznie wygenerowane
# 14.Krótki opis zbioru danych
# 15.Gdzie znaleziony zostal zbiór danych
# 16.Jaka jest proporcja klas targetu / Jaki jest rozklad wartosci targetu
# 17.Ile unikalnych wartosci (levels) maja poszczególne zmienne kategoryczne
library(dplyr)
df <- read.csv('mushrooms.csv')

ankieta <- function(df){
  #1.
  kat <- 0
  for(i in 1:ncol(df)){
    if(typeof(df[,i]) == 'character'){
      kat = kat+1
    }
  }
  cat(paste("Sa", kat, "kolumny kategoryczne \n"))
  #2.
  licz <- 0
  for(i in 1:ncol(df)){
    if(typeof(df[,i]) == 'numeric'){
      licz = licz + 1
    }
  }
  cat(paste("Jest",licz,"kolumn liczbowych \n"))
  #3.
  cat("Nie ma kolumn time stampowych \n")
  #4.
  cat("Targetem zbioru danych jest class \n")
  #5.
  cat(paste("Typem targetu jest", typeof(df[,1]), "\n"))
  #6.
  cat(paste("Jest", nrow(df) - nrow(na.omit(df)), 'obserwacji posiadajacych co najmniej jeden brak w danych \n'))
}
ankieta(ramka)

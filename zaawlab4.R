library(dplyr)
library(tidyr)
library(ggplot2)
library(PogromcyDanych)
library(stringi)
library(stringr)
#1
auta2012 %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(desc(n))

#2
auta2012 %>% 
  filter(Marka == 'Toyota') %>% 
  group_by(Model) %>% 
  count() %>% 
  arrange(desc(n))

#3
auta2012 %>% 
  filter(Rok.produkcji == 2007,Rodzaj.paliwa=='olej napedowy (diesel)') %>% 
  count()

#4
auta2012 %>% 
  group_by(Kolor) %>% 
  summarise(przebieg = median(Przebieg.w.km, na.rm = TRUE)) %>% 
  arrange(przebieg)

#5
auta2012 %>% 
  filter(Rok.produkcji == 2007) %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(desc(n))
#6
typeof(auta2012$Rok.produkcji)

auta2012 %>% 
  filter(Marka == 'Toyota',Rok.produkcji %in% c(2007,2008)) %>% 
  group_by(Model,Rok.produkcji) %>% 
  summarise(cena = mean(Cena.w.PLN)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Rok.produkcji, values_from = cena) %>% 
  mutate(diff = `2008`-`2007`) %>% 
  select(Model, diff) %>% 
  arrange(desc(diff))
    
#7
auta2012 %>% 
  filter(Rodzaj.paliwa =='olej napedowy (diesel)',
         Rok.produkcji == 2007) %>% 
  group_by(Marka) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  arrange(desc(srednia_cena))
#8
auta2012 %>% 
  filter(str_detect(Wyposazenie.dodatkowe,'klimatyzacja')) %>% 
  count()

#9
auta2012 %>% 
  filter(KM > 100) %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(desc(n))

#10
auta2012 %>% 
  filter(Marka == 'Toyota', Rodzaj.paliwa %in% c('benzyna','olej napedowy (diesel)')) %>% 
  group_by(Model,Rodzaj.paliwa) %>% 
  summarise(srednia = mean(Cena.w.PLN)) %>% 
  pivot_wider(names_from = Rodzaj.paliwa, values_from = srednia) %>% 
  mutate(diff = abs(benzyna - `olej napedowy (diesel)`)) %>% 
  select(Model, diff) %>% 
  arrange(desc(diff))

#11
auta2012 %>% 
  filter(Rok.produkcji == 2007,Rodzaj.paliwa =='olej napedowy (diesel)') %>% 
  group_by(Marka) %>% 
  summarise(srednia = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(srednia)

#12
auta2012 %>% 
  group_by(Marka) %>% 
  summarise(procent = sum(str_count(Wyposazenie.dodatkowe,'klimatyzacja'))/n()) %>% 
  arrange(desc(procent))

#13
auta2012 %>% 
  filter(Cena.w.PLN>50000) %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(desc(n))

#14
auta2012 %>% 
  filter(Marka == 'Toyota') %>% 
  group_by(Model) %>% 
  summarise(przebieg = median(Przebieg.w.km, na.rm = TRUE)) %>% 
  arrange(desc(przebieg))

#15
auta2012 %>% 
  filter(Rok.produkcji==2007,Rodzaj.paliwa =='olej napedowy (diesel)') %>% 
  group_by(Model) %>% 
  summarise(cena = mean(Cena.w.PLN)) %>% 
  arrange(desc(cena))

#16
auta2012 %>% 
  group_by(Model) %>% 
  summarise(procent = sum(str_count(Wyposazenie.dodatkowe,'klimatyzacja'))/n()) %>% 
  arrange(desc(procent))
#17
auta2012 %>% 
  filter(Cena.w.PLN<50000) %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(desc(n))

#18  
auta2012 %>% 
  filter(Marka == 'Toyota', Rok.produkcji==2007) %>% 
  group_by(Model) %>% 
  summarise(cena = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(desc(cena))
#19
auta2012 %>% 
  filter(Rok.produkcji==2007,Rodzaj.paliwa =='olej napedowy (diesel)') %>% 
  group_by(Model) %>% 
  summarise(cena = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(cena)
#20

auta2012 %>% 
  group_by(Kolor) %>% 
  summarise(przebieg = median(Przebieg.w.km, na.rm = TRUE)) %>% 
  arrange(desc(przebieg))






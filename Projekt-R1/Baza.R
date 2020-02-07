  setwd("/home/makbet/ProjektR")
  #Pierwszym krokiem będzie ściągnięcie niezbędnych bibliotek, które pomogą stworzyć odpowiednie pliki oraz wykresy
  library(xlsx)
  library(ggplot2)
  library(dplyr)
  library(hrbrthemes)
  library(data.table)
  
#W tym etapie ładuję plik do odczytu i przekształcam go w data frame, by ułatwić sobie pracę z danymi 
  dane<-read.xlsx("ceny.xlsx", sheetIndex=2,stringsAsFactors=FALSE)
  dane<-data.frame(dane)
  dane$Wartosc<-as.numeric(dane$Wartosc)
  
#Ten etap jest jednym z najważnieszjych - zbieram średnią cenę wszystkich towarów i łącze je w średnią rocznę zakupu. Dane zapisuję do pliku
  wojewodztwa_ceny<-tapply(dane$Wartosc,list(dane$Rok,dane$Nazwa),FUN=mean,na.rm=TRUE)
  wojewodztwa_ceny<-data.frame(wojewodztwa_ceny)
  print(wojewodztwa_ceny)
  write.xlsx((format(wojewodztwa_ceny, digits=3)),"Dane_usrednione.xlsx")
  
  #Teraz policzymy srednia cene krajowa w ciagu kazdego roku
TowarRok<-tapply(dane$Wartosc,list(dane$Rok,dane$Towar),FUN=mean,na.rm=TRUE)
TowarRok<-data.frame(TowarRok,stringsAsFactors = FALSE)

#Zbieram średnią cenę z każdego roku do listy
lista_srednich_cen<-c(
  
  Srednia2006<-mean(as.numeric(TowarRok[c(1),c(1:10)]),na.rm=TRUE),

  Srednia2007<-mean(as.numeric(TowarRok[c(2),c(1:10)]),na.rm=TRUE),

  Srednia2008<-mean(as.numeric(TowarRok[c(3),c(1:10)]),na.rm=TRUE),

  Srednia2009<-mean(as.numeric(TowarRok[c(4),c(1:10)]),na.rm=TRUE),

  Srednia2010<-mean(as.numeric(TowarRok[c(5),c(1:10)]),na.rm=TRUE),

  Srednia2011<-mean(as.numeric(TowarRok[c(6),c(1:10)]),na.rm=TRUE),

  Srednia2012<-mean(as.numeric(TowarRok[c(7),c(1:10)]),na.rm=TRUE),

  Srednia2013<-mean(as.numeric(TowarRok[c(8),c(1:10)]),na.rm=TRUE),

  Srednia2014<-mean(as.numeric(TowarRok[c(9),c(1:10)]),na.rm=TRUE),

  Srednia2015<-mean(as.numeric(TowarRok[c(10),c(1:10)]),na.rm=TRUE),

  Srednia2016<-mean(as.numeric(TowarRok[c(11),c(1:10)]),na.rm=TRUE),

  Srednia2017<-mean(as.numeric(TowarRok[c(12),c(1:10)]),na.rm=TRUE),

  Srednia2018<-mean(as.numeric(TowarRok[c(13),c(1:10)]),na.rm=TRUE),

  Srednia2019<-mean(as.numeric(TowarRok[c(14),c(1:10)]),na.rm=TRUE)
)
#Na potrzeby wykresów i ramek, tworzę listę z latami od 2006 do 2019
lata<-c(2006:2019)

lista_srednich<-data.frame(
  lata=c(lata),
  ceny=c(lista_srednich_cen)
)     
print(lista_srednich)

#Zmieniam kolumny z wierszami, aby ułatwić robienie wykresów
Roczne_ceny<-t(wojewodztwa_ceny)
Roczne_ceny<-as.data.frame(Roczne_ceny)
nazwy<-row.names(Roczne_ceny)

#Dzieki tej części, będę mógł wykazać gdzie i kiedy było najdrożej, a gdzie i kiedy najtaniej
nazwy_do_listy<-rep(nazwy,each=14)
minmax<-data.frame(
  wojewodztwa=c(nazwy_do_listy),
  wartosci=c(wojewodztwa_ceny$DOLNOŚLĄSKIE,wojewodztwa_ceny$KUJAWSKO.POMORSKIE,wojewodztwa_ceny$LUBELSKIE,wojewodztwa_ceny$LUBUSKIE,
             wojewodztwa_ceny$ŁÓDZKIE, wojewodztwa_ceny$MAŁOPOLSKIE,wojewodztwa_ceny$MAZOWIECKIE,wojewodztwa_ceny$OPOLSKIE,
             wojewodztwa_ceny$PODKARPACKIE,wojewodztwa_ceny$PODLASKIE,wojewodztwa_ceny$POMORSKIE,wojewodztwa_ceny$ŚLĄSKIE,
             wojewodztwa_ceny$ŚWIĘTOKRZYSKIE,wojewodztwa_ceny$WARMIŃSKO.MAZURSKIE,wojewodztwa_ceny$WIELKOPOLSKIE,
             wojewodztwa_ceny$ZACHODNIOPOMORSKIE),
  lata=c(lata)
)
DT<-data.table(minmax)


#CZAS NA PODSUMOWANIE
#Ponizej zalaczam plik ze statystcznym podsumowaniem kazdego wojewodztwa
WC<-summary(wojewodztwa_ceny)
print(WC)
write.xlsx(as.matrix(WC),"wojewodztwa-podsumowanie-statystyczne.xlsx")


#Teraz będzie lista najniższych cen względem 13 lat  w każdym województwie
najnizszeCeny<-as.data.frame(DT[ , .SD[which.min(wartosci)], by = wojewodztwa])
najnizszeCeny<-najnizszeCeny[order(najnizszeCeny$wartosci),]
write.xlsx(najnizszeCeny,"Najniższe ceny - województwa.xlsx")

#Czas sprawdzić jaka wartość była najniższą
Najnizsza_Cena_W_Polsce<-subset(minmax,wartosci==min(wartosci))
print(Najnizsza_Cena_W_Polsce)


#Wiemy już, że w Śląsku w 2006 roku średnia cena była najniższa. Jak wypadła reszta województw w okresie 13 lat?
#png(file="Wykres najnizszych cen.png",width=700,height=500)
ggplot(najnizszeCeny,aes(x=reorder(wojewodztwa,-wartosci), y=wartosci)) +
  geom_bar(stat = "identity", width=0.5,fill="deepskyblue1")+
  geom_text(aes(label=sprintf("%0.2f", round(wartosci, digits = 2))), vjust=0.5, color="black", size=5,position = position_stack(vjust = 0.1))+
  geom_text(aes(label=lata),vvjust=0.5, color="black", size=5,position = position_stack(vjust = 0.5))+
  coord_flip()+
  ggtitle("Najniższe ceny towarów w województwie w okresie 2006-2019")+
  theme_minimal()
#dev.off()

#Teraz lista najwyższych cen
najwyzszeCeny<-as.data.frame(DT[ , .SD[which.max(wartosci)], by = wojewodztwa])
najwyzszeCeny<-najwyzszeCeny[order(najwyzszeCeny$wartosci),]
write.xlsx(najwyzszeCeny,"Najwyższe ceny w województwach.xlsx")

#Czas sprawdzic najwyzsza wartosc w okresie 13 lat
print(subset(minmax,wartosci==max(wartosci)))
 
#A terazw wykres przedstawiający wartości największych cen w okresie 2006-2019 na obszarze 16 województw
#pdf(file="Wykres najwyzszych cen.pdf",width=700,height=500)
ggplot(najwyzszeCeny,aes(x=reorder(wojewodztwa,wartosci), y=wartosci)) +
  geom_bar(stat = "identity", width=0.5,fill="#E69F00")+
  geom_text(aes(label=sprintf("%0.2f", round(wartosci, digits = 2))), vjust=0.5, color="black", size=5,position = position_stack(vjust = 0.1))+
  geom_text(aes(label=lata),vvjust=0.5, color="black", size=5,position = position_stack(vjust = 0.5))+
  coord_flip()+
  ggtitle("Najwyzsze ceny towarów w województwie w okresie 2006-2019")+
  theme_minimal()
#dev.off()

#Poniższy wykres przedstawia zmianę krajowych cen omawianych produktów i usług w okresie 2006-2019 na obszarze całe Polski
#png(file="Wykres cen w Polsce.png",width=700,height=500)
ggplot(lista_srednich, aes(x=lata, y=ceny)) +
  geom_line( color="grey") +
  theme_ipsum_rc()+
  geom_point(shape=4, color="black", fill="#69b3a2", size=6) +
  ggtitle("Zmiana srednich cen w Polsce w latach 2006-2019")+
  ylab("Ceny w zł")+
  scale_x_continuous(breaks = round(seq(min(lista_srednich$lata), max(lista_srednich$lata), by = 1),1))+
  scale_y_continuous(breaks = round(seq(min(lista_srednich$ceny), max(lista_srednich$ceny), by = 5),1)) 
#dev.off()

#Zgodnie z powyższym wykresem, jeden z największych wzrost cen można zaobserwować od 2016 roku
#Wiąże się to z wdrożeniem wielu programów socjalnych, w tym 500+
#Poniżej natomiast znajduje się wartość korelacji między minialnym wynagrodzeniem w danym roku a ceną towarów

#korelacja między cenami a średnią wynagrodzenia
srednie_wynagrodzenie<-c(899,936,1126,1276,1317,
                         1386,1500,1600,1680 ,1750,
                         1850,2000,2100,2250)
ceny_a_wynagrodzenie<-data.frame(
  lata=c(lata),
  wynagrodzenie=c(srednie_wynagrodzenie),
  ceny=c(lista_srednich_cen)
)
korelacja<-cor(ceny_a_wynagrodzenie$ceny,ceny_a_wynagrodzenie$wynagrodzenie)
print(korelacja)
#Korelacja jest bardzo wysoka, więc sprawdzę teraz regresję liniową

regresja<-lm(ceny_a_wynagrodzenie$ceny~ceny_a_wynagrodzenie$wynagrodzenie)

plot(ceny_a_wynagrodzenie$wynagrodzenie,ceny_a_wynagrodzenie$ceny, col="blue", main="Regresja wynagrodzenie~ceny",
     abline(regresja),cex=1.3, pch= 16, xlab= "minimalne wynagrodzenie", ylab="ceny")

#Jasno z tego wynika, że wraz z nagłym wzrostem płac i programów socjalnych, wzrasta cena produktów, a tym samym wzrasta inflacja
#Wnioski - dzięki odpowiedniej przeróbce danych, można uzyskać odpowiedzi na wiele pytań - od średniej ceny produktów, po wzrost miesięcznej ceny cytryny
#Jednak najważniejszym wnioskiem jest fakt, że ceny towarów rosną, zarobki również
#W niedalekiej przyszłości może to doprowadzić do dużej inflacji, a ceny wrócą do wartości sprzed denominacji z 1995 roku
#Brak danych niektórych cen rozwiązałem innym plikiem GUS-owski, na którym te dane się znajdowały.
#Innym rozwiązaniem byłaby predykcja przyszłych cen na bazie dostępnych
  
#Teraz czas na pytanie - co jeszcze można zrobić z tymi danymi?
#Można zebrać dane średniej ceny ARTYKUŁÓW SPOŻYWCZYCH z każdego roku dla każdego województwa
przyklad1<-tapply(dane$Wartosc,list(dane$Towar,dane$Nazwa,dane$Rok),FUN=mean,na.rm=TRUE)
przyklad1<-data.frame(przyklad1,stringsAsFactors = FALSE) 
przyklad1<-data.frame(przyklad1[-c(7,9,10),],stringsAsFactors = FALSE)#Tutaj usuwam węgiel oraz spodnie i pranie
write.xlsx(przyklad1,file="Srednie ceny artykułów spożywczych.xlsx")

listaProduktow<-row.names(przyklad1) #Tutaj jest uniwersalna lista nazw
#Przykład zrobimy dla Dolnego Śląska z 2006 roku
przyklad1_Dolnys2006<-data.frame(produkt=c(listaProduktow),ceny=przyklad1$DOLNOŚLĄSKIE.2006,stringsAsFactors = FALSE)
ggplot(przyklad1_Dolnys2006,aes(x=produkt, y=ceny)) +
     geom_bar(stat = "identity", width=0.4,fill="red")+
     geom_text(aes(label=sprintf("%0.2f", round(ceny, digits = 2))), vjust=0.5, color="white", size=4.5,position = position_stack(vjust = 0.5))+
     ylab("Ceny w zł")+
     coord_flip()+
     ggtitle("Srednie ceny produktow w Dolnyśląsk 2006")+
     theme_minimal()

#Teraz spróbujemy określić jak zmieniała się cena towaru w skali kraju w przeciągu 13 lat
przyklad2<-tapply(dane$Wartosc,list(dane$Rok,dane$Towar),FUN=mean,na.rm=TRUE)
przyklad2<-data.frame(przyklad2)
write.xlsx(przyklad2,file="Ceny poszczególnych towarów na przestrzeni 13 lat.xlsx")
#Mamy ceny kazdego towaru w każdym roku, teraz czas na przykładowy wykres

#Dla cytryny 
ggplot(przyklad2,aes(x=wartosci, y=lata))+
  geom_line(aes(x=as.numeric(przyklad2$czekolada.mleczna...za.100g), colour="red"))+
  ggtitle("Zmiana ceny cytryny w latach 2006-2019")+
  xlab("Cena w zł")
  
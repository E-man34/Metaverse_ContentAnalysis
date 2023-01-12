setwd("~/Desktop/Quaerys/Metaverso")


# CARICAMENTO LIBRERIE

library(rtweet)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(tm)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(stringr)
library(openxlsx)
library(tuber)
library(syuzhet)
library(topicmodels)
library(tibble)
library(reshape2)
library(wordcloud2)
library(tidyr)
library(readxl)

install.packages("devtools")
devtools::install_github("hadley/emo")
library(emo)



#SELEZIONARE SOLO ALCUNE COLONNE DATASET
RDD_EN_comm <- select(Metaverso_RDD_EN_com, "comment")
RDD_EN <- select(Metaverso_RDD_EN, "title","text")
RDD_IT_comm <- select(Metaverso_RDD_IT_com, "comment")
RDD_IT <- select(Metaverso_RDD_IT, "title","text")
YT_EN_comm <- select(Metaverso_YT_EN_commenti, "textOriginal")
YT_EN <- select(Metaverso_YT_EN, "title","description")
YT_IT_comm <- select(Metaverso_YT_IT_com, "textOriginal")
YT_IT <- select(Metaverso_YT_IT, "title","description")

Metaverse <- select(Metaverse, "textDisplay","textOriginal")
Meta <- select(Meta,"textDisplay","textOriginal")



# --------------------- MANIPOLAZIONE DATASET --------------------------- 


#UNIONE 2 COLONNE IN 1
RDD_EN <- unite(RDD_EN,
                col = "comment", #colonna da creare
                c("title","text"), #colonne da mantenere
                sep = " ", #separatore
                remove = TRUE, #mantenere le colonne iniziali + colonna extra
                na.rm = TRUE) #non prendere in considerazione le celle vuote (NA)

RDD_IT <- unite(RDD_IT,
                col = "comment", #colonna da creare
                c("title","text"), #colonne da mantenere
                sep = " ", #separatore
                remove = TRUE, #mantenere le colonne iniziali + colonna extra
                na.rm = TRUE) #non prendere in considerazione le celle vuote (NA)


YT_EN <- unite(YT_EN,
                col = "comment", #colonna da creare
                c("title","description"), #colonne da mantenere
                sep = " ", #separatore
                remove = TRUE, #mantenere le colonne iniziali + colonna extra
                na.rm = TRUE) #non prendere in considerazione le celle vuote (NA)


YT_IT <- unite(YT_IT,
               col = "comment", #colonna da creare
               c("title","description"), #colonne da mantenere
               sep = " ", #separatore
               remove = TRUE, #mantenere le colonne iniziali + colonna extra
               na.rm = TRUE) #non prendere in considerazione le celle vuote (NA)


Metaverse <- unite(Metaverse,
               col = "text", #colonna da creare
               c("textDisplay","textOriginal"), #colonne da mantenere
               sep = " ", #separatore
               remove = TRUE, #mantenere le colonne iniziali + colonna extra
               na.rm = TRUE) #non prendere in considerazione le celle vuote (NA)


Meta <- unite(Meta,
               col = "text", #colonna da creare
               c("textDisplay","textOriginal"), #colonne da mantenere
               sep = " ", #separatore
               remove = TRUE, #mantenere le colonne iniziali + colonna extra
               na.rm = TRUE) #non prendere in considerazione le celle vuote (NA)



#RINOMINO COLONNE
colnames(YT_EN_comm)<- c("comment")
colnames(YT_IT_comm)<- c("comment")



#UNIONE DATASET
Reddit_EN <- rbind(RDD_EN_comm, RDD_EN)
Reddit_IT <- rbind(RDD_IT_comm, RDD_IT)
Youtube_EN <- rbind(YT_EN_comm, YT_EN)
Youtube_IT <- rbind(YT_IT_comm, YT_IT)





# --------------------- PULIZIA DATASET --------------------------- 

# Reddit_EN
Reddit_EN$comment <- gsub("#\\$+", "", Reddit_EN$comment) #rimuovere hastag 
Reddit_EN$comment <- gsub("https\\$*", "", Reddit_EN$comment) #rimuovere link
Reddit_EN$comment <- gsub("@\\$+", "", Reddit_EN$comment) #rimuovere menzioni
Reddit_EN$comment <- gsub("amp", "", Reddit_EN$comment) #rimuovere alcuni caratteri speciali
Reddit_EN$comment <- gsub("[\r\n]", "", Reddit_EN$comment) #rimuovere interruzioni di riga
Reddit_EN$comment <- gsub("[[:punct:]]", "", Reddit_EN$comment) #rimuovere segni di punteggiatura 
Reddit_EN$comment <- gsub("[[:digit:]]", "", Reddit_EN$comment) #rimuovere numeri
Reddit_EN$comment <- gsub("[^\x01-\x7F]", "", Reddit_EN$comment) #rimuovere caratteri non ASCII


# Reddit_IT
Reddit_IT$comment <- gsub("#\\$+", "", Reddit_IT$comment) #rimuovere hastag 
Reddit_IT$comment <- gsub("https\\$*", "", Reddit_IT$comment) #rimuovere link
Reddit_IT$comment <- gsub("@\\$+", "", Reddit_IT$comment) #rimuovere menzioni
Reddit_IT$comment <- gsub("amp", "", Reddit_IT$comment) #rimuovere alcuni caratteri speciali
Reddit_IT$comment <- gsub("[\r\n]", "", Reddit_IT$comment) #rimuovere interruzioni di riga
Reddit_IT$comment <- gsub("[[:punct:]]", "", Reddit_IT$comment) #rimuovere segni di punteggiatura 
Reddit_IT$comment <- gsub("[[:digit:]]", "", Reddit_IT$comment) #rimuovere numeri
Reddit_IT$comment <- gsub("[^\x01-\x7F]", "", Reddit_IT$comment) #rimuovere caratteri non ASCII


# Youtube_EN
Youtube_EN$comment <- gsub("#\\$+", "", Youtube_EN$comment) #rimuovere hastag 
Youtube_EN$comment <- gsub("https\\$*", "", Youtube_EN$comment) #rimuovere link
Youtube_EN$comment <- gsub("@\\$+", "", Youtube_EN$comment) #rimuovere menzioni
Youtube_EN$comment <- gsub("amp", "", Youtube_EN$comment) #rimuovere alcuni caratteri speciali
Youtube_EN$comment <- gsub("[\r\n]", "", Youtube_EN$comment) #rimuovere interruzioni di riga
Youtube_EN$comment <- gsub("[[:punct:]]", "", Youtube_EN$comment) #rimuovere segni di punteggiatura 
Youtube_EN$comment <- gsub("[[:digit:]]", "", Youtube_EN$comment) #rimuovere numeri
Youtube_EN$comment <- gsub("[^\x01-\x7F]", "", Youtube_EN$comment) #rimuovere caratteri non ASCII


# Youtube_IT
Youtube_IT$comment <- gsub("#\\$+", "", Youtube_IT$comment) #rimuovere hastag 
Youtube_IT$comment <- gsub("https\\$*", "", Youtube_IT$comment) #rimuovere link
Youtube_IT$comment <- gsub("@\\$+", "", Youtube_IT$comment) #rimuovere menzioni
Youtube_IT$comment <- gsub("amp", "", Youtube_IT$comment) #rimuovere alcuni caratteri speciali
Youtube_IT$comment <- gsub("[\r\n]", "", Youtube_IT$comment) #rimuovere interruzioni di riga
Youtube_IT$comment <- gsub("[[:punct:]]", "", Youtube_IT$comment) #rimuovere segni di punteggiatura 
Youtube_IT$comment <- gsub("[[:digit:]]", "", Youtube_IT$comment) #rimuovere numeri
Youtube_IT$comment <- gsub("[^\x01-\x7F]", "", Youtube_IT$comment) #rimuovere caratteri non ASCII


#Metaverse
Metaverse$text <- gsub("#\\$+", "", Metaverse$text) #rimuovere hastag 
Metaverse$text <- gsub("https\\$*", "", Metaverse$text) #rimuovere link
Metaverse$text <- gsub("@\\$+", "", Metaverse$text) #rimuovere menzioni
Metaverse$text <- gsub("amp", "", Metaverse$text) #rimuovere alcuni caratteri speciali
Metaverse$text <- gsub("[\r\n]", "", Metaverse$text) #rimuovere interruzioni di riga
Metaverse$text <- gsub("[[:punct:]]", "", Metaverse$text) #rimuovere segni di punteggiatura 
Metaverse$text <- gsub("[[:digit:]]", "", Metaverse$text) #rimuovere numeri
Metaverse$text <- gsub("[^\x01-\x7F]", "", Metaverse$text) #rimuovere caratteri non ASCII


#Meta
Meta$text <- gsub("#\\$+", "", Meta$text) #rimuovere hastag 
Meta$text <- gsub("https\\$*", "", Meta$text) #rimuovere link
Meta$text <- gsub("@\\$+", "", Meta$text) #rimuovere menzioni
Meta$text <- gsub("amp", "", Meta$text) #rimuovere alcuni caratteri speciali
Meta$text <- gsub("[\r\n]", "", Meta$text) #rimuovere interruzioni di riga
Meta$text <- gsub("[[:punct:]]", "", Meta$text) #rimuovere segni di punteggiatura 
Meta$text <- gsub("[[:digit:]]", "", Meta$text) #rimuovere numeri
Meta$text <- gsub("[^\x01-\x7F]", "", Meta$text) #rimuovere caratteri non ASCII


#PULIZIA DA EMOTICONS
#install.packages("devtools")
#devtools::install_github("hadley/emo")
#library(emo)
#dataset$text <- ji_replace_all(dataset$text,"")




# --------------------- TOKENIZZAZIONE --------------------------- 

# Reddit_EN
Reddit_EN <- Reddit_EN %>% #comando pipe in cui il risultato di questa riga è l'input della riga dopo
  select(comment) %>%
  unnest_tokens(word,comment) #dividiamo la colonna "comment"  nelle singole parole
Reddit_EN <- Reddit_EN %>% #si sovrascrive l'oggetto
  anti_join(stop_words_english) #da cui si sottraggono le stopwords
Reddit_EN <- Reddit_EN %>% #si sovrascrive l'oggetto
  anti_join(stop_words_spanish) #da cui si sottraggono le stopwords
Reddit_EN <- Reddit_EN %>% #si sovrascrive l'oggetto
  anti_join(stopwords) #da cui si sottraggono le stopwords


#Reddit_EN_ <- VCorpus(VectorSource(Reddit_EN)) #trasformare "message" in un vettore VCorpus (è volatile solo per la singola sessione)
#Reddit_EN_ <- tm_map(Reddit_EN_, content_transformer(tolower)) #inserire come primo parametro l'ogg su cui si vuole lavorare (vet) e come 2° parametro si dirà quale operazione svolgere sul 1° parametro (trasformare tutto in minuscolo)
#Reddit_EN_ <- tm_map(Reddit_EN_, content_transformer(removeWords), stopwords$word) #rimuovere stopwords dal file 
#Reddit_EN_ <- tm_map(Reddit_EN_, content_transformer(removeWords), stopwords("english"))

#funzione di pulizia dei caratteri
#(f <- content_transformer(function(x,pattern) gsub(pattern, "", x, perl = T)))

#pulizia caratteri
#Reddit_EN_ <- tm_map(Reddit_EN_, f, "[[:digit:]]+") #rimuovere numeri
#Reddit_EN_ <- tm_map(Reddit_EN_, f, "[^_[:^punct:]]") #rimuovere segni punteggiatura
#Reddit_EN_ <- tm_map(Reddit_EN_, f, "https||S*") #rimuovere link
#Reddit_EN_ <- tm_map(Reddit_EN_, f, "amp") #rimuovere caratteri speciali
#Reddit_EN_ <- tm_map(Reddit_EN_, f, "[\r\n]+") #rimuovere interruzioni di riga
#Reddit_EN_ <- tm_map(Reddit_EN_, content_transformer(stripWhitespace)) #rimuovere spazi bianchi extra
#Reddit_EN_ <- tm_map(Reddit_EN_, content_transformer(PlainTextDocument)) #trasformare in plain text x le operazioni successive




# Youtube_EN
Youtube_EN <- Youtube_EN %>% #comando pipe in cui il risultato di questa riga è l'input della riga dopo
  select(comment) %>%
  unnest_tokens(word,comment) #dividiamo la colonna "comment"  nelle singole parole
Youtube_EN <- Youtube_EN %>% #si sovrascrive l'oggetto 
  anti_join(stop_words_english) #da cui si sottraggono le stopwords
Youtube_EN <- Youtube_EN %>% #si sovrascrive l'oggetto 
  anti_join(stop_words_spanish) #da cui si sottraggono le stopwords
Youtube_EN <- Youtube_EN %>% #si sovrascrive l'oggetto 
  anti_join(stopwords) #da cui si sottraggono le stopwords



# Reddit_IT
Reddit_IT <- Reddit_IT %>% #comando pipe in cui il risultato di questa riga è l'input della riga dopo
  select(comment) %>%
  unnest_tokens(word,comment) #dividiamo la colonna "comment" dei message nelle singole parole
Reddit_IT <- Reddit_IT %>% #si sovrascrive l'oggetto 
  anti_join(stopwords) #da cui si sottraggono le stopwords
Reddit_IT <- Reddit_IT %>% #si sovrascrive l'oggetto 
  anti_join(stop_words_english) #da cui si sottraggono le stopwords
Reddit_IT <- Reddit_IT %>% #si sovrascrive l'oggetto 
  anti_join(stop_words_spanish) #da cui si sottraggono le stopwords



# Youtube_IT
Youtube_IT <- Youtube_IT %>% #comando pipe in cui il risultato di questa riga è l'input della riga dopo
  select(comment) %>%
  unnest_tokens(word,comment) #dividiamo la colonna "comment" dei message nelle singole parole
Youtube_IT <- Youtube_IT %>% #si sovrascrive l'oggetto 
  anti_join(stopwords) #da cui si sottraggono le stopwords
Youtube_IT <- Youtube_IT %>% #si sovrascrive l'oggetto 
  anti_join(stop_words_english) #da cui si sottraggono le stopwords
Youtube_IT <- Youtube_IT %>% #si sovrascrive l'oggetto 
  anti_join(stop_words_spanish) #da cui si sottraggono le stopwords



# Metaverse
Metaverse <- Metaverse %>% #comando pipe in cui il risultato di questa riga è l'input della riga dopo
  select(text) %>%
  unnest_tokens(word,text) #dividiamo la colonna "comment" dei message nelle singole parole
Metaverse <- Metaverse %>% #si sovrascrive l'oggetto 
  anti_join(stopwords) #da cui si sottraggono le stopwords
Metaverse <- Metaverse %>% #si sovrascrive l'oggetto 
  anti_join(stop_words_english) #da cui si sottraggono le stopwords
Metaverse <- Metaverse %>% #si sovrascrive l'oggetto 
  anti_join(stop_words_spanish) #da cui si sottraggono le stopwords



# Meta
Meta <- Meta %>% #comando pipe in cui il risultato di questa riga è l'input della riga dopo
  select(text) %>%
  unnest_tokens(word,text) #dividiamo la colonna "comment" dei message nelle singole parole
Meta <- Meta %>% #si sovrascrive l'oggetto 
  anti_join(stopwords) #da cui si sottraggono le stopwords
Meta <- Meta %>% #si sovrascrive l'oggetto 
  anti_join(stop_words_english) #da cui si sottraggono le stopwords
Meta <- Meta %>% #si sovrascrive l'oggetto 
  anti_join(stop_words_spanish) #da cui si sottraggono le stopwords



#UNIONE DATASET ITA (YOUTUBE + REDDIT)

dataset_italiano <- rbind(Youtube_IT,Reddit_IT)


#PULIZIA DATASET ITA
dataset_italiano_16 <- VCorpus(VectorSource(dataset_italiano)) #trasformare in un vettore VCorpus (è volatile solo per la singola sessione)
dataset_italiano_16 <- tm_map(dataset_italiano_16, content_transformer(tolower)) #inserire come primo parametro l'ogg su cui si vuole lavorare (vet) e come 2° parametro si dirà quale operazione svolgere sul 1° parametro (trasformare tutto in minuscolo)
dataset_italiano_16 <- tm_map(dataset_italiano_16, content_transformer(removeWords), stopwords$word) #rimuovere stopwords dal file 
dataset_italiano_16 <- tm_map(dataset_italiano_16, content_transformer(removeWords), stopwords("english"))
dataset_italiano_16 <- tm_map(dataset_italiano_16, content_transformer(removeWords), stopwords("italian"))
dataset_italiano_16 <- tm_map(dataset_italiano_16, content_transformer(removeWords), stopwords("spanish"))


#funzione di pulizia dei caratteri
(f <- content_transformer(function(x,pattern) gsub(pattern, "", x, perl = T)))

#pulizia caratteri
dataset_italiano_16 <- tm_map(dataset_italiano_16, f, "[[:digit:]]+") #rimuovere numeri
dataset_italiano_16 <- tm_map(dataset_italiano_16, f, "[^_[:^punct:]]") #rimuovere segni punteggiatura
dataset_italiano_16 <- tm_map(dataset_italiano_16, f, "https||S*") #rimuovere link
dataset_italiano_16 <- tm_map(dataset_italiano_16, f, "amp") #rimuovere caratteri speciali
dataset_italiano_16 <- tm_map(dataset_italiano_16, f, "[\r\n]+") #rimuovere interruzioni di riga
dataset_italiano_16 <- tm_map(dataset_italiano_16, f, "commercianteper") #rimuovere parola specifica
dataset_italiano_16 <- tm_map(dataset_italiano_16, f, "loreverso") #rimuovere parola specifica
dataset_italiano_16 <- tm_map(dataset_italiano_16, f, "manda") #rimuovere parola specifica
dataset_italiano_16 <- tm_map(dataset_italiano_16, content_transformer(stripWhitespace)) #rimuovere spazi bianchi extra
dataset_italiano_16 <- tm_map(dataset_italiano_16, content_transformer(PlainTextDocument)) #trasformare in plain text x le operazioni successive

Dataset_Italiano_16 <- TermDocumentMatrix(dataset_italiano_16) #creazione matrice
dataset_italiano_16 <- as.matrix(Dataset_Italiano_16) #trasformare TDM in matrice normale

dataset_italiano_16 <- data.frame(dataset_italiano_16)

#parole più frequenti DATASET ITA

dataset_italiano_16 <- sort(rowSums(dataset_italiano_16), decreasing = TRUE) #calcolare la somma delle frequenze di una parola (righe) e ordiniamo in modo decrescente
dataset_italiano_16 <- data.frame(parola = names(dataset_italiano_16), freq = dataset_italiano_16) #creazione data frame
dataset_italiano_16_sub <- dataset_italiano_16[1:20,] #subset con i top 20
write.xlsx(dataset_italiano_16_sub,"20_ParoleFrequenti_dataset_ITA.xlsx")
write.xlsx(dataset_italiano_16,"ParoleFrequenti_dataset_ITA.xlsx")

#grafico 20 parole più frequenti DATASET ITA
ggplot(dataset_italiano_16_sub, aes(x=reorder(parola,freq),y=freq, fill=parola)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+
  labs(title = "Le 20 parole più frequenti di Youtube e Reddit in ITA\n", x="Parole", y="Frequenza")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali


# --------------------- FREQ WORD --------------------------- 

#PAROLE PIU FREQUENTI REDDIT (EN) 

Reddit_EN_2 <- table(Reddit_EN)
head(sort(table(Reddit_EN_2), decreasing = TRUE), n = 20)
Reddit_EN_2 <- as.data.frame(Reddit_EN_2) #convertire la tabella in dataframe
Reddit_EN_2 <- Reddit_EN_2[order(-Reddit_EN_2$Freq),] #ordinare dataframe in ordine decrescente

c <- c("Termine", "Freq") #etichette ID intestazione
names(Reddit_EN_2) <- c

write.xlsx(Reddit_EN_2,"ParoleFrequenti_Reddit_EN.xlsx")


#20 termini più usati Reddit_EN 
Reddit_EN_sub <- Reddit_EN_2[1:20,] #subset con i top 20 

ggplot(Reddit_EN_sub, aes(x=reorder(Termine,Freq),y=Freq, fill=Termine)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+ 
  labs(title = "Le 20 parole più frequenti Reddit_EN\n", x="Parole", y="Frequenza")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali




#PAROLE PIU FREQUENTI REDDIT (IT) 

Reddit_IT_2 <- table(Reddit_IT)
head(sort(table(Reddit_IT_2), decreasing = TRUE), n = 20)
Reddit_IT_2 <- as.data.frame(Reddit_IT_2) #convertire la tabella in dataframe
Reddit_IT_2 <- Reddit_IT_2[order(-Reddit_IT_2$Freq),] #ordinare dataframe in ordine decrescente

c <- c("Termine", "Freq") #etichette ID intestazione
names(Reddit_IT_2) <- c

write.xlsx(Reddit_IT_2,"ParoleFrequenti_Reddit_IT.xlsx")

#20 termini più usati Reddit_IT 

Reddit_IT_sub <- Reddit_IT_2[1:20,] #subset con i top 20 

ggplot(Reddit_IT_sub, aes(x=reorder(Termine,Freq),y=Freq, fill=Termine)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+ 
  labs(title = "Le 20 parole più frequenti Reddit_IT\n", x="Parole", y="Frequenza")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali




#PAROLE PIU FREQUENTI YOUTUBE (EN) 

Youtube_EN_2 <- table(Youtube_EN)
head(sort(table(Youtube_EN_2), decreasing = TRUE), n = 20)
Youtube_EN_2 <- as.data.frame(Youtube_EN_2) #convertire la tabella in dataframe
Youtube_EN_2 <- Youtube_EN_2[order(-Youtube_EN_2$Freq),] #ordinare dataframe in ordine decrescente

c <- c("Termine", "Freq") #etichette ID intestazione
names(Youtube_EN_2) <- c

write.xlsx(Youtube_EN_2,"ParoleFrequenti_Youtube_EN.xlsx")


#20 termini più usati Youtube_EN 
Youtube_EN_sub <- Youtube_EN_2[1:20,] #subset con i top 20 

ggplot(Youtube_EN_sub, aes(x=reorder(Termine,Freq),y=Freq, fill=Termine)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+ 
  labs(title = "Le 20 parole più frequenti Youtube_EN\n", x="Parole", y="Frequenza")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali




#PAROLE PIU FREQUENTI YOUTUBE (IT) 

Youtube_IT_2 <- table(Youtube_IT)
head(sort(table(Youtube_IT_2), decreasing = TRUE), n = 20)
Youtube_IT_2 <- as.data.frame(Youtube_IT_2) #convertire la tabella in dataframe
Youtube_IT_2 <- Youtube_IT_2[order(-Youtube_IT_2$Freq),] #ordinare dataframe in ordine decrescente

c <- c("Termine", "Freq") #etichette ID intestazione
names(Youtube_IT_2) <- c

write.xlsx(Youtube_IT_2,"ParoleFrequenti_Youtube_IT.xlsx")


#20 termini più usati Youtube_IT 
Youtube_IT_sub <- Youtube_IT_2[1:20,] #subset con i top 20 

ggplot(Youtube_IT_sub, aes(x=reorder(Termine,Freq),y=Freq, fill=Termine)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+ 
  labs(title = "Le 20 parole più frequenti Youtube_IT\n", x="Parole", y="Frequenza")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali



#PAROLE PIU FREQUENTI METAVERSE

Metaverse2 <- table(Metaverse)
head(sort(table(Metaverse2), decreasing = TRUE), n = 20)
Metaverse2 <- as.data.frame(Metaverse2) #convertire la tabella in dataframe
Metaverse2 <- Metaverse2[order(-Metaverse2$Freq),] #ordinare dataframe in ordine decrescente

c <- c("Termine", "Freq") #etichette ID intestazione
names(Metaverse2) <- c

write.xlsx(Metaverse2,"ParoleFrequenti_Metaverse.xlsx")


#20 termini più usati Youtube_IT 
Metaverse_sub <- Metaverse2[1:20,] #subset con i top 20 

ggplot(Metaverse_sub, aes(x=reorder(Termine,Freq),y=Freq, fill=Termine)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+ 
  labs(title = "Le 20 parole più frequenti Youtube ENG sul Metaverso\n", x="Parole", y="Frequenza")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali




#PAROLE PIU FREQUENTI META 

Meta_2 <- table(Meta)
head(sort(table(Meta_2), decreasing = TRUE), n = 20)
Meta_2 <- as.data.frame(Meta_2) #convertire la tabella in dataframe
Meta_2 <- Meta_2[order(-Meta_2$Freq),] #ordinare dataframe in ordine decrescente

c <- c("Termine", "Freq") #etichette ID intestazione
names(Meta_2) <- c

write.xlsx(Meta_2,"ParoleFrequenti_Meta.xlsx")


#20 termini più usati Youtube_IT 
Meta_sub <- Meta_2[1:20,] #subset con i top 20 

ggplot(Meta_sub, aes(x=reorder(Termine,Freq),y=Freq, fill=Termine)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+ 
  labs(title = "Le 20 parole più frequenti Reddit ENG su Meta\n", x="Parole", y="Frequenza")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali




#PAROLE PIU FREQUENTI DATASET_ITALIANO 

dataset_italiano2 <- table(dataset_italiano)
head(sort(table(dataset_italiano2), decreasing = TRUE), n = 20)
dataset_italiano2 <- as.data.frame(dataset_italiano2) #convertire la tabella in dataframe
dataset_italiano2 <- dataset_italiano2[order(-dataset_italiano2$Freq),] #ordinare dataframe in ordine decrescente

c <- c("Termine", "Freq") #etichette ID intestazione
names(dataset_italiano2) <- c

write.xlsx(dataset_italiano2,"ParoleFrequenti_dataset_italiano.xlsx")


#20 termini più usati Youtube_IT 
dataset_italiano_sub <- dataset_italiano2[1:20,] #subset con i top 20 

ggplot(dataset_italiano_sub, aes(x=reorder(Termine,Freq),y=Freq, fill=Termine)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+ 
  labs(title = "Le 20 parole più frequenti Youtube e Reddit ITA\n", x="Parole", y="Frequenza")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali




#EXPORT CSV DATASET

write.csv(Reddit_EN,"/Users/Emanuele/Desktop/Reddit_EN.csv", row.names = FALSE)
write.csv(Reddit_IT,"/Users/Emanuele/Desktop/Reddit_IT.csv", row.names = FALSE)
write.csv(Youtube_EN,"/Users/Emanuele/Desktop/Youtube_EN.csv", row.names = FALSE)
write.csv(Youtube_IT,"/Users/Emanuele/Desktop/Youtube_IT.csv", row.names = FALSE)
write.csv(Metaverse,"/Users/Emanuele/Desktop/Metaverse.csv", row.names = FALSE)
write.csv(Meta,"/Users/Emanuele/Desktop/Meta.csv", row.names = FALSE)
write.csv(dataset_italiano,"/Users/Emanuele/Desktop/dataset_italiano.csv", row.names = FALSE)

dataset_italiano_16 <- select(dataset_italiano_16,"parola")
write.csv(dataset_italiano_16,"/Users/Emanuele/Desktop/dataset_ITA.csv", row.names = FALSE)




# --------------------- LINK WORD --------------------------- 

vet_link <- VCorpus(VectorSource(Reddit_EN)) #trasformare "message" in un vettore VCorpus (è volatile solo per la singola sessione)
#Reddit_EN_2 <- tm_map(vet_link, content_transformer(removeWords), stopwords("english"))
Reddit_EN_2 <- tm_map(vet_link, content_transformer(tolower)) #inserire come primo parametro l'ogg su cui si vuole lavorare (vet) e come 2° parametro si dirà quale operazione svolgere sul 1° parametro (trasformare tutto in minuscolo)
Reddit_EN_2 <- tm_map(Reddit_EN_2, content_transformer(removeWords), stopwords$word) #rimuovere stopwords dal file 
#dataset_Link <- tm_map(dataset_Link, content_transformer(removeWords), c("","")) #nelle virgolette si inserisce le singole parole da rimuovere a piacimento nella propria analisi

#funzione di pulizia dei caratteri
(f <- content_transformer(function(x,pattern) gsub(pattern, "", x, perl = T)))

#pulizia caratteri
Reddit_EN_2 <- tm_map(Reddit_EN_2, f, "[[:digit:]]+") #rimuovere numeri
Reddit_EN_2 <- tm_map(Reddit_EN_2, f, "[^_[:^punct:]]") #rimuovere segni punteggiatura
Reddit_EN_2 <- tm_map(Reddit_EN_2, f, "https||S*") #rimuovere link
Reddit_EN_2 <- tm_map(Reddit_EN_2, f, "amp") #rimuovere caratteri speciali
Reddit_EN_2 <- tm_map(Reddit_EN_2, f, "[\r\n]+") #rimuovere interruzioni di riga
Reddit_EN_2 <- tm_map(Reddit_EN_2, content_transformer(stripWhitespace)) #rimuovere spazi bianchi extra
Reddit_EN_2 <- tm_map(Reddit_EN_2, content_transformer(PlainTextDocument)) #trasformare in plain text x le operazioni successive



#creare una matrice termini-documenti 
Reddit_EN_2.TDM_link <- TermDocumentMatrix(Reddit_EN_2) #creazione matrice
Reddit_EN_2.tm_link <- as.matrix(Reddit_EN_2.TDM_link) #trasformare TDM in matrice normale


#calcolo co-occorrenza per termini ed esportazione dataframe
assoc_link <- as.data.frame(findAssocs(Reddit_EN_2.TDM_link, "metaverse", corlimit = 0.1)) 
assoc_link <- data.frame(termini = rownames(assoc_link), corlimit = c(assoc_link[,1]))
write.xlsx(assoc_link, "associazioniParole_people.xlsx")

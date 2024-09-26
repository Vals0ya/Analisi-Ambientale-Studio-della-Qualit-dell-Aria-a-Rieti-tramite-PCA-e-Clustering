require(tidyverse)
require(magrittr)
library(lubridate)
library(dplyr)
library(tidyr)
require(ggplot2)
require(gridExtra)
library(FactoMineR)
library(FactoInvestigate)
library(Factoshiny)
library(factoextra)
library(plotly)
data<- DatiRieti
remove(bfi,bfi.keys,Covid19Regioni,Economics,letter, Stirpat,StockReturns,wiki4HE)
any(is.na(data))
dati_selezionati <- data[, 1:9]
dati_melting <- reshape2::melt(dati_selezionati)
#Il messaggio "No id variables; using all as measure variables" indica che la funzione melt ha interpretato tutte le colonne del tuo dataframe come variabili di misura piuttosto che come variabili di identificazione
ggplot(dati_melting, aes(x = variable, y = value)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue") +
  labs(x = "Variabile", y = "Valori") +
  ggtitle("Confronto tra le colonne da 1 a 9")

#L'elemento più pericoloso è l'NO2
istogramma_NO2 <- ggplot(data, aes(x = seq_along(NO2), y = NO2)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Istogramma di NO2", x = "Indice", y = "Frequenza") +
  theme_minimal()
#qual è il dato con l'NO2 più alto?

which.max(data$NO2)

# Grafico a dispersione con linea di regressione: relazione approssimativa tra le variabili NO2 e Oxylene.
scatter_con_regressione <- ggplot(data, aes(x = NO2, y = Oxylene)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Scatter plot tra NO2 e Oxylene con regressione lineare", x = "NO2", y = "Oxylene") +
  theme_minimal()
#regressione ------------------------------------------------------------
correlazioni <- cor(data[, 1:9], data$Temp.Excursion)#Correlazione tra valori e escursione termica #regressione tra variabili da 1 a 9 con escursione termica  
modello <- lm(Temp.Excursion ~ Toluene + Oxylene + Etilbenzene + NO2 + SO2 + PM10 + O3 + CO + PM25, data = data)
summary(modello)
modello2 <- lm(Average.Temp ~ Toluene + Oxylene + Etilbenzene + NO2 + SO2 + PM10 + O3 + CO + PM25, data = data)
summary(modello2)

st_data<-  as.data.frame(scale(data))#standardizzo
# clustering gerarchico ---------------------------------------------------
plot(st_data)
#calcolo le distanze con i vari metodi
x.dist1=dist(st_data,method="euclidean")

x.dist2=dist(st_data,method="manhattan")

x.dist3=dist(st_data,method="minkowski", p=3)

# dendrogrammi
hc.single1=hclust(x.dist1,method="single",members=NULL)
hc.single2=hclust(x.dist2,method="single",members=NULL)
hc.single3=hclust(x.dist3,method="single",members=NULL)
hc.ward=hclust(x.dist1,method="ward",members=NULL)

plot(hc.single1)
plot(hc.single2)
plot(hc.single3)
plot(hc.ward)
#Ancora non riesco a capire quanti cluster ci sono, con nessun metodo.

# -- ----------------------------------------------------------------------


# Metodo del gomito per capire quanti cluster ci sono  

# Utilizza il pacchetto "cluster" per calcolare l'inerzia
library(cluster)
set.seed(9910)

# Crea una serie di clustering con diversi numeri di cluster
k_values <- 1:10
inerzia <- numeric(length(k_values))

for (k in k_values) {
  kmeans_model <- kmeans(st_data, centers = k)#uso st_data che sono i dati standardizzati ma senza le prime due colonne
  inerzia[k] <- kmeans_model$tot.withinss
}

# Plot del metodo del gomito
plot(k_values, inerzia, type = "b", xlab = "Numero di Cluster", ylab = "Inerzia")

#migliore è 2
#Nel grafico, cerca il punto in cui l'inerzia inizia a diminuire a un ritmo più lento, creando una forma simile a un "gomito". Questo valore potrebbe essere un'indicazione del numero ottimale di cluster. Provo con un altro metodo. 

#METODO SILHUETTE E KMEANS
# Provo diverse configurazioni di cluster e calcolo le silhouette
for (k in 2:9) {
  kmeans_result <- kmeans(st_data, centers = k)
  silhouette_result <- silhouette(kmeans_result$cluster, dist(st_data))
  avg_silhouette <- mean(silhouette_result[, "sil_width"])
  cat("Number of clusters:", k, "- Average silhouette:", avg_silhouette, "\n")
}

#esploro come varia la silhouette media al variare del numero di cluster. Puoi utilizzo queste informazioni per identificare il numero di cluster che massimizza la coesione interna e la separazione tra i cluster. Il migliore è 2 cluster 

#provo a dividerlo in due gruppi 
hicluste1=cutree(hc.single1,k=2) #metodo euclideo
hicluste2=cutree(hc.single2,k=2)#metodo manhattan
hicluste3=cutree(hc.single3,k=2)#metodo minkowski

plot(st_data, col=hicluste1,main="Single Linkage - Euclidean distance")
plot(st_data, col=hicluste2,main="Single Linkage - Manhattan distance")
plot(st_data, col=hicluste3,main="Single Linkage - Minkowski distance")

hiward=cutree(hc.ward,k=2)
plot(st_data, col=hiward,main="Ward's minimum variance - Euclidean distance")
#Il migliore è ward 
table(hicluste1)#metodo euclideo
table(hicluste2)#metodo manhattan
table(hicluste3)#metodo minkowski
table(hiward) #metodo di ward
#capisco che la migliore è con il metodo ward, perchè distribuito più omogeneo.
plot(st_data$PM10, col=hiward)
# PCA ---------------------------------------------------------------------

pac <- PCA(st_data)
summary(pac)
#la prima variabile sintetica spiega 69% della variabilità totale dei dati. Se guardo la riga "Cumulative % var" vedo quanto ogni volta aggiungendno la variabile sintetica vedo della variabilità totale.
#In "variables" guardando cos2 vedo la correlazione tra la variabile e la dimensione.
#[le freccie vicineindicano correlazione, mentre se sono in quadranti opposti c'è correlazione negativa. 
#la freccia corta vuol dire che la correlazione non è molto alta, la variabile ha poca importanza.

#scegliere quante variabili usare utilizzando la varianza spiegata. Scelgo dove sta il gomito perchè dopo esso diminuisce sempre di meno
fviz_screeplot(pac, addlabels = TRUE, ylim = c(0, 60))

#quali sono le variabili più importanti
fviz_pca_var(pac, col.var = "contrib",col.quali.sup = "magenta",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)
# grafico delle componenti principali per le variabili utilizzando le dimensioni 3 e 4. Però queste dimensioni spiegano solo il 12% 

plot(pac, choix="var", title="Variables PCA graph", axes=3:4)


#seleziona individui basati su un valore soglia del coseno al quadrato (cos2).
plot(pac, cex=0.8,select="cos2 0.6")
#seleziona l'individuo basandosi sul suo contributo
plot(pac, cex=0.8, select="contrib 1")

#selezionare specifiche variabili nel grafico delle componenti principali basandosi sui contributi.
plot(pac, choix="var", select="contrib 6")#seleziono la metà

plot(pac, choix="var", select="contrib 1") #seleziono la più "importante"
# Qual è il dato con l'Oxylene più alto?
which.max(st_data$Oxylene)

#Come si posiziona la riga 326 a livello di Oxylene dato che è la  variabile più importante?
ggplot(data = st_data, aes(x = seq_along(Oxylene), y = Oxylene)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_point(data = subset(st_data, abs(Oxylene - 3.481562979) < 0.0001), 
             aes(x = 326, y = 3.481562979), color = "red", size = 3) +
  geom_vline(xintercept = 326, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Valore Oxylene per ogni dato",
       x = "indici",
       y = "valore Oxylene") +
  theme(legend.position = "none", axis.text.y = element_text(angle = 0, hjust = 1))


#seleziono la seconda variabile più "importante"
plot(pac, choix="var", select="contrib 2") 
#Come si posiziona la riga 326 a livello di O3 dato che è la seconda variabile più importante?
ggplot(data = st_data, aes(x = seq_along(O3), y = O3)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_point(data = subset(st_data, abs(O3 - (-2.48856610)) < 0.0001), 
             aes(x = 326, y = -2.48856610), color = "red", size = 3) +
  geom_vline(xintercept = 326, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Valore O3 per ogni dato",
       x = "indici",
       y = "valore O3") +
  theme(legend.position = "none", axis.text.y = element_text(angle = 0, hjust = 1))


#in effetti il dato con indice 326 ha la seconda variabile più importante molto bassa


#ed a livello di wind speed?
ggplot(data = st_data, aes(x = seq_along(Wind.Speed), y = Wind.Speed)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_point(data = subset(st_data, abs(Wind.Speed - (-1.033073687)) < 0.0001), 
             aes(x = 326, y = -1.033073687), color = "red", size = 3) +
  geom_vline(xintercept = 326, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Valore velocità vento per ogni dato",
       x = "indici",
       y = "velocità del vento") +
  theme(legend.position = "none", axis.text.y = element_text(angle = 0, hjust = 1))

#come avevamo visto dal grafico all'inizio la velocità del vento e l'oxylene hanno una correlazione negativa. 



#seleziono individui basati sul coseno al quadrato: solo gli individui con un impatto significativo sulla varianza totale vengono inclusi nel grafico
plot(pac, cex=0.8, select="cos2 0.7", title="indici", cex.main=1.1, cex.axis=0.9, shadow=TRUE, auto="y")
#anche qui possiamo vedere come l'indice 326 sia molto significativo

Investigate(pac)
PCAshiny(pac)



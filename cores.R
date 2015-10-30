require(xlsx, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(stringr, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(TTR, quietly = TRUE)
if(!exists("plotCCAA")) source('plotCCAA.R', encoding="UTF-8")
if(!exists("plotComb")) source('plotCombustible.R', encoding="UTF-8")

download.file("http://www.cores.es/sites/default/files/archivos/estadisticas/consumos-pp-ccaa-provincias.xlsx", 
              "consumos.xlsx", mode = "wb")
df<-read.xlsx2("consumos.xlsx", sheetName="Consumos", startRow = 6, header = TRUE)
df<-filter(df, Año != "")

df1<-mutate(df, Date = as.Date(paste(Año, Mes, "1"), "%Y %B %d"))
for(i in 5:11)  {
      df1[,i]<-round(as.numeric(as.character(df1[,i])),0)
}
df1[,5:11][is.na(df1[,5:11])]<-0
names(df1)<- str_replace_all(names(df1), "[^[:alnum:]]", "")
df1<-df1[, c(12, 1:11)]
levels(df1$CCAA) <- gsub("Total Nacional", "España", levels(df1$CCAA))
df1<-mutate(df1, Gasolina = Gasolina97IO + Gasolina95IO + Gasolina98IO)
df1<-mutate(df1, Gasóleo = GasóleoA + GasóleoB + GasóleoC)

df1ma<-df1
df1ma<-mutate(df1ma, ind=paste0(CCAA, Provincia))
lista<-distinct(select(df1ma, ind))
for(j in 1:nrow(lista)){
      for(i in 6:14)  {
            df1ma[df1ma$ind==lista$ind[j],i]<-round(SMA(df1ma[df1ma$ind==lista$ind[j],i], n=12),0)
      }
}
df1ma<-select(df1ma, -ind)

pdf("CORES.pdf")
      
plotCCAA("España", c("2000-01-01", "end"))

plotCCAA("Cataluña", c("2000-01-01", "end"))

plotComb("Gasóleo", c("2000-01-01", "end"))

plotComb("Gasolina", c("2000-01-01", "end"))

dev.off()
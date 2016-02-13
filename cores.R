require(openxlsx, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(stringr, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(TTR, quietly = TRUE)
require(grid, quietly = TRUE)
require(gridExtra, quietly = TRUE)
require(forecast, quietly = TRUE)
if(!exists("plotCCAA")) source('plotCCAA.R', encoding="UTF-8")
if(!exists("plotComb")) source('plotCombustible.R', encoding="UTF-8")

download.file("http://www.cores.es/sites/default/files/archivos/estadisticas/consumos-pp-ccaa-provincias.xlsx", 
              "consumos.xlsx", mode = "wb")
df<-read.xlsx("consumos.xlsx", sheet="Consumos", startRow = 6)
df<-filter(df, Año != "")
df<-filter(df, Año > 1997 & Año < 2020)
df<-select(df, -contains("bio"))

df1<-mutate(df, Date = as.Date(paste(Año, Mes, "1"), "%Y %B %d"))
for(i in 5:11)  {
      df1[,i]<-round(as.numeric(as.character(df1[,i])),0)
}
df1[,5:11][is.na(df1[,5:11])]<-0
names(df1)<- str_replace_all(names(df1), "[^[:alnum:]]", "")
df1<-df1[, c(12, 1:11)]
df1$CCAA <- gsub("Total Nacional", "España", df1$CCAA)
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

ts<-ts(filter(df1, CCAA == "España")$Gasóleo, frequency=12, start = c(df1$Año[1],1))
comp<-decompose(ts, type = "multiplicative")
var.estacional<-data.frame(mes=format(ISOdate(2000, 1:12, 1), "%B"), Gasóleo=comp$figure)
var.estacional$mes<-factor(var.estacional$mes, levels=format(ISOdate(2000, 1:12, 1), "%B"))

ts<-ts(filter(df1, CCAA == "España")$Gasolina, frequency=12, start = c(df1$Año[1],1))
comp<-decompose(ts, type = "multiplicative")
var.estacional$Gasolina<-comp$figure
var.estacional.g<-gather(data=var.estacional, key=Combustible, value=IndiceMensual, 2:3)
var.estacional[,2:3]<-round(var.estacional[,2:3],3)

pdf("CORES.pdf")
      
plotCCAA("España", c("2000-01-01", "end"))

plotCCAA("Cataluña", c("2000-01-01", "end"))

plotComb("Gasóleo", c("2000-01-01", "end"))

plotComb("Gasolina", c("2000-01-01", "end"))

p<-ggplot(var.estacional.g, aes(x=mes, y=IndiceMensual, group=Combustible, color=Combustible))
p<-p+geom_line(size=1.5)
p<-p+geom_hline(yintercept=1)
p<-p+theme(axis.text.x=element_text(angle = 45, hjust = 1))
print(p)

grid.newpage()

grid.table(var.estacional, rows=rep("",12))

dev.off()

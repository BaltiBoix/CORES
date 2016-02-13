options( java.parameters = "-Xmx4g" )
require(openxlsx, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(stringr, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(TTR, quietly = TRUE)
if(!exists("plotCCAA")) source('plotCCAA.R', encoding="UTF-8")
if(!exists("plotComb")) source('plotCombustible.R', encoding="UTF-8")
if(!exists("plotProducto")) source('plotProducto.R', encoding="UTF-8")

if(!file.exists("consumos-pp.xlsx")) 
      download.file("http://www.cores.es/sites/default/files/archivos/estadisticas/consumos-pp.xlsx", 
      "consumos-pp.xlsx", mode = "wb")

dftemp<-read.xlsx("consumos-pp.xlsx", sheet="GLPs", startRow = 6)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<- tolower(str_replace_all(names(dftemp), "[^[:alnum:]]", ""))
names(dftemp)[3:7]<-c("glpenvasado", "glpgranel", "glpautomocion", "glpotros", "totalglps")

df<-dftemp

dftemp<-read.xlsx("consumos-pp.xlsx", sheet = "Gasolinas", startRow = 6, cols = 1:11)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<- tolower(str_replace_all(names(dftemp), "[^[:alnum:]]", ""))
names(dftemp)[11]<-"totalgasolinas"

df<-merge(df, dftemp)

dftemp<-read.xlsx("consumos-pp.xlsx", sheet = "Querosenos", startRow = 6, cols = 1:5)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<- tolower(str_replace_all(names(dftemp), "[^[:alnum:]]", ""))
names(dftemp)[3:5]<-c("kerosenoaviacion", "kerosenootros", "totalkerosenos")

df<-merge(df, dftemp)

dftemp<-read.xlsx("consumos-pp.xlsx", sheet = "Gasoleos", startRow = 6, cols = 1:10)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<- tolower(str_replace_all(names(dftemp), "[^[:alnum:]]", ""))
names(dftemp)[10]<-"totalgasoleos"

df<-merge(df, dftemp)

dftemp<-read.xlsx("consumos-pp.xlsx", sheet = "Fueloleos", startRow = 6, cols = 1:7)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<- tolower(str_replace_all(names(dftemp), "[^[:alnum:]]", ""))
names(dftemp)[7]<-"totalfueloleos"

df<-merge(df, dftemp)

dftemp<-read.xlsx("consumos-pp.xlsx", sheet = "Otros Productos", startRow = 6, cols = 1:7)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<- tolower(str_replace_all(names(dftemp), "[^[:alnum:]]", ""))
names(dftemp)[7]<-"totalotros"

df<-merge(df, dftemp)

names(df)<- str_replace_all(names(df), "ó", "o")

for(i in 3:37)  {
      df[,i]<-round(as.numeric(as.character(df[,i])),0)
}

df[,3:37][is.na(df[,3:37])]<-0

df<-filter(df, mes != "total")

df<-mutate(df, date = as.Date(paste(año, mes, "1"), "%Y %B %d"))
df<-arrange(df, date)

for(i in 3:37){
      for(j in 1:nrow(df)){
            if(df[j, i] == 0) {
                  df[j, i]<-NA
            }else{
                  break()
            }
      }
}

dfma<-df
for(i in 3:37){
      dfma[,i]<-round(SMA(dfma[,i], n=12),0)
}


pdf("CORES1.pdf")

dfg<-gather(data=df, key=producto, value=consumomensual, 3:37)
dfmag<-gather(data=dfma, key=producto, value=consumomensual, 3:37)

dfg<-filter(dfg, consumomensual > 10)
dfmag<-filter(dfmag, consumomensual > 10)

df1g<-filter(dfg, substring(producto,1,5) == "total")
df1mag<-filter(dfmag, substring(producto,1,5) == "total")

plotProducto()

df1g<-filter(dfg, grepl("glp", producto))
df1mag<-filter(dfmag, grepl("glp", producto))

plotProducto()

df1g<-filter(dfg, grepl("gasolina", producto))
df1mag<-filter(dfmag, grepl("gasolina", producto))

plotProducto()

df1g<-filter(dfg, producto %in% names(df)[20:27])
df1mag<-filter(dfmag, producto %in% names(df)[20:27])

plotProducto()

df1g<-filter(dfg, producto %in% names(df)[28:32])
df1mag<-filter(dfmag, producto %in% names(df)[28:32])

plotProducto()

df1g<-filter(dfg, producto %in% names(df)[33:37])
df1mag<-filter(dfmag, producto %in% names(df)[33:37])

plotProducto()

dev.off()

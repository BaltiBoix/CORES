require(openxlsx, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(tidyr, quietly = TRUE)
#require(stringr, quietly = TRUE)
require(zoo, quietly = TRUE)

xls.file<-"../CORES/consumos-pp.xlsx"

if(!file.exists("consumos-pp.xlsx")) 
      download.file("http://www.cores.es/sites/default/files/archivos/estadisticas/consumos-pp.xlsx", 
      xls.file, mode = "wb")

dftemp<-read.xlsx(xls.file, sheet="GLPs", startRow = 6)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<-c("anyo", "mes", "GLP.envasado", "GLP.granel", "GLP.automocion", "GLP.otros", "GLP.total")

pp.df<-dftemp

dftemp<-read.xlsx(xls.file, sheet = "Gasolinas", startRow = 6, cols = 1:11)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<- c('anyo','mes','GSNA.GS97','GSNA.GS95','GSNA.GS98','GSNA.bioetanol','GSNA.mezcla',
                  'GSNA.auto','GSNA.aviacion','GSNA.otras','GSNA.total')

pp.df<-merge(pp.df, dftemp)

dftemp<-read.xlsx(xls.file, sheet = "Querosenos", startRow = 6, cols = 1:5)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<- c('anyo','mes','KERO.aviacion','KERO.otros','KERO.total')

pp.df<-merge(pp.df, dftemp)

dftemp<-read.xlsx(xls.file, sheet = "Gasoleos", startRow = 6, cols = 1:10)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<- c('anyo','mes','GOIL.goa','GOIL.biodiesel','GOIL.biomezcla',
                  'GOIL.auto','GOIL.gob','GOIL.goc','GOIL.otros','GOIL.total')

pp.df<-merge(pp.df, dftemp)

dftemp<-read.xlsx(xls.file, sheet = "Fueloleos", startRow = 6, cols = 1:7)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<- c('anyo','mes','FUEL.fo1','FUEL.fo2','FUEL.fobia',
                  'FUEL.otros','FUEL.total')

pp.df<-merge(pp.df, dftemp)

dftemp<-read.xlsx(xls.file, sheet = "Otros Productos", startRow = 6, cols = 1:7)
dftemp<-filter(dftemp, Mes != "")
names(dftemp)<- c('anyo','mes','OTROS.lubricantes','OTROS.asfalto',
                  'OTROS.coque','OTROS.otros','OTROS.total')

pp.df<-merge(pp.df, dftemp)

pp.df<- pp.df %>% 
      mutate_each(funs(ch2num = ifelse(is.na(.), NA, as.numeric(.))), -c(1:2)) %>%
      filter(mes != "total") %>%
      mutate(fecha = as.yearmon(paste(mes, anyo))) %>%
      arrange(fecha) %>%
      select(fecha, everything()) %>%
      mutate_each(funs(consumo2kt=ifelse(is.na(.), NA, ./1000)), -c(1:3))

saveRDS(pp.df, file = 'data/consumos-pp.RDS')      

ppg.df<-pp.df %>%
      gather(key=gen.par, consumo.kt, -c(1:3)) %>%
      separate(gen.par, into = c("familia", "producto"), sep = "\\.") %>%
      select(-anyo, -mes)


zpp<-pp.df %>% select(fecha, -anyo, -mes, starts_with("GOIL"))
                  
zpp<-zoo(x = select(zpp, -fecha), order.by=zpp$fecha)

autoplot(zpp) + facet_free() + geom_smooth()



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

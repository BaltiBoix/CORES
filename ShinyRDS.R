require(openxlsx, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(zoo, quietly = TRUE)
require(tidyr, quietly = TRUE)

xls.file<-"../CORES/consumos.xlsx"

download.file("http://www.cores.es/sites/default/files/archivos/estadisticas/consumos-pp-ccaa-provincias.xlsx", 
              xls.file, mode = "wb")
df<-read.xlsx("../CORES/consumos.xlsx", sheet="Consumos", startRow = 6)
df<-filter(df, Año > 1997 & Año < 2020)
df<-select(df, -contains("bio"))
names(df)<-c("Anyo","Mes","CCAA","Provincia","GS97","GS95","GS98","GOA","GOB","GOC","FOBIA")

df<-mutate(df, fecha=as.yearmon(paste0(Mes,Anyo)))
df<-select(df, c(12,3:11))
df[is.na(df)] <- 0
df[,4:10]<-df[,4:10]/1000
saveRDS(df, file = '../shinyCORES/data/consumos-pp-ccaa-provincias.RDS')


xls.file<-"../CORES/consumos-pp.xlsx"

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

saveRDS(pp.df, file = '../shinyCORES/data/consumos-pp.RDS') 

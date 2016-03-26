require(openxlsx, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(zoo, quietly = TRUE)
#require(tidyr, quietly = TRUE)
#require(lattice, quietly = TRUE)

#download.file("http://www.cores.es/sites/default/files/archivos/estadisticas/consumos-pp-ccaa-provincias.xlsx", 
#              "consumos.xlsx", mode = "wb")
df<-read.xlsx("../CORES/consumos.xlsx", sheet="Consumos", startRow = 6)
df<-filter(df, Año > 1997 & Año < 2020)
df<-select(df, -contains("bio"))
names(df)<-c("Anyo","Mes","CCAA","Provincia","GS97","GS95","GS98","GOA","GOB","GOC","FOBIA")

df<-mutate(df, fecha=as.yearmon(paste0(Mes,Anyo)))
df<-select(df, c(12,3:11))
df[is.na(df)] <- 0
df[,4:10]<-df[,4:10]/1000
saveRDS(df, file = 'consumos-pp-ccaa-provincias.RDS')

CCAA.Sel<-c("Madrid", "Cataluña")
Provincia.Sel<-"Total"
xlim.Sel<-c(as.yearmon(paste0(2006,"-01-01")), as.yearmon(paste0(2016,"-01-01")))
prod.Sel<-c('GOA')
norm.Sel<-FALSE

iprod.Sel<-which(names(df) %in% prod.Sel, arr.ind=T)

z<-df %>% filter(CCAA==CCAA.Sel & Provincia==Provincia.Sel) %>% select(1, iprod.Sel)
if(NROW(z)>0) {
      z<-zoo(x=z[,2:NCOL(z)], order.by = z[,1])

      if(norm.Sel){
            nor<-sapply(window(z, start = xlim.Sel[1], end = xlim.Sel[1]+11/12),function(x) 100/mean(x))
            if(NCOL(z)>1) nor<-matrix(rep(nor,each=NROW(z)),ncol=NCOL(z))
            z<-z * nor
      }
      
      #zz<-rollmean(z,12, align = 'right')
      #names(zz)<-paste0(names(zz),"_mean")
      #z<-merge(z,zz)
      
      #xyplot(z, layout=c(1, ncol(z)), ylab='kt', main=CCAA.Sel, xlim=xlim.Sel, type=c("l","g", "r"))
      g<-autoplot(z, na.rm = TRUE)
      if(NCOL(z)>1) g<-g + facet_free()
      g<-g + scale_x_yearmon(limits=xlim.Sel)
      g<-g + geom_line(color='blue', size=0.5, na.rm = TRUE)
      g<-g + geom_smooth(se=F, size=1.5, na.rm = TRUE)
      g<-g + xlab("Fecha")+ylab("kt/mes")+ggtitle(CCAA.Sel)
      g<-g + theme(axis.text = element_text(size = 12),
                   plot.title = element_text(size = 16, face='bold'),
                   strip.text = element_text(size = 16, face='bold'),
                   axis.title.x = element_text(size = 14),
                   axis.title.y = element_text(size = 14),
                   panel.border = element_rect(linetype = 'solid', color = 'red', fill = NA),
                   strip.background = element_rect(linetype = 'solid', color = 'darkred', fill = 'gray'),
                   panel.grid.major = element_line(colour = "grey"),
                   panel.grid.minor = element_blank())
      print(g)
}
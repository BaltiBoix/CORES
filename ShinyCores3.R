require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(zoo, quietly = TRUE)
require(forecast, quietly = TRUE)
require(gridExtra, quietly=TRUE)
require(ggseas, quietly=TRUE)
require(scales, quietly=TRUE)

pp.df<-readRDS('data/consumos-pp.RDS')

ppg.df<-pp.df %>%
      gather(key=gen.par, consumo.kt, -c(1:3)) %>%
      separate(gen.par, into = c("familia", "producto"), sep = "\\.") %>%
      mutate(tipo='obs') %>%
      select(fecha, familia, producto, tipo, consumo.kt) 

familia.list<-unique(ppg.df$familia)
for(famItem in familia.list){

      producto.list<-(ppg.df %>% filter(familia==famItem) %>% select(producto) %>% distinct())[,1]
      for(prodItem in producto.list){
            
            z<-ppg.df %>% filter(familia==famItem & producto==prodItem) %>% 
                  select(fecha, consumo.kt) %>%
                  mutate(consumo.kt = ifelse(is.na(consumo.kt), 0, consumo.kt))
            z<-zoo(x=z$consumo.kt, order.by = z$fecha)
            z.stl<-stl(z, s.window = 'periodic', t.window = 13, s.degree = 1, robust = TRUE)
            n=NROW(z)
            zz<-data.frame(fecha=as.yearmon(time(z.stl$time.series)), 
                           familia=rep(famItem,n), 
                           producto=rep(prodItem,n), 
                           tipo=rep("trend",n),
                           consumo.kt=z.stl$time.series[,2])
            ppg.df<-merge(ppg.df, zz, all = TRUE)

      }
}
ppg.df<-ppg.df %>% mutate(consumo.kt = ifelse(consumo.kt < 0.001, 0, consumo.kt))


xlim.Sel<-c(as.yearmon(paste0(2010,"-01-01")), as.yearmon(paste0(2016,"-01-01")))

famItem<-"GOIL"
prodItem<-'total'

df<-ppg.df %>% filter(familia==famItem & producto==prodItem)

g<-ggplot(data=df, aes(x=fecha, y=consumo.kt, color=tipo, size=tipo))
#g<-g + facet_wrap(~familia + producto, scales = "free_y")
g<-g + scale_color_manual(values=c("red", "blue"), breaks=c("obs", "trend"), labels=c("Observado", "Desestacionalizado"))
g<-g + scale_size_manual(values=c(0.25, 1.25), breaks=c("obs", "trend"), labels=c("Observado", "Desestacionalizado"))
g<-g + geom_line( na.rm=TRUE)
g<-g + scale_x_yearmon(limits=xlim.Sel)
g<-g + ggtitle(paste("Consumo mensual", famItem, prodItem))
g<-g + theme(panel.background = element_rect(colour = "red"),
             plot.title = element_text(size = 16, face='bold', color='blue'),
             panel.grid.major= element_line(size = 0.25, colour = "red", linetype = "dotted"),
             legend.position = 'bottom',
             legend.text = element_text(size = 14),
             legend.title=element_blank())

df1<-df %>% filter(tipo=="trend") %>% 
      mutate(dif1=consumo.kt-lag(consumo.kt,1)) %>%
      mutate(dif2=dif1-lag(dif1,1)) %>%
      select(fecha, dif1, dif2) %>%
      gather(key=tipo, value=dif.kt, -fecha) %>%
      mutate(dif.kt = ifelse(abs(dif.kt) < 0.001, NA, dif.kt))
      
p<-ggplot()
p<-p + geom_bar(data=filter(df1, tipo=="dif2"), aes(x=fecha, y=dif.kt, fill="dif2"), stat = "identity", alpha=0.5)
p<-p + geom_line(data=filter(df1, tipo=="dif1"), aes(x=fecha, y=dif.kt, color="dif1"), size=1)
p<-p + scale_x_yearmon(limits=xlim.Sel)
p<-p + scale_color_manual(values="blue", breaks="dif1", labels="Variación mensual del consumo")
p<-p + scale_fill_manual(values="red", breaks="dif2", labels="velocidad de Variación mensual del consumo")
p<-p + theme(panel.background = element_rect(colour = "red"),
             panel.grid.major= element_line(size = 0.25, colour = "red", linetype = "dotted"),
             legend.position = 'bottom',
             legend.text = element_text(size = 14),
             legend.title=element_blank())

grid.arrange(g, p, ncol=1)


seasonplot(as.ts(z),ylab="Consumo Gasolina t/mes", xlab="Year", 
           main="Seasonal plot: Consumo de gasolina",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

ggplot(filter(df, tipo=="obs"), 
       aes(x=strftime(fecha, "%m"), y=consumo.kt, group=strftime(fecha,format="%Y"), color=strftime(fecha,format="%Y"))) +
       geom_line()

z<-zoo(x=pp.df$GOIL.total, order.by=pp.df$fecha)
z<-zoo(x=filter(ppg.df,familia==famItem & producto==prodItem & tipo=="obs")[,5], order.by=pp.df$fecha)
fit<-ets(z, restrict=TRUE)
zfcast<-forecast(fit, h=12)
ggfcast<-funggcast(z,zfcast)

xlim.Sel<-c(as.yearmon(paste0(2010,"-01-01")), as.yearmon(paste0(2017,"-01-01")))
g<-ggplot(data=ggfcast,aes(x=date,y=observed)) 
g<-g+geom_line(col='red', size=1)
g<-g+geom_line(aes(y=fitted),col='blue')
g<-g+geom_line(aes(y=forecast))
g<-g+geom_ribbon(aes(ymin=lo80,ymax=hi80),alpha=.25)
g<-g+scale_x_yearmon(n=12, limits=xlim.Sel)
g<-g+scale_y_continuous(name='kt')
g<-g+ggtitle(paste('ETS Fit to', paste('Consumo mensual', famItem, prodItem), 
             '\n (black=forecast, blue=fitted, red=data, shadow=80% conf. interval)'))
g<-g + theme(panel.background = element_rect(colour = "red"),
             plot.title = element_text(size = 16, face='bold', color='blue'),
             panel.grid.major= element_line(size = 0.25, colour = "red", linetype = "dotted"),
             axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
             axis.title.x = element_text(size = 14),
             legend.position = 'bottom',
             legend.text = element_text(size = 14),
             legend.title=element_blank())
g

#*********************************************************************************************

famItem<-"GSNA"
prodItem<-'GS97'

df<-ppg.df %>% filter(familia==famItem & producto==prodItem & tipo=="obs")
z<-zoo(x=filter(ppg.df,familia==famItem & producto==prodItem & tipo=="obs")[,5], order.by=pp.df$fecha)

z<-pp.df %>% select(fecha, -anyo, -mes, one_of("GLP.otros"))
z<-zoo(x = select(z, -fecha), order.by=z$fecha)

fit<-seas(as.ts(z), forecast.save = "fct", forecast.probability = 0.95)

#ggsdc(df, aes(x=fecha, y=consumo.kt), method = 'seas')+ geom_line() + scale_x_yearmon()

#data.frame(fecha=as.yearmon(index(outlier(fit))), outliers=coredata(outlier(fit))) %>% filter(!is.na(outliers))

zz0<-data.frame(fecha=as.character(as.yearmon(time(original(fit)))),
               original=drop(coredata(original(fit))),
               stringsAsFactors = FALSE) 

zz<-data.frame(fecha=as.character(as.yearmon(time(final(fit)))),
               outlier=coredata(outlier(fit)),
               final=coredata(final(fit)), 
               trend=coredata(trend(fit)),
               stringsAsFactors = FALSE) 
zz<- left_join(zz, zz0, by='fecha')

zzf<-data.frame(fecha=as.character(as.yearmon(time(series(fit, 'forecast.forecasts')))), 
                series(fit, 'forecast.forecasts'),
                stringsAsFactors = FALSE)

zz<- full_join(zz, zzf) %>% mutate(fecha=as.yearmon(fecha))

g<-ggplot(zz)
g<-g + geom_line(aes(x=fecha, y=original, color='original'), size=0.5)
g<-g + geom_text(aes(x=fecha, y=original, label=outlier))
g<-g + geom_label(aes(x=fecha, y=original, label=outlier, color='outlier'))
g<-g + geom_line(aes(x=fecha, y=final, color='final'), size=1)
g<-g + geom_line(aes(x=fecha, y=trend, color='trend'), size=1.5)
g<-g + geom_line(aes(x=fecha, y=forecast, color='forecast'), size=1)
g<-g + geom_ribbon(aes(x=fecha, ymin = lowerci, ymax = upperci), fill = "grey70", alpha=0.5)
g<-g + scale_color_manual(values=c('original'='chartreuse4', 'final'='black', 'trend'='blue', 'forecast'='red', 'outlier'='orange1'),
                          breaks=c('original', 'final', 'trend', 'forecast', 'outlier')) 
g<-g + scale_x_yearmon(limits=c(zz[1,1], zz[NROW(zz),1]))
g<-g + theme(panel.background = element_rect(colour = "red"),
             plot.title = element_text(size = 16, face='bold', color='blue'),
             panel.grid.major= element_line(size = 0.25, colour = "red", linetype = "dotted"),
             axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
             axis.title.x = element_blank(),
             legend.position = 'bottom',
             legend.text = element_text(size = 14),
             legend.title=element_blank())
g

monthplot(fit)

zzz<- zz %>% mutate(mes=factor(format(fecha, "%B"), levels = format(ISOdate(2000, 1:12, 1), "%B")))

g<-ggplot(zzz, aes(x=fecha, y=original))
g<-g + geom_line(colour='blue')
g<-g + facet_wrap(~ mes, nrow=1)
g<-g + stat_smooth(method = "lm", formula = y ~ 1, se = FALSE, colour = "blue")
g<-g + scale_x_yearmon(limits=c(zz[1,1], zz[NROW(zz),1]))
g<-g + theme(panel.background = element_rect(colour = "red"),
             plot.title = element_text(size = 16, face='bold', color='blue'),
             panel.grid.major= element_line(size = 0.25, colour = "red", linetype = "dotted"),
             axis.text.x = element_blank(),
             axis.title.x = element_blank(),
             legend.position = 'bottom',
             legend.text = element_text(size = 14),
             legend.title=element_blank())
g

zzz<-zz %>% 
      mutate(dif1=(trend-lag(trend,12))/lag(trend,12)) %>%
      mutate(dif2=dif1-lag(dif1,1))

g<-ggplot(zzz, aes(x=fecha, y=dif1))
g<-g + geom_line(aes(colour='dif1'))
g<-g + geom_bar(aes(x=fecha, y=dif2, fill='dif2'), stat = "identity", alpha=0.5, position = "identity")
g<-g + geom_hline(yintercept = 0, color='chartreuse4')
g<-g + scale_x_yearmon()
g<-g + scale_y_continuous(labels=percent)
g<-g + scale_color_manual(values=c('dif1'='blue'), labels = 'variación anual del consumo')
g<-g + scale_fill_manual(values=c('dif2'='chartreuse4'), labels = 'velocidad de variación anual del consumo')
g<-g + scale_x_yearmon()
g<-g + ggtitle(paste('Consumo mensual', famItem, prodItem))
g<-g + theme(panel.background = element_rect(colour = "red"),
             plot.title = element_text(size = 16, face='bold', color='blue'),
             panel.grid.major= element_line(size = 0.25, colour = "red", linetype = "dotted"),
             axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
             axis.title.x = element_blank(),
             legend.position = 'bottom',
             legend.text = element_text(size = 14),
             legend.title=element_blank())
g

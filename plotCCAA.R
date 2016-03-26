plotCCAA<-function(CCAAstring, Datelims=NULL){
      
      if(is.null(Datelims)){
            Datelims<-c(format(df1$Date[1]), format(df1$Date[nrow(df1)]))
      }else{
            if(Datelims[1]=="start") Datelims[1]<-format(df1$Date[1])
            if(Datelims[2]=="end") Datelims[2]<-format(df1$Date[nrow(df1)])
      }
      
      dfCCAA<-filter(df1,CCAA==CCAAstring, Provincia=="Total", 
                     Date >= as.Date(Datelims[1]), Date <= as.Date(Datelims[2]))
      dfCCAA<-select(dfCCAA, -CCAA, -Provincia, -Año, -Mes)
      dfCCAAg<-gather(data=dfCCAA, key=Combustible, value=ConsumoMensual, 2:10)
      
      dfCCAAma<-filter(df1ma,CCAA==CCAAstring, Provincia=="Total", 
                       Date >= as.Date(Datelims[1]), Date <= as.Date(Datelims[2]))
      dfCCAAma<-select(dfCCAAma, -CCAA, -Provincia, -Año, -Mes)
      dfCCAAmag<-gather(data=dfCCAAma, key=Combustible, value=ConsumoMensualma, 2:10)
      
      dfCCAAg<-full_join(dfCCAAg, dfCCAAmag, by=c("Date", "Combustible"))
      
      Orden<-c("Gasolina97IO", "Gasolina95IO", "Gasolina98IO", "GasóleoA",    
               "GasóleoB", "GasóleoC", "Gasolina", "Gasóleo", "FuelóleoBIA")
      dfCCAAg$Combustible<-factor(dfCCAAg$Combustible, levels = Orden)

      p<-ggplot()
      p<-p+geom_line(data=dfCCAAg, aes(x=Date, y=ConsumoMensual/1E4, colour="valor mensual"))
      p<-p+geom_line(data=dfCCAAg, aes(x=Date, y=ConsumoMensualma/1E4, colour="media movil 12 meses"), size=1)
      p<-p+facet_wrap(~ Combustible, scales = "free_y")
      p<-p + scale_y_continuous("Consumo mensual t/mes ( x 10,000)")
      p<-p+ggtitle(bquote(atop(.(paste("Consumo de combustibles en", CCAAstring)), 
                               atop(italic(.(paste(paste(rep(" ",40), collapse=" "), "fuente: CORES"))), ""))))
      p<-p + theme(panel.background = element_rect(colour = "pink"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_text(size = 14),
                   plot.title = element_text(size=20),
                   axis.text.y = element_text(size=12),
                   axis.text.x = element_text(size=12, angle = 45, hjust = 1),
                   strip.text.x = element_text(size=12),
                   strip.background = element_rect(colour = "black", size=0.25),
                   panel.border = element_rect(fill=NA, colour = "black", size=0.5),
                   legend.title=element_blank(),
                   legend.text = element_text(size=14),
                   legend.position = "bottom")
      p<-p+scale_colour_manual(values = c("valor mensual" = "gray", "media movil 12 meses" = "blue"))
      p
}
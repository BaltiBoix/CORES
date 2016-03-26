plotComb<-function(Combstring, Datelims=NULL){
      
      if(is.null(Datelims)){
            Datelims<-c(format(df1$Date[1]), format(df1$Date[nrow(df1)]))
      }else{
            if(Datelims[1]=="start") Datelims[1]<-format(df1$Date[1])
            if(Datelims[2]=="end") Datelims[2]<-format(df1$Date[nrow(df1)])
      }
      
      dfComb<-filter(df1, Provincia=="Total", Date >= as.Date(Datelims[1]), Date <= as.Date(Datelims[2]))
      dfComb<-select(dfComb, Date, CCAA, ConsumoMensual=match(Combstring, names(dfComb)))

      dfCombma<-filter(df1ma, Provincia=="Total", Date >= as.Date(Datelims[1]), Date <= as.Date(Datelims[2]))
      dfCombma<-select(dfCombma, Date, CCAA, ConsumoMensualma=match(Combstring, names(dfCombma)))

      dfCombg<-full_join(dfComb, dfCombma, by=c("Date", "CCAA"))

      p<-ggplot()
      p<-p+geom_line(data=dfCombg, aes(x=Date, y=ConsumoMensual/1E4, colour= "valor mensual"))
      p<-p+geom_line(data=dfCombg, aes(x=Date, y=ConsumoMensualma/1E4, colour="media móvil 12 meses"), size=1)
      p<-p+facet_wrap(~ CCAA, scales = "free_y", ncol = 4)
      p<-p + scale_y_continuous("Consumo mensual t/mes ( x 10,000)")
      p<-p+ggtitle(bquote(atop(.(paste("Consumo de", Combstring)), 
                               atop(italic(.(paste(paste(rep(" ",40), collapse=" "), "fuente: CORES"))), ""))))
      p<-p + theme(panel.background = element_rect(colour = "pink"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_text(size = 14),
                   plot.title = element_text(size=20),
                   axis.text.y = element_text(size=10),
                   axis.text.x = element_text(size=8, angle = 45, hjust = 1),
                   strip.text.x = element_text(size=8),
                   strip.background = element_rect(colour = "black", size=0.25),
                   panel.border = element_rect(fill=NA, colour = "black", size=0.5),
                   legend.title=element_blank(),
                   legend.text = element_text(size=12),
                   legend.position = "bottom")
      p<-p+scale_colour_manual(values = c("valor mensual" = "gray", "media móvil 12 meses" = "blue"))
      p
}
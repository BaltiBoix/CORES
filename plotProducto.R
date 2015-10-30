plotProducto<-function(){

      p<-ggplot()
      p<-p+geom_line(data=df1g, aes(x=date, y=consumomensual/1E4, colour="valor mensual"))
      p<-p+geom_line(data=df1mag, aes(x=date, y=consumomensual/1E4, colour="media movil 12 meses"), size=1)
      p<-p+facet_wrap(~ producto, scales = "free_y")
      p<-p + scale_y_continuous("Consumo mensual t/mes ( x 10,000)")
      p<-p+ggtitle(bquote(atop(.(paste("Consumo de productos petrolíferos", "")), 
                               atop(italic(.(paste(paste(rep(" ",40), collapse=" "), "fuente: CORES"))), ""))))
      p<-p + theme(panel.background = element_rect(colour = "pink"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_text(size = 14),
                   plot.title = element_text(size=20),
                   axis.text.y = element_text(size=12),
                   axis.text.x = element_text(size=12),
                   strip.text.x = element_text(size=12),
                   strip.background = element_rect(colour = "black", size=0.25),
                   panel.border = element_rect(fill=NA, colour = "black", size=0.5),
                   legend.title=element_blank(),
                   legend.text = element_text(size=14),
                   legend.position = "bottom")
      p<-p+scale_colour_manual(values = c("valor mensual" = "gray", "media movil 12 meses" = "blue"))
      print(p)

}
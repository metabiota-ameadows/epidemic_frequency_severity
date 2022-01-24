library(ggplot2)
library(data.table)
theme_set(theme_bw())
options(scipen = 99999)
figDat <- fread("./data/figureData.csv")
figDat[, .(xlab, reported_deaths, fitDeath, fitDeathCovid)]
figDat <- figDat[Event_start_demidecade <2030, ]

figDat

logybreak_b <- c(10^seq(0, 12, 1))
logylab <- format(logybreak_b, big.mark = ",")

apx_fig <- ggplot() + 
  geom_point(data= figDat[Event_start_demidecade <= 2020],
             aes(x = Event_start_demidecade, y = reported_deaths))+ 
  geom_line(data = figDat[Event_start_demidecade <= 2020], 
            aes(x = Event_start_demidecade, y = fitDeath), color = "navy")+
  geom_line(data = figDat[Event_start_demidecade >= 2020], 
            aes(x = Event_start_demidecade, y = fitDeath), color = "navy", linetype = "dotted")+
  geom_line(data = figDat[Event_start_demidecade <=2020 ], 
            aes(x = Event_start_demidecade, y = fitDeathCovid), color = "firebrick")+
  geom_line(data = figDat[Event_start_demidecade >= 2020], 
            aes(x = Event_start_demidecade, y = fitDeathCovid), color = "firebrick", linetype = "dotted")+
  geom_line(data = figDat,
            aes(x = Event_start_demidecade, y = uprDeathPICovid), color = "firebrick1",  linetype = "dashed")+
  geom_line(data = figDat, 
            aes(x = Event_start_demidecade, y = lwrDeathPICovid), color = "firebrick1",  linetype = "dashed")+
  scale_x_continuous(breaks = seq(1960, 2025, by = 5))+
  ylab("Deaths") +  xlab("")+
  scale_y_log10(labels = logylab, breaks = logybreak_b) +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust= 0.5))


ggsave(apx_fig, filename = "./figures/apx_fig.jpeg", dpi = 300, width = 4.5, height = 4,
       units = "in", device = "jpeg")


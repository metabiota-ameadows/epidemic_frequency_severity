library(ggplot2)
library(data.table)
theme_set(theme_bw())
options(scipen = 99999)
figDat <- fread("./data/figureData.csv")
figDat <- figDat[Event_start_demidecade <2030, ]
logybreak <- c(1, 10, 100, 500)
fig1a <- ggplot() + 
  geom_point(data= figDat[Event_start_demidecade <= 2015],
             aes(x = Event_start_demidecade, y = reported_events))+ 
  geom_line(data = figDat[Event_start_demidecade <=2015 ], 
            aes(x = Event_start_demidecade, y = fitEvent), color = "navy")+
  geom_line(data = figDat[Event_start_demidecade >= 2015], 
            aes(x = Event_start_demidecade, y = fitEvent), color = "navy", linetype = "dotted")+
  geom_line(data = figDat,
            aes(x = Event_start_demidecade, y = uprEventPI), color = "steelblue",  linetype = "dashed")+
  geom_line(data = figDat, 
            aes(x = Event_start_demidecade, y = lwrEventPI), color = "steelblue",  linetype = "dashed")+
  scale_x_continuous(breaks = seq(1960, 2025, by = 5))+ 
  theme(axis.text.x = element_blank())+
  ylab("Outbreaks") +  xlab("")+
  scale_y_log10(breaks = logybreak) +
  ggtitle("a)")
fig1a

logybreak_b <- c(10^seq(0, 8, 1))
logylab <- format(logybreak_b, big.mark = ",")

fig1b <- ggplot() + 
  geom_point(data= figDat[Event_start_demidecade <= 2015],
             aes(x = Event_start_demidecade, y = reported_deaths))+ 
  geom_line(data = figDat[Event_start_demidecade <=2015 ], 
            aes(x = Event_start_demidecade, y = fitDeath), color = "navy")+
  geom_line(data = figDat[Event_start_demidecade >= 2015], 
            aes(x = Event_start_demidecade, y = fitDeath), color = "navy", linetype = "dotted")+
  geom_line(data = figDat,
            aes(x = Event_start_demidecade, y = uprDeathPI), color = "steelblue",  linetype = "dashed")+
  geom_line(data = figDat, 
            aes(x = Event_start_demidecade, y = lwrDeathPI), color = "steelblue",  linetype = "dashed")+
  scale_x_continuous(breaks = seq(1960, 2025, by = 5))+
  ylab("Deaths") +  xlab("")+
  scale_y_log10(labels = logylab, breaks = logybreak_b) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 5))+
  ggtitle("b)")
fig1b

fig1c <- ggplot() + 
  geom_point(data= figDat[Event_start_demidecade <= 2015],
             aes(x = Event_start_demidecade, y = reported_events), size = 0.75)+ 
  geom_line(data = figDat[Event_start_demidecade <=2015 ], 
            aes(x = Event_start_demidecade, y = fitEvent), color = "navy")+
  geom_line(data = figDat[Event_start_demidecade >= 2015], 
            aes(x = Event_start_demidecade, y = fitEvent), color = "navy", linetype = "dotted")+
  geom_line(data = figDat,
            aes(x = Event_start_demidecade, y = uprEventPI), color = "steelblue",  linetype = "dashed")+
  geom_line(data = figDat, 
            aes(x = Event_start_demidecade, y = lwrEventPI), color = "steelblue",  linetype = "dashed")+
  scale_x_continuous(breaks = seq(1960, 2025, by = 5), labels = figDat$xlab)+
  ylab("Outbreaks") +  xlab("")+
  coord_cartesian(ylim=c(0, 75))+
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust= 0.5))+
  ggtitle("c)")
fig1c
fig1d <- ggplot() + 
  geom_point(data= figDat[Event_start_demidecade <= 2015],
             aes(x = Event_start_demidecade, y = reported_deaths))+ 
  geom_line(data = figDat[Event_start_demidecade <=2015 ], 
            aes(x = Event_start_demidecade, y = fitDeath), color = "navy")+
  geom_line(data = figDat[Event_start_demidecade >= 2015], 
            aes(x = Event_start_demidecade, y = fitDeath), color = "navy", linetype = "dotted")+
  geom_line(data = figDat,
            aes(x = Event_start_demidecade, y = uprDeathPI), color = "steelblue",  linetype = "dashed")+
  geom_line(data = figDat, 
            aes(x = Event_start_demidecade, y = lwrDeathPI), color = "steelblue",  linetype = "dashed")+
  scale_x_continuous(breaks = seq(1960, 2025, by = 5), labels = figDat$xlab)+
  ylab("Deaths") +  xlab("")+
  coord_cartesian(ylim=c(0, 10000))+
  scale_y_continuous(labels = format(seq(2500, 10000, 2500), big.mark = ","), breaks = seq(2500, 10000, 2500)) +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust= 0.5))+
  ggtitle("d)")

fig1 <- gridExtra::grid.arrange(fig1a, fig1b, fig1c, fig1d, ncol = 2)

ggsave(fig1, filename = "./figures/fig1.jpeg", dpi = 300, width = 8, height = 8.5,
       units = "in", device = "jpeg")


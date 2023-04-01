source("R/convplot.R")
library(tidyverse,ggthemes,ggrepel)
load("data/ifadv.rda")

extract <- convplot(data=ifadv,uid="dutch-04-091-125268", before=0,after=30000,datamode=T)

extract |>
  ggplot(aes(x=participant_int,label=utterance,colour=participant,fill=participant)) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  coord_cartesian(xlim=c(0,10)) +
  labs(y="time (ms)",x="") +
  scale_y_reverse() +
  geom_rect(aes(ymin=begin0,ymax=end0,xmin=participant_int-0.4,xmax=participant_int+0.4)) +
  ggrepel::geom_text_repel(aes(y=begin0,x=participant_int),
            hjust=0,direction="y",nudge_x= ifelse(extract$participant_int==1,2,1))


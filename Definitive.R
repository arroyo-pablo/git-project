library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggtext)
library(ggsci)
library(pdftools)
library(ggforce)
library(glue)
library(systemfonts)
library(cowplot)


data<- read_delim("Clean_Data.csv", delim = ";", col_types = "cnnnn"  )

data_longer<-data %>% 
  pivot_longer(c("2018", "2021", "2022", "2023")) %>% 
  mutate(
    Comunidad=as.factor(Comunidad),
    name=as.factor(name),
  )

sysfonts::font_add_google("Be Vietnam Pro")
showtext::showtext_opts(dpi=300) ##Ojooo para que no se modifiquen los tamaños.

p<-ggplot(data_longer) +
  aes(x=as.factor(Comunidad), y=as.numeric(value))+
  
  geom_bar(position = "dodge", stat = "identity", width = 0.63) + #Code to create the whole plot in genera #Code to change from a bar plot in vertical to a horizontal position.
  
  aes(fill = name) + #Code to start playing with the colors of the bars.
  
  #scale_fill_discrete(breaks=c("2023","2022","2021", "2018")) +
  
  scale_fill_manual(breaks=c("2023","2022","2021", "2018"), 
                    values = c("#ff1e26", "#5f9dc6", 	"#8ccaf9", "#c5daeb"), #Changing the order 2023, 2022, 2021 and adding the colours.
  )+ #With this code we change the insane of the bars, but not the outside!!!!
  
  scale_y_continuous(
    breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000),
    limits = c(0, 3100),
    sec.axis = dup_axis(),
    name = NULL,
    expand = c(0,0)
  ) +
  
  scale_x_discrete(
    name = NULL,
    limits = c("Ceuta", "Melilla", "La Rioja", "Navarra", "Baleares", "Cantabria", "Canarias", "Asturias", "Extremadura", "Murcia", "Aragón", "País Vasco", "Castilla-La Mancha", "Castilla y León", "Galicia", "C. Valenciana", "Com. de Madrid", "Cataluña", "Andalucia"),
    labels = c("Ceuta", "Melilla", "La Rioja", "Navarra", "Baleares", "Cantabria", "Canarias", "Asturias", "Extremadura", "Murcia", "Aragón", "País Vasco", "Castilla-La Mancha", "Castilla y León", "Galicia", "C. Valenciana", "Com. de Madrid", "Cataluña", "Andalucia"),
    expand = c(0,0)
  ) +
  
  labs(
    title = "EL REPARTO DE LA INVERSIÓN ESTATAL POR CCAA",
    subtitle = "INVERSIÓN REAL \n\n\n",
    caption = "Se añaden 20,52 millones de euros para Ceuta y Melilla \n"
  ) +
  
  labs(fill = "En millones de euros") +
  
  theme_minimal()+
  
  theme(
   
    panel.grid = element_blank(), #Code to set the background of the graph empty.
    #panel.grid.major.x = element_line(linetype = "dotted", color = "#7FB5B5"), #Adding vertical dotted lines.
    axis.title.x = element_text(hjust=.5, color="black", size=10, face = "bold", margin = margin(10,0,0,0)),
    axis.text = element_text(color="black", size=8 ),
    #panel.grid.major.y = element_line(color = "black", linetype = "solid"),
    axis.text.y = element_text(color="black",
                               size=10), #Changing the color and size of the communities.
    #axis.line.y = element_line(color = "black"), #Adding the black line after the name of the communities.
    axis.ticks.x.top = element_line(
      size = .5#,
      #arrow = arrow(length = unit(.2, "lines"), ends = "first", type = "closed")
    ), #I have to change the arrow for a normal line, but it´s how it works I think.
    axis.ticks.x.bottom = element_line(
      size = .5#,
      #arrow = arrow(length = unit(.2, "lines"), ends = "last", type = "closed")
    ),
    plot.title = element_text(hjust=-0, size=11, color="black",lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    #plot.title = element_text(size = 14, face = "bold", family = "Noto Sans"),
    plot.subtitle = element_text(hjust=0, vjust = 1.2, size=9.5, color= "black", face = "bold", margin=margin(10,0,10,0)),
    #plot.subtitle = element_text(size = 12, face= "bold", colour = "black", family = "Noto Sans"),
    plot.caption = element_text(hjust=.5,margin=margin(10,0,0,0), size=8, color="black", face="bold"),
    #plot.caption = element_text(size = 12, face= "bold", colour = "black", family = "Noto Sans")
    plot.background = element_rect(color="#ffffff", fill="#ffffff"),
    plot.margin = margin(5,5,5,5),
    #legend.position = c(0.08,0.75), I need to play with it to modify where the legend should be
    legend.direction = "horizontal",
    legend.position = c(-0.05, 1.05), #The first number to move it left or right, the second to move it top or down
    legend.margin = margin(0),
    legend.title = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 8),
    legend.spacing.x = unit(0.09, 'cm'),
    legend.key.size = unit(0.6, "cm"),
    legend.key.height= unit(0.3, 'cm'),
    legend.key.width = unit(0.65, "cm"),
    legend.spacing.y = unit(0.1,'cm')
    )+ ##Code to add a label above each column. I couldn´t find another solution, if I decided to group it disappeared-
  guides(fill= guide_legend(title.position = "top", title.hjust = 0)) + #Code to add the tittle of the legend 1 line above.
  theme(plot.title.position = "plot")+ #This is the way that GGPLOT have to aling to the whole plot intead of just the panel.
  theme(plot.caption.position = "plot")+  # I think that it is doing nothing at all, but together, with the line below it works.
  theme(plot.caption = element_markdown(hjust = 0))
  

p2<-p + geom_text(
  aes(label= ifelse(value == c("2318.9"), value, NA)), 
  position = position_dodge(0.8),
  hjust = -0.1,
  vjust = 0.75, size = 2.8,
  colour = "#262626"
) +
  geom_text(
    aes(label= ifelse(value == c("2308.9"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("1305.4"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1, 
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("1269.5"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("1077.9"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("955.8"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("600.7"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("558.8"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("547.9"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("481.5"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) + 
  geom_text(
    aes(label= ifelse(value == c("473.9"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  )+
  geom_text(
    aes(label= ifelse(value == c("420.9"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("391.9"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("296.6"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("186.3"), value, NA)), 
    position = position_dodge(0.8),
    hjust = - 0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("103.8"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) + 
  geom_text(
    aes(label= ifelse(value == c("71.3"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) + 
  geom_text(
    aes(label= ifelse(value == c("45.3"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  geom_text(
    aes(label= ifelse(value == c("28.9"), value, NA)), 
    position = position_dodge(0.8),
    hjust = -0.1,
    vjust = 0.75, size = 2.8,
    colour = "#262626"
  ) +
  #geom_vline(xintercept=c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 17.5, 18.5, 19.5), linetype="dashed") +
  
  
  coord_flip(clip = "off", ylim = c(0, 3100))+
  geom_segment(
    data = data.frame(x = seq(1.5, 18.5, 1), ymin =0, ymax = 3100),
    aes(x = x, xend = x, y = ymin, yend = ymax),
    inherit.aes = FALSE,
    color = "#6c6c6b"
  ) +
  theme(axis.text.y = element_text(hjust = 0)) #This is the code to aling the name of the comunities to the left.


p3<- ggdraw(add_sub(p2, "FUENTE:PGE 2023, 2022, 2021 Y 2018 \nP.Arroyo | EL MUNDO GRAFICOS", hjust = 0.98, vjust = 0.85, size = 7.8, color = "black", lineheight = 0.85))


ggsave("pepitos_320.png", height = 12, width = 4, dpi = 320)
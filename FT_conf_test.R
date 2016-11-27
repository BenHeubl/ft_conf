# ft theme: 

install.packages("ggplot2")

theme_ft <-   theme(legend.position = "top",
                    plot.background = element_rect(fill = "#F5E8D7"),
                    panel.grid.minor = element_line(size = 0.2, color = '#A6A29F', linetype = 2),
                    panel.grid.major.x = element_line(size = 0.2, color = '#A6A29F', linetype = 2),
                    panel.grid.major.y = element_line(size = 0.2, color = '#A6A29F', linetype = 2),
                    legend.background = element_rect(fill = "#F5E8D7"),
                    panel.background = element_rect(fill = "#F5E8D7"),
                    plot.title = element_text(hjust = 0.005, vjust = 2),
                    text = element_text(family="OfficinaSanITC Book"))




# Status of PPP projects awarded during the Aquino administration
# public-private partnership projects (PPPs)
sheets1 <- read.csv("/Users/BH/BLOG/FT_confidential/ft_conf/sheet1.csv", header = T, stringsAsFactors = F)
View(sheets1)
library(tidyverse)
library(forcats)

# str(sheets1)
sheet_1_plot <- ggplot(sheets1, aes(fct_reorder(project, cost), cost)) + 
  # theme_minimal(base_family = "RobotoCondensed-Regular") +
  # theme(plot.title=element_text(family="Roboto-Bold")) +
  # theme(legend.title=element_blank()) +
  scale_fill_manual(values = c("#8F223A","#505B70", "#A6A29F"))+
  scale_colour_manual(values = c("#8F223A","#505B70", "#A6A29F"))+
  geom_bar(show.legend = T,stat = "identity", aes(fill= status)) +
  # geom_point(show.legend = T,aes(color= status)) + 
  theme_ft + coord_flip() +
theme(legend.position = c(0.7, 0.2)) +
  labs(x = "",
       y = "Cost (pesos, bn)",
       title = "Status of PPP projects awarded during the Aquino administration",
       subtitle = "Source: ") +
  ggsave("/Users/BH/BLOG/FT_confidential/ft_conf/Sheet1.png", height = 15, width = 30, units = "cm")
  
  # xlab("") + ylab("Cost (pesos, bn)") + 


gg <- sheet_1_plot

subtitle <- 
  "Source: PPP Center of the Philippines"

ggplot_with_subtitle(gg, subtitle,
                     bottom_margin=20, lineheight=0.9)




##### 
# removed lenovo and nokia from data for the Phillipines, no data available to understand 
# year-on-year differences
sheets2 <- read.csv("/Users/BH/BLOG/FT_confidential/ft_conf/Sheet2.csv", header = T, stringsAsFactors = F)
str(sheets2)

p2 <- ggplot(sheets2, aes(brand, share, frame = year,  show.legend = F)) +
  # geom_bar(color = "#8F223A", alpha = 0.3, 
  #          show.legend = T,stat = "identity", aes(fill= "#8F223A")) +
  geom_point(color = "#8F223A", alpha = 0.3) +
  geom_path(aes(cumulative = TRUE, group = brand), size = 0.4, color = "#8F223A", linetype = 2) +
  # 8F223A 
  scale_colour_manual(values = c("#8F223A", "red")) +  
  geom_text(show.legend = F, aes(label = year,
                                 color = "#8F223A"
  ), check_overlap = T, nudge_x = 0.3) +
  # geom_smooth(aes(group = year, color = "#8F223A"), method = "lm", show.legend = FALSE, se = F, size = 0.4, color = 'red', linetype = 2) +
  # scale_x_log10() + 
  theme_ft + ggtitle("Year: ") + 
  facet_wrap(~country, scales = "free", nrow = 1) + 
  theme(legend.position = c(0.7, 0.2)) +
  labs(x = "",
       y = "Cost (pesos, bn)",
       title = "Five phone brands consumers most likely to purchase in next six months by country \n YEAR:") + 
  # ggsave("/Users/BH/BLOG/FT_confidential/ft_conf/Sheet2.png", height = 20, width = 50, units = "cm")

p2
gg_animate(p2, "/Users/BH/BLOG/FT_confidential/ft_conf/Sheet2.gif", interval = 1, ani.width=1400, ani.height=500)


### sheet3
sheets3 <- read.csv("/Users/BH/BLOG/FT_confidential/ft_conf/Sheet3.csv", header = T, stringsAsFactors = F)
str(sheets3)

View(sheets3)
sheets3 <- sheets3 %>%
  group_by(category) %>%
  mutate(is_max = as.numeric(max(rating)))

ggplot(sheets3, aes(fct_reorder(category, rating), rating)) + 
  # theme_minimal(base_family = "RobotoCondensed-Regular") +
  # theme(plot.title=element_text(family="Roboto-Bold")) +
  # theme(legend.title=element_blank()) +
  scale_fill_manual(values = c("#8F223A","#505B70", "#A6A29F", "#FFCFB3", "black", "grey", 
                               "#00B0F1", "#237491", "#A36A2D"))+
  scale_colour_manual(values = c("#8F223A","#505B70", "#A6A29F"))+
  guides(alpha=FALSE) +
  geom_bar(stat = "identity", position = "dodge",
           aes(fill= channel, alpha = if_else(rating == is_max, 1, 0.9))) +
  # geom_point(show.legend = T,aes(color= status)) + 
  theme_ft + coord_flip() +
  theme(legend.position = c(0.85, 0.92)) +
  labs(x = "",
       y = "Share",
       title = "Mobile phone purchase channels",
       subtitle = "Source: ") + ylim(c(0, 60)) +
  # sub: Q: Via which channel do you plan to purchase your next mobile phone?
  # Source: FT Confidential Research
  ggsave("/Users/BH/BLOG/FT_confidential/ft_conf/Sheet3.png", height = 30, width = 25, units = "cm")



## sheet 4
sheets4 <- read.csv("/Users/BH/BLOG/FT_confidential/ft_conf/Sheet4.csv", header = T, stringsAsFactors = F)
str(sheets4)

p4 <- ggplot(sheets4, aes(fertility, value, frame = year,  show.legend = F)) +
  # geom_bar(color = "#8F223A", alpha = 0.3, 
  #          show.legend = T,stat = "identity", aes(fill= "#8F223A")) +
  geom_point(show.legend = T, aes(color = country), alpha = 0.3) +
  geom_path(aes(cumulative = TRUE, group = country, color = country), size = 0.4, linetype = 2) +
  # 8F223A 
  # scale_colour_manual(values = c("#8F223A", "red")) +  
  scale_colour_manual(values = c("#8F223A","#505B70", "#A6A29F", "#00B0F1", "#237491", "#A36A2D"))+
  geom_text(show.legend = F, aes(label = paste0(year),
                                 color = country
  ), check_overlap = T) +
  # geom_smooth(aes(group = year, color = "#8F223A"), method = "lm", show.legend = FALSE, se = F, size = 0.4, color = 'red', linetype = 2) +
  # scale_x_log10() + 
  theme_ft + ggtitle("Year: ") + 
  facet_wrap(~indicator, scales = "free", nrow = 1) + 
  theme(legend.position = c(0.4, 0.7)) +
  labs(x = "Fertility rates",
       y = "Gross national income per capita ($) and poverty incidence (%)",
       title = "Asean fertility rates per year compared to income and poverty levels for 2010")  +
  ggsave("/Users/BH/BLOG/FT_confidential/ft_conf/Sheet4.png", height = 15, width = 20, units = "cm")
  
  # p4
  # gg_animate(p4, ani.width=1400, ani.height=500)

p4 <- ggplot(sheets4, aes(fertility, value, frame = year,  show.legend = F)) +
  # geom_bar(color = "#8F223A", alpha = 0.3, 
  #          show.legend = T,stat = "identity", aes(fill= "#8F223A")) +
  geom_point(show.legend = F, aes(color = country), alpha = 0.3) +
  geom_path(aes(cumulative = TRUE, group = country), size = 0.4, color = "#8F223A", linetype = 2) +
  # 8F223A 
  # scale_colour_manual(values = c("#8F223A", "red")) +  
  scale_colour_manual(values = c("#8F223A","#505B70", "#A6A29F", "#00B0F1", "#237491", "#A36A2D"))+
  geom_text(nudge_y = 1, show.legend = F, aes(label = paste0(country, ": ", year),
                                              color = country
  ), check_overlap = T) +
  # geom_smooth(aes(group = year, color = "#8F223A"), method = "lm", show.legend = FALSE, se = F, size = 0.4, color = 'red', linetype = 2) +
  # scale_x_log10() + 
  theme_ft + ggtitle("Year: ") + 
  facet_wrap(~indicator, scales = "free", nrow = 1) + 
  theme(legend.position = c(0.7, 0.2)) +
  labs(x = "Fertility rates",
       y = "Gross national income per capita ($) and poverty incidence (%)",
       title = "Asean poverty and fertility rates\n YEAR:") 

gg_animate(p4, "/Users/BH/BLOG/FT_confidential/ft_conf/Sheet4.gif", interval = 1, ani.width=1400, ani.height=500)




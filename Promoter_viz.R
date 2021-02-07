#!/usr/local/bin/R
# Writting by: Omar R. Bringas
# Date: 06-02-2021
# Use: Promoter Vizualization. Promoter analized from https://www.dna.affrc.go.jp/PLACE/?action=newplace.

# Libraries
library(dplyr)
library(ggplot2)
library(ggbeeswarm)

# Data 
df <- read.csv("Promoter_Plant.csv", header = TRUE) %>%
  mutate(Category  = factor(Category, levels = levels(Category)[c(5,4,3,2,6,7,1)]))
head(df)
# prom4.1_box             Category start
# 1 ABREATCONSENSUS ABA/Déficit hídrico   -187
# 2 ABREATCONSENSUS ABA/Déficit hídrico   -147
# 3      ABREATRD22 ABA/Déficit hídrico   -148
# 4     ABRELATERD1 ABA/Déficit hídrico   -186
# 5     ABRELATERD1 ABA/Déficit hídrico   -146
# 6     ABRELATERD1 ABA/Déficit hídrico   -127

# Custom themes

##
theme_promoter <- function(){
  theme(
    # Plot/panel
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    # Grid
    #panel.grid.major.y = element_line(color = "grey50", linetype = "solid", size = 2, alpha = 0.1), 
    # X
    axis.line.x = element_line(colour = "black", size = 1),
    axis.ticks.x = element_line(color = "black", size = 0.5),
    axis.ticks.length.x = unit(0.25, "cm"),
    # Y
    axis.line.y =  element_line(colour = "grey50", size = 1),
    axis.ticks.y = element_line(color = "grey50", size =0.5),
    axis.ticks.length.y = unit(0.25, "cm"),
    # Texto
    plot.title = element_text(color = "black", hjust = 0.5, size = 12),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 10),
    plot.caption = element_text(color ="black", size = 6)
  )
}

# A cyberpunk theme
# Credit: https://github.com/business-science/free_r_tips/blob/master/016_cyberpunk_ggplot/016_cyberpunk_ggplot.R

clr_bg <- "black"
clr_bg2 <- "gray10"
clr_grid <- "gray30"
clr_text <- "#d600ff"

theme_cyberpunk <- function() {
  theme(
    # Plot/ Panel
    plot.background = element_rect(fill = clr_bg, colour = clr_bg),
    #plot.margin = margin(1.5, 2, 0.5, 1.5, "cm"),
    panel.background = element_rect(fill = clr_bg, color = clr_bg),
    panel.grid = element_blank(),
    # 
    axis.ticks.x = element_line(colour = clr_grid, size = 1),
    axis.line.y = element_line(colour = clr_grid, size = 0.5),
    axis.line.x = element_line(colour = clr_grid, size = 0.5),
    axis.ticks.length.y = unit(0.25, "cm"),
    axis.ticks.length.x = unit(0.25, "cm"),
    # text
    plot.title = element_text(colour = clr_text, hjust = 0.5, size = 12),
    axis.text = element_text(colour = clr_text,  size = 10),
    axis.title = element_text(colour = clr_text),
    plot.caption = element_text(color =clr_text, size = 6)
  )
}

# Make a Custom plot with first theme
prom_normal <- ggplot(df, aes(x = Category, y = start)) +
  geom_beeswarm(aes(color = Category), shape = "\u2691", size = 6, cex = 2.5) +
  geom_vline(xintercept = c(1:length(unique(df$Category))), color = "grey50", size =2, linetype = "solid", alpha = 0.1) +
  guides(color = FALSE) +
  labs(title = "Elementos en cis en el promotor de un gen en plantas.",
       y = NULL, x = "Categoría", caption = "Promotor analizado en New PLACE\n(https://www.dna.affrc.go.jp/PLACE/?action=newplace)") +
  coord_flip() +
  theme_promoter() 

prom_normal #+ ggsave("Promoter_viz_theme1.jpg", width = 12, height = 8, units = "in", dpi = 300)

# Make a plit with a cyber theme

prom_cyber <- ggplot(df, aes(x = Category, y = start)) +
  geom_beeswarm(aes(color = Category), shape = "\u2691", size = 6, cex = 2.5) +
  geom_vline(xintercept = c(1:length(unique(df$Category))), color = clr_grid, size =2, linetype = "solid", alpha = 0.3) +
  guides(color = FALSE) +
  labs(title = "Elementos en cis en el promotor de un gen en plantas.",
       y = NULL, x = "Categoría", caption = "Promotor analizado en New PLACE\n(https://www.dna.affrc.go.jp/PLACE/?action=newplace)") +
  coord_flip() +
  theme_cyberpunk()

prom_cyber #+ ggsave("Promoter_viz_theme2.jpg", width = 12, height = 8, units = "in", dpi = 300)

library(tidyverse)
library(terra)
library(ozmaps)
library(paletteer)

# Shortcut plotting functions
prettyplot <- function() {
  theme_classic() +
    theme(
      text = element_text(family = "serif", size = 12, colour = "black"),
      legend.position = "none",
      axis.title.y = element_text(vjust = 1.5),
      axis.title.x = element_text(vjust = 1.5),
      legend.title = element_blank()
    )
}

rm_y_axis <- function() {
  theme(
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank()
  )
}

rm_x_axis <- function() {
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
  )
}

# Functions for nicer mapping

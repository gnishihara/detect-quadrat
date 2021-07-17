# Quadrat Detection Function
# Greg Nishihara
# 2021 July 17

# Load packages
# パッケージを読み込む
library(tidyverse)
library(magick)
source("detect_quadrat.R")

f1 = "~/Lab_Data/tanimaes/share_files/quadrat_01_210318.JPG"
f2 = "~/Lab_Data/tanimaes/share_files/quadrat_02_210623.JPG"

img1 = image_read(f1)
img2 = image_read(f2)

imgggplot1 = img1 |> image_ggplot()
imgggplot2 = img2 |> image_ggplot()
hdata1 = img1 |> detect_quadrat()
hdata2 = img2 |> detect_quadrat()

p1 = imgggplot1 + 
  geom_hline(aes(yintercept = yintercept), data = hdata1, color = "white", size = 2) +
  geom_vline(aes(xintercept = xintercept), data = hdata1, color = "white", size = 2)
p2 = imgggplot2 + 
  geom_hline(aes(yintercept = yintercept), data = hdata2, color = "white", size = 2) +
  geom_vline(aes(xintercept = xintercept), data = hdata2, color = "white", size = 2)


ofile1 = basename(f1) |> str_replace(".JPG", "_squared.jpg")
ofile2 = basename(f2) |> str_replace(".JPG", "_squared.jpg")
ggsave(ofile1, p1, width = 80, units = "mm", dpi = 100)
ggsave(ofile2, p2, width = 80, units = "mm", dpi = 100)








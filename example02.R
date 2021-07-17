# Example with squares
# 
library(tidyverse)
library(magick)
library(ggrepel)

source("detect_quadrat.R")


f1 = "~/Lab_Data/tanimaes/share_files/quadrat_01_210318.JPG"
img1 = image_read(f1)

imgggplot1 = img1 |> image_ggplot()
hdata1 = img1 |> detect_quadrat()

# Determine the sampling points here.
yint = hdata1 |> drop_na(yintercept) |> pull(yintercept) |> as.integer()
xint = hdata1 |> drop_na(xintercept) |> pull(xintercept) |> as.integer()

# The offset is needed to avoid the vertical and horizontal lines
# Off set is in pixels
offset = 100
yint = round(yint + c(offset,-offset), -1)
xint = round(xint + c(offset,-offset), -1)

# The sampling point will be chosen randomly over an evenly spaced grid.
N = 250
y = seq(yint[1], yint[2], by = N)
x = seq(xint[1], xint[2], by = N)

z = expand_grid(x,y) |> sample_n(50)

z = z |> 
  arrange(desc(y),x) |> 
  mutate(id = 1:n()) |> 
  mutate(group = (id %/% 10)+1) |> 
  mutate(group = factor(group))


# This is to determine the squares
# The image coordinates are flipped in the detect_quadrat()
# so, we need to flip it back when squares are sampled.
# s: is the size of the square in pixels.
WIDTH = img1 |> image_info()$width
z = z |> 
  mutate(y2 = WIDTH-y) |> 
  mutate(s = 200) |> 
  mutate(crop = str_glue("{s}x{s}+{x-(s/2)}+{y2-(s/2)}"))

dout = z |> 
  mutate(img = map2(crop, id, function(CROP,n) {
    image_crop(img1, CROP) |> 
      image_annotate(n, 
                     size = 30,
                     font = "Noto Sans",
                     weight = 700,
                     gravity = "southwest",
                     color = "white") |> 
      image_border(color = "white", geometry = "2x2")
  }))
 
# Extract the images from the tibble and restructure it so that
# we can use it in magick.
imgs = dout |> pull(img) 
imgs = do.call(c, unlist(imgs)) # Very important part.
i1 = image_append(imgs[1:10])
i2 = image_append(imgs[11:20])
i3 = image_append(imgs[21:30])
i4 = image_append(imgs[31:40])
i5 = image_append(imgs[41:50])
p0 = image_append(c(i1, i2, i3, i4, i5), stack = T) |> image_resize("3000x")
p0 = p0 |> image_ggplot() # make it a ggplot to append to the other plot.

################################################################################

p1 = imgggplot1 +
  geom_hline(aes(yintercept = yintercept), data = hdata1, color = "white", size = 2) +
  geom_vline(aes(xintercept = xintercept), data = hdata1, color = "white", size = 2) +
  geom_point(aes(x = x, y =y), 
             shape = 22, size = 5, 
             stroke = 1,
             data = z,
             col = "white") +
  geom_label_repel(aes(x = x, y = y, label = id, fill = group),
                   color = "white",
                   data = z,
                   size = 3,
                   box.padding = grid::unit(2, "mm"),
                   min.segment.length = unit(10, "mm"),
                   seed = 2020) +
  annotate("text", x = 50, y = 50,
           label = basename(f1), color = "white",
           vjust = 0, hjust = 0, size = 6) + 
  scale_color_viridis_d() +
  guides(fill = "none")

# This part is a bit hacky.
# Save the plot to a png file, then resize with magick.
# You do not need to resize if you want to look at the original ggplot.
# Which is big.
library(patchwork)
p01 = p0/p1


ofile1 = basename(f1) |> str_replace(".JPG", "_with_squares.png")
ggsave(str_glue("{ofile1}"), p01, height = 5000, units = "px")

# Optional part to decrease file size.
sfile1 = ofile1 |> str_replace(".png", "_small.png")
image_read(ofile1) |> 
  image_trim() |>
  image_resize("1000x") |> 
  image_write(sfile1)
















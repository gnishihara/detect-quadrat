# Quadrat Detection Function
# Greg Nishihara
# 2021 July 17

# Load packages
# パッケージを読み込む
library(tidyverse)
library(magick)


# Quadrat Detection Function
# img: magick で読み込んだ画像 must be a magick image
# analysis_W: 処理画像の大きさ (pixel) size of image for processing
# 処理画像の大きさは小さいほど処理がはやいが、1000　の pixel 幅しかテストしていない
# 処理画像の解像度を変えたら、Canny法とHough変換のパラメータの変換も必要でしょう（試していない）
# If the size of the image is changed from the defautl 1000, then you will probably
# need to change the parameters for the Canny filter and the Hough transform.

detect_quadrat = function(img, analysis_W = 1000) {
  ################################################################################
  # Decrease the size of the image to speed up the process
  # ここで処理用の画像をつくる
  conversion_rate = image_info(img)$width / analysis_W
  imgs = img |> image_resize(str_glue("{analysis_W}x")) 
  H = image_info(imgs)$height
  W = image_info(imgs)$width
  
  ################################################################################
  # Canny edge detection filter
  # Canny 法によるエッジ検出
  canny_radius = 0
  canny_sigma = 1
  canny_lower = 5
  canny_upper = 10
  
  CANNY = str_glue("{canny_radius}x{canny_sigma}+{canny_lower}%+{canny_upper}%")
  
  img2 = imgs |> 
    image_channel("GRAY") |> 
    image_equalize()  |> 
    image_canny(CANNY) 
  
  ##############################################################################
  # Create a black edge and box to remove as many unneeded edges as possible.
  # Hough 変換 (ハフ変換) に渡すエッジを減らしたいので、画像の縁と中央を黒く塗る
  box = image_blank(0.6*H, 0.6*H, color = "black")
  z = image_blank(W, H, "transparent")
  z = image_border(z,color = "black", geometry = "10x10")
  z = z |> image_resize(str_glue("{W}x"))
  
  img2 = image_composite(img2, z, gravity = "center")
  img2 = image_composite(img2, box, gravity = "center")
  
  # Thin the edges, to reduce the size of unimportant edges.
  img2 = img2 |> 
    image_morphology("Thinning", "LineJunctions", iterations = 15) |> 
    image_morphology("Thinning", "LineEnds", iterations = 10) |> 
    image_despeckle(10)
  
  ##############################################################################
  # Apply a Hough transform to detect straight lines
  # Hough変換で直線を検出する
  hough_width   = 30
  hough_height  = 30
  hough_threshold = 110
  
  HOUGH  = str_glue("{hough_width}x{hough_height}+{hough_threshold}")
  tmp = img2 |> image_hough_txt(geometry = HOUGH, format = "mvg") 
  
  ##############################################################################
  # Extract the results from the Hough transform and remove lines that are clearly
  # not horizontal or vertical.
  # Hough変換で検出線の情報はここで集めました。鉛直と水平の線だけ残すようにしています。
  # 
  hdata = tibble(z = read_lines(tmp, skip = 3)) |> 
    separate(z, sep = " ", into = str_glue("v{1:8}")) |> 
    separate(v2, into = c("x1", "y1"), sep = ",") |> 
    separate(v3, into = c("x2", "y2"), sep = ",") |> 
    select(x1:y2, count = v6, angle = v7, distance = v8) |> 
    mutate(across(everything(), as.numeric)) |> 
    mutate(group = ifelse(between(angle, 85, 95), "horizontal", "vertical")) |> 
    filter(!between(angle, 95, 175)) |>
    filter(!between(angle, 5, 85)) |>
    mutate(across(c(y1,y2), ~ H - .))
  hdata = hdata |> mutate(across(c(x1,x2,y1,y2), ~conversion_rate * .))
  
  # 画像のよって、縦と横線は複数検出されます。もっといい方法を考えるまでは、
  # 平均をとレンジでまとめる。
  # Sometimes there are multiple vertical and horizontal lines. Until I get a 
  # better idea, I am just using the mean values.
  hdata |> 
    group_nest(group) |> 
    mutate(data = map2(data, group, function(X,G) {
      if(str_detect(G, "vertical")) {
        z = X |> mutate(x = (x1 + x2)/2) |> pull(x) |> range()
        tibble(xintercept = z)
      } else {
        z = X |> mutate(y = (y1 + y2)/2) |> pull(y) |> range()
        tibble(yintercept = z)
      }
    })) |> 
    unnest(data)
}


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











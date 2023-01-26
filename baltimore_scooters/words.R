library(tidyverse)
library(magick)
library(glue)
library(colorspace)
library(showtext)
library(extrafont)

# Load `header` list with needed data
header <- readRDS("header.rds")
colors <- header$colors
swatchplot(colors)

text_color <- colors[5]

img <- image_read(header$outfile)


annot <- glue("This map shows the availability of dockless vehicles in Baltimore for January 14—15, 2023. ",
              "Scooter data is collected every 15 minutes ",
              "and binned into 200 meter hexagons.") %>% 
  str_wrap(40)
cat(annot)


test2 <- img %>% 
  image_crop(geometry = "4000x4000+0+0", gravity = "center") %>% 
  image_annotate(text = "Baltimore Scooter Availability", gravity = "northwest",
                 location = "+200+100", font = "Manrope",
                 color = text_color,
                 size = 225, weight = 900) %>% 
  image_annotate(text = annot, gravity = "southwest",
                 location = "+200+400", font = "Manrope",
                 weight = 500,
                 color = alpha(text_color, .75),
                 size = 100) %>% 
  image_annotate(text = "Data from BCDOT's public API",
                 gravity = "southwest",
                 location = "+200+150", font = "Manrope",
                 color = alpha(text_color, .5),
                 size = 60, weight = 700) %>% 
  image_annotate(text = "Graphic: Harrison DeFord (@oonuttlet)", gravity = "southwest",
                 location = "+200+225", font = "Manrope",
                 color = alpha(text_color, .5),
                 size = 60, weight = 700) %>% 
  image_write("bin/baltimore_scooters_annot.png")

image_read("bin/baltimore_scooters_annot.png") %>% 
  image_scale(geometry = "40%x") %>% 
  image_write("bin/baltimore_scooters_twitter.jpg")


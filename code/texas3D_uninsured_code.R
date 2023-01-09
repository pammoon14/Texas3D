library(tidyverse)

## Part 1: Plotting a 2D map

# Load texas_cdc file
uninsured <- read.csv("C:\\Users\\pmoon\\OneDrive\\Desktop\\Texas3D\\data\\texas_uninsured_csv.csv")


# Read shapefile
library(sf)
mymap <- st_read("data/shapefiles/geo_export_6791d085-0532-43cb-bb45-2def40798f3e.shp")



# Join mymap with median_income with pipes
library(dplyr)
uninsured_mapdata <- mymap %>% left_join(uninsured, by="name")


# Plotting
library(ggplot2) 
library(colorspace)
uninsured_map = ggplot(uninsured_mapdata)+ 
                    geom_sf(data = uninsured_mapdata, aes(fill = uninsured_percent))+
                    scale_fill_gradient(low="#E3784D", high="#D61010")+
                labs(fill = "Percent Uninsured")


uninsured_map= uninsured_map + theme(
                        legend.title = element_text(face = "bold"),
                        legend.text=element_text(color= "#24190C"),
                        legend.position = "right",
                        legend.background = element_rect(fill = "transparent"),
                        axis.line = element_blank(), 
                        axis.text.x=element_blank(), axis.title.x=element_blank(),
                        axis.text.y=element_blank(), axis.title.y=element_blank(),
                        axis.ticks=element_blank(), 
                        plot.background = element_rect(fill = "#FFEEDB"),
                        panel.background = element_rect(fill = "#FFEEDB"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.border = element_blank())
                        
uninsured_map



################


# Part 2: Convert to 3D Map

library(rayshader)


plot_gg(uninsured_map,
        multicore = TRUE,
        shadow_intensity = 0.7,
        width= 5,
        height=5,
        scale=250,
        zoom = 0.65,
        windowsize=c(1400,866))
        

# Render Videos (Credits for code snippet go to cyda - Carrie Lo & Yeung Wong on GitHub)
library(rgl)
install.packages("av")
library(av)


# Video 1
phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = 0 + 45 * sin(seq(0,359,length.out = 360) * pi/180)
zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull = c(zoomvec, rev(zoomvec))
render_movie(filename = 'output1', type = "custom", frames = 360,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)
render_snapshot("uninsured_map.png")

# Video 2
transition_values <- function(from, to, steps = 10, one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin"))){stop("type must be one of: 'cos', 'lin'")}
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range)/2
  if (type == "cos") {scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))}
  else if (type == "lin"){
    if (one_way) {xout <- seq(1, -1, length.out = steps)} 
    else {xout <- c(seq(1, -1, length.out = floor(steps/2)), seq(-1, 1, length.out = ceiling(steps/2)))}
    scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y }
  middle - half_width * scaling
}
theta <- transition_values(from = 0, to = 360, steps = 360, one_way = TRUE, type = "lin")
phi <- transition_values(from = 10, to = 70, steps = 360, one_way = FALSE, type = "cos")
zoom <- transition_values(from = 0.4, to = 0.8, steps = 360, one_way = FALSE, type = "cos")
render_movie(filename = 'output2', type = "custom", frames = 360,  phi = phi, zoom = zoom, theta = theta)

# Close Windows
rgl.close()

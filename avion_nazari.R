#####################################################################################
## Reproducing Planes tiles located at the Patio of the Gilded Room (Cuarto Dorado)
## Alhambra Palace in Granada. UNESCO World Heritage Site, Andalusia - Spain
#####################################################################################

## Pictures : https://www.flickr.com/photos/robven/3142025904

## Load libraries ---------------------------------------------
pacman::p_load(ggplot2, data.table, dplyr)

## Functions --------------------------------------------------
rotate_90 <- function(dt, id_new){
  new_dt <- copy(dt)
  val <- max(new_dt$x)
  
  new_dt[, x := val - dt$y]
  
  new_dt[, y := dt$x]
  
  new_dt[, id := as.factor(id_new)]
  new_dt[, value := as.numeric(id_new)]
  
  return(new_dt)
}

mirror_y_down <- function(dt, id_new) {
  new_dt <- copy(dt)
  val <- max(new_dt$x)
  new_dt[, y := - y]
  new_dt[, id := as.factor(id_new)]
  new_dt[, value := as.numeric(id_new)]
  
  return(new_dt)
}

mirror_x_right <- function(dt, times = 1, id_new) {
  new_dt <- copy(dt)
  val <- max(new_dt$x)
  
  new_dt[, x := 2 *times*val - x]
  new_dt[, id := as.factor(id_new)]
  new_dt[, value := as.numeric(id_new)]
  
  return(new_dt)
}

two_lines <-  function(dt){
  new_dt <- copy(dt)
  val <- (max(dt$x) - min(dt$x)) / 2
  new_dt[, x := x + val]
  new_dt[, y := y + 6]
  new_dt[, value := value + 2]
  new_dt[, id := as.factor(value)]
  new_dt[value == 4, value := 2]
  
  dt <- rbindlist(list(dt, new_dt), use.names = TRUE)
  
  dt[, id := as.factor(10 * as.integer(as.character(id)))]
  
  
  return(dt)
}

extend_right <- function(dt, times = 1, coef_right = 2/3){
  
  new_dt <- copy(dt)
  val <- (max(dt$x) - min(dt$x)) * coef_right
  
  x_vector <- as.vector(outer(
    X = new_dt$x,
    Y = (0:times) * val,
    FUN = "+"
  ))
  
  y_vector <- rep.int(new_dt$y, times = (1 + times))
  
  id_vector <- as.factor(as.vector(outer(
    X = as.integer(new_dt$id) * 100,
    Y = (0:times),
    FUN = "+"
  )))
  
  value_vector <- rep.int(new_dt$value, times = (1 + times))
  
  out_dt <- data.table(id = id_vector,
                       x = x_vector,
                       y = y_vector,
                       value = value_vector)
  
  return(out_dt)
}

extend_up <- function(dt, times = 1, coef_right = 2/3){
  
  new_dt <- copy(dt)
  val <- (max(dt$y) - min(dt$y)) * coef_right
  
  x_vector <- rep.int(new_dt$x, times = (1 + times))
  
  y_vector <- as.vector(outer(
    X = new_dt$y,
    Y = (0:times) * val,
    FUN = "+"
  ))
  
  id_vector <- as.factor(as.vector(outer(
    X = as.integer(new_dt$id) * 100,
    Y = (0:times),
    FUN = "+"
  )))
  
  value_vector <- rep.int(new_dt$value, times = (1 + times))
  
  times_vector <- rep((0:times) %% 3, each = nrow(dt)) + 3
  
  out_dt <- data.table(id = id_vector,
                       x = x_vector,
                       y = y_vector,
                       value = value_vector,
                       times_val = times_vector)
  
  out_dt[value == 3, value := times_val]
  
  out_dt[, times_val := NULL]
  
  return(out_dt)
}


## create data -----------------------------------------------
poligon_dt <- data.table(id = as.factor("1"),
                         x = c(0,1,3,3,6),
                         y = c(0,3,3,1,0),
                         value = 1L)

poligon2_dt <- rotate_90(poligon_dt, id_new = 2)

box_dt <- rbindlist(
  list(
    # first color
    poligon_dt,
    mirror_y_down(poligon_dt, id_new = 1),
    mirror_x_right(poligon_dt, id_new = 1), #3
    mirror_y_down(mirror_x_right(poligon_dt, id_new = 1), id_new = 1), #3
    # second color
    poligon2_dt,
    mirror_y_down(poligon2_dt, id_new = 2), #4
    mirror_x_right(poligon2_dt, id_new = 2),
    mirror_y_down(mirror_x_right(poligon2_dt, id_new = 2), id_new = 2) #4
  ),
  use.names = TRUE
)

double_box_dt <- two_lines(box_dt)

extended_up_dt <- extend_up(dt = double_box_dt, times = 5)

wall_dt <- extend_right(dt = extended_up_dt, times = 5)

## Visualization ---------------------------------------------
cols <- c(
  "1" = "#0a080d", # negro
  "2" = "#d4c4bb", # beige
  
  "3" = "#b5691c", # naranja
  "4" = "#011f58", # azul
  "5" = "#3f9382"  # verde
)

cols <- c(
  "1" = "#EAEBE6", # blanco
  "2" = "#000000", # negro
  
  "3" = "#EAEBE6", # blanco
  "4" = "#EAEBE6", # blanco
  "5" = "#EAEBE6"  # blanco
)


p1 <- poligon_dt %>%
  ggplot(aes(x, y)) +
  geom_polygon(aes(fill = factor(value), group = id)) +
  coord_fixed(xlim = c(0, 80), ylim = c(-5, 70)) +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = cols)
          
p2 <- box_dt %>%
  ggplot(aes(x, y)) +
  geom_polygon(aes(fill = factor(value), group = id)) +
  coord_fixed(xlim = c(0, 80), ylim = c(-5, 70)) + 
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = cols)

p3 <- double_box_dt %>%
  ggplot(aes(x, y)) +
  geom_polygon(aes(fill = factor(value), group = id)) +
  coord_fixed(xlim = c(0, 80), ylim = c(-5, 70)) + 
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = cols)

p4 <- extended_up_dt %>%
  ggplot(aes(x, y)) +
  geom_polygon(aes(fill = factor(value), group = id)) +
  coord_fixed(xlim = c(0, 80), ylim = c(-5, 70)) + 
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = cols)

p5 <- wall_dt %>%
  ggplot(aes(x, y)) +
  geom_polygon(aes(fill = factor(value), group = id)) +
  coord_fixed(xlim = c(0, 80), ylim = c(-5, 70)) + 
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = cols)

## Animation ---------------------------------------------------
animation::saveGIF(
  expr = {
    plot(p1)
    plot(p2)
    plot(p3)
    plot(p4)
    plot(p5)
  },
  movie.name = here::here("output", "animation_5colors.gif")
)


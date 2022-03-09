library(tidyverse)

# Create random path

create_randompath <- function(x, y, xend, yend, max_width, axis = "x") {
  y_start <- y
  top <- y_start + max_width / 2
  bottom <- y_start - max_width / 2
  
  for (i in 1:10) {
    y_start[i+1] <- y + rnorm(1, mean = 0, sd = max_width / 3)
    if (y_start[i+1] > top) {
      y_start[i+1] <- top - .01
    } else if (y_start[i+1] < bottom) {
      y_start[i+1] <- bottom + .01
    }
  }
  
  data.frame(
    x = seq(from = x, to = xend, by = (xend - x) / (length(y_start) - 1)),
    y = y_start
  )
}

t <- create_randompath(x = 1, y = 1, xend = 1000, max_width = .1)

as.data.frame(spline(t$x, t$y), method = "hyman") %>%
  ggplot(aes(x, y)) +
  geom_path() 

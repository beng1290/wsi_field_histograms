easy.y.scales <- function(x){
  
  tt = max(x)
  if (tt <= 1){
    if (tt != .5){
      tt1 = round(tt, 2) + .5
    }
    if(tt1 >= 1){
      tt1 = 1
    }
    ybre <- seq(0, tt1, .25)
    scaleFUN <- function(x) sprintf("%.2f", x)
    tt2 <- min(tt)
  } else {
    tt1 = round(tt, -1) + 1
    tt2 = 1
    ybre <- c(1, 5, 10, 15, 20, 50, 100, 200, 500, 1000, 2000)
    scaleFUN <- function(x) sprintf("%.0f", x)
  }
  #
  out <- list(tt1=tt1, scaleFUN = scaleFUN, ybre=ybre, ttlo=tt2)
}
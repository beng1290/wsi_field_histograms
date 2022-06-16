easy.x.scales <- function(x){
  
  tt = max(x)
  if (tt <= 1){
    if (tt != .5){
      tt1 = round(tt, 2) + .5
    } else {tt1 = tt}
    if(tt1 >= 1){
      tt1 = 1
    }
    ybre <- seq(0, tt1, .25)
    scaleFUN <- function(x) sprintf("%.2f", x)
  } else if (tt < 95 & tt > 1){
    tt1 = round(tt, -1) + 10
    if(tt1 == 100){
      tt1 = 95
    }
    ybre <- seq(0, tt1, 5)
    scaleFUN <- function(x) sprintf("%.0f", x)
  } else if (tt >= 95 & tt < 500){
    tt1 = round(tt, -2) + 100
    ybre <- seq(0, tt1, 50)
    scaleFUN <- function(x) sprintf("%.0f", x)
  } else if (tt >= 500 & tt < 950){
    tt1 = round(tt, -2) + 100
    if(tt1 == 1000){
      tt1 = 950
    }
    ybre <- seq(0, tt1, 100)
    scaleFUN <- function(x) sprintf("%.0f", x)
  } else if (tt >= 950 & tt < 3000){
    tt1 = round(tt, -2) + 100
    ybre <- seq(0, tt1, 100)
    scaleFUN <- function(x) sprintf("%.0f", x)
  } else if (tt >= 3000 & tt < 5000){
    tt1 = round(tt, -3) + 500
    ybre <- seq(0, tt1, 500)
    scaleFUN <- function(x) sprintf("%.0f", x)
  } else if (tt >= 5000 & tt < 9500){
    tt1 = round(tt, -3) + 1000
    if(tt1 == 10000){
      tt1 = 9500
    }
    ybre <- seq(0, tt1, 1000)
    scaleFUN <- function(x) sprintf("%.0f", x)
  } else {
    tt1 = round(tt, -3) + 1000
    ybre <- seq(0, tt1, 1000)
    scaleFUN <- function(x) sprintf("%.0f", x)
  }
  #
  out <- list(tt1=tt1, scaleFUN = scaleFUN, ybre=ybre)
}
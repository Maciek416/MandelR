width = 256
height = 256
xmin = -2.45
ymin = -2.45
wh = 3
xmax = xmin + wh
ymax = ymin + wh
dx = (xmax - xmin) / width
dy = (ymax - ymin) / height
maxiterations <- 24

mandelRow <- function(x,y,width,dx) {
 result <- array(1, width)
 for(i in 1:width){
   a <- x
   b <- y
   n <- 0
   aa <- 0
   bb <- 0
   while(n < maxiterations && aa + bb <= maxiterations) {
     aa <- a * a
     bb <- b * b
     twoab <- (2 * a * b)
     a <- aa - bb + x
     b <- twoab + y
     n <- n + 1
   }
   if(n == maxiterations){
     result[i] <- 0
   } else {
     #result[i] <- ((n * 1000) %% 255)
     result[i] <- n
   }
   x <- x + dx
 }
 result
}

mandelBrot <- function(width,height,dy,dx,xmin,ymin) {
 result <- array(1:(width*height),dim=c(width,height))
 y <- ymin
 for(i in 1:height){
   result[i,] <- mandelRow(xmin, y, width, dx)
   y <- y + dy
 }
 result
}

mandel <- mandelBrot(width,height,dy,dx,xmin,ymin)
x <- 2 * (1:nrow(mandel))
y <- 2 * (1:ncol(mandel))

image(x, y, mandel, col = topo.colors(255), axes = FALSE)
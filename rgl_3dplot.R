# Funciones tomadas de 
# https://en.wikipedia.org/wiki/Test_functions_for_optimization


library(rgl)
library(plot3D)
x <- seq(-15.2,15.2, by = 0.1)
y <- x 
a <- mesh(x,y)
z <- 10*2+(a$x^2 - 10*cos(2*pi*a$x)+a$y^2 - 10*cos(2*pi*a$y))
surf3D(a$x,a$y,z,theta = 15,phi = 35,bty = "b",shade = 0.1,colvar = z)
image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
        colkey = list(dist = .0, shift = 0.229,
                      side = 3, length = 0.3, width = 1,
                      cex.clab = 1.2, col.clab = "black", line.clab = 2,
                      col.axis = "black", col.ticks = "black", cex.axis = 0.8))

## rastering función. 

x <- seq(-5.2,5.2, by = 0.1)
y <- x 
a <- mesh(x,y)
z <- 10  + ((a$x^2 - 10*cos(2*pi*a$x)) +(a$y^2 - 10*cos(2*pi*a$y)))
surf3D(a$x,a$y,z/100,theta = 15,phi = 35,bty = "b",shade = 0.1,colvar = z)

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

x <- seq(-5.120,5.120, by = 0.05)
y <- x 
a <- mesh(x,y)
z <- (10*2  + ((a$x^2 - 10*cos(2*pi*a$x)) +(a$y^2 - 10*cos(2*pi*a$y))))
surf3D(a$x,a$y,z,theta = 50,phi = 25,bty = "b",shade = 0.0,resfac = c(15,15),add = FALSE)

image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
        colkey = list(dist = .0, shift = 0.229,
                      side = 3, length = 0.3, width = 1,
                      cex.clab = 1.2, col.clab = "black", line.clab = 2,
                      col.axis = "black", col.ticks = "black", cex.axis = 0.8))

## Función Ackley 

x <- seq(-5,5, by = 0.05)
y <- x 
a <- mesh(x,y)
z <- -20*exp(-0.2*sqrt(0.5*(a$x^2 + a$y^2))) - exp(0.5*(cos(2*pi*a$x) + cos(2*pi*a$y))) + exp(1) + 20
surf3D(a$x,a$y,z,theta = 50,phi = 35,bty = "b",shade = 0.0,resfac = c(15,15),add = FALSE)
image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
        colkey = list(dist = .0, shift = 0.229,
                      side = 3, length = 0.3, width = 1,
                      cex.clab = 1.2, col.clab = "black", line.clab = 2,
                      col.axis = "black", col.ticks = "black", cex.axis = 0.8))

## Funcion Baele

x <- seq(-5,5, by = 0.05)
y <- x 
a <- mesh(x,y)
# se invierte la funcón 
z <- -((1.5 - a$x + a$x*a$y) + (2.25 - a$x + a$x*a$y^2)^2 + (2.625 - a$x + a$x*a$y^3)^2)
surf3D(a$x,a$y,z,theta = 50,phi = 35,bty = "b",shade = 0.0,resfac = c(15,15),add = FALSE)
image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
        colkey = list(dist = .0, shift = 0.229,
                      side = 3, length = 0.3, width = 1,
                      cex.clab = 1.2, col.clab = "black", line.clab = 2,
                      col.axis = "black", col.ticks = "black", cex.axis = 0.8))


# Función Levi. 

x <- seq(-10,10, by = 0.05)
y <- x 
a <- mesh(x,y)
z <- (sin(3*pi*a$x))^2 + (a$x - 1)^2 * (1 + (sin(3*pi*a$y))^2) + (a$y - 1)^2 *(1+(sin(2*pi*a$y)^2))
surf3D(a$x,a$y,z,theta = 50,phi = 35,bty = "b",shade = 0.0,resfac = c(15,15),add = FALSE)
image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
        colkey = list(dist = .0, shift = 0.229,
                      side = 3, length = 0.3, width = 1,
                      cex.clab = 1.2, col.clab = "black", line.clab = 2,
                      col.axis = "black", col.ticks = "black", cex.axis = 0.8))

# Cross in Tray esta funeción tien 4 soculiones. 

x <- seq(-10,10, by = 0.05)
y <- x 
a <- mesh(x,y)
z <- -0.0001*(abs(sin(a$x)*sin(a$y)*exp(abs(100 - sqrt(a$x^2 + a$y^2)/pi ))) + 1)^0.1
surf3D(a$x,a$y,z,theta = 50,phi = 40,bty = "b",shade = 0.0,resfac = c(15,15),add = FALSE)
image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
        colkey = list(dist = .0, shift = 0.229,
                      side = 3, length = 0.3, width = 1,
                      cex.clab = 1.2, col.clab = "black", line.clab = 2,
                      col.axis = "black", col.ticks = "black", cex.axis = 0.8))


#Función Eggholder 

x <- seq(-512,512, by = 2)
y <- x 
a <- mesh(x,y)
z <- -(a$y + 47)*sin(sqrt(abs(a$x/2 + a$y + 47))) - a$x*sin(sqrt(abs(a$x - a$y - 47)))
surf3D(a$x,a$y,z,theta = 50,phi = 40,bty = "b",shade = 0.0,resfac = c(15,15),add = FALSE)
image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
        colkey = list(dist = .0, shift = 0.229,
                      side = 3, length = 0.3, width = 1,
                      cex.clab = 1.2, col.clab = "black", line.clab = 2,
                      col.axis = "black", col.ticks = "black", cex.axis = 0.8))


# Función Holder table. 
# Tiene 4 minimos. 

x <- seq(-10,10, by = 0.1)
y <- x 
a <- mesh(x,y)
z <- -abs(sin(a$x)*cos(a$y)*exp(abs(1 - sqrt(a$x^2 + a$y^2)/pi)))
surf3D(a$x,a$y,z,theta = 50,phi = 45,bty = "b",shade = 0.0,resfac = c(15,15),add = FALSE)
image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
        colkey = list(dist = .0, shift = 0.229,
                      side = 3, length = 0.3, width = 1,
                      cex.clab = 1.2, col.clab = "black", line.clab = 2,
                      col.axis = "black", col.ticks = "black", cex.axis = 0.8))





















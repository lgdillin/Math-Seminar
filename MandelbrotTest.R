

init.values = seq(-2.001, 0.25, 0.0000001)
growth = rep(0, length(init.values))
n = 500

for(i in 1:length(init.values)) {
  z = 0
  c = init.values[i]
  for(j in 1:n) {
    z = z^(2) + c
  }
  if(!is.finite(z) || abs(z) > 10) {
    growth[i] = 2.01
  } else {
    growth[i] = z 
  }
}
plot(init.values, growth, xlab = "Given y-intercept", ylab = "Orbit Value", main = "z = z^(2) + c", type="p", cex=.2)

c = 0.2
x=0
for(i in 1:1000000) {
  x = x^2 + c
  if(i %% 1000 == 0) {
    print(x) 
  }
}


z = complex(real = 0, imaginary = 0)
c = complex(real = 1, imaginary = -1)

#c= -2
#z = .5
n = 20
for(i in 1:n) {
  z = z^(2) + c
  print(z)
}


init.x = seq(-2, 2, 0.01)
init.y = seq(-2, 2, 0.01)
growth = rep(0, length(init.x) * length(init.y))
plot.x = rep(0, length(init.x))
plot.y = rep(0, length(init.y))
n = 20

for(x in length(init.x)) {
  for(y in length(init.y)) {
    c = complex(real = init.x[x], imaginary = init.y[y])
    z = complex(real = 0, imaginary = 0)
    
    for(i in 1:n) {
      z = z^(2) + c
    }
    if(Mod(z) < Mod(c)) {
      #growth[i] = z
      plot.x[x] = init.x[x]
      plot.y[y] = init.y[y]
    } else {
      #growth[i] = z 
    }
  }
}
plot(plot.x, plot.y, xlab = "Given y-intercept", 
     ylab = "Orbit Value", main = "z = z^(2) + c", type="p", cex=.2)




in.mandelbrot.set <- function(c, iterations = 20, bound = 2) {
  z <- 0
  for (i in 1:iterations) {
    z <- z ** 2 + c
    if (Mod(z) > bound) {
      return(0)
    }
  }
  return(1)
}

resolution <- 0.001
sequence <- seq(-1, 1, by = resolution)
m <- matrix(nrow = length(sequence), ncol = length(sequence))
cc = rep(0, length(sequence)^2)
for (x in sequence) {
  for (y in sequence) {
    mandelbrot <- in.mandelbrot.set(complex(real = x, imaginary = y))
    #cc
    #m[round((x + resolution + 1) / resolution), round((y + resolution + 1) / resolution)] <- mandelbrot
  }
}
plot(m)


jpeg('mandelbrot.jpg')
image(m)
dev.off()





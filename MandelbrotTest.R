

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


z = complex(real = 0, imaginary = 0)
c = complex(real = 0, imaginary = 1)
n = 200
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
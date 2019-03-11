

init.values = seq(-2.001, 0.25, 0.00001)
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


z = 0
c = -2.1
n = 200
for(i in 1:n) {
  z = z^(2) + c
  print(z)
}
z
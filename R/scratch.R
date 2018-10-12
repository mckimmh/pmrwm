# Testing rumMH.R

#Generate obesrved data
x = rexp(1000, 5)

# test single step
stepMH(x, 1, 0.03, sigma_aux = 0.02)

plot(runMH(x, lambda = 2, sigma = 0.03, N = 10000, sigma_aux = NULL)$lambda)

pmhrun = runMH(x, 2, 0.03, 10000, sigma_aux = 0.1)
plot(pmhrun)
hist(pmhrun)

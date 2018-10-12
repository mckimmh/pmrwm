# Testing rumMH.R

#Generate obesrved data
x = rexp(1000, 5)

# test single step
stepMH(x, 1, 0.03, sigma_aux = 0.02)


#test with normal M-H
mh_run = runMH(x, lambda = 2, sigma = 0.03, N = 10000, sigma_aux = NULL)$lambda
plot(mh_run$lambda)

#test with pseudo M-H
pmhrun = runMH(x, 2, 0.03, 10000, sigma_aux = 0.1)
plot(pmhrun$lambda)
hist(pmhrun)

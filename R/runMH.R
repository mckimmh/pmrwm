# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#v_is are randomly generated to estimate integral (when likelihood is )
#v_is could also be the actual estimate of the integrals (you could go a level up)


# reference
# https://darrenjw.wordpress.com/2010/09/20/the-pseudo-marginal-approach-to-exact-approximate-mcmc-algorithms/




#' Function to calculate likelihood or 'noisy'/auxillary likelihood.
#'
#' @name get_ll
#'
#' @param x data
#' @param lambda initial proposal for parameter value
#' @param sigma_aux the SD of the auxillary distribution (relative magnitude of noise introduced to the target dist).  Set to NULL to run regular M-H.


get_ll = function(x, lambda, sigma_aux = NULL){
  if(!is.null(sigma_aux)){
    ll = sum(dexp(x, rate = lambda, log = T)) + rlnorm(1, 0, sigma_aux) # = v_i
    # p13 - there are certain assumptions that pi^(theta) must satisfy and that
    # what happens if the noise isn't distributed normally
    # this should be an estimate of the likelihood
    # in this case, we *can* calculate the likelihood because we're generating the data, so we add some noise in to simulate the fact that this is just an estimator of the likelihood
  }else{
    ll = sum(dexp(x, rate = lambda, log = T))   #difference in the Pseudo Marginal is that we can't calculate this?
  }

  ll
}



#' Perform single M-H step
#'
#' @name stepMH
#'
#' @param x data
#' @param lambda initial proposal for parameter value
#' @param sigma the SD of the proposal distribution. is not the expected jump size (depends on the posterior, but also the prob that a proposal is accepted, so a function of the posterior)
#' @param sigma_aux the SD of the auxillary distribution (relative magnitude of noise introduced to the target dist).  Set to NULL to run regular M-H.



stepMH = function(x, lambda, sigma, sigma_aux = NULL){

  lambda_prime = lambda + sigma * rnorm(1)   # new proposal  # q(lambda', lambda) where sigma is the scaling parameter
  a_prob = exp(get_ll(x, lambda_prime, sigma_aux = sigma_aux) - get_ll(x, lambda, sigma_aux = sigma_aux))  #subtract instead of divide because we're working with logs
  #on average, we should be subtracting out the added noise
  #do we need transition dist?  since the proposal dist is symmetric (norm), I don't think so?

  u = runif(1)

  if(u < a_prob){
    lambda = lambda_prime
  }

  return(data.frame(lambda = lambda
              , proposal = lambda_prime
              , u = u
              , a_prob = a_prob
              , accepted = (lambda == lambda_prime)
              ))
}



#' Run M-H chain
#'
#' @name runMH
#'
#' @param x data
#' @param lambda initial proposal for parameter value
#' @param sigma the SD of the proposal distribution. is not the expected jump size (depends on the posterior, but also the prob that a proposal is accepted, so a function of the posterior)
#' @param N the number of iterations of M-H that you'd like to run
#' @param sigma_aux the SD of the auxillary distribution (relative magnitude of noise introduced to the target dist).  Set to NULL to run regular M-H.



runMH = function(x, lambda, sigma, N, sigma_aux = NULL){
  chain = NULL
  lambda_i = lambda

  for(i in seq_len(N)){
    print(chain[i,])

    one_step = stepMH(x, lambda_i, sigma, sigma_aux = sigma_aux)

    if(is.null(chain)){
      chain = one_step
    }else{
      chain = cbind(chain, one_step)
    }

    #update lambda for next iteration
    lambda_i = chain[i,]$lambda
  }

  return(chain)
}





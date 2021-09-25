run <- function(x, ...) {
  UseMethod("run", x)
}
run.speechModel <- function(x, features, params = NULL, maxiter = 100) {
  if ( is.null(params))
    params <- x$params
    
   tryCatch(
     {
       mod <- hmm(features## try once
                  ,
                  ## weights = weights,
                  nstates = params$nstates,
                  control = list(lambda = params$lambda,
                                 standardize = FALSE,
                                 maxiter = 50),
                  par = list(method = 'random-spherical')
       )
       if (mod$convergence$resets > 10){
         stop('exhibits state collapse')
       }
       mod
       mod$lstateprobs <- NULL
       mod$zetas <- NULL
       mod$par <- NULL
     },
     error = function(e){
       cat('ERROR:', as.character(e), '\n')
     })
  
  if ( !exists("mod") )
    mod <- list()
  mod
}

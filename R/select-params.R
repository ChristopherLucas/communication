#' @title Select hyper parameters for HMM model
#' @importFrom gtools rdirichlet
#' @importFrom purrr rerun
#' @description Get features and return the hyperparameters with most likelihood
#' @param features with labels
#' @param params parameters for CV - data.frame with columns nstates and lambda
#' @param nfold Number of folds 
#' @param cleanFeatures clean features before running the model
#' @return model object
#' @export
selectParams <- function(features,  params = NULL, nfold = 5, cleanFeatures = T) {
  model <- newModel()
  model$params <- params
  
  if ( is.null(params)  )
    model$params <- expand.grid(
      nstates = c(3, 5, 7),
      lambda = c(.05, .1)
    ) 
  labels <- unique(purrr::map_chr(features, ~ attr(.x,"label")))
  
  features <- standardizeFeatures(features)
  
  if ( !cleanFeatures ) {
    cleanFeatures <- function(x) x
  }
  
  models <- map(labels, function(x) {
    iterParams(model, features = cleanFeatures(features, label = x) , nfold)
  })
  
  
  models
  
}

iterParams <- function(model, features, nfold) {
  
  model$params$oos.llh.fold <- 0
  
  sampleFolds <- sampleFold(length(features), nfold)
  for (i in 1:nrow(model$params)) {
    for (j in unique(sampleFolds)) {
      
      model$params$oos.llh.fold[i] <- model$params$oos.llh.fold[i] + calculateLlhFold(model, features, sampleFolds, j, i)
      
    }
    
  }
  return(model)
  
}

calculateLlhFold <- function(model, features, sampleFolds, j, i) {
  mod <- rerun(10,
               run(model, features[sampleFolds != j], params = model$params[i,],maxiter = 10)
  )

  mod.best <- mod[[
    which.max(sapply(mod, function(x) max(x$llh_seq)))
    ]]
  
  ## calculate oos llh
  oos.llh.fold <- llh(features[sampleFolds != j], mod.best)$llh_total
  oos.llh.fold
  
}

sampleFold <- function(n, nfold) {
  vec <- vector(mode = "numeric", length = n)
  j <- 1
  for (i in 1:n)  {
    vec[i] <- j
    j <- j + 1
    if ( j  > nfold )
      j <- 1
  }
  sample(vec)
}

newModel <- function() {
  model <- list()
  class(model) <- "speechModel"
  model
}


cleanFeatures <- function(v, label) {
  purrr::keep(v, ~ attr(.x, "label") == label) %>%
    purrr::map(function(x) {
      x['timestamps'] <- NULL
      x['raw_data'] <- NULL
      x <- x[(1:(nrow(x)-1)),]
#      x <- x[,c(1:2,4:7, 35:47)]
      x <- as.matrix(x) 
      x
    })
}

standardizeFeatures <- function(features) {
  if ( !is.list(features) ) 
    features <- list(features)
  
  purrr::map(features, function(x)  { 
    means <- purrr::map2(x, colnames(x), ~ ifelse(.y %in% c("timestamps","raw_data"), 0, mean(.x, na.rm = T)))
    sds <- purrr::map2(x, colnames(x), ~ ifelse(.y %in% c("timestamps","raw_data"), 0, sd(.x, na.rm = T)))

    columnsNames <- purrr::discard(colnames(x), ~ .x %in% c("timestamps","raw_data"))
    for (i in columnsNames) {
      x[[i]] <- (x[[i]] - means[[i]])/sds[[i]]
    }
    x
    }
  )
}
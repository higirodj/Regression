
# Load libraries
library(tidyverse)

#----------------------------------------------------------------------------------------
### Coding up the stepwise algorithms from Page 207 and 209 in ISL

## Algorithm for (forward/backward)
# 1. pick (null/full) model M_{0}
# 2. [loop] for p=1,...,P check (adding/subtracting) single variable from M_{p-1} using Rsq or RSS
# 3. select model with best AIC, BIC, Adj-Rsq, etc. 

## make function that specifies direction and variables to consider
# inputs: data (with responses and considered inputs), direction, response column name
# output: list of models to evaluate for 
make_step_regmods <- function(dat, direction="backward", yname, output="mod_list"){
  # number of inputs to consider
  P <- ncol(dat) - 1
  # initialize empty list to put selected models and inputs into
  mod_list <- list(NULL)
  input_list <- list(NULL)
  # need to keep track of which inputs have been selected along the way
  input <- data.frame(input_name=names(dat)[!names(dat) %in% yname],
                      in_mod=TRUE, order_removed=NA, stringsAsFactors = FALSE)
  mod_list[[P]] <- lm(as.formula(paste(yname," ~ .")) , data=dat)
  input_list[[P]] <- input$input_name
  
  # loops over all subset sizes and iteratively adds based on next best Rsq
  for(p in P:2){
    # list of inputs still in model
    input2consider <- input$input_name[input$in_mod]
    #initalize vector to store R squares
    modRsqs <- rep(NA,length(input2consider))
    for(i in 1:length(input2consider)){
      temp_mod <- update(mod_list[[p]], paste(".~.-",input2consider[i]))
      modRsqs[i] <- summary(temp_mod)$r.squared
    }
    input2drop <- input2consider[which.max(modRsqs)]
    # update model in mod_list, mark input as removed, record order dropped
    mod_list[[p-1]] <-update(mod_list[[p]], paste(".~.-",input2drop))
    input[input$input_name==input2drop, "in_mod"] <- FALSE
    input[input$input_name==input2drop, "order_removed"] <- P-p+1
    # update list of inputs in each model (for use in CV evaluation)
    input_list[[p-1]] <- input$input_name[input$in_mod]
  }
  print(arrange(input[,c("input_name","order_removed")],order_removed))
  if(output=="mod_list") return(mod_list)
  if(output=="input_list") return(input_list)
}



#----------------------------------------------------------------------------------------
### Still a bit tricky to get CV error rates out
# CV cohort generator
add_cv_cohorts <- function(dat,cv_K){
  if(nrow(dat) %% cv_K == 0){ # if perfectly divisible
    dat$cv_cohort <- sample(rep(1:cv_K, each=(nrow(dat)%/%cv_K)))
  } else { # if not perfectly divisible
    dat$cv_cohort <- sample(c(rep(1:(nrow(dat) %% cv_K), each=(nrow(dat)%/%cv_K + 1)),
                              rep((nrow(dat) %% cv_K + 1):cv_K,each=(nrow(dat)%/%cv_K)) ) )
  }
  return(dat)
}

## Making k-fold CV-MSE calculating function
# In order to evaluate with training and test separate, need to extract inputs from each model
# fit with inputs using train cohorts, predict for test cohort, repeat for each cohort
# record error rates for each. 
stepwise_cvmse <- function(cv_K=n, input_list, dat, yname){
  P <- length(input_list)
  dat <- add_cv_cohorts(dat,cv_K)
  # fit model with sample inputs as forward selected models
  cvmse <- rep(NA,P)
  for(p in 1:P){
    preds <- rep(NA, nrow(dat))
    for(cv_k in 1:cv_K){
      datasub <- dat[dat$cv_cohort!=cv_k , c(yname,input_list[[p]]) ]
      submod <- lm(as.formula(paste(yname,"~ ."))  , datasub)
      preds[dat$cv_cohort==cv_k] <- predict(submod, dat[dat$cv_cohort==cv_k , ])
    }
    cvmse[p] <- mean((preds-dat[,yname])^2)
  }
  cvmse
}


generate_data = function(n,p){
  covariates = matrix(rnorm(n*p),nrow=n,ncol=p)
  responses = rnorm(n)
  list(covariates,responses)
}

model_select = function(covariates, responses, cutoff){
  model=lm(responses ~ covariates)
  pvals = summary(model)$coefficients[,4]
  keep=which(pvals<cutoff)-1
  keep=keep[keep!=0]
  if(length(keep)==0){
    return(c())
  }
  model2=lm(responses ~ covariates[,keep])
  summary(model2)$coefficients[,4]
}

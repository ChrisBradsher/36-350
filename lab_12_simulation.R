generate_data = function(n,p){
  covariates = as.matrix(rnorm(n*p),nrow=n,ncol=p)
  responses = rnorm(n)
  list(covariates,responses)
}
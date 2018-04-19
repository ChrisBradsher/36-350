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

run_simulation = function(n_trials,n,p,cutoff){
  res=numeric()
  if(n_trials==1){return(res)}
  for(i in 2:n_trials){
    data=generate_data(n,p)
    model=model_select(data[[1]],data[[2]],cutoff)
    res=c(res,model)
  }
  print(res)
  hist(res)
}
run_simulation(100,100,10,0.05)
run_simulation(100,1000,10,0.05)
run_simulation(100,10000,10,0.05)
run_simulation(100,100,20,0.05)
run_simulation(100,1000,20,0.05)
run_simulation(100,10000,20,0.05)
run_simulation(100,100,50,0.05)
run_simulation(100,1000,50,0.05)
run_simulation(100,10000,50,0.05)
#They are not uniformly distributed. Hopefully they weren't supposed to be.
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
  write(res,file=paste("l12datan_trials",n_trials,"n",n,"p",p,"cutoff",cutoff,".txt",sep=""),ncolumns=1)
}

make_plot = function(datapath){
  file=scan(datapath)
  hist(file)
}

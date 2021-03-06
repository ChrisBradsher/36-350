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

challenge_model_select = function(covariates,response,cutoff){
  #Arbitrarily decide to use half the data for modeling
  selection=sample(length(response),floor(length(response)*0.5))
  model.cov=covariates[selection,]
  model.res=response[selection]
  model=lm(model.res ~ model.cov)
  pvals = summary(model)$coefficients[,4]
  keep=which(pvals<cutoff)-1
  keep=keep[keep!=0]
  if(length(keep)==0){
    return(c())
  }
  #Now use other half for p-values
  pval.cov=covariates[-selection,]
  pval.res=response[-selection]
  model2=lm(pval.res ~ pval.cov[,keep])
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

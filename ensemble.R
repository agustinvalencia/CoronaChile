ensemble_models = function(data, burndown, forecast=0) {
  # training data
  #n = nrow(data)
  #data$Day = as.Date(data$Day)
  #data$x = 1:n
  #data$cum = cumsum(data$NewCases)
  
  # testing structures
  days = data$Day
  if (forecast != 0) {
    extra_days <- seq(days[n], by="day", length.out = (forecast+1))
    extra_days <- extra_days[2:length(extra_days)] #first day of extra day is repeated
    days = c(days, extra_days)
  }    
  all_preds <- data.frame (
    Day = days, 
    x = 1:(n+forecast)
  )
  preds_names = c()
  # Run the shit
  for(i in 1:(n-burndown)) {
    # training
    date_split <- data$Day[i-1+burndown]
    train_idxs <- which(data$Day <= date_split)
    train <- data[train_idxs,]
    model <- lm(formula = log(cum) ~ x, data=train)
    
    # testing
    preds <- exp(predict(model, all_preds))
    preds_names[i] = paste("preds_",date_split,sep="")
    all_preds = cbind(all_preds, preds)
    colnames(all_preds) = c("Day", "x", preds_names)
  }
  
  return(all_preds)
}



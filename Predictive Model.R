set.seed(1)
nFold = 10
valNum = floor(runif(nrow(datb))*nFold)+1
getRMSE = function(modelCall){
	modelPerformance = rep(NA,nFold)
	for(fold in 1:nFold){
		#Step 2i: Get the training and validation data for this fold
		trainingData = subset(datb,valNum!=fold)
		validationData = subset(datb,valNum==fold)
		#Step 2ii: Estimate the model for this training data
		model = update(modelCall,data=trainingData)
		#Step 2iii: Calculate out of sample MSE for this validationData
		validRMSE =  mean((validationData$card - predict(model,validationData))^2)^.5
		#Store model performance		
		modelPerformance[fold] = validRMSE
	}
	return(mean(modelPerformance))
}
#Linear Regression: the last one is the best 
getRMSE(lm(card~.,data=datb))
getRMSE(lm(card~.^2,data=datb))
getRMSE(lm(card~expenditure*(reports+share+income+active)+share+poly(age,3)+factor(months),data=datb))
getRMSE(lm(card~reports+income+poly(expenditure,13)+owner+majorcards+active,data=datb))
getRMSE(lm(card~log(reports+1)+poly(expenditure,13)+income+log(share)+poly(age,3)+factor(months)+active,data=datb))#0.2938908


#MARS: the best one is marked
getRMSE(earth(card~.,data=datb))
getRMSE(earth(card~.,degree=3,data=datb))
getRMSE(earth(card~.,degree=2,thres=.1,data=datb))
getRMSE(earth(card~.,degree=3,thres=0,data=datb))#this is the best one, RMSE = 0.2819769
getRMSE(earth(card~expenditure*(reports+share+income+active)+share+age,data=datb))
getRMSE(earth(card~reports+income+expenditure+owner+majorcards+active,thres=0,data=datb))


#Chosen Model:
chosenModel2 = earth(card~.,degree=3,thres=0,data=datb)
#RMSE2 = mean((datb$card-predict(chosenModel2,datb))^2)^.5 = 0.2803566

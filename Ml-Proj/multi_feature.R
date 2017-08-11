
setwd('D://sem2//Machine Learning//Ml-Proj')

#X = read.xlsx("default.xls",sheetIndex = 1)

data = read.delim("data.csv",sep = ",",stringsAsFactors = F,header = T,na.strings="")

df = data.frame(data)

punish <- function(payval){
ifelse(payval==-2 , -3,payval)
ifelse(payval==-1, -2, payval)
ifelse(payval==0,-1,payval)
return(payval)
}

feat_diff_amt_1 = ((df$LIMIT_BAL - df$BILL_AMT2 - df$PAY_AMT1) * punish(df$PAY_0) )/ df$LIMIT_BAL
feat_diff_amt_2 = ((df$LIMIT_BAL - df$BILL_AMT3 - df$PAY_AMT2) * punish(df$PAY_2))/ df$LIMIT_BAL
feat_diff_amt_3 = ((df$LIMIT_BAL - df$BILL_AMT4 - df$PAY_AMT3) * punish(df$PAY_3))/ df$LIMIT_BAL
feat_diff_amt_4 = ((df$LIMIT_BAL - df$BILL_AMT5 - df$PAY_AMT4) * punish(df$PAY_4))/ df$LIMIT_BAL
feat_diff_amt_5 = ((df$LIMIT_BAL - df$BILL_AMT6 - df$PAY_AMT5) * punish(df$PAY_5))/ df$LIMIT_BAL
total = feat_diff_amt_1+feat_diff_amt_2+feat_diff_amt_3+feat_diff_amt_4+feat_diff_amt_5

#feat_diff_amt_1 = ((df$LIMIT_BAL - df$BILL_AMT1 - df$PAY_AMT1) * punish(df$PAY_0) )/ df$LIMIT_BAL
#feat_diff_amt_2 = ((df$LIMIT_BAL - df$BILL_AMT2 - df$PAY_AMT2) * punish(df$PAY_2))/ df$LIMIT_BAL
#feat_diff_amt_3 = ((df$LIMIT_BAL - df$BILL_AMT3 - df$PAY_AMT3) * punish(df$PAY_3))/ df$LIMIT_BAL
#feat_diff_amt_4 = ((df$LIMIT_BAL - df$BILL_AMT4 - df$PAY_AMT4) * punish(df$PAY_4))/ df$LIMIT_BAL
#feat_diff_amt_5 = ((df$LIMIT_BAL - df$BILL_AMT5 - df$PAY_AMT5) * punish(df$PAY_5))/ df$LIMIT_BAL
#feat_diff_amt_6 = ((df$LIMIT_BAL - df$BILL_AMT6 - df$PAY_AMT6) * punish(df$PAY_6))/ df$LIMIT_BAL
#total = feat_diff_amt_1+feat_diff_amt_2+feat_diff_amt_3+feat_diff_amt_4+feat_diff_amt_5+feat_diff_amt_6


totalcbind = cbind(total,df$default.payment.next.month)


amount_paid = df$PAY_AMT1+df$PAY_AMT2+df$PAY_AMT3+df$PAY_AMT4+df$PAY_AMT5+df$PAY_AMT6
diff_limit =  df$BILL_AMT1 / df$LIMIT_BAL
#drops <- c("SEX","MARRIAGE")
#drops <- c("PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")
#drops <- c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")
#drops <- c("BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6")
#df = df[ , !(names(df) %in% drops)]

#df["expenditure"] = NA
#df["expenditure"] = expenditure


#df["amount_paid"] = NA
#df["amount_paid"] = amount_paid


#df["diff_limit"] = NA
#df["diff_limit"] = diff_limit


df["total"] = NA
df["total"] = total


  head(df)
  write.table(df, file = "MyDataAll6Style.csv",row.names=FALSE, na="",sep=",")
#write.csv(df, file = "MyData.csv")

X = df
train = X[1:(nrow(X)*0.75),]
test = X[((nrow(X)*0.75)+1):nrow(X),]

train = train[,2:ncol(train)]
test = test[,2:ncol(test)]

train = data.frame(train)
test = data.frame(test) 

model= glm(train[,ncol(train)]~.,data=train[,-ncol(train)])
pr = predict(model, test[,-ncol(test)],type="response")
res = as.matrix(pr)
tp=0
tn=0
fp=0
fn=0
for (i in 1:nrow(res))
{ 
  if(res[i]>=0.5)
  { 
    result = 1;
  }
  else
  {
    result = 0;
  }
  if(result==1 && result==test[i,ncol(test)-1])
  { tp = tp + 1;}
  else if (result==0 && result==test[i,ncol(test)-1])
  {tn = tn + 1;}
  else if (result==0 && test[i,ncol(test)-1]==1)
  {fn = fn + 1;}
  else
  {fp = fp+1;}
}
print(tp+tn)
print(" True Positives = ")
print(tp)

print(" True Negatives = ")
print(tn)

print(" False Positives = ")
print(fp)

print(" False Negatives = ")
print(fn)


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


df$LIMIT_BAL = scale(df$LIMIT_BAL,center = TRUE, scale = TRUE)
df$BILL_AMT1 = scale(df$BILL_AMT1,center = TRUE, scale = TRUE)
df$BILL_AMT2 = scale(df$BILL_AMT2,center = TRUE, scale = TRUE)
df$BILL_AMT3 = scale(df$BILL_AMT3,center = TRUE, scale = TRUE)
df$BILL_AMT4 = scale(df$BILL_AMT4,center = TRUE, scale = TRUE)
df$BILL_AMT5 = scale(df$BILL_AMT5,center = TRUE, scale = TRUE)
df$BILL_AMT6 = scale(df$BILL_AMT6,center = TRUE, scale = TRUE)
df$PAY_AMT1 = scale(df$PAY_AMT1,center = TRUE, scale = TRUE)
df$PAY_AMT2 = scale(df$PAY_AMT2,center = TRUE, scale = TRUE)
df$PAY_AMT3 = scale(df$PAY_AMT3,center = TRUE, scale = TRUE)
df$PAY_AMT4 = scale(df$PAY_AMT4,center = TRUE, scale = TRUE)
df$PAY_AMT5 = scale(df$PAY_AMT5,center = TRUE, scale = TRUE)
df$PAY_AMT6 = scale(df$PAY_AMT6,center = TRUE, scale = TRUE)

write.table(df, file = "MyDataAll6StyleNormalize3.csv",row.names=FALSE, na="",sep=",")

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
  if(result==1 && result==test[i,ncol(test)])
  { tp = tp + 1;}
  else if (result==0 && result==test[i,ncol(test)])
  {tn = tn + 1;}
  else if (result==0 && test[i,ncol(test)]==1)
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

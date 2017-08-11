
     setwd('D://sem2//Machine Learning//Ml-Proj')
     
     #X = read.xlsx("default.xls",sheetIndex = 1)
     
     data = read.delim("data.csv",sep = ",",stringsAsFactors = F,header = T,na.strings="")
     
     df = data.frame(data) 
     

     
     
     plotdf =  data.frame(df$LIMIT_BAL, df$SEX,df$EDUCATION,df$MARRIAGE,df$AGE,df$PAY_0,df$PAY_2,df$PAY_6,df$BILL_AMT1,df$BILL_AMT2,df$BILL_AMT6,df$PAY_AMT1,df$PAY_AMT2,df$PAY_AMT6,df$default.payment.next.month)
     
     
     plot(plotdf)
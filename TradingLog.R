g2_data<-read.csv("http://cybercyclone.com/econ/classes/Econ4085/group2_output.csv",header=TRUE)

my_trading_log<-function(arg1 = f2_output$Date, arg2 = f2_output$Tickers ,arg3= f2_output$Alphas)

  {
  require(quantmod)
  require(openxlsx)
  
  transaction_type<-rep("Hold", length(arg2))
  transaction_amount<-rep(NA, length(arg2))
  
  my_dates<-seq(from= as.Date(arg1),length= 2,by="1 months")[2]
  
  for (ticker in 1:length(arg2)) 
    {
    ticker_date<-getSymbols(arg2[ticker], src="yahoo", auto.assign = FALSE, from = arg1, to = my_dates)
    i_price<-as.numeric(ticker_date[1,grep("Open", names(ticker_date))])
    
    if(arg3[ticker]>0.00000001) 
    { 
      transaction_type[ticker]<-"Buy"
      transaction_amount[ticker]<-floor(10000/i_price)
    }
    
    else if (arg3[ticker]<(-0.00000001)) 
    {
      transaction_type[ticker]<-"Sell"
      transaction_amount[ticker]<-0
    }
  }
  
  
  transaction_df<-data.frame(Date = rep(arg1, length(arg2)), ticker = arg2, transaction_type = transaction_type,
                             transaction_amount = transaction_amount, 
                             stringsAsFactors = FALSE)
  
  
  write.table(transaction_df, file = "transaction_log.csv", sep = ",")
  return(transaction_df)
}
my_trading_log()





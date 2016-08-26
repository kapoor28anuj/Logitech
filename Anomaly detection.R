# I have commented some plot functions in this code which can be used to visualize the working of the code


# Packages to be used,for first time remove # from line 5 to install pacman 
# install.packages("pacman")
library(pacman)
pacman::p_load(lubridate,zoo,xts,forecast,RPostgreSQL,dbscan,"RPostgreSQL",dplyr)
# library(dbscan)
# library(lubridate)
# require(zoo)
# require(xts)
# require("RPostgreSQL")
# library(forecast)

# connecting to POstGreSQL server
drv <- dbDriver("PostgreSQL")
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "csc_amr",
                 host = "******", port = 5432,
                 user = "*****", password = "*****") # use the login credentials
# retrieving the database to work on
pos_intern_project=dbGetQuery(con,"select* from pos_intern_project ")

#getting the SKU-seller of top 80%  of sales

# Extracting sum of sales of each combination
Skucount=dbGetQuery(con,"select product_sku,from_account_name,sum(net_sellthru) as sum from pos_intern_project group by product_sku,from_account_name order by sum desc")
Skucount$percentage_of_total=100*Skucount$sum/sum(Skucount$sum)  # getting % contribution in sales

# getting cumulative sale %
for (i in (2:nrow(Skucount))){
  Skucount[1,'cum_per']=Skucount[1,'percentage_of_total']               
  Skucount[i,'cum_per']=Skucount[i,'percentage_of_total']+Skucount[i-1,'cum_per']
}
# Applying pareto rule
useful_sku_seller=Skucount[Skucount$cum_per<80,] 

#getting month and day from dates 
pos_intern_project['month']=month(pos_intern_project$fscl_week_end_dt)
pos_intern_project['day']=mday(pos_intern_project$fscl_week_end_dt)

# removing 21-31 dates from march and 1-7 dates of april
workable_data=pos_intern_project[!((pos_intern_project$day %in% c(21,31) & pos_intern_project$month==3)|(pos_intern_project$day %in% c(1:7) & pos_intern_project$month==4) ) ,]     

# cleaning of CDW, since CDW has multiple  datapoints on same date (because its sale to multiple retailers)
cdw=workable_data[workable_data$from_account_name=="CDW",]

# combining sales of multiple outlets in one
upp <- aggregate(cdw$net_sellthru, list(cdw$product_sku,cdw$fscl_week_end_dt), sum, na.rm=TRUE)   
# renaming columns
colnames(upp)=c("product_sku","fscl_week_end_dt","net_sellthru")
upp$from_account_name="CDW"
upp['month']=month(upp$fscl_week_end_dt)
upp['day']=mday(upp$fscl_week_end_dt)
upp=upp[!((upp$day %in% c(21,31) & upp$month==3)|(upp$day %in% c(1:7) & upp$month==4) ) ,]              
# selecting required attributes
workabledata=select(workable_data,fscl_week_end_dt,product_sku,from_account_name,net_sellthru,month,day)
# removing CDW from main database
workabledata=subset(workabledata,from_account_name!="CDW")
# combining cleaned CDW to workabledata
workable_data_new=rbind(workabledata,upp)

workable_data=workable_data_new

#initialisation
count=NULL
total=NULL
mark=NULL
pointer=NULL
test=NULL
test1=NULL
# Getting rid of combinations with sales less than 20 for last 20 weeks 
for (i in 1:nrow(useful_sku_seller)) {
  print(i)
  newdata=as.data.frame(workable_data[workable_data$product_sku== useful_sku_seller[i,1] & workable_data$from_account_name==useful_sku_seller[i,2],])
  count=append(count,sum(newdata$net_sellthru==0))
  total=append(total,sum(!is.na(newdata$net_sellthru)))
  mark=NULL
  for(i in (nrow(newdata)-20):nrow(newdata)){
    
    mark=append(mark,ifelse(newdata[(nrow(newdata)-19):nrow(newdata),'net_sellthru'] <20,1,0))}
  
  pointer=append(pointer,ifelse(sum(mark)==21,1,0))  
}
test=as.data.frame(cbind(count,total,pointer))
test['%']=(test$count/test$total)*100
test=cbind(test,useful_sku_seller)
val_test=test[test$`%`>10,]
test1=test[test$pointer==0,]

result_median=NULL
newdata=NULL
#iterating loop over the rows in test1
for (i in 1:nrow(test1)){
  print(i)
  # extracting sales datapoints for each SKU-seller and storing in a list format
  newdata[i]=list(workable_data[workable_data$product_sku== test1[i,'product_sku'] & workable_data$from_account_name==test1[i,"from_account_name"],])
  # assuring that there are atleast 2 values in a combination
  if (nrow(newdata[[i]])>1){print(i)
    # making sales into a xts format i.e. time series format
    sales <- xts(newdata[[i]]['net_sellthru'], order.by=as.Date(as.Date(sapply(newdata[i], "[[", 'fscl_week_end_dt')), "%Y/%m/%d"))  
    # developing database to use for dbscan
    compare=as.data.frame(newdata[[i]]['fscl_week_end_dt'])
    compare['sales']=newdata[[i]]['net_sellthru'] 
    # a bit redundant but have done it to check number of sale points are more than a year (could have done it in first comparison)
    if (nrow(compare)>49){
      # applying density base clustering
      dbscan=dbscan(as.ts(compare),max(mean(compare$sales)/2,80),minPts=4)
      # applying decomposition
      decombeermult = decompose (ts((sales),freq=round(min(49,nrow(compare)/4))), type = "additive")
      testset=as.data.frame(newdata[[i]]['fscl_week_end_dt'])
      testset['dates']=sort(testset$fscl_week_end_dt)
      testset['cluster']= dbscan$cluster
      testset['sku']=newdata[[i]]['product_sku']
      testset['seller']=newdata[[i]]['from_account_name']
      testset['sales']=newdata[[i]]['net_sellthru']
      
      # plot(sales,main=paste(useful_sku_seller[i,1],useful_sku_seller[i,2]))
      # random error method
      testset['random']=decombeermult$random
      testset['tr_se']=decombeermult$trend+decombeermult$seasonal # sum of trend and seasonality 
      
      ###############################################
      #forecasting usage for end points
      # plot(compare,type='l')
      # since decompose can't work on last 24 points I predicted those points using trend+seasonality and then found anomalies
      Sales3 <- xts(testset$tr_se, order.by=as.Date(as.Date(testset$dates)), "%Y/%m/%d")
      Sales3=as.data.frame(Sales3,row.names=FALSE)
      Sales3=Sales3[!is.na(Sales3$`decombeermult$trend`),]
      y <- ts(Sales3, frequency=round(min(49,nrow(compare)/4)))
      fit <- tslm(y ~ trend+season)
      forecast=as.data.frame(forecast(fit, h=24,level=25))
      # plot(forecast(fit, h=24,level=25))
      
      # placing the foreacasted values in trend+seasonality in database for comparison
      testset[(nrow(testset)-23):nrow(testset),'tr_se']=forecast$`Point Forecast`
      testset[(nrow(testset)-23):nrow(testset),'random']=testset[(nrow(testset)-23):nrow(testset),'sales']-testset[(nrow(testset)-23):nrow(testset),'tr_se']
      # developing bounding interval on randomness
      testset['upperlimit']=median(testset$random,na.rm=TRUE)+1.5*sd(testset$random,na.rm=TRUE)
      testset['lowerlimit']=median(testset$random,na.rm=TRUE)-1.5*sd(testset$random,na.rm=TRUE)
      # tagging points depicted by dbscan and decomposition as anomalies
      testset['anomaly']=ifelse((testset$random>testset$upperlimit | testset$random<testset$lowerlimit|testset$cluster==0)&!is.na(testset$random), 1, 0)
      testset['ano_ini']=testset$anomaly  
    
      # plot(testset$dates,testset$sales, col=ifelse(testset$anomaly==1, "red", "black"),type='b')
      
      # making two more iterations of ensemble model
      for (j in 1:2){
        # estimating rolling median
        testset['median']=rollapplyr(testset$sales,8 ,median, fill = 0)
        # imputing value of rolling median in place of anomalies
        testset$sales=ifelse(testset$anomaly==1 ,testset$median,testset$sales)
        sales  <- xts(testset$sales, order.by=as.Date(testset$dates, "%Y/%m/%d"))
        compare$sales= testset$sales
        dbscan= dbscan(as.ts(compare),max(mean(compare$sales)/2.5,80),minPts=4)
        decom49 = decompose (ts((sales),freq=round(min(49,nrow(compare)/4))), type = "additive")
        testset$cluster=dbscan$cluster
  
        # random error method
        testset$random=decom49$random
        testset['tr_se']=decom49$trend+decom49$seasonal
        
        #forecasting usage for end points
        # plot(compare,type='l')
        Sales3 <- xts(testset$tr_se, order.by=as.Date(as.Date(testset$dates)), "%Y/%m/%d")
        Sales3=as.data.frame(Sales3,row.names=FALSE)
        Sales3=Sales3[!is.na(Sales3$`decom49$trend`),]
        y <- ts(Sales3, frequency=round(min(49,nrow(compare)/4)))
        fit <- tslm(y ~ trend+season)
        forecast=as.data.frame(forecast(fit, h=24,level=25))
        # plot(forecast(fit, h=24,level=25))
        # plot(fitted(fit), col ="blue", lwd = 2)
        testset[(nrow(testset)-23):nrow(testset),'tr_se']=forecast$`Point Forecast`
        testset[(nrow(testset)-23):nrow(testset),'random']=testset[(nrow(testset)-23):nrow(testset),'sales']-testset[(nrow(testset)-23):nrow(testset),'tr_se']

        testset$upperlimit=median(testset$random,na.rm=TRUE)+1.5*sd(testset$random,na.rm=TRUE)
        testset$lowerlimit=median(testset$random,na.rm=TRUE)-1.5*sd(testset$random,na.rm=TRUE)
        testset$anomaly=ifelse((testset$random>testset$upperlimit | testset$random<testset$lowerlimit|testset$cluster==0)&!is.na(testset$random), 1, 0)

        # plot(testset$dates,testset$sales, col=ifelse(testset$anomaly==1, "red", "black"),type='b')
      }
      testset['trend']=decom49$trend
      testset['or_sales']=newdata[[i]]['net_sellthru']
      testset$anomaly=ifelse(testset$or_sales==testset$sales,0,1)
      # plot(testset$dates,testset$or_sales, col=ifelse(testset$anomaly==1, "red", "black"),type='b')
      # lines(testset$dates,testset$sales,col='orange')
      result_median=rbind(result_median,testset)
      rm(testset)
      rm(compare)
    }
  }}
# writing result into the server
dbWriteTable(con, "pos_project_median", value=result_median,overwrite=TRUE,row.names=FALSE)

group=dbGetQuery(con,"select product_group_desc,product_sku from gmdm_product_sku_d ")

result=merge(result_median,group,by.x = "sku",by.y = "product_sku")
result['product_group']=result$product_group_desc



#Anomaly data
Promo_data=result[result$anomaly==1,]
Promo_data$fscl_week_end_dt=NULL
Promo_data$cluster=NULL
Promo_data$random=NULL
Promo_data$upperlimit=NULL
Promo_data$lowerlimit=NULL
Promo_data$anomaly=NULL
Promo_data$trend=round(Promo_data$trend)
Promo_data['lift%']=(Promo_data$or_sales-Promo_data$sales)*100/Promo_data$sales

#loss data
dip_data=Promo_data[Promo_data$`lift%`<0,]
dip_data['Month']=month(dip_data$dates)

#gain data
gain_data=Promo_data[Promo_data$`lift%`>0,]
gain_data['Effect']=ifelse(gain_data$`lift%`>100,"Very Effective",ifelse(gain_data$`lift%`>50,"Effective","Less Effective"))



# to disconnect the connection to the server
dbDisconnect(con)


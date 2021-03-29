setwd("/Users/liz/Documents/DAPT/DSD/")

libraries <- c("lubridate","dplyr")
lapply(libraries,library, character.only=TRUE)


#Import data set and covert date field to date values
df <- read.csv('DSDPartners_Data.csv',stringsAsFactors = FALSE)
datecol <- c('Createdate','RecDeliveryDate','Previous2DelDate','MaxScanDate','MaxShipDate')
df[datecol] <- lapply(df[datecol], as.Date, "%m/%d/%Y")

#Add previous delivery date feature
df_ddate <- df %>% group_by(CustStorItemTriadID) %>% arrange(Createdate) %>% 
  mutate(PrevDelDate = lag(RecDeliveryDate)) 
df <- df_ddate %>% group_by(CustStorItemTriadID) %>% arrange(desc(Createdate)) %>%
  mutate(PrevDeliveryDate = if_else(!is.na(PrevDelDate),PrevDelDate,lag(Previous2DelDate))) %>%
  select(-PrevDelDate)

#Add prev ship quantity feature
df <- df %>% group_by(CustStorItemTriadID) %>% arrange(desc(Createdate)) %>%
  mutate(PrevShippedQty = lag(QtyShippedTwoOrderAgo),ActualShipped = lag(QtyShippedTwoOrderAgo,2))
df$PrevShippedQty[is.na(df$PrevShippedQty)] <- 0
df$ActualShipped[is.na(df$ActualShipped)] <- 0

#Add prev order quantities feature
df_onceago <- df %>% group_by(CustStorItemTriadID) %>% arrange(desc(Createdate)) %>%
  mutate(OnceOrderAgo = lag(TwoOrderAgo))
df <- df_onceago %>% group_by(CustStorItemTriadID) %>% arrange(Createdate) %>% 
  mutate(OneOrderAgo = if_else(!is.na(OnceOrderAgo),OnceOrderAgo,lag(OriginalPropOrderQty))) %>%
  select(-OnceOrderAgo)


#Add lead time feature
df$lead_time <- df$RecDeliveryDate-df$Createdate
#lead_time <- df %>% group_by(lead_time) %>% summarise(num_recs=n())

#Add new client feature
df$new_client <- ifelse(is.na(df$Previous2DelDate),1,0)

#Add skipped delivery feature
df$skipped_ship <- ifelse((df$PrevDeliveryDate>df$MaxShipDate & df$OneOrderAgo>0),1,0)
df$skipped_ship[is.na(df$skipped_ship)] <- 0

#Add indicator for never shipped to particular CustStoreItemTriad
df$never_shipped <- ifelse((is.na(df$MaxShipDate)),1,0)

#Add feature - order as % of max order
df$lastorder_maxorder <- ifelse(df$MaxDeliveredQty==0 | is.na(df$MaxDeliveredQty),0,df$OneOrderAgo/df$MaxDeliveredQty)
df$lastorder_maxorder[is.na(df$lastorder_maxorder)] <- 0

#Add column for average delivery size, then add feature for current original order/avg, then one order ago/avg
avgqty <- df %>% group_by(CustStorItemTriadID) %>% summarize(avgqty = mean(PropOrderQty))
df <- left_join(df,avgqty)
df <- df %>% mutate(originalqty_avg = OriginalPropOrderQty/avgqty, lastqty_avg = OneOrderAgo/avgqty, twoagoqty_avg = TwoOrderAgo/avgqty)

df$originalqty_avg[is.infinite(df$originalqty_avg)|is.na(df$originalqty_avg)] <- 0
df$lastqty_avg[is.infinite(df$lastqty_avg)|is.na(df$lastqty_avg)] <- 0
df$twoagoqty_avg[is.infinite(df$twoagoqty_avg)|is.na(df$twoagoqty_avg)] <- 0



#Add alternative "Original" calculation vs. OriginalPropOrder
df$originalqty_alternative <- df$WeightData + df$BaseOrder + df$TrueUpAdjQty
df$OGvsModifiedOG <-  ifelse(((df$WeightData + df$BaseOrder + df$TrueUpAdjQty)==df$OriginalPropOrderQty),1,0)

#export
write.csv(df, "DSD_features.csv",row.names = FALSE)


#Casepack sizes
df_casepacks <- df %>% filter(ConversionUnits>1) %>% 
  select(CustStorItemTriadID,PrevDeliveryDate,QuantOneOrderAgo,last_baseorder,PrevShippedQty,Createdate,ConversionResidual,ConversionFactor,ConversionUnits,BaseOrder,OriginalPropOrderQty,PropOrderQty,ActualShipped,TUAIssue) %>%
  arrange(CustStorItemTriadID,Createdate)
casepack_smry <- df %>% group_by(ConversionResidual) %>% summarise(nu=n())



#Creae Date is always greater than Max Scan
df$Create_MaxScan <- df$Createdate>=df$MaxScanDate
create_maxscan <- df %>% group_by(Create_MaxScan) %>% summarise(nu=n())

#Create Date is always greater than MaxShip
df$Create_MaxShip <- df$Createdate>=df$MaxShipDate
create_maxship <- df %>% group_by(Create_MaxShip) %>% summarise(nu=n())


#Add prev baseorder quantities feature
df <- df %>% group_by(CustStorItemTriadID) %>% arrange(Createdate) %>% 
  mutate(last_baseorder = lag(BaseOrder)) 

#If Prev2DelDate is null, we can assume the client is new.
df_14119988 <- df_onceago[df_onceago$CustStorItemTriadID=="20459896",]

#Conversion residual is always positive. Is there a reason they always round down (ie: send fewer than is ordered) instead of sending slightly more
#with a casepack?
#Cols to drop
#Conversion factor is always .75
unique(df$ConversionFactor)

df_maxord <- df[is.na(df$MaxShipDate),]

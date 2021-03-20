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

#Add prev order quantities feature
df <- df %>% group_by(CustStorItemTriadID) %>% arrange(desc(Createdate)) %>%
  mutate(QuantOneOrderAgo = lag(TwoOrderAgo))

#Add prev baseorder quantities feature
df <- df %>% group_by(CustStorItemTriadID) %>% arrange(Createdate) %>% 
  mutate(last_baseorder = lag(BaseOrder)) 



#Add lead time feature
df$lead_time <- df$RecDeliveryDate-df$Createdate
#lead_time <- df %>% group_by(lead_time) %>% summarise(num_recs=n())

#Add new client feature
df$new_client <- ifelse(is.na(df$Previous2DelDate),1,0)

#Add skipped delivery feature
df$skipped_ship <- ifelse((df$PrevDelDate>df$MaxShipDate & df$QuantOneOrderAgo>0),1,0)
create_maxship <- df %>% group_by(skipped_ship) %>% summarise(nu=n())


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


#If Prev2DelDate is null, we can assume the client is new.
df_14119988 <- df[df$CustStorItemTriadID=="14119988",] %>% select(Createdate,RecDeliveryDate,Previous2DelDate,MaxScanDate,MaxShipDate)

#Conversion residual is always positive. Is there a reason they always round down (ie: send fewer than is ordered) instead of sending slightly more
#with a casepack?
#Cols to drop
#Conversion factor is always .75
unique(df$ConversionFactor)


library(ggplot2) #Visualizing data
library(dplyr) #Data manipulation functions
library(tidyr) #Data manipulation functions
library(lubridate) # functions for dates and times
#Set working directory to directory with all recipient files
setwd("E:\\ucla\\exports")
fields <- c('EmailID',	'RecipientID',	'EmailAddress',	'SendStatus',	
              'SendStatusDesc',	'SendStatusDt',	'IsSending',	'CustomData1',	
              'CustomData2',	'CustomData3',	'CustomData4',	'CustomData5',	
              'CustomData6',	'CustomData7',	'CustomData8',	'CustomData9',	
              'CustomData10',	'CustomData11',	'CustomData12',	'CustomData13',	
              'CustomData14',	'CustomData15',	'CustomData16',	'CustomData17',	
              'CustomData18',	'CustomData19',	'CustomData20',	'SendingServerID',	
              'SegmentCode',	'FinderNumber',	'MarketingEffortID',	'EmailOpenedCount',	
              'LinkClickedCount')

recipients <- data.frame(EmailID = '' ,	RecipientID = '' ,	EmailAddress = '' ,	SendStatus = '' ,	
                         SendStatusDesc = '' ,	SendStatusDt = '' ,	IsSending = '' ,	CustomData1 = '' ,	
                         CustomData2 = '' ,	CustomData3 = '' ,	CustomData4 = '' ,	CustomData5 = '' ,	
                         CustomData6 = '' ,	CustomData7 = '' ,	CustomData8 = '' ,	CustomData9 = '' ,	
                         CustomData10 = '' ,	CustomData11 = '' ,	CustomData12 = '' ,	CustomData13 = '' ,
                         CustomData14 = '' ,	CustomData15 = '' ,	CustomData16 = '' ,	CustomData17 = '' ,
                         CustomData18 = '' ,	CustomData19 = '' ,	CustomData20 = '' ,	SendingServerID = '' ,	
                         SegmentCode = '' ,	FinderNumber = '' ,	MarketingEffortID = '' ,	EmailOpenedCount = '' ,	
                         LinkClickedCount = '')

for(fileName in list.files()){
  print(paste("Reading ", fileName, "..."))
  df <- read.csv(fileName, header=TRUE, stringsAsFactors = FALSE)
  recipients<- rbind(df, recipients)
  rm(df)
}
recipients$EmailOpenedCount <- as.integer(recipients$EmailOpenedCount)
recipients$SendingServerID <- as.integer(recipients$SendingServerID)
recipients <- recipients %>% 
                            select(EmailID, RecipientID, EmailAddress, SendStatus, SendStatusDesc,
                                   SendingServerID, EmailOpenedCount, LinkClickedCount, IsSending)
recipients$OpenStatus <- ifelse(recipients$SendStatus=="opened",1,0)

setwd("E:\\ucla")
emails <- read.csv("Anderson Emails sent.csv")
emails$EmailID <- as.integer(emails$EmailID)
emails <- emails %>% 
      mutate(SendDt =  as.POSIXct(strptime(DtSent, format="%m/%d/%y %H:%M"))) %>%
      mutate(weekday = lubridate::wday(SendDt, label = TRUE)) %>%
      mutate(month = as.factor(lubridate::month(SendDt, label=TRUE))) %>%
      mutate(hour_of_day = as.integer(hour(SendDt))) %>%
      mutate(EmailID = as.character(EmailID)) %>%
      mutate(time_of_day = ifelse(hour_of_day <11, "morning", 
                                  ifelse(hour_of_day >=11 & hour_of_day < 15, "midday", 
                                         "evening")))


ggplot(emails, aes(x=month)) + geom_bar(aes(fill=EmailCategoryTitle)) + theme_classic()


emailAll  <- inner_join(emails, recipients, by = c("EmailID" ="EmailID"))
open_rates <- emailAll %>% 
  select(EmailID, weekday, month, EmailCategoryTitle, OpenStatus) %>%
  group_by(EmailID, weekday, month, EmailCategoryTitle) %>%
  summarise(open_rate = mean(OpenStatus))
  
  
ggplot(open_rates, aes(x=weekday, y=month))+ geom_raster(aes(fill=open_rate))+theme_minimal()



#Load All Degrees File
setwd("F:\\")
alumni <- read.csv("All Degrees.csv", stringsAsFactors = FALSE)
alumni <- alumni %>%
                    select(CONSTITUENTLOOKUPID, GENDER, BIRTH_DATE,DEGREE1_YEAR, Program,PRIMARYEMAILADDRESS)

recipient_alumni <- inner_join(recipients, alumni, by = c("EmailAddress"= "PRIMARYEMAILADDRESS")) %>%
                    inner_join(emails, by=c("EmailID"="EmailID"))


alumni_open_rates <- recipient_alumni %>%
                                        select(time_of_day, weekday, GENDER, OpenStatus) %>%
                                        group_by(time_of_day,weekday, GENDER, time_of_day) %>%
                                        summarise(open_rate = mean(OpenStatus)) %>%
                                        filter(GENDER != "Unknown")
 
ggplot(alumni_open_rates, aes(x=weekday, y=time_of_day))+geom_raster(aes(fill=open_rate))+
    theme_minimal()

deg_yr_open_rates <- recipient_alumni %>%
                                    select(DEGREE1_YEAR, OpenStatus) %>%
                                    group_by(DEGREE1_YEAR) %>%
                                    summarise(open_rate = mean(OpenStatus)) 
                                        
    
    
    
ggplot(deg_yr_open_rates, aes(x=DEGREE1_YEAR, y=open_rate)) + geom_jitter()


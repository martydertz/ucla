library(dplyr)
library(ggplot2)
library(tidyr)

#Set working directory to wherever files are
setwd("K:/Ard/Alumni Relations/Martin Dertz/Projects/ea_mailer_report/exports/recipient_lists")
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
}
recipients$EmailOpenedCount <- as.integer(recipients$EmailOpenedCount)
recipients$SendingServerID <- as.integer(recipients$SendingServerID)



write.csv(recipients, file = "all_recipients.csv")
setwd("K:/Ard/Alumni Relations/Martin Dertz/Projects/ea_mailer_report/exports")
emails <- read.csv("Anderson Emails sent.csv")
emails$EmailID <- as.integer(emails$EmailID)
emails <- emails %>% 
  mutate(SendDt = as.Date(DtSent, format="%m/%d/%y %H:%M")) %>%
  mutate(weekday = lubridate::wday(SendDt, label = TRUE)) %>%
  mutate(month = as.factor(lubridate::month(SendDt, label=TRUE)))

ggplot(emails, aes(x=month)) + geom_bar(aes(fill=EmailCategoryTitle)) + theme_classic()


emailAll  <- merge(emails, recipients, by = c("EmailID", "EmailID"))
emailAll$OpenStatus <- ifelse(emailAll$SendStatus == "opened", 1, 0)
open_rates <- emailAll %>% 
  select(EmailID, weekday, month, EmailCategoryTitle, OpenStatus) %>%
  group_by(EmailID, weekday, month, EmailCategoryTitle) %>%
  summarise(open_rate = mean(OpenStatus))
  
  
ggplot(open_rates, aes(x=weekday, y=month))+
  geom_raster(aes(fill=open_rate))+
  theme_classic()+facet_grid(EmailCategoryTitle~.)

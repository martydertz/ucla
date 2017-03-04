library(RODBC) #Connect and query to external SQL Server
library(reshape2) #For dataframe casting and melting

##Get email name and degree data from CRM using All Degrees file
wd <- "K:\\Ard\\Alumni Relations\\Data Management\\DPR\\Updated All Degrees File"
setwd(wd)
ecrm_email <- read.csv("All Degrees.csv", na.strings =c("", "#NA", "<N/A>", "<NA>", "N/A"), strip.white=TRUE)
alumCols <- c("CONSTITUENTLOOKUPID", "FIRSTNAME", "LAST_NAME",
              "DEGREE1_YEAR", "DEGREE1_CODE","DEGREE1_MAJOR_CODE",
              "HOME_EMAILADDRESS", "BUSINESS_EMAILADDRESS", "PROGRAM")
ecrm_email <- ecrm_email[alumCols]

#Get degree and email and preferred name from TOAD
##Create connection to Anderson's SQL server
dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=sqldb.anderson.ucla.edu;
                              database=alumni;
                              trusted_connection=true')
##Use connection to select all active emails and corresponding CRM ID's 
sqlString <- "SELECT a.id, a.preferred_name, d.type, d.email
              FROM alum_agsmdata a JOIN alum_address d ON a.id = d.id JOIN alum_edu e ON e.id = a.id
              WHERE d.status ='A' "
toad_email <- unique(sqlQuery(dbhandle, sqlString))
##Get all emails from email log
sqlString <- "SELECT l.id, l.old_email, l.new_email
FROM alum_email_log l"
all_emails <- unique(sqlQuery(dbhandle, sqlString))
##Close Connection
odbcClose(dbhandle)
##Reshape data to fit All Degrees file format
toad_email <- dcast(toad_email, id + preferred_name ~ type, value.var = "email")

##Merge TOAD and CRM dataframes using alumni ID's
alumni <- merge(toad_email, ecrm_email, by.x = "id", by.y="CONSTITUENTLOOKUPID")
##Rename columns makes reading algorithm easier
alumni <- plyr::rename(alumni, c("HOME_EMAILADDRESS" = "crm_h_email", "LAST_NAME" = "crm_last_name",
                                "FIRSTNAME" = "crm_first_name", "BUSINESS_EMAILADDRESS" = "crm_b_email",
                                "B" = "toad_b_email", "H"="toad_h_email", "P"="toad_p_email"))

##Lowercase all text columns 
charCols <- c("preferred_name", "toad_b_email", "toad_h_email", "toad_p_email", "crm_h_email", "crm_b_email",
              "crm_last_name", "crm_first_name")
alumni[,charCols] <- lapply(alumni[,charCols], tolower)

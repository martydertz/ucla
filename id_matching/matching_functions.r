##Match on email -- for every email provided check all TOAD and CRM emails. Only use unique matches
matchEmails <- function(missingDataFrame, refDataCurrent, refDataEmails, emailCols = c("Email")){
  #For each missing record
  numEmails <- length(emailCols)
  for(i in seq(dim(missingDataFrame)[[1]])){
    found <- FALSE
    curEmail <-0
    #For each email provided
    while(!found & curEmail < numEmails){
        curEmail <- curEmail + 1
        colName <- emailCols[curEmail]
        email <- as.character(missingDataFrame[i, colName])
        # Any current email matches
        m <- subset(refDataCurrent, toad_b_email == email  | toad_h_email == email | toad_p_email == email |
                      crm_h_email == email | crm_b_email == email |  anderson_email == email)
        if(dim(m)[1]== 1){
          missingDataFrame[i,'CRM.ID'] = m[['id']]
          missingDataFrame[i, 'Match.Method'] = 'Email Matched'
          found <- TRUE
          }
        if(!found){
          m <- subset(refDataEmails, old_email == email | new_email == email)
          if(dim(m)[1]== 1){
            missingDataFrame[i,'CRM.ID'] = m[['id']]
            missingDataFrame[i, 'Match.Method'] = 'Email Matched'
            found <- TRUE
        }
        }
    }
  }
  return(missingDataFrame)
}
#Matching First and Last Names Exactly
exactMatchFirstLastNameYearProgram<- function(missingDataFrame, refDataCurrent, 
                                                   colNames = c("First.Name", "Last.Name","Year", "Program")){
  fNameColumn <- colNames[1]
  lNameColumn <- colNames[2]
  yearColumn <- colNames[3]
  programColumn <- colNames[4]
  #For each record missing ID
  for(i in seq(dim(missingDataFrame)[1])){
    fName <- missingDataFrame[i, fNameColumn]
    lName <- missingDataFrame[i, lNameColumn]
    year <- missingDataFrame[i, yearColumn]
    prog <- missingDataFrame[i, programColumn]
    m <- subset(refDataCurrent, crm_first_name == fName & crm_last_name == lName & DEGREE1_YEAR == year & PROGRAM == prog)
    if(dim(m)[1] == 1){
      missingDataFrame[i,'CRM.ID'] = m[['id']]
      missingDataFrame[i, 'Match.Method'] = 'Full Name Exact Match + Year + Program'
    }
  }
  return(missingDataFrame)
}

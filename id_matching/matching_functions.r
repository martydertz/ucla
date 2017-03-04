##Match on email -- for every email provided check all TOAD and CRM emails. Only use unique matches
matchEmails <- function(df, emailColumns = c("Email")){
  #For each missing record
  for(i in seq(dim(df)[[1]])){
    found <- FALSE
    #For each email provided
    for(columnName in emailColumns){
      if(!found){
        email <- as.character(missing[i, columnName])
        # Any current email matches
        m <- subset(alumni, toad_b_email == email  | toad_h_email == email | toad_p_email == email |
                      crm_h_email == email | crm_b_email == email |  anderson_email == email)
        if(dim(m)[1] == 0){
          m <- subset(all_emails, old_email == email | new_email == email)
        }
        if(dim(m)[1]== 1){
          missing[i,'CRM.ID'] = m[['id']]
          missing[i, 'Match.Method'] = 'Email Matched'
          found <- TRUE
        }
      }
    }
  }
}


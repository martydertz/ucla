
df <- read.csv('all_and_empl.csv')
names(df) <- gsub("Relationships.Employment.Information.", "", names(df))
names(df) <- gsub("Relationships.Related.Constituent.", "", names(df))
names(df)[1] <- "Full.Name"


library(reshape2)
head(dcast(df, Lookup.ID ~ Job.title, value.var = "Job.title"))
df$Lookup.ID <- as.factor(df$Lookup.ID)
df$Start.datef <- strptime(df$Start.date, format = "%m/%d/%Y")
df$End.datef <- strptime(df$End.date, format = "%m/%d/%Y")
cur_emp <- subset(df, Job.schedule == "Current")
p_emp <- subset(df, Job.schedule == "Former")

sp_df <- split(p_emp, p_emp$Lookup.ID)

d <- sp_df[[1]]
d <- d[,c("Lookup.ID", "Job.title", "End.date", "Start.date", "Lookup.ID.1", "Name")]
d <-plyr::rename(d,replace= c("Job.title"="Former.Title") )
comb <- merge( cur_emp,d, by.x="Lookup.ID", by.y="Lookup.ID", all=TRUE)
sp_df[4]

for(i in seq(1:length(sp_df)){
    emp <- sp_df[[i]][,c("Lookup.ID", "Job.Title", "End.date", "Start.date", "Name")]
    emp <- emp[order(emp$End.date, decreasing=FALSE)]
    if(dim(emp[1] > 0)){
        for(m in 1:nrow(emp)){
            emp.i <- emp[i,]
        }
    }
}

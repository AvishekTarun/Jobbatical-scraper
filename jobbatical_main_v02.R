library(XML)
library(RCurl)
library(magrittr)
library(stringr)
library(car)
#write.csv(cbind(job_id,job_links,Sys.Date()), paste(Sys.Date(),".csv",""))

curl_head<-"https://jobbatical.com/explore"
  url_head<-(getURL(curl_head))
    job_head_data<-htmlTreeParse(url_head,useInternalNodes = TRUE)

job_links_raw <- xpathSApply(job_head_data, "//a/@href") %>% as.vector
  job_links_partial <- job_links_raw[grep("/l/",job_links_raw)]
    job_links<-paste("https://jobbatical.com",job_links_partial,sep="")

job_id<-gsub(pattern="/l/",replacement="",job_links_partial) %>% 
  gsub(pattern="/.*",replacement="")


company_name <- xpathSApply(job_head_data, "//span[@class='company']/label",xmlValue) %>% 
  as.vector

job_title <- xpathSApply(job_head_data, "//span[@class='company']",xmlValue) %>% as.vector
for(i in 1:length(job_id)){
  job_title[i]<-gsub(company_name[i],"", job_title[i])
}


job_location<-xpathSApply(job_head_data, "//span/label",xmlValue) %>% as.vector


main_data<-cbind.data.frame(job_id,job_links,company_name,job_title,Sys.Date())
#head(output)

child_data<-data.frame()

source("jobbatical_child_v02.R")
 
#child_data<-lapply(job_links[1:10],job_internal_data)

for(i in 1:length(job_links)){
  child_data<-rbind.data.frame(child_data,job_internal_data(job_links[i]))
}

current<-cbind.data.frame(main_data,child_data)
write.csv(current, paste(Sys.Date(),".csv"))


current<-read.csv(paste(Sys.Date(),".csv"))
current$X<-NULL
current$impressions<-recode(current$impressions,"NA='0'")
current$following<-recode(current$following,"NA='0'")

#write.csv(final_output, "current.csv")



old<-read.csv("current.csv")
old$X<-NULL
old$impressions<-recode(old$impressions,"NA='0'")
old$following<-recode(old$following,"NA='0'")

str(old)
summary(old)

new<-vector()
last_impressions<-vector()
last_following<-vector()

for(i in 1:nrow(current)){
  new[i]<-!current$job_id[i] %in% old$job_id[] 
  row<-match(current$job_id[i],old$job_id[])
  last_impressions[i]<-old$impressions[row]
  last_following[i]<-old$following[row]
}


new<-recode(new,"TRUE='New';FALSE='Old'")
last_impressions<-recode(last_impressions,"NA='0'")
last_following<-recode(last_following,"NA='0'")


current<-cbind(current,new,
               today_impressions=current$impressions-last_impressions, 
               today_following=current$following-last_following)


current<-rbind.data.frame(current,old)
write.csv(current,"current.csv")






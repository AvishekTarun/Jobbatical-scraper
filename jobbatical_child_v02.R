library(XML)
library(RCurl)
library(magrittr)
library(stringr)


job_internal_data<-function(curl_child){
  
#curl_child<-"https://jobbatical.com/l/409669041/take-on-typeforms-remote-tech-support-challenge"
#job_links[106]

url_child<-(getURL(curl_child))
job_child_data<-htmlTreeParse(url_child,useInternalNodes = TRUE)


job_tags<-xpathSApply(job_child_data,"//section [@class='keywords']/a/@href") %>% 
  as.vector %>% 
  gsub(pattern="/explore/",replacement="") %>% 
  gsub(pattern='\\+',replacement=" ") %>% 
  paste(collapse=", ")

love<-xpathSApply(job_child_data,"//div [@id='curve']/p/span/b",xmlValue) %>% 
  as.vector
impressions<-love[1]
following<-love[2]


header<-xpathSApply(job_child_data,"//h1/span",xmlValue) %>% 
  as.vector %>% gsub(pattern=".*Starting ",replacement="") %>% 
  gsub(pattern="\n*",replacement="") %>%
  gsub(pattern="\t*",replacement="") %>%
  gsub(pattern="\\s+", replacement=" ")


starting_date<-paste("01",substr(header, 1,3),"2017") %>% as.Date("%d %b %Y")


map_data<-header<-xpathSApply(job_child_data,"//h3 [@style='display:block']",xmlValue) %>% 
  as.vector %>% 
  gsub(pattern="Find out more on Teleport Cities!",replacement="") %>%
  strsplit(", ")
  
if (length(map_data)==0){
  city="remote"
  country="remote"
} else {
  city<-map_data[[1]][1] 
  country<-map_data[[1]][2]
  }


child_data_frame<-cbind.data.frame(impressions,following,starting_date,city,country,job_tags)
#rbind.data.frame(child_data,child_data_frame)

}
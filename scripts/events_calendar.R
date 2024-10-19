library(tidyverse)
library(rvest)
library(lubridate)

date <- "2024/10/9" #set this to control output. Specify start date

start_date <- ymd(date)
end_date <- ymd(date) + 6
date_range <- interval(start_date,end_date)
year <- as.character(year(start_date))

top_url <- paste("https://events.reed.edu/calendar/week/",date,"/?experience=&order=date",sep="")
final_df <- as.data.frame(matrix(ncol=5,nrow=0))
colnames(final_df) <- c("Url","Date","Title","Description","Location")

while (!is.na(top_url)) {

  message(top_url)
  page <- read_html(top_url)
  url_list <- page %>% html_nodes(".em-card_title") %>% html_children() %>% html_attr("href")
  
  result_df <- as.data.frame(matrix(ncol=5,nrow=0))
  colnames(result_df) <- c("Url","Date","Title","Description","Location")
  
  for (i in 1:length(url_list)) {
    message(i)
    
    url <- url_list[i]
    
    page <- read_html(url)
    
    title <- page %>% html_nodes(".em-header-card_title") %>% html_text2()
    description <- page %>% html_nodes(".em-about_description") %>% html_text()
    if (length(page %>% html_nodes(".em-about_name")) > 0) {
      location <- page %>% html_nodes(".em-about_name") %>% html_text2()  
    } else {
      location <- NA
    }
    
    if (length(page %>% html_nodes(".em-list_dates__content"))) {
      dates_list <- page %>% html_nodes(".em-list_dates__content") %>% html_nodes("li") %>% html_text()
      df <- as.data.frame(matrix(ncol=5,nrow=length(dates_list)))
      colnames(df) <- c("Url","Date","Title","Description","Location")
      
      for (z in 1:length(dates_list)) {
        df[z,]$Date <- dates_list[z]
      }
      
    } else {
      date <- page %>% html_nodes(".em-date") %>% html_text2()
      df$Date <- date
    }
    
    df$Title <- title
    df$Description <- description
    df$Url <- url
    df$Location <- location
    
    result_df <- rbind(result_df,df)

  }
  
  final_df <- rbind(final_df,result_df)
  
  page <- read_html(top_url)
  buttons <- page %>% html_nodes(".em-pagination-item")
  top_url <- NA
  for (j in 1:length(buttons)) {
    button <- buttons[j]
    if (button %>% html_attr("aria-label") == "Next page") {
      top_url <- button %>% html_attr("href")
      top_url <- paste("https://events.reed.edu/",top_url,sep="")
    }
  }

}

final_df <- unique(final_df)

final_df <- cbind(final_df,Full_Date=NA)
final_df <- cbind(final_df,Time=NA)
for (i in 1:length(final_df$Date)) {
  message(i)
  test_text <- final_df[i,]$Date
  if (grepl(year,test_text,fixed=TRUE)) {
    final_df[i,]$Full_Date <- as.character(mdy(substr(test_text,0,str_locate_all(pattern=year,test_text)[[1]][1,][2])))
    final_df[i,]$Time <- str_trim(substr(test_text,str_locate_all(pattern=year,test_text)[[1]][1,][2]+1,str_length(test_text)))
  }
  curr_desc <- final_df[i,]$Description
  if (str_length(curr_desc) > 1500) {
    curr_desc <- substr(curr_desc,0,1500)
    curr_desc <- paste(curr_desc," [...]",sep="")
    final_df[i,]$Description <- curr_desc
  }
}

final_df <- final_df %>% arrange(Full_Date)
final_df <- final_df %>% filter(ymd(Full_Date) %within% date_range)

final_df$Full_Date <- ymd(final_df$Full_Date)
final_df <- cbind(final_df,Human_Dates=NA)
final_df$Human_Dates <- stamp("Friday, March 1, 1999")(final_df$Full_Date)
write.csv(final_df,"data/events-calendar.csv",row.names=FALSE)

#now knit the events-calendar.Rmd file to HTML

source("~/Documents/Developer/R/events-calendar/functions.R")

url <- "https://www.reed.edu/community_safety/past-blotters-activity.html"
page <- read_html(url)

links_list <- page %>% html_nodes("p") %>% html_children() %>% html_attr("href")
links_df <- as.data.frame(links_list)
links_df <- links_df %>% filter(!is.na(links_list))
links_df <- links_df %>% filter(grepl("blotters",links_list))
links_df$links_list <- paste("https://reed.edu/community_safety/",links_df$links_list,sep="")

df <- as.data.frame(matrix(ncol=8,nrow=0))
colnames(df) <- c("Case#","Date","Time","Description","Location","Date_Time","Notes","Human_Dates")

for (i in 1:length(links_df$links_list)) {
  url <- links_df[1,]
  
  temp_df <- getFullReports(url)
  temp_df$Date <- date(temp_df$Date_Time)
  temp_df <- cbind(temp_df,Human_Dates=NA)
  temp_df$Human_Dates <- stamp("Friday, March 1, 1999")(temp_df$Date)
  
  page <- read_html(url)
  lead <- page %>% html_nodes(".lead") %>% html_text()
  lead_df <- as.data.frame(matrix(ncol=1,nrow=1))
  colnames(lead_df) <- c("lead")
  lead_df$lead <- lead
  
  df <- rbind(df,temp_df)
}


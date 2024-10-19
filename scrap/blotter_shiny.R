source("~/Documents/Developer/R/events-calendar/functions.R")

url <- "https://www.reed.edu/community_safety/past-blotters-activity.html"
page <- read_html(url)

links_list <- page %>% html_nodes("p") %>% html_children() %>% html_attr("href")
links_df <- as.data.frame(links_list)
links_df <- links_df %>% filter(!is.na(links_list))
links_df <- links_df %>% filter(grepl("blotters",links_list))
links_df$links_list <- paste("https://reed.edu/community_safety/",links_df$links_list,sep="")

url <- links_df[1,]

df <- getFullReports(url)
df$Date <- date(df$Date_Time)
df <- cbind(df,Human_Dates=NA)
df$Human_Dates <- stamp("Friday, March 1, 1999")(df$Date)

page <- read_html(url)
lead <- page %>% html_nodes(".lead") %>% html_text()
lead_df <- as.data.frame(matrix(ncol=1,nrow=1))
colnames(lead_df) <- c("lead")
lead_df$lead <- lead
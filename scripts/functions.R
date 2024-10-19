library(tidyverse)
library(rvest)
library(plyr)
library(lubridate)

getCounts <- function(url) {
  
  page <- read_html(url)
  nodes <- page %>% html_nodes("p")
  for (j in 1:length(nodes)) {
    curr_node <- nodes[j]
    curr_text <- curr_node %>% html_text()
    if (grepl("Unlocks:",curr_text)) {
      counts <- curr_node
    }
  }
  count_text <- counts %>% html_text2()
  type_list <- strsplit(count_text,"\n")
  type_list <- type_list[[1]]
  
  type_df <- as.data.frame(type_list)
  type_df <- type_df %>% filter(type_list != "")
  
  type_df <- data.frame(do.call('rbind', strsplit(as.character(type_df$type_list),': ',fixed=TRUE)))
  colnames(type_df) <- c("Type","Incidents")
  
  return(type_df)
  
}

getDate <- function(url) {
  page <- read_html(url)
  date <- page %>% html_nodes(".lead") %>% html_text()
  date <- strsplit(date," - ",fixed=TRUE)[[1]]
  date <- mdy(date)
  return(date)
}

getStartDate <- function(url) {
  return(getDate(url)[1])
}

getEndDate <- function(url) {
  return(getDate(url)[2])
}

getCompleteTypeFrame <- function(url) {
  df <- getCounts(url)
  df <- cbind(df,startDate=NA)
  df$startDate <- getStartDate(url)
  df <- cbind(df,endDate=NA)
  df$endDate <- getEndDate(url)
  return(df)
}

parse_case_header <- function(node) {
  void_check <- node %>% html_text()
  message(void_check)
  
  children <- node %>% html_children()
  length_check <- length(children)
  z <- 1
  while (z <= length_check) {
    check_text <- children[z] %>% html_text
    if ((check_text == "") | (check_text == "Vehicle")) {
      children <- children[-z]
      z <- z
    } else {
      z <- z + 1
    }
    length_check <- length(children)
  }
  num_rows <- length(children) / 2
  temp_df <- as.data.frame(matrix(ncol=2,nrow=num_rows))
  colnames(temp_df) <- c("Detail","Value")
  for (i in 1:length(children)) {
    if ((i %% 2) != 0) {
      temp_df[i,]$Detail <- children[i] %>% html_text()
      temp_df[i,]$Value <- children[i+1] %>% html_text()
    }
  }
  temp_df <- temp_df %>% filter(!is.na(Detail))
  temp_df$Detail <- gsub(":","",temp_df$Detail)
  
  merged_date <- paste(temp_df[which(temp_df$Detail == "Date"),]$Value,temp_df[which(temp_df$Detail == "Time"),]$Value,sep=" ")
  merged_date <- parse_date_time(merged_date,orders=c("ymdHM"))
  
  transposed_df <- t(temp_df)
  colnames(transposed_df) <- transposed_df[1,]
  colnames(transposed_df) <- gsub(" ","",colnames(transposed_df))
  transposed_df <- as.data.frame(transposed_df)
  rownames(transposed_df) <- 1:length(transposed_df$Date)
  transposed_df <- transposed_df[-c(1),]
  rownames(transposed_df) <- 1:length(transposed_df$Date)
  transposed_df <- cbind(transposed_df,Date_Time=NA)
  transposed_df$Date_Time <- merged_date
  
  if ("Notes" %in% colnames(transposed_df)) {
    transposed_df <- transposed_df %>% select(!Notes) 
  }
  
  return(transposed_df)
  
}

getFullReports <- function(url) {
  
  page <- read_html(url)
  nodes <- page %>% html_nodes("p")
  
  df <- as.data.frame(matrix(ncol=5,nrow=0))
  colnames(df) <- c("Case#","Date","Time","Description","Location")
  
  for (j in 1:length(nodes)) {
    curr_node <- nodes[j]
    curr_text <- curr_node %>% html_text()
    if (grepl("Case #",curr_text)) {
      case_header <- curr_node
      case_notes <- nodes[j+1]
      if (grepl("VOID",case_header %>% html_text()) | 
          grepl("TEST CASE",case_header %>% html_text()) | 
          grepl("Incomplete",case_header %>% html_text()) | 
          grepl("incomplete",case_header %>% html_text()) |
          grepl("Transport - Alcohol",case_header %>% html_text()) |
          grepl("Possession",case_header %>% html_text())) {
        message("void report")
      } else {
        temp_row <- parse_case_header(case_header)
        temp_row <- cbind(temp_row,Notes=NA)
        temp_row$Notes <- case_notes %>% html_text
        df <- rbind.fill(df,temp_row) 
      }
    }
  }
  
  return(df)
  
}

vec_word_search <- function(vec,word) {
  cloned_vec <- c()
  for (i in 1:length(vec)) {
    cloned_vec[i] <- str_detect(vec[i],word)
  }
  return(cloned_vec)
}







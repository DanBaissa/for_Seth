library(tidyverse)
library(rvest)
library(rlist)
library(XML)
library(httr)


years <- 2015:2019 # select the years that you want

college_data <- c() #creates an empty dataset


drafts <- paste0("https://www.pro-football-reference.com/years/", years, "/draft.htm")

for (j in 1:length(drafts)) {
  
  page <- read_html(drafts[j])
  
  links <- page %>% 
    html_nodes("a") %>% # get the CSS nodes
    html_text() # extract the link text
  
  urls <- page %>%
    html_nodes("a") %>% # get the CSS nodes
    html_attr("href") # extract the URLs
  
  url_data <- data.frame(links, urls) # making a dataset of URLS to filter out unwanted info
  
  college <- url_data %>%
    filter(links == "College Stats") # filtering only the college stats links
  
  for (i in 1:length(college$urls)) {
    
    player <- read_html(college$urls[i]) # reads in their college stats page
    
    name <- player %>%
      html_nodes("h1 span") %>% #reads the players name
      html_text()
    
    tables <- player %>% # reads the top table. Their most important stats except this does not include QB rushing
      html_table()
    
    d <- as.data.frame(tables) %>% # Makes the table into a dataset
      mutate(player = name) %>%
      mutate(draft_year = years[j]) %>% # creates a variable for draft year
      head(-1) %>%
      slice(-1)
    
    college_data <- bind_rows(college_data, d)
    
  }
  
  
}

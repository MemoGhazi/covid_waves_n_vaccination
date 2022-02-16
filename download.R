library(rvest)
library(purrr)
library(tidyverse)
library(stringr)

# # ----------------------- Scrape victoria Covid cases --------------------------------
# url_vic <- "https://covidlive.com.au/report/daily-cases/vic"
# html <- read_html(url_vic)
# vic <- html %>% html_node(".DAILY-CASES") %>%
#   html_table(trim = TRUE) %>% select(DATE,NEW)
# 
# # ----------------------- Scrape nsw Covid cases -------------------------------------
# url_nsw <- "https://covidlive.com.au/report/daily-cases/nsw"
# html <- read_html(url_nsw)
# nsw <- html %>% html_node(".DAILY-CASES") %>%
#   html_table(trim = TRUE) %>% select(DATE,NEW)

# -----------------------Scrape all states Covid cases-------------------------------------
url_main <- "https://covidlive.com.au"
html_main <- read_html(url_main)
territories_case_urls <- html_main %>% html_elements(".CASES table a") %>% html_attr('href') %>% str_c(url_main, .)
df_cases <- data.frame()
for (url in territories_case_urls){ 
  
  territory_html <- read_html(url)
  # url of cases stats
  cases_url <- territory_html %>% html_elements(".DAILY-CASES.STD-3 h2 a") %>% html_attr('href') %>% str_c(url_main,.,sep = "")
  cases_html <- read_html(cases_url)
  # Get the table
  new_cases_count <- cases_html %>% html_node(".DAILY-CASES") %>%
    html_table(trim = TRUE) %>% select(DATE,NET)
  
  # Do split to extract location name from url
  location_name <- url %>% str_split("/") %>% .[[1]]
  location_name <- tail(location_name,n=1)
  
  # put scarpe value in data frame
  if (dim(df_cases)[1] != 0) {
    temp <- data.frame(date=new_cases_count$DATE, territory=new_cases_count$NET ,stringsAsFactors = FALSE)
    names(temp) <- c('date',location_name) 
    df_cases <- df_cases %>% full_join(temp, by="date")
    print(names(df_cases))
  } else{
    df_cases <- data.frame(date=new_cases_count$DATE, territory=new_cases_count$NET ,stringsAsFactors = FALSE)
    names(df_cases) <- c('date',location_name) 
  }
}

# ----------------- Scrape all locations vaccinations stats--------------------

territories_vac_urls <- html_main %>% html_elements(".VACCINATIONS table a") %>% html_attr('href') %>% str_c(url_main, .)
df <- data.frame()
for (url in territories_vac_urls){ 
  
  territory_html <- read_html(url)
  # url of vaccination stats
  vaccinations_url <- territory_html %>% html_elements(".DAILY-VACCINATIONS-PEOPLE.MIX-3 h2 a") %>% html_attr('href') %>% str_c(url_main,.,sep = "")
  vaccinations_html <- read_html(vaccinations_url)
  vaccinations_count <- vaccinations_html %>% html_node(".DAILY-VACCINATIONS-PEOPLE") %>%
    html_table(trim = TRUE) %>% select(DATE,SECOND,'12+')
  
  # Do split to extract location name from url
  location_name <- url %>% str_split("/") %>% .[[1]]
  location_name <- tail(location_name,n=1)
  
  ## put scarpe value in data frame
  if (dim(df)[1] != 0) {
        dd <- data.frame(date=vaccinations_count$DATE, territory=vaccinations_count$SECOND, percent=vaccinations_count$'12+'  ,stringsAsFactors = FALSE)
    names(dd) <- c('date',location_name, paste('percent_',location_name, sep = "")) 
    df <- df %>% full_join(dd, by="date")
     print(names(df))
  } else{
    df <- data.frame(date=vaccinations_count$DATE, territory=vaccinations_count$SECOND, percent=vaccinations_count$'12+' ,stringsAsFactors = FALSE)
    names(df) <- c('date',location_name, paste('percent_',location_name, sep = "")) 
    }
}

#---------------------------- CLEANING and change TYPE -----------------------------------------------------
df_cases$date <- as.Date(as.POSIXct(df_cases$date, format="%d %h %y"))
states <- c('vic','nsw','qld','act','wa','sa','nt','tas','australia') 
df_cases[,-1]  <- df_cases[,-1] %>%
  mutate_at(states,list(~as.numeric(str_replace_all(.,',','') )))
df_cases[is.na(df_cases)] <- 0

df$date <- as.Date(as.POSIXct(df$date, format="%d %h %y"))
states_percent <- c('percent_vic','percent_nsw','percent_qld','percent_act','percent_wa','percent_sa','percent_nt','percent_tas','percent_australia') 
df[,-1]  <- df[,-1] %>%
  mutate_at(states,list(~as.numeric(str_replace_all(.,',','') ))) %>% 
  mutate_at(states_percent,list(~as.numeric(str_replace_all(.,'%','') )))




save(df_cases, df, file = 'examdata.RData')


rm(list=ls())


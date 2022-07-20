# Web Scraper for all issues of journal "Sovetskaya etnografiya" from https://www.booksite.ru/etnogr/index.htm 

install.packages(c("rvest","stringr","purrr"))
library(rvest)
library(stringr)
library(purrr)

dir.create("sovetskaya_etnografiya")
setwd("sovetskaya_etnografiya")

url <- "https://www.booksite.ru/etnogr/index.htm"

suburls <- url %>%
  read_html() %>% 
  html_nodes(xpath = "//tr/td/font/a") %>% 
  html_attr("href") %>%
  str_c("https://www.booksite.ru/etnogr/", .) %>%
  map(read_html) %>%
  map(html_nodes, xpath = "//tr/td/font/a") %>%
  map(html_attr, "href") %>%
  unlist() %>% 
  str_c("https://www.booksite.ru/etnogr/", .) %>%
  walk2(., basename(.), download.file, mode = "wb")
suburls
  


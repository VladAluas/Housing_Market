# Libraries ---------------------------------------------------------------
library(xml2)
library(rvest)
library(tidyverse)
library(parallel)
library(doBy)
library(vroom)
library(foreach)
library(doParallel)

# Variables ---------------------------------------------------------------

# In this section we will store our Website variable and the number of pages per website

main_url <- "https://www.imobiliare.ro/vanzare-case-vile/cluj?id=172884604&pagina="

max_pages <- 75

num_cores <- parallel::detectCores() - 1

cl <- parallel::makeCluster(num_cores)

# Functions ---------------------------------------------------------------

# We need a function that will give us all the URL's for the different pages of the website

webs <- function(url, max_pages){
  webs <- vector("list", max_pages)
  for(i in seq_along(1:max_pages)){
    webs[[i]] <- paste(url, i, sep = "")
  }
  webs
}

# Extract the Price
price_func <- function(x){
  paste(xml_nodes(x, '[class="identificator-oferta"]') %>% html_text(),
        html_nodes(x, "[class='pret first blue']") %>% 
          #html_nodes("[style='cursor: help']") %>% 
          html_text(),
        sep = ":")
  
}

description_func <- function(x){
  paste(xml_nodes(x, '[class="identificator-oferta"]') %>% html_text(),
        html_nodes(x, "[class='lista-tabelara']") %>% html_nodes("li") %>% html_text(),
        sep = ":")
}

description_func_2 <- function(x){
  paste(xml_nodes(x, '[class="identificator-oferta"]') %>% html_text(),
        html_nodes(x, "[class='lista-tabelara mobile-list']") %>% html_nodes("li") %>% html_text(),
        sep = ":")
}


title_func <- function(x){
  paste(xml_nodes(x, '[class="identificator-oferta"]') %>% html_text(),
        html_nodes(x, "[class='titlu']") %>% html_nodes("h1") %>% html_text(),
        sep = ":")
}

geo_func <- function(x){
  html_nodes(x, 'iframe') %>%
    xml_attr("data-src")
}

offer_func <- function(x){
  xml_nodes(x, '[class="identificator-oferta"]') %>% html_text()
}

vat_func <- function(x){
  paste(xml_nodes(x, '[class="identificator-oferta"]') %>% html_text(),
        html_nodes(x, "[class='tva']")%>% html_text(),
        sep = ":")
}

table_func <- function(x){
  df <- as_tibble_col(column_name = "Web", x = NULL)
  
  for (i in seq(x)) {
    df <- df %>% rbind(x[[i]] %>% tibble())
  }
  
  df
}
# Transformation ----------------------------------------------------------

# 1. We need a list of all the URL's
web_page_list <- webs(main_url, max_pages) 

# 2. We need to read the HTML code source for all the pages. Since the `read_html()` function does not work with lists, we will use `lapply()`

doParallel::registerDoParallel(cl) 

system.time(web_html <- parallel::parLapply(cl, web_page_list, function(x) as.character(xml2::read_html(x))))

system.time(web_html <- lapply(web_html, read_html))

stopImplicitCluster()

# 3. To get the actual information, you need to access a different link, so we will extract those links and repeat the process for the new list

web_page_list_final <- lapply(web_html, function (x) html_nodes(x, "[itemprop='name']") %>% html_attr("href")) %>% unique()  

web_page_list_final <- unlist(web_page_list_final) %>% unique()

# Now that we have the new list with the final web addresses, we will just need to read the HTML's of the 8000 announcements

doParallel::registerDoParallel(cl) 

system.time(web_html_final <- parallel::parLapply(cl, web_page_list_final, function(x) as.character(xml2::read_html(x)))) 

stopImplicitCluster()

system.time(web_html_final <- lapply(web_html_final, read_html))


# Extracting data from the HTML

price <- lapply(web_html_final, price_func)

description <- lapply(web_html_final, description_func)

description_2 <- lapply(web_html_final, description_func_2)

title <- lapply(web_html_final, title_func)

offer_id <- lapply(web_html_final, offer_func)

geo_coords <- lapply(web_html_final, geo_func)

vat <- lapply(web_html_final, vat_func)

website <- table_func(web_page_list_final)

# Transformation of the lists into a tibble

# We will start with the Geographical Coordinates
# We can estrct them from the url to the google maps

df_geo <- geo_coords  %>%
  as.character() %>%
  unlist() %>%
  as.tibble() %>%
  distinct() %>%
  mutate(ID = str_remove(str_extract(value, "/id/[A-Z0-9]+"), "/id/"),
         Latitude = str_remove(str_extract(value, "/lat/[0-9]+.[0-9]+"), "/lat/"),
         Longitude = str_remove(str_extract(value, "/lon/[0-9]+.[0-9]+"), "/lon/")) %>%
  select(-value) %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

df_price <- price %>%
  unlist() %>%
  as.tibble() %>%
  distinct() %>%
  separate(value, into = c("ID", "Price"), sep = ":") %>%
  mutate(Price = as.numeric(str_remove(str_extract(Price, "USD[0-9]+.[0-9]+"), "USD")) * 1000,
         Price = ifelse(Price < 10000, Price * 1000, Price))

df_desc <- description %>%
  unlist() %>%
  as.tibble() %>%
  distinct() %>%
  separate(value, into = c("ID", "Attribute", "Value"), sep = ":") %>%
  group_by(ID) %>%
  spread(key = Attribute, value = Value) %>%
  ungroup()

df_desc_2 <- description_2 %>%
  unlist() %>%
  as.tibble() %>%
  distinct() %>%
  separate(value, into = c("ID", "Attribute", "Value"), sep = ":") %>%
  group_by(ID) %>%
  spread(key = Attribute, value = Value) %>%
  ungroup() %>%
  select(-V1)

df_vat <- vat %>%
  unlist() %>%
  as.tibble() %>%
  distinct() %>%
  separate(value, into = c("ID", "VAT"), sep = ":") %>%
  mutate(VAT = ifelse(str_detect(VAT, "TVA"), "TVA", "Fara TVA")) %>%
  distinct()

df_title <- title %>%
  unlist() %>%
  as.tibble() %>%
  distinct() %>%
  separate(value, into = c("ID", "Title"), sep = ":")

<<<<<<< HEAD
df_website <- website %>%
  renameCol(".", "Website") %>%
  mutate(ID = as.character(str_extract_all(Website, "X[A-Z0-9]+"))) %>%
  as_tibble()

=======
>>>>>>> 6ca07d0d19faf1efc65558ef44067ecc92c6755b
# Need to rename the columns to eliminate Romanian characters or it will crash the IDE

names(df_desc)   <- c("ID", "Bathrooms", "Balconies", "Kitchens", "Rooms", "Terraces",  "Built Area (Ground Cver)", "Developed Area", "Terraces Area", "Usable Area")
names(df_desc_2) <- c("ID", "Construction Year", "Street Front", "Roof Cover", "Fronts no.", "Garages", "Parking Spots", "Height Regiment", "Resistence Structure", "Land area")


# Merging the tibbles

df <- df_title %>%
  mutate(Date = as.Date(Sys.Date())) %>%
  left_join(df_desc, by = "ID") %>%
  left_join(df_desc_2, by = "ID") %>%
  left_join(df_price, by = "ID") %>%
  left_join(df_vat, by = "ID") %>%
  left_join(df_website, by = "ID") %>%
  mutate(`Total Price` = ifelse(VAT == "TVA", Price + (Price * 0.19), Price)) %>%
  left_join(df_geo, by = "ID") %>%
  mutate(`Construction Year`        = as.numeric(str_extract(`Construction Year`, "[0-9]+")),
         `Built Area (Ground Cver)` = as.numeric(str_extract(`Built Area (Ground Cver)`, "[0-9]+")),
         `Developed Area`           = as.numeric(str_extract(`Developed Area`, "[0-9]+")),
         `Terraces Area`            = as.numeric(str_extract(`Terraces Area`, "[0-9]+")),
         `Usable Area`              = as.numeric(str_extract(`Usable Area`, "[0-9]+")),
         `Land area`                = as.numeric(str_extract(`Land area`, "[0-9]+")),
         `Balconies`                = as.numeric(str_extract(`Balconies`, "[0-9]+")),
         `Type`                     = "Casa proprie")

vroom_write(df,paste(getwd(), "_Datasets/Data_Imobiliare_Case_", Sys.Date(), ".csv", sep = ""), delim = ",")








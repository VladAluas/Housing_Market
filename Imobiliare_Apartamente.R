# Libraries ---------------------------------------------------------------
library(xml2)
library(rvest)
library(tidyverse)
library(doBy)
library(vroom)
library(foreach)
library(doParallel)


# Variables ---------------------------------------------------------------

# In this section we will store our Website variable and the number of pages per website

main_url <- "https://www.imobiliare.ro/vanzare-apartamente/cluj?id=175690324&pagina="

max_pages <- 350

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
  require(magrittr)
  require(rvest)
  
  paste(xml_nodes(x, '[class="identificator-oferta"]') %>% html_text(),
        html_nodes(x, "[class='pret']") %>% 
          html_nodes("[class='titlu valoare-fara-credit']") %>% 
          xml_attr("data-pret_euro"),
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
        html_nodes(x, '[class="titlu"]') %>% html_nodes("h1") %>% html_text(),
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

rm(main_url, max_pages)


# 2. We need to read the HTML code source for all the pages. Since the `read_html()` function does not work with lists, we will use `lapply()`

doParallel::registerDoParallel(cl) 

system.time(web_html <- parallel::parLapply(cl, web_page_list, function(x) as.character(xml2::read_html(x))))

system.time(web_html <- lapply(web_html, read_html))

stopImplicitCluster()

# 3. To get the actual information, you need to access a different link, so we will extract those links and repeat the process for the new list

web_page_list_final <- lapply(web_html, function (x) html_nodes(x, "[itemprop='name']") %>% html_attr("href")) %>% unique()

web_page_list_final <- web_page_list_final %>% unlist() %>% unique()


#Now that we have the new list with the final web addresses, we will just need to read the HTML's of the 8000 announcements

doParallel::registerDoParallel(cl) 


system.time(web_html_final <- parallel::parLapply(cl, web_page_list_final, function(x) as.character(xml2::read_html(x))))

stopImplicitCluster()

system(web_html_final <- lapply(web_html_final, read_html))




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
# We can extract them from the url to the Google maps

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
  distinct()%>%
  separate(value, into = c("ID", "Price"), sep = ":") %>%
  mutate(Price = as.numeric(Price))

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
  ungroup()



df_vat <- vat %>%
  unlist() %>%
  as.tibble() %>%
  distinct() %>%
  separate(value, into = c("ID", "VAT"), sep = ":") %>%
  mutate(VAT = ifelse(str_detect(VAT, "TVA"), "TVA", "Fara TVA")) %>%
  distinct()
rm(vat)


# rm(vat)

df_title <- title %>%
  unlist() %>%
  as.tibble() %>%
  distinct() %>%
  separate(value, into = c("ID", "Title"), sep = ":")


df_website <- website %>%
  renameCol(".", "Website") %>%
  mutate(ID = as.character(str_extract_all(Website, "X[A-Z0-9]+"))) %>%
  as_tibble()


# Need to rename the columns to eliminate Romanian characters or it will crash the IDE

names(df_desc)   <- c("ID", "Compartments", "Comfort", "Floor", "Bathrooms", "Kitchens", "Rooms", "Built Area", "Usable Area", "Total Usable Area")
names(df_desc_2) <- c("ID", "V1","Construction Year", "Balconies", "Garages", "Parking Spots", "Height Regiment", "Resistence Structure", "Type")


# Merging the tibbles

df <- df_title %>%
  mutate(Date = as.Date(Sys.Date())) %>%
  left_join(df_desc, by = "ID") %>%
  left_join(df_desc_2, by = "ID") %>%
  left_join(df_price, by = "ID") %>%
  left_join(df_vat, by = "ID") %>%
  left_join(df_website,  by = "ID") %>%
  mutate(`Total Price` = ifelse(VAT == "TVA", Price + (Price * 0.19), Price)) %>%
  left_join(df_geo, by = "ID") %>%
    mutate(`Construction Year` = as.numeric(str_extract(`Construction Year`, "[0-9]+")),
           `Floor` = as.numeric(str_extract(Floor, "[0-9]")),
           `Built Area` = as.numeric(str_extract(`Built Area`, "[0-9]+")),
           `Usable Area` = as.numeric(str_extract(`Usable Area`, "[0-9]+")),
           `Total Usable Area` = as.numeric(str_extract(`Total Usable Area`, "[0-9]+")),
           `Balconies` = as.numeric(str_extract(`Balconies`, "[0-9]+"))) %>%
  select(-V1)

vroom_write(df,paste(getwd(), "_Datasets/Data_Imobiliare_Apartamente_", Sys.Date(), ".csv", sep = ""), delim = ",")



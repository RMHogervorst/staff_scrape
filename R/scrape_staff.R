## basic website
staff_url <- "https://www.universiteitleiden.nl/en/social-behavioural-sciences/psychology/methodology-and-statistics/staff"
# packages
library(tidyverse)
library(xml2)
library(rvest)
library(purrr)
all_staff <- read_html(staff_url) %>% 
    html_nodes(xpath = '//*[(@id = "content")]//a') %>% 
    as_list() 
# 
# read_html(staff_url) %>% 
#     html_nodes("#content a") %>% 
#     html_text() %>% 
#     .[2] %>% nchar()

# testing stuff with 1 of the list
# all_staff[[12]]$img %>% attr("src") # get image.
# attr(all_staff[[1]][["img"]], "src")
# all_staff[[13]]$div$span[[1]] # get title
# all_staff[[12]]$div$strong[[1]] # name
# all_staff[[9]][["div"]][["span"]][[1]]#this works, but not map
# map_chr(all_staff, function(x) unlist(x[["div"]][["span"]]))
# all_staff[[9]][["div"]][["span"]]%>% unlist()
# all_staff[[17]][["div"]][["span"]][[1]]
# all_staff[[12]] %>% attr("href") # link


## prepopulate a data frame
rows <-  length(all_staff)
staff_frame <- data_frame(
    name = character(rows),
    title = character(rows),
    imagelink = character(rows),
    link = character(rows))

#' simple function to deal with missing values (empty lists)
get_title <- function(x){
    resultlist <-  x[["div"]][["span"]]
    length_ls<- length(resultlist)
    result<- ifelse(length_ls == 0, NA_character_, resultlist[[1]])
    result
}
# get_title(all_staff[[17]]) test.


# map over every item en transform into data frame. 
staff_frame <- staff_frame %>% 
    mutate(
        name = map_chr(all_staff, function(x) x$div$strong[[1]]),
        imagelink = map_chr(all_staff, function(x) attr(x[["img"]], "src")),
        title = map_chr(all_staff, function(x) get_title(x)),
        link = map_chr(all_staff, function(x) attr(x,"href")))

# then use the resulting links to download effective informaiton 
# from page of researcher. 
examplelink <- staff_frame %>% filter(name == "Roel Hogervorst") %>% .$link

# first we test it again with one link
# use p , .basics to get info.  (using selector gadget to determine name)
basic_result <- read_html(staff_frame$link[7]) %>% 
    html_nodes(".basics") %>% 
    as_list()
# [[1]]$dl$dd$a[[1]]  # contains email adress
# attr(,"href") contains phonenumber
# [[1]]$dl$dd[[1]] # contains name (with title)
# basic_result[[1]][["dl"]]["dd"][[1]][[1]] # official name
# basic_result[[1]][[6]][[7]][["a"]] %>% attr("href") # phone number
# basic_result[[1]][[6]][[11]][["a"]][[1]] # email
# basic_result[[1]][[1]][[1]] # complete name

get_basic_result <- function(link, xpath){
    read_html(link) %>% 
        html_nodes(xpath) %>% 
        as_list()
}
#get_basic_result(staff_frame$link[6], ".basics" )

extract_contact_info <- function(result){
    data_frame(complete_name = result[[1]][[1]][[1]],
               official_name = result[[1]][["dl"]]["dd"][[1]][[1]],
               phone_number = result[[1]][[6]][[7]][["a"]] %>% attr("href"),
               email = result[[1]][[6]][[11]][["a"]][[1]])
}
#extract_contact_info(basic_result)

# master function that takes the two functions and returns a row
return_contact_info <- function(link){
    get_basic_result(link, ".basics") %>% 
    extract_contact_info()
}
# return_contact_info(staff_frame$link[5])
# now make it purrrrr

contact_info_staff <- map_df(staff_frame$link, return_contact_info)
#View(contact_info_staff)


# could just join them on order. because why not.
# but in real life not the case.
# visual check if the names are identical
data_frame(ci_name = contact_info_staff$complete_name,
           sf_name = staff_frame$name) %>% 
    View()

all_info <- full_join(contact_info_staff, staff_frame, by = c("complete_name"="name")) 

## options
# trek laatste cijfers van telefoonnummer, of haal tel eraf

# zoek aantal dr's in groep, aantal phd

# doe wat per groep title (moet je professor van bla bla onderscheiden van assisten professor)


### search for news from person (don't know why the university doesn't build an aPI)
baseurl <- "https://www.universiteitleiden.nl/en/search/subject?gsaentity_category=News&c=news&q="
name_paste <- function(name){
    paste(unlist(strsplit(name, " ")), collapse = "+")
}

create_link <- function(name){
    paste0(baseurl, name_paste(name))
}
#create_link("Roel Hogervorst")

webresult <- get_basic_result(link = create_link("Zsuzsa Bakk"), xpath = "#content")
webresult[[1]]$h2[[1]] # number of results
webresult[[1]]$ul %>% length()# 8 items
webresult[[1]][["ul"]][[1]]$a$p[[2]]


retrieve_number_of_news <- function(name){
    webresult <- create_link(name) %>% 
        get_basic_result("#content")
    
    webresult[[1]][["h2"]][[1]]
}
#retrieve_number_of_news(contact_info_staff$complete_name[14])

news <- map_chr(contact_info_staff$complete_name, retrieve_number_of_news)
all_info$news <- news
write_csv(all_info, path = "example_completed_file.csv")

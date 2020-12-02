library(RSelenium)
library("xml2")
library("dplyr")
library("xlsx")
library("rvest")
library("stringr")
library("dplyr")

setwd("C:/Users/user/Desktop/DZ/Course_4/Data_scraping/Graduate_work/Scraping functions")
source("overall 1.1.R", encoding = "UTF-8")
source("firstpage 1.1.R", encoding = "UTF-8")
source("secondpage 1.1.R", encoding = "UTF-8")
source("thirdpage 1.1.R", encoding = "UTF-8")
source("choose_needed 1.1.R", encoding = "UTF-8")
source("change_names 1.1.R", encoding = "UTF-8")
source("organize 1.1.R", encoding = "UTF-8")
source("fourthpage 1.1.R", encoding = "UTF-8")
source("add_region_name.R", encoding = "UTF-8")

setwd("C:/Users/user/Desktop/DZ/Course_4/Data_scraping/Graduate_work/Getting know of an address")

source("big_func.R", encoding = "UTF-8")
source("clean_up.R", encoding = "UTF-8")
source("check_for_settlements_names.R", encoding = "UTF-8")
source("click.R", encoding = "UTF-8")
source("collect_names_of_settlements.R", encoding = "UTF-8")
source("find_real_name.R", encoding = "UTF-8")
source("gyabt.R", encoding = "UTF-8")
source("intermediate.R", encoding = "UTF-8")
source("is_it_so_hard_to_establish_universal_standards.R", encoding = "UTF-8")
source("is.even.R", encoding = "UTF-8")
source("last_part.R", encoding = "UTF-8")
source("outstanding_case.R", encoding = "UTF-8")
source("passport_click.R", encoding = "UTF-8")
source("sc_collect_names.R", encoding = "UTF-8")
source("sc_intermediate.R", encoding = "UTF-8")
source("sc_last_part.R", encoding = "UTF-8")
source("what_region.R", encoding = "UTF-8")

rd <- rsDriver(browser = "firefox")
remDr <- rd[["client"]]

link <- c("http://www.komi.vybory.izbirkom.ru/region/komi?action=show&vrn=21120001136916&region=11&prver=1&pronetvd=1",
          "http://www.belgorod.vybory.izbirkom.ru/region/belgorod?action=show&vrn=2312000860230&region=31&prver=1&pronetvd=1",
          "http://www.voronezh.vybory.izbirkom.ru/region/voronezh?action=show&vrn=23620001509764&region=36&prver=0&pronetvd=0",
          "http://www.kaluga.vybory.izbirkom.ru/region/kaluga?action=show&vrn=2402000975017&region=40&prver=1&pronetvd=1",
          "http://www.kostroma.vybory.izbirkom.ru/region/kostroma?action=show&vrn=24420001040782&region=44&prver=1&pronetvd=1",
          "http://www.kurgan.vybory.izbirkom.ru/region/kurgan?action=show&vrn=24520001118783&region=45&prver=0&pronetvd=0",
          "http://www.magadan.vybory.izbirkom.ru/region/magadan?action=show&vrn=2492000341821&region=49&prver=1&pronetvd=1",
          "http://www.novosibirsk.vybory.izbirkom.ru/region/novosibirsk?action=show&vrn=25420001876696&region=54&prver=0&pronetvd=0",
          "http://www.ryazan.vybory.izbirkom.ru/region/ryazan?action=show&vrn=2622000963952&region=62&prver=0&pronetvd=0",
          "http://www.chelyabinsk.vybory.izbirkom.ru/region/chelyabinsk?action=show&vrn=27420001816660&region=74&prver=0&pronetvd=0",
          "http://www.yamal-nenetsk.vybory.izbirkom.ru/region/yamal-nenetsk?action=show&vrn=2882000693261&region=89&prver=0&pronetvd=0")
link <- "http://www.kostroma.vybory.izbirkom.ru/region/kostroma?action=show&vrn=24420001040782&region=44&prver=1&pronetvd=1"
parties <- c("единая россия", "кпрф", "справедливая россия", "лдпр")
x <- overall(link = link, remDr = remDr, parties = parties)
data <- read.csv("Данные УИКов для ЕДГ")

x1 <- x[!grepl("Цифровые", x[,8]),]
x1[,8] <- sub("УИК", "", x1[,8])

o <- func(x, data)
o <- o[which(!is.na(o[,10])),]
o <- clean_up(o)
o$raion <- NA
sum(is.na(o[,10]))

results <- what_region(table = o, sf = remDr)
prblms <- filter(results, raion %in% NA)

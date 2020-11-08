library(RSelenium)
library("xml2")
library("dplyr")
library("xlsx")
library("rvest")
library("stringr")
library("dplyr")

setwd("C:/Users/user/Desktop/DZ/Course_4/Data_scraping/Scraping_functions_v1.1")
source("overall 1.1.R", encoding = "UTF-8")
source("firstpage 1.1.R", encoding = "UTF-8")
source("secondpage 1.1.R", encoding = "UTF-8")
source("thirdpage 1.1.R", encoding = "UTF-8")
source("choose_needed 1.1.R", encoding = "UTF-8")
source("change_names 1.1.R", encoding = "UTF-8")
source("organize 1.1.R", encoding = "UTF-8")
source("fourthpage 1.1.R", encoding = "UTF-8")
source("add_region_name.R", encoding = "UTF-8")

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
x1 <- overall(link = link, remDr = remDr, parties = parties)
debug(add_region_name)
data <- read.csv("Данные УИКов для ЕДГ")
x <- read.csv("Данные избиркомов для ЕДГ", row.names = 1)
total_data <- read.csv("Адреса УИКов и прочее")


link <- "http://www.komi.vybory.izbirkom.ru/region/komi?action=show&vrn=21120001136916&region=11&prver=1&pronetvd=1"

### Check-check for region names
sf <- remDr
num <- 5
x <- thirdpage(link = link, sf = sf, num = num)
res <- sf$getPageSource()
doc <- read_html(res[[1]])
g <- add_region_name(table = x, doc = doc)
h <- change_names(table = g, parties = parties)
u <- html_nodes(doc, xpath = "/html/body/table[2]/tbody/tr[1]/td[2]/h4")
o <- html_text(u)

debug(add_region_name)

### Check-check for new package
x <- str_locate("fruit", "i")
x

z <- str_sub("fruit", 1, 3)
z
help(str_sub)
unique(x$region)
help(read.csv)

###
data <- read.csv("Данные УИКов для ЕДГ")
z <- read.csv("Данные избиркомов для ЕДГ")
data$region <- tolower(data$region)
x$address <- NA
for (i in seq_len(length(x[,1]))) {
        for (j in seq_len(length(data[,1]))) {
                if ((x[i,9] == data[j,7])&(x[i,8] == data[j,10])) {
                        x[i,10] <- data[j,11]
                } else {
                        next
                }
        }
        x
}

help(seq_len)
z <- x

### Функция, добавляющая адресс !!!
func <- function(x, data) {
        data$region <- tolower(data$region)
        x$address <- NA
        x <- x[!grepl("Цифровые", x[,8]),]
        for (i in seq_len(length(x[,1]))) {
                x[i,8] <- sub("УИК ", "", x[i,8])
        }
        for (i in seq_len(length(x[,1]))) {
                tempr <- filter(data, region == x[i,9])
                tempr <- filter(tempr, name == x[i,8])
                if (length(tempr$region) != 0) {
                        if ((x[i,9] == tempr$region)&(x[i,8] == tempr$name)) {
                                x[i, 10] <- tempr$address
                        }
                } else {
                        next
                }
        }
        x
}
o <- func(x = x1, data = data)
debug(func)
x1 <- x1[,-10]
### Функция, добавляющая тип населенного пункта и район (село или город + район)

setwd("C:/Users/user/Desktop/DZ/Course_4/Data_scraping/Data")
data <- read.csv("Данные УИКов для ЕДГ")
x <- read.csv("Данные избиркомов для ЕДГ", row.names = 1)

z <- o[which(o[,9] == "коми"),]


#### Стратегия №1
raion <- c()
z <- na.omit(z)
come_on <- function(z) {
        for (i in seq_len(length(z[,1]))) {
                if (grepl("район", z[i,10])) {
                        loc <- str_locate(z[i,10], "район")
                        comma <- str_locate_all(z[i,10], ",")
                        comma <- comma[[1]][max(which(comma[[1]][,1] < loc[1])),]
                        raion <- c(raion, str_sub(z[i,10], start = comma[1] + 2, end = loc[1] - 2))
                }
        }
        raion
}

debug(come_on)
unique(raion)
raion <- come_on(z)
i <- 33
y <- z[which(grepl("  Княжпогостский", z[,10])),10]


#### Стратегия №2
rd <- rsDriver(browser = "firefox")
remDr <- rd[["client"]]
remDr$navigate("https://rosstat.gov.ru/dbscripts/munst/")
res <- remDr$getPageSource()
doc <- read_html(res[[1]])
nodes <- html_nodes(doc, xpath = "/html/body/div/table/tbody/tr[2]/td/table/tbody/tr/td[6]")
element <- remDr$findElement(using = "xpath", "/html/body/div/table/tbody/tr[2]/td/table/tbody/tr/td[6]")
remDr$mouseMoveToLocation(webElement = element)
remDr$click()


### Поиск и вывод названий районов !!!
res <- remDr$getPageSource()
doc <- read_html(res[[1]])
nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div")
text <- html_text(nodes)
text <- text[text != ""]
text <- text[-grep("\\(до", text)]
text <- text[-length(text)]
izvelkaem_nazvania_raionov <- function(text) {
        raion <- c()
        for (i in seq_len(length(text))) {
                loc <- str_locate(text[i], "район ")
                rai <- str_sub(text[i], start = as.vector(loc)[2] + 1)
                if (!is.na(rai)) {
                        raion <- c(raion, rai)
                }
        }
        raion
}
raiony <- izvelkaem_nazvania_raionov(text = text)
z1 <- z
z1$raion <- NA
vnosim_raiony_v_tabl <- function(raiony) {
        for (i in seq_len(length(raiony))) {
              z1[grep(raiony[i],z1[,10]),11] <- raiony[i]  
        }
        z1
}
z2 <- vnosim_raiony_v_tabl(raiony)

### Поиск одноименных с районами н.п.!
is.even <- function(num) {
        if (num == 0) {
                FALSE
        } else {
                if (num %% 2 == 0) {
                        TRUE
                } else {
                        FALSE
                }
        }
}


res <- remDr$getPageSource()
doc <- read_html(res[[1]])
nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div")
nodes <- nodes[-33]
#ciy_nodes <- nodes[(length(nodes)-1):length(nodes)] #?is it necessary?
webElem <- remDr$findElement("css", "body")
for (i in seq_len(length(nodes) - 2)) {
        if (!is.even(i)) {
                element <- remDr$findElement(using = "xpath", paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i), "]", sep = ""))
                remDr$mouseMoveToLocation(webElement = element)
                webElem$sendKeysToElement(list(key = "end"))
                remDr$click()
        }
}
i <- 1


### big func !!!
table <- z1
sf <- remDr
link = "https://rosstat.gov.ru/scripts/db_inet2/passport/munr.aspx?base=munst87"
i <- 3
big_func <- function(table, sf) {
        # sf$navigate(link)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div")
        if (grepl("районы", html_text(nodes[length(nodes)]))) {
                nodes <- nodes[-length(nodes)]
        }
        for (i in seq_len(length(nodes) - 2)) {
                if (!is.even(i)) {
                        text <- html_text(nodes[i])
                        if (!grepl("\\(до", text)) {
                                loc <- str_locate(text, "район ")
                                rai <- str_sub(text, start = as.vector(loc)[2] + 1)
                                table[grep(rai, table[,10]),11] <- rai
                                click(i = i, sf = sf)
                                sol <- search_for_coincidence(i = i, rai = rai, sf = sf)
                                click(i = i, sf = sf)
                                table[which((is.na(table[,11])) & grepl(sol, table[,10])),11] <- rai ### is it going to work?
                        } else {
                                next
                        }
                } else {
                        next
                }
        }
        for (i in (length(nodes)-1):length(nodes)) {
                if (!is.even(i)) {
                        click(i, sf)
                } else {
                        res <- sf$getPageSource()
                        doc <- read_html(res[[1]])
                        nodes_city <- html_nodes(doc, xpath = paste("/html/body/form/table/tbody/tr[2]/td/div/div[",
                                                                    as.character(i),"]/div"))
                        cities <- html_text(nodes_city[2:length(nodes_city)])
                        for (j in cities) {
                                table[grep(j, table[,10]), 11] <- j
                        }
                }
        }
        table
}

### click function !!!
click <- function(i, sf) {
        element <- remDr$findElement(using = "xpath", paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i), "]", sep = ""))
        remDr$mouseMoveToLocation(webElement = element)
        remDr$click()
}

### функция для поиска одноименного с районом н.п. !!!
search_for_coincidence <- function(rai, i, sf) {
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i + 1),"]/div", sep = ""))
        text <- html_text(nodes)
        text <- text[-grep("поселени", text)]
        text <- text[-grep("район", text)]
        for (j in seq_len(str_length(rai))) {
                exm <- str_sub(rai, start = 1, end = j)
                if (length(text[grep(exm, text)]) == 1) {
                        sol <- text[grep(exm, text)]
                }
        }
        sol
}

j <- 1
sol <- text[2]
element <- remDr$findElement(using = "xpath", paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i), "]", sep = ""))
remDr$mouseMoveToLocation(webElement = element)
remDr$open()



### it's the time to test that shit out
table <- z1
sf <- remDr
link = "https://rosstat.gov.ru/scripts/db_inet2/passport/munr.aspx?base=munst87"
df <- big_func(link = link, table = table, sf = remDr)
debug(big_func)


### bif_func v1.1

big_func <- function(link, table, sf) {
        banned <- c()
        sf$navigate(link)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div")
        nodes <- nodes[-length(nodes)]
        for (i in seq_len(length(nodes) - 2)) {
                if (!is.even(i)) {
                        text <- html_text(nodes[i])
                        temp <- collect_raiony(text = text, table = table, i = i, sf = sf)
                        if (class(temp) == "data.frame") {
                                table <- temp
                        } else {
                                banned <- temp
                        }
                } else {
                        next
                }
        }
        browser()
        for (i in (length(nodes)-1):length(nodes)) {
                if (!is.even(i)) {
                        click(i, sf)
                } else {
                        res <- sf$getPageSource()
                        doc <- read_html(res[[1]])
                        nodes_city <- html_nodes(doc, xpath = paste("/html/body/form/table/tbody/tr[2]/td/div/div[",
                                                                    as.character(i),"]/div"))
                        cities <- html_text(nodes_city[2:length(nodes_city)])
                        for (j in cities) {
                                table[grep(j, table[,10]), 11] <- j
                        }
                }
        }
        table
}

### collect_raiony
collect_raiony <- function(text, table, i, sf) {
        if (!grepl("\\(до", text)) {
                loc <- str_locate(text, "район ")
                rai <- str_sub(text, start = as.vector(loc)[2] + 1)
                table[grep(rai, table[,10]),11] <- rai
                click(i = i, sf = sf)
                sol <- search_for_coincidence(i = i, rai = rai, sf = sf)
                click(i = i, sf = sf)
                table[which((is.na(table[,11])) & grepl(sol, table[,10])),11] <- rai ### is it going to work?
        } else {
                loc <- str_locate(text, "район ")
                banned <- str_sub(text, start = as.vector(loc)[2] + 1)
        }
}

### search for banned (add later)

### bin_func for several links
big_func <- function(link, table, sf) {
        for (a in link){
                sf$navigate(a)
                res <- sf$getPageSource()
                doc <- read_html(res[[1]])
                nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div")
                nodes <- nodes[-length(nodes)]
                for (i in seq_len(length(nodes) - 2)) {
                        if (!is.even(i)) {
                                text <- html_text(nodes[i])
                                if (!grepl("\\(до", text)) {
                                        loc <- str_locate(text, "район ")
                                        rai <- str_sub(text, start = as.vector(loc)[2] + 1)
                                        table[grep(rai, table[,10]),11] <- rai
                                        click(i = i, sf = sf)
                                        sol <- search_for_coincidence(i = i, rai = rai, sf = sf)
                                        click(i = i, sf = sf)
                                        table[which((is.na(table[,11])) & grepl(sol, table[,10])),11] <- rai ### is it going to work?
                                } else {
                                        next
                                }
                        } else {
                                next
                        }
                }
                for (i in (length(nodes)-1):length(nodes)) {
                        if (!is.even(i)) {
                                click(i, sf)
                        } else {
                                res <- sf$getPageSource()
                                doc <- read_html(res[[1]])
                                nodes_city <- html_nodes(doc, xpath = paste("/html/body/form/table/tbody/tr[2]/td/div/div[",
                                                                            as.character(i),"]/div"))
                                cities <- html_text(nodes_city[2:length(nodes_city)])
                                for (j in cities) {
                                        table[grep(j, table[,10]), 11] <- j
                                }
                        }
                }
                table
        }    
}



### what_region
what_region <- function(table, sf) {
        reg_names <- unique(table[,9])
        for (i in reg_names) {
                sf$navigate("https://rosstat.gov.ru/dbscripts/munst/")
                res <- sf$getPageSource()
                doc <- read_html(res[[1]])
                for (j in 3:11) {
                        nodes <- html_nodes(doc, xpath = paste("/html/body/table[2]/tbody/tr[2]/td/p[",
                        as.character(j), "]/a", sep = ""))
                        site_names <- html_text(nodes)
                        if (sum(grepl(i, site_names, ignore.case = TRUE)) >= 1) {
                                num <- grep(i, site_names, ignore.case = TRUE)
                                link_node <- html_nodes(doc, xpath = paste("/html/body/table[2]/tbody/tr[2]/td/p[",
                                as.character(j), "]/a[", as.character(num), "]", sep = ""))
                                link <- html_attr(link_node, name = "href")
                                passport_click(link = link, sf = sf)
                                table[which(table[,9] == i),] <- big_func(table = table[which(table[,9] == i),], sf = sf)
                        }
                }
        }
        table
}

i <- reg_names
num <- grep(i, site_names, ignore.case = TRUE)
link_node <- html_nodes(doc, xpath = paste("/html/body/table[2]/tbody/tr[2]/td/p/a[", as.character(num),
                                           "]", sep = ""))
link <- html_attr(link_node, name = "href")

### passport_function
passport_click <- function(link, sf) {
        sf$navigate(link)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        element <- remDr$findElement(using = "xpath", "/html/body/div/table/tbody/tr[2]/td/table/tbody/tr/td[6]")
        remDr$mouseMoveToLocation(webElement = element)
        remDr$click()
}

### overall test
rd <- rsDriver(browser = "firefox")
remDr <- rd[["client"]]
check_check <- what_region(table = test_dt, sf = remDr)
debug(what_region)
debug(big_func)
test_dt <- filter(o, region %in% c("коми", "белгородская область"))
test_dt$raion <- NA
test_dt <- test_dt[which(!is.na(test_dt[,10])),]
test_dt <- filter(o, region %in% c("белгородская область"))


nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div[2]")
html_text(nodes)
html_attrs(nodes)
atrs <- html_attrs(nodes)
grepl("onclick", html_attrs(nodes))


### Очередная итерация big_func
nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div")
if (sum(grepl("onclick", html_attrs(nodes))) >= 1) {
        names_to_test <- c()
        for (j in grep("onclick", html_attrs(nodes))) {
                click(i = j, sf = sf)
                n <- get_your_ass_back_there
                names_to_test <- c(names_to_test, n)
                click(i = j, sf = sf)
        }
}



big_func <- function(table, sf) {
        # sf$navigate(link)
        Sys.sleep(3)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div")
        if (grepl("районы", html_text(nodes[length(nodes)]))) {
                nodes <- nodes[-length(nodes)]
        }
        for (i in seq_len(length(nodes) - 2)) {
                if (!is.even(i)) {
                        if (grepl("районы", html_text(nodes[1]))) {
                                i <- i + 1
                        }
                        if ((length(nodes) - 2) == i) {
                                next
                        }
                        text <- html_text(nodes[i])  
                        if (!grepl("\\(до", text)) {
                                rai <- find_real_name(text)
                                table[grep(rai, table[,10]),11] <- rai
                                path <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i), "]", sep = "")
                                names_to_test <- collect_names_of_settlements(i = i, sf = sf, path = path)
                                sol <- check_for_settlements_names(names_to_test, rai)
                                table[which((is.na(table[,11])) & grepl(sol, table[,10])),11] <- rai ### is it going to work?
                        } else {
                                next
                        }
                } else {
                        next
                }
        }
        for (i in (length(nodes)-1):length(nodes)) {
                if (!is.even(i)) {
                        path <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i), "]", sep = "")
                        click(i, path)
                } else {
                        res <- sf$getPageSource()
                        doc <- read_html(res[[1]])
                        nodes_city <- html_nodes(doc, xpath = paste("/html/body/form/table/tbody/tr[2]/td/div/div[",
                                                                    as.character(i),"]/div"))
                        cities <- html_text(nodes_city[2:length(nodes_city)])
                        for (j in cities) {
                                table[grep(j, table[,10]), 11] <- j
                        }
                }
        }
        table
}

### collect_names_of_settlements ?
collect_names_of_settlements <- function(i, sf, path) {
        click(sf, path = path)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = paste("/html/body/form/table/tbody/tr[2]/td/div/div[",
                                               as.character(i + 1), "]/div", sep = ""))
        names_to_test <- c()
        if (sum(grepl("onclick", html_attrs(nodes))) >= 1) {
                for (j in grep("onclick", html_attrs(nodes))) {
                        path1 <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[",
                                      as.character(i + 1), "]/div[", as.character(j), "]", sep = "")
                        click(sf = sf, path = path1)
                        path2 <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i + 1),"]/div[",
                                       as.character(j + 1), "]/div/a", sep = "")
                        n <- gyabt(sf = sf, path2)
                        names_to_test <- c(names_to_test, n)
                        click(sf = sf, path = path1)
                }
        } else {
                path2 <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i + 1),"]/div/a", sep = "")
                n <- gyabt(sf = sf, path = path2)
        }
        names_to_test <- c(names_to_test, n)
        click(i, path = path)
        names_to_test
}

### gyabt (архив)
gyabt <- function(i, j, sf) {
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i + 1),"]/div[",
                                               as.character(j + 1), "]/div/a", sep = ""))
        text <- html_text(nodes)
        if (length(grep("поселени", text)) != 0) {
                text <- text[-grep("поселени", text)]
        }
        if (length(grep("район", text)) != 0) {
                text <- text[-grep("район", text)]   
        }
        if (length(grep("Город ", text)) != 0) {
                text[grep("Город ", text)] <- str_sub(text[grep("Город ", text)], start = as.vector(str_locate(text[grep("Город ", text)][1], "Город"))[2] + 2)
        }
        if (length(grep("Поселок ", text)) != 0) {
                text[grep("Поселок ", text)] <- str_sub(text[grep("Поселок ", text)], start = as.vector(str_locate(text[grep("Поселок ", text)][1], "Поселок"))[2] + 2)
        }
        text
}

### Новая версия

gyabt <- function(sf, path) {
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = path)
        text <- html_text(nodes)
        if (length(grep("поселени", text)) != 0) {
                text <- text[-grep("поселени", text)]
        }
        if (length(grep("район", text)) != 0) {
                text <- text[-grep("район", text)]   
        }
        if (length(grep("Город ", text)) != 0) {
                text[grep("Город ", text)] <- str_sub(text[grep("Город ", text)], start = as.vector(str_locate(text[grep("Город ", text)][1], "Город"))[2] + 2)
        }
        if (length(grep("Поселок ", text)) != 0) {
                text[grep("Поселок ", text)] <- str_sub(text[grep("Поселок ", text)], start = as.vector(str_locate(text[grep("Поселок ", text)][1], "Поселок"))[2] + 2)
        }
        text
}

### check_for_settlements_names
check_for_settlements_names <- function(text, rai) {
        sol <- c()
        for (j in seq_len(str_length(rai))) {
                exm <- str_sub(rai, start = 1, end = j)
                if (length(text[grep(exm, text)]) == 1) {
                        sol <- text[grep(exm, text)]
                }
        }
        if (length(sol) == 0) {
        rai
        } else {
                sol
        }
}

### find_real_name
find_real_name <- function(text) {
        mr_coordinates <- str_locate(tolower(text), "муниципальный район")
        if (grepl(" и ", text)) {
                i_coordinates <- str_locate(text, " и ")
                if (as.vector(mr_coordinates)[1] < as.vector(i_coordinates)[1]) {
                        if (as.vector(mr_coordinates[1]) == 1) {
                                name <- str_sub(text, start = as.vector(mr_coordinates)[2] + 2, end = as.vector(i_coordinates)[1] - 1)
                        } else {
                                name <- str_sub(text, start = 1, end = as.vector(mr_coordinates)[1] - 2)
                        }
                } else {
                        if (as.vector(mr_coordinates)[2] == str_length(text)) {
                                name <- str_sub(text, start = as.vector(i_coordinates)[2] + 1, end = as.vector(mr_coordinates)[1] - 2)
                        } else {
                                name <- str_sub(text, start = as.vector(mr_coordinates[2]) + 2, end = str_length(text))
                        }
                }
        } else {
                if (as.vector(mr_coordinates)[1] == 1) {
                        name <- str_sub(text, start = as.vector(mr_coordinates[2]) + 2, end = str_length(text))
                } else {
                        name <- str_sub(text, start = 1, end = as.vector(mr_coordinates)[1] - 2)
                }
        }
        name
}

### click v1.1
click <- function(sf, path) {
        element <- remDr$findElement(using = "xpath", path)
        remDr$mouseMoveToLocation(webElement = element)
        remDr$click()
}

### test
results <- big_func(table = test, sf = remDr)
debug(big_func)
debug(last_part)
res <- remDr$getPageSource()

results <- what_region(table = test_oa, sf = remDr)
prblms <- filter(results, raion %in% NA)

nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[2]") #!!!
nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div[4]/div")
text <- html_text(nodes)

### clean_up the raw data

clean_up <- function(table) {
        table1 <- table
        for (i in seq_len(length(table[,10]))) {
                table1[i,10] <- str_sub(table[i,10], start = 1, end = as.vector(str_locate(table[i,10], " дом "))[1] - 2)
                if (i == 125) {
                        browser()
                }
        }
        table1
}

clean_up <- function(table) {
        table1 <- table
        for (i in seq_len(length(table[,10]))) {
                if (grepl("улица", table[i,10])) {
                        table1[i,10] <- str_sub(table[i,10], start = 1, end = as.vector(str_locate(table[i,10], " улица"))[1] - 2)
                } else {
                        if (grepl("дом", table[i,10])) {
                                table1[i,10] <- str_sub(table[i,10], start = 1, end = as.vector(str_locate(table[i,10], " дом"))[1] - 2)   
                        }
                }
        }
        table1
}


sum(is.na(test[,10]))
test <- clean_up(test_dt)
test <- test[which(!is.na(test[,10])),]
debug(clean_up)

test_oa <- filter(o, region %in% c("коми", "белгородская область", "воронежская область", "калужская область", "костромская область",
                                   "курганская область", "магаданская область", "новосибирская область", "рязанская область",
                                   "челябинская область", "ямало-ненецкий ао"))
test_oa <- filter(o, region %in% c("ямало-ненецкий ао"))
test_oa <- test_oa[which(!is.na(test_oa[,10])),]
test_oa <- clean_up(test_oa)
test_oa$raion <- NA
sum(is.na(test_oa[,10]))

debug(clean_up)
unique(o[,9])
remDr$open()

### intermediate function 
intermediate <- function(nodes, sf, i, table) {
        text <- html_text(nodes[i])  
        if (!grepl("\\(до", text)) {
                rai <- find_real_name(text)
                table[grep(rai, table[,10]),11] <- rai
                path <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i), "]", sep = "")
                names_to_test <- collect_names_of_settlements(i = i, sf = sf, path = path)
                sol <- check_for_settlements_names(names_to_test, rai)
                table[which((is.na(table[,11])) & grepl(sol, table[,10])),11] <- rai ### is it going to work?
        }
        table
}

### conclusion
last_part <- function(sf, table, nodes) {
        path <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(length(nodes) - 1), "]", sep = "")
        click(sf, path)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes_city <- html_nodes(doc, xpath = paste("/html/body/form/table/tbody/tr[2]/td/div/div[",
                                                    as.character(length(nodes)),"]/div"))
        cities <- html_text(nodes_city[2:length(nodes_city)])
        if (grepl("\"город ", cities[1])) {
                cities <- is_it_so_hard_to_establish_universal_standards(cities)
        }
        for (j in cities) {
                table[grep(j, table[,10]), 11] <- j
        }
        table
}

### another big_func

big_func <- function(table, sf) {
        # sf$navigate(link)
        Sys.sleep(3)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div")
        if (grepl("районы", html_text(nodes[length(nodes)]))) {
                nodes <- nodes[-length(nodes)]
        }
        if (grepl("районы", html_text(nodes[1]))) {
                for (i in 2:(length(nodes) - 2)) {
                        if (is.even(i)) {
                               table <- intermediate(nodes = nodes, sf = sf, i = i, table = table) 
                        }
                }
        } else {
                for (i in seq_len(length(nodes) - 2)) {
                        if(!is.even(i)) {
                                table <- intermediate(nodes = nodes, sf = sf, i = i, table = table)
                        }
                }
        }
        table <- last_part(sf = sf, table = table, nodes = nodes)
        table
}

###
is_it_so_hard_to_establish_universal_standards <- function(cities) {
        locations <- str_locate(cities, "\"город ")
        for (i in seq_len(length(cities))) {
                cities[i] <- str_sub(cities[i], start = (locations[i,2] + 1), end = (str_length(cities[i]) - 1))
        }
        cities
}

### 

big_func <- function(table, sf) {
        # sf$navigate(link)
        Sys.sleep(3)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div")
        if (grepl("районы", html_text(nodes[length(nodes)]))) {
                nodes <- nodes[-length(nodes)]
        }
        if (grepl("муниципальные", tolower(html_text(nodes[1])))) {
                stop_pls <- grep("муниципальные", tolower(html_text(nodes)))
                if (is.even(stop_pls[length(stop_pls)] + 1)) {
                        for (i in (stop_pls[length(stop_pls)] + 1):(length(nodes) - 2)) {
                                if (is.even(i)) {
                                        table <- intermediate(nodes = nodes, sf = sf, i = i, table = table) 
                                }   
                        }
                } else {
                        for (i in (stop_pls[length(stop_pls)] + 1):(length(nodes) - 2)) {
                                if (!is.even(i)) {
                                        table <- intermediate(nodes = nodes, sf = sf, i = i, table = table)
                                }
                        }
                }
        } else {
                for (i in seq_len(length(nodes) - 2)) {
                        if(!is.even(i)) {
                                table <- intermediate(nodes = nodes, sf = sf, i = i, table = table)
                        }
                }
        }
        table <- last_part(sf = sf, table = table, nodes = nodes)
        table
}


### find_real_name
find_real_name <- function(text) {
        mr_coordinates <- as.vector(str_locate(tolower(text), "муниципальный район"))
        if (grepl(" и ", text)) {
                i_coordinates <- as.vector(str_locate(text, " и "))
                gor_coordinates <- as.vector(str_locate(tolower(text), "город"))
                if (gor_coordinates[1] < i_coordinates[1]) {
                        string <- str_sub(text, start = i_coordinates[2] + 1)
                        rai_coordinates <- as.vector(str_locate(string, "район"))
                        if (rai_coordinates[2] == str_length(string)) {
                                white_space_coord <- str_locate(string, " ")
                                name <- str_sub(string, end = as.vector(white_space_coord[1] - 1))
                        } else {
                                white_space_coord <- str_locate_all(string, " ")
                                name <- str_sub(string, start = (white_space_coord[length(white_space_coord[,1]),][2] + 1))
                        }
                } else {
                        string <- str_sub(text, end = i_coordinates[1] - 1)
                        rai_coordinates <- as.vector(str_locate(string, "район"))
                        if (rai_coordinates[2] == str_length(string)) {
                                white_space_coord <- str_locate(string, " ")
                                name <- str_sub(string, end = as.vector(white_space_coord[1] - 1))
                        } else {
                                white_space_coord <- str_locate_all(string, " ")
                                name <- str_sub(string, start = (white_space_coord[length(white_space_coord[,1]),][2] + 1))
                        }
                }
        } else {
                if (mr_coordinates[1] == 1) {
                        name <- str_sub(text, start = mr_coordinates[2] + 2, end = str_length(text))
                } else {
                        name <- str_sub(text, start = 1, end = mr_coordinates[1] - 2)
                }
        }
        name
}

### 

big_func <- function(table, sf) {
        # sf$navigate(link)
        Sys.sleep(3)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div")
        if (grepl("районы", html_text(nodes[length(nodes)]))) {
                nodes <- nodes[-length(nodes)]
        }
        if ((grepl("муниципальные", tolower(html_text(nodes[1]))))|(grepl("область", html_text(nodes[1])))) {
                stop_pls <- grep("муниципальные", tolower(html_text(nodes)))
                num <- stop_pls[length(stop_pls)] + 1
                if (grepl("область", html_text(nodes[1]))) {
                        num <- num + 1
                }
                for (i in stop_pls) {
                        if (grepl("onclick", html_attrs(nodes[i]))) {
                                num <- num + 1
                        }
                }
                if (is.even(num)) {
                        for (i in num:(length(nodes) - 2)) {
                                if (is.even(i)) {
                                        table <- intermediate(nodes = nodes, sf = sf, i = i, table = table) 
                                }   
                        }
                } else {
                        for (i in num:(length(nodes) - 2)) {
                                if (!is.even(i)) {
                                        table <- intermediate(nodes = nodes, sf = sf, i = i, table = table)
                                }
                        }
                }
        } else {
                for (i in seq_len(length(nodes) - 2)) {
                        if(!is.even(i)) {
                                table <- intermediate(nodes = nodes, sf = sf, i = i, table = table)
                        }
                }
        }
        table <- last_part(sf = sf, table = table, nodes = nodes)
        table
}


### conclusion
last_part <- function(sf, table, nodes) {
        path <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(length(nodes) - 1), "]", sep = "")
        click(sf, path)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes_city <- html_nodes(doc, xpath = paste("/html/body/form/table/tbody/tr[2]/td/div/div[",
                                                    as.character(length(nodes)),"]/div"))
        cities <- html_text(nodes_city[2:length(nodes_city)])
        if (grepl("\"город ", cities[1])) {
                cities <- is_it_so_hard_to_establish_universal_standards(cities)
        }
        those_with_gorod <- grep("Город", cities)
        for (i in those_with_gorod) {
                temp <- cities[i]
                for (j in seq_len(length(table[,1]))) {
                        if (grepl(temp,table[j,10])) {
                                ans <- TRUE
                                break
                        } else {
                                ans <- FALSE
                        }
                }
                if (!ans) {
                        cities[i] <- sub("Город", "город", cities[i])
                }
        }
        for (j in cities) {
                table[grep(j, table[,10]), 11] <- j
        }
        table
}


### New way of discovering buttons

big_func <- function(table, sf) {
        # sf$navigate(link)
        Sys.sleep(3)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div")
        if (length(html_attrs(nodes[length(nodes)])[[1]]) == 0) {
                nodes <- nodes[-length(nodes)]
        }
        if (grepl("муниципальные", tolower(html_text(nodes[1])))|(length(html_attrs(nodes[1])[[1]]) == 0)) {
                num <- 1
                stop_pls <- grep("муниципальные", tolower(html_text(nodes)))
                if (length(stop_pls != 0)) {
                        num <- stop_pls[length(stop_pls)] + 1    
                }
                for (i in stop_pls) {
                        if (grepl("onclick", html_attrs(nodes[i]))) {
                                num <- num + 1
                        }
                }
                for (i in num:length(nodes)) {
                        if (length(html_attrs(nodes[i])[[1]]) == 0) {
                                num <- num + 1
                        }
                }
                if (is.even(num)) {
                        for (i in num:(length(nodes) - 2)) {
                                if (is.even(i)) {
                                        table <- intermediate(nodes = nodes, sf = sf, i = i, table = table) 
                                }   
                        }
                } else {
                        for (i in num:(length(nodes) - 2)) {
                                if (!is.even(i)) {
                                        table <- intermediate(nodes = nodes, sf = sf, i = i, table = table)
                                }
                        }
                }
        } else {
                for (i in seq_len(length(nodes) - 2)) {
                        if(!is.even(i)) {
                                table <- intermediate(nodes = nodes, sf = sf, i = i, table = table)
                        }
                }
        }
        table <- last_part(sf = sf, table = table, nodes = nodes)
        table
}


### conclusion
last_part <- function(sf, table, nodes) {
        path <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(length(nodes) - 1), "]", sep = "")
        click(sf, path)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes_city <- html_nodes(doc, xpath = paste("/html/body/form/table/tbody/tr[2]/td/div/div[",
                                                    as.character(length(nodes)),"]/div"))
        cities <- html_text(nodes_city[2:length(nodes_city)])
        if (grepl("\"город ", cities[1])) {
                cities <- is_it_so_hard_to_establish_universal_standards(cities)
        }
        those_with_gorod <- grep("Город", cities)
        for (i in those_with_gorod) {
                temp <- cities[i]
                for (j in seq_len(length(table[,1]))) {
                        if (grepl(temp,table[j,10])) {
                                ans <- TRUE
                                break
                        } else {
                                ans <- FALSE
                        }
                }
                if (!ans) {
                        cities[i] <- sub("Город", "город", cities[i])
                }
        }
        for (j in cities) {
                if (grepl("Бердск", j)) {
                        browser()
                }
                if (grepl("городской округ", j)) {
                        if (sum(grepl(j, table[,10])) >= 1) {
                                table[grep(j,table[,10]),11] <- j
                        }
                        loc <- as.vector(str_locate(j, " городской округ"))
                        gor_okrug <- str_sub(j, end = loc[1] - 1)
                        final <- c()
                        for (u in 2:str_length(gor_okrug)) {
                                gorname <- str_sub(gor_okrug, start = 1, end = u)
                                matches <- grep(paste("город", gorname, sep = " "), table[,10])
                                if (length(matches) != 0) {
                                        final <- matches
                                } else {
                                        next
                                }
                        }
                        if (length(final) != 0) {
                                final <- final[is.na(table[final,11])]
                                table[final, 11] <- gor_okrug
                        }
                } else {
                        table[grep(j, table[,10]), 11] <- j
                }
        }
        table
}


### check-check
sample_test <- results[sample(length(results[,1]), size = 100),]

### what_region
what_region <- function(table, sf) {
        reg_names <- unique(table[,9])
        for (i in reg_names) {
                sf$navigate("https://rosstat.gov.ru/dbscripts/munst/")
                res <- sf$getPageSource()
                doc <- read_html(res[[1]])
                if (grepl("ямало", i)|grepl("ханты", i)|grepl("тюмен", i)) {
                        link_node <- html_nodes(doc, xpath = "/html/body/table[2]/tbody/tr[2]/td/p[8]/a[3]")
                        link <- html_attr(link_node, name = "href")
                        passport_click(link = link, sf = sf)
                        table[which(table[,9] == i),] <- outstanding_case(table = table[which(table[,9] == i),], sf = sf, name = i)
                        # special case of tumen'
                } else {
                        for (j in 3:11) {
                                nodes <- html_nodes(doc, xpath = paste("/html/body/table[2]/tbody/tr[2]/td/p[",
                                                                       as.character(j), "]/a", sep = ""))
                                site_names <- html_text(nodes)
                                if (sum(grepl(i, site_names, ignore.case = TRUE)) >= 1) {
                                        num <- grep(i, site_names, ignore.case = TRUE)
                                        link_node <- html_nodes(doc, xpath = paste("/html/body/table[2]/tbody/tr[2]/td/p[",
                                                                                   as.character(j), "]/a[", as.character(num), "]", sep = ""))
                                        link <- html_attr(link_node, name = "href")
                                        passport_click(link = link, sf = sf)
                                        table[which(table[,9] == i),] <- big_func(table = table[which(table[,9] == i),], sf = sf)
                                }
                        }  
                }
        }
        table
}

###
outstanding_case <- function (table, sf, name) {
        Sys.sleep(3)
        click(sf = sf, path = "/html/body/form/table/tbody/tr[2]/td/div/div[1]")
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div")
        tx <- html_text(nodes[2:9])
        if (grepl("тюмен", name)) {
                click(sf = sf, path = "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[2]")
                res <- sf$getPageSource()
                doc <- read_html(res[[1]])
                nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[3]/div")
                base <- "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[3]/div["
                for (i in 2:43) {
                        if(is.even(i)) {
                                table <- sc_intermediate(nodes, sf, i, table, base)  
                        }
                }
                x <- 4
                base <- "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div["
                table <- sc_last_part(sf = sf, table = table, x = x, base = base)
        }
        if (grepl("ханты", name)) {
                click(sf = sf, path = "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[6]")
                click(sf = sf, path = "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[7]/div[2]")
                res <- sf$getPageSource()
                doc <- read_html(res[[1]])
                nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[7]/div[3]/div")
                base <- "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[7]/div[3]/div["
                for (i in 2:19) {
                        if(is.even) {
                                table <- sc_intermediate(nodes, sf, i, table, base)
                        }
                }
                x <- 4
                base <- "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[7]/div["
                table <- sc_last_part(sf = sf, table = table, x = x, base = base)
        }
        if (grepl("ямало", name)) {
                click(sf = sf, path = "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[8]")
                click(sf = sf, path = "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[9]/div[2]")
                res <- sf$getPageSource()
                doc <- read_html(res[[1]])
                nodes <- html_nodes(doc, xpath = "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[9]/div[3]/div")
                base <- "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[9]/div[3]/div["
                for (i in 2:15) {
                        if(is.even(i)) {
                                table <- sc_intermediate(nodes, sf, i, table, base)
                        }
                }
                x <- 4
                base <- "/html/body/form/table/tbody/tr[2]/td/div/div[2]/div[9]/div["
                table <- sc_last_part(sf = sf, table = table, x = x, base = base)
        }
        table
}


###
sc_intermediate <- function(nodes, sf, i, table, base) {
        text <- html_text(nodes[i])
        rai <- find_real_name(text)
        table[grep(rai, table[,10]),11] <- rai
        path <- paste(base, as.character(i), "]", sep = "")
        names_to_test <- sc_collect_names_of_settlements(i = i, sf = sf, path = path, base = base)
        sol <- check_for_settlements_names(names_to_test, rai)
        table[which((is.na(table[,11])) & grepl(sol, table[,10])),11] <- rai ### is it going to work?
        table
}
###
sc_collect_names_of_settlements <- function(i = i, sf = sf, path = path, base = base) {
        click(sf, path = path)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = paste(base, as.character(i + 1), "]/div", sep = ""))
        names_to_test <- c()
        if (sum(grepl("onclick", html_attrs(nodes))) >= 1) {
                for (j in grep("onclick", html_attrs(nodes))) {
                        path1 <- paste(base, as.character(i + 1), "]/div[", as.character(j), "]", sep = "")
                        click(sf = sf, path = path1)
                        path2 <- paste(base, as.character(i + 1), "]/div[", as.character(j + 1), "]/div/a", sep = "")
                        n <- gyabt(sf = sf, path2)
                        names_to_test <- c(names_to_test, n)
                        click(sf = sf, path = path1)
                }
        } else {
                path2 <- paste(base, as.character(i + 1), "]/div/a", sep = "")
                n <- gyabt(sf = sf, path = path2)
        }
        names_to_test <- c(names_to_test, n)
        click(i, path = path)
        names_to_test
}

###
sc_last_part <- function(sf, table, base, x) {
        click(sf, path = paste(base, as.character(x), "]"))
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes_city <- html_nodes(doc, xpath = paste(base, as.character(x + 1), "]/div"))
        cities <- html_text(nodes_city[2:length(nodes_city)])
        if (grepl("\"город ", cities[1])) {
                cities <- is_it_so_hard_to_establish_universal_standards(cities)
        }
        those_with_gorod <- grep("Город", cities)
        for (i in those_with_gorod) {
                temp <- cities[i]
                for (j in seq_len(length(table[,1]))) {
                        if (grepl(temp,table[j,10])) {
                                ans <- TRUE
                                break
                        } else {
                                ans <- FALSE
                        }
                }
                if (!ans) {
                        cities[i] <- sub("Город", "город", cities[i])
                }
        }
        for (j in cities) {
                if (grepl("Бердск", j)) {
                        browser()
                }
                if (grepl("городской округ", j)) {
                        if (sum(grepl(j, table[,10])) >= 1) {
                                table[grep(j,table[,10]),11] <- j
                        }
                        loc <- as.vector(str_locate(j, " городской округ"))
                        gor_okrug <- str_sub(j, end = loc[1] - 1)
                        final <- c()
                        for (u in 2:str_length(gor_okrug)) {
                                gorname <- str_sub(gor_okrug, start = 1, end = u)
                                matches <- grep(paste("город", gorname, sep = " "), table[,10])
                                if (length(matches) != 0) {
                                        final <- matches
                                } else {
                                        next
                                }
                        }
                        if (length(final) != 0) {
                                final <- final[is.na(table[final,11])]
                                table[final, 11] <- gor_okrug
                        }
                } else {
                        table[grep(j, table[,10]), 11] <- j
                }
        }
        table
}
        


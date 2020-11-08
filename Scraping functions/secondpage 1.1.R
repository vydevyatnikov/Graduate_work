secondpage <- function (link, sf) {
        sf$navigate(link)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes2 <- html_nodes(doc, xpath = "/html/body/table[3]/tbody/tr[4]/td/table[4]/tbody/tr/td[2]/div/table/tbody/tr[1]/td/nobr/a")
        nodes3 <- html_nodes(doc, xpath = "/html/body/table[3]/tbody/tr[4]/td/table[5]/tbody/tr/td[2]/div/table/tbody/tr[1]/td/nobr/a")
        if ((length(nodes2) == 0)&(length(nodes3) == 0)) {
                while (length(nodes2) == 0){
                        Sys.sleep(as.integer(runif(n = 1, min = 5, max = 10)))
                        res <- sf$getPageSource()
                        doc <- read_html(res[[1]])
                        nodes2 <- html_nodes(doc, xpath = "/html/body/table[3]/tbody/tr[4]/td/table[4]/tbody/tr/td[2]/div/table/tbody/tr[1]/td/nobr/a")
                        nodes3 <- html_nodes(doc, xpath = "/html/body/table[3]/tbody/tr[4]/td/table[5]/tbody/tr/td[2]/div/table/tbody/tr[1]/td/nobr/a")
                }
                if (length(nodes2) != 0){
                        num <- 4
                        atrb <- html_attr(nodes2, name = "href")
                } else {
                        num <- 5
                        atrb <- html_attr(nodes3, name = "href")
                }
                counter <- 0
                for (i in atrb) {
                        counter <- counter + 1
                        if (counter == 1) {
                                table <- thirdpage(link = i, sf = sf, num = num)
                                finalDataTable <- table
                        } else {
                                table <- thirdpage(link = i, sf= sf, num = num)
                                finalDataTable <- rbind(finalDataTable, table)
                        }
                }
        } else {
                if (length(nodes2) != 0){
                        num <- 4
                        atrb <- html_attr(nodes2, name = "href")
                } else {
                        num <- 5
                        atrb <- html_attr(nodes3, name = "href")
                }
                counter <- 0
                for (i in atrb) {
                        counter <- counter + 1
                        if (counter == 1) {
                                table <- thirdpage(link = i, sf = sf, num = num)
                                finalDataTable <- table
                        } else {
                                table <- thirdpage(link = i, sf = sf, num = num)
                                finalDataTable <- rbind(finalDataTable, table)
                        }
                }
        }
        finalDataTable <- add_region_name(table = finalDataTable, doc = doc)
        finalDataTable
}
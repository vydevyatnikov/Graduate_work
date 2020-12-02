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
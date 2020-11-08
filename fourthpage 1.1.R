fourthpage <- function(link, sf, num){
        Sys.sleep(3)
        sf$navigate(link)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes3 <- html_nodes(doc, xpath = paste("/html/body/table[3]/tbody/tr[4]/td/table[", as.character(num), "]/tbody/tr/td[2]/div/table/tbody/tr/td/nobr/b", sep = ""))
        nodes3_alt <- html_nodes(doc, xpath = paste("/html/body/table[3]/tr[4]/td/table[", as.character(num), "]/tr/td[2]/div/table/tr/td/nobr/b", sep = ""))
        if ((length(nodes3) == 0)&(length(nodes3_alt) == 0)) {
                while ((length(nodes3) == 0)&(length(nodes3_alt) == 0)) {
                        Sys.sleep(as.integer(runif(n = 1, min = 5, max = 10)))
                        res <- sf$getPageSource()
                        doc <- read_html(res[[1]])
                        nodes3 <- html_nodes(doc, xpath = paste("/html/body/table[3]/tbody/tr[4]/td/table[", as.character(num), "]/tbody/tr/td[2]/div/table/tbody/tr/td/nobr/b", sep = ""))
                        nodes3_alt <- html_nodes(doc, xpath = paste("/html/body/table[3]/tr[4]/td/table[", as.character(num), "]/tr/td[2]/div/table/tr/td/nobr/b", sep = ""))
                }
        }
        if (length(nodes3) != 0) {
                rn_nodes <- html_nodes(doc, xpath = paste("/html/body/table[3]/tbody/tr[4]/td/table[", as.character(num), "]/tbody/tr/td[2]/div/table/tbody/tr[1]/td/nobr/a", sep = ""))
                cn_nodes <- html_nodes(doc, xpath = paste("/html/body/table[3]/tbody/tr[4]/td/table[", as.character(num), "]/tbody/tr/td[1]/table/tbody/tr/td[2]/nobr", sep = ""))
                data_body <- html_text(nodes3)
                row_names <- html_text(rn_nodes)
                col_names <- html_text(cn_nodes)
                col_names <- col_names[-(which(nchar(col_names) == 1))][-1]
                table <- choose_needed(data = data_body, rows = row_names, columns <- col_names)
                table
        } else {
                rn_nodes <- html_nodes(doc, xpath = paste("/html/body/table[3]/tr[4]/td/table[", as.character(num), "]/tr/td[2]/div/table/tr[1]/td/nobr/b", sep = ""))
                cn_nodes <- html_nodes(doc, xpath = paste("/html/body/table[3]/tr[4]/td/table[", as.character(num), "]/tr/td[1]/table/tr/td[2]/nobr", sep = ""))
                data_body <- html_text(nodes3_alt)
                row_names <- html_text(rn_nodes)
                col_names <- html_text(cn_nodes)
                col_names <- col_names[-(which(nchar(col_names) == 1))][-1]
                table <- choose_needed(data = data_body, rows = row_names, columns <- col_names)
                table
        }
        
}
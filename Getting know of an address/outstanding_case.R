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
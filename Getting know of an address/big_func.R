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
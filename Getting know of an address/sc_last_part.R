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
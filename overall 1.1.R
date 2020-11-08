overall <- function (link, remDr, parties) {
        overall_table <- data.frame()
        z <- 0
        for (i in link) {
                z <- z + 1
                table <- firstpage(link = i, sf = remDr)
                table <- change_names(table = table, parties = parties)
                if (z == 1) {
                        overall_table <- rbind(overall_table, table)
                } else {
                        overall_table <- organize(overall_table, table)
                }
        }
        overall_table
}
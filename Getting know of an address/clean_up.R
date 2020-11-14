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
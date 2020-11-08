change_names <- function (table, parties) {
        changed <- c()
        colnames(table) <- tolower(colnames(table))
        for (i in seq_len(length(colnames(table))-2)) {
                table[,i] <- as.numeric(table[,i])
        }
        for (i in parties) {
                if (sum(grepl(i, colnames(table))) >= 1) {
                        colnames(table)[grepl(i, colnames(table))] <- i
                        changed <- c(changed, which(grepl(i, colnames(table)))) # Нужно проверить тему с changed
                }
        }
        changed <- c(1, 2, changed, length(colnames(table)) - 1, length(colnames(table)))
        unchanged <- table[,-changed]
        others <- rowSums(as.data.frame(unchanged))
        table <- table[,changed]
        table$другие <- others
        table <- table[,c(seq_len(length(colnames(table))-3), length(colnames(table)), length(colnames(table))-2, length(colnames(table))-1)]
        if (sum(grepl("\\.", colnames(table))) >= 1) {
                x <- which(grepl("\\.", colnames(table), perl = TRUE))
                table <- table[,-x]
        }
        table
}
choose_needed <- function (data, rows, columns) {
        columns <- tolower(columns)
        if (length(rows) == length(data)/length(columns)) {
                table <- matrix(data = data, nrow = length(rows), ncol = length(columns))
                table <- as.data.frame(table)
                colnames(table) <- columns
                needed_columns <- c(which(grepl("список", columns)|grepl("число действительных", columns)|grepl("списки", columns)),
                                    (which(grepl("не учтенных", columns))+1):length(columns))
                table <- table[,needed_columns]
                table$UIK <- rows
                colnames(table)[which(grepl("список", colnames(table))|grepl("списки", colnames(table)))] <- "Число избирателей, внесенных в списки на момент окончания голосования"
                colnames(table)[which(grepl("число действительных", colnames(table)))] <- "Число действительных бюллетеней"
                table
        } else {
                stop("Something wrong with number of rows/columns")
        }
}
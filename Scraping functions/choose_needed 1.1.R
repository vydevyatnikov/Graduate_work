choose_needed <- function (data, rows, columns) {
        columns <- tolower(columns)
        if (length(rows) == length(data)/length(columns)) {
                table <- matrix(data = data, nrow = length(rows), ncol = length(columns))
                table <- as.data.frame(table)
                colnames(table) <- columns
                needed_columns <- c(unique(c(grep("список", columns), grep("число действительных", columns), grep("списки", columns))),
                                    (c(grep("не учтенных", columns), grep("неучтенных", columns), grep("не учтённых", columns))[length(c(grep("не учтенных", columns), grep("неучтенных", columns), grep("не учтённых", columns)))]+1):length(columns))
                table <- table[,needed_columns]
                table$UIK <- rows
                colnames(table)[c(grep("список", colnames(table)), grep("списки", colnames(table)))] <- "Число избирателей, внесенных в списки на момент окончания голосования"
                colnames(table)[grep("число действительных", colnames(table))] <- "Число действительных бюллетеней"
                table
        } else {
                stop("Something wrong with number of rows/columns")
        }
}
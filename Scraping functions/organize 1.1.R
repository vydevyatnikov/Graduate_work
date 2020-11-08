organize <- function (table_one, table_two) {
        absent_one <- colnames(table_two[!(colnames(table_two) %in% colnames(table_one))])
        for (i in absent_one) {
                vec <- rep(NA, length(table_one[,1]))
                table_one <- cbind(table_one, as.data.frame(vec))
                colnames(table_one)[length(table_one[1,])] <- i
        }
        absent_two <- colnames(table_one[!(colnames(table_one) %in% colnames(table_two))])
        for (i in absent_two) {
                vec <- rep(NA, length(table_two[,1]))
                table_two <- cbind(table_two, as.data.frame(vec))
                colnames(table_two)[length(table_two[1,])] <- i
        }
        ft <- rbind(table_one, table_two)
        ft
}
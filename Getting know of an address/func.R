func <- function(x, data) {
        data$region <- tolower(data$region)
        x$address <- NA
        x <- x[!grepl("Цифровые", x[,8]),]
        for (i in seq_len(length(x[,1]))) {
                x[i,8] <- sub("УИК ", "", x[i,8])
        }
        for (i in seq_len(length(x[,1]))) {
                tempr <- filter(data, region == x[i,9])
                tempr <- filter(tempr, name == x[i,8])
                if (length(tempr$region) != 0) {
                        if ((x[i,9] == tempr$region)&(x[i,8] == tempr$name)) {
                                x[i, 10] <- tempr$address
                        }
                } else {
                        next
                }
        }
        x
}
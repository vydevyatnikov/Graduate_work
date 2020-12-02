check_for_settlements_names <- function(text, rai) {
        sol <- c()
        for (j in seq_len(str_length(rai))) {
                exm <- str_sub(rai, start = 1, end = j)
                if (length(text[grep(exm, text)]) == 1) {
                        sol <- text[grep(exm, text)]
                }
        }
        if (length(sol) == 0) {
                rai
        } else {
                sol
        }
}
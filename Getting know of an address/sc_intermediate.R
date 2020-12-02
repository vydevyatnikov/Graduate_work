sc_intermediate <- function(nodes, sf, i, table, base) {
        text <- html_text(nodes[i])
        rai <- find_real_name(text)
        table[grep(rai, table[,10]),11] <- rai
        path <- paste(base, as.character(i), "]", sep = "")
        names_to_test <- sc_collect_names_of_settlements(i = i, sf = sf, path = path, base = base)
        sol <- check_for_settlements_names(names_to_test, rai)
        table[which((is.na(table[,11])) & grepl(sol, table[,10])),11] <- rai ### is it going to work?
        table
}
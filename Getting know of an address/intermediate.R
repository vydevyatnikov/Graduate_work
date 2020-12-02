intermediate <- function(nodes, sf, i, table) {
        text <- html_text(nodes[i])  
        if (!grepl("\\(до", text)) {
                rai <- find_real_name(text)
                table[grep(rai, table[,10]),11] <- rai
                path <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i), "]", sep = "")
                names_to_test <- collect_names_of_settlements(i = i, sf = sf, path = path)
                sol <- check_for_settlements_names(names_to_test, rai)
                table[which((is.na(table[,11])) & grepl(sol, table[,10])),11] <- rai ### is it going to work?
        }
        table
}
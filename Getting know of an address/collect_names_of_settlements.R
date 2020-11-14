collect_names_of_settlements <- function(i, sf, path) {
        click(sf, path = path)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = paste("/html/body/form/table/tbody/tr[2]/td/div/div[",
                                               as.character(i + 1), "]/div", sep = ""))
        names_to_test <- c()
        if (sum(grepl("onclick", html_attrs(nodes))) >= 1) {
                for (j in grep("onclick", html_attrs(nodes))) {
                        path1 <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[",
                                       as.character(i + 1), "]/div[", as.character(j), "]", sep = "")
                        click(sf = sf, path = path1)
                        path2 <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i + 1),"]/div[",
                                       as.character(j + 1), "]/div/a", sep = "")
                        n <- gyabt(sf = sf, path2)
                        names_to_test <- c(names_to_test, n)
                        click(sf = sf, path = path1)
                }
        } else {
                path2 <- paste("/html/body/form/table/tbody/tr[2]/td/div/div[", as.character(i + 1),"]/div/a", sep = "")
                n <- gyabt(sf = sf, path = path2)
        }
        names_to_test <- c(names_to_test, n)
        click(i, path = path)
        names_to_test
}
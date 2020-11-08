firstpage <- function(link, sf) {
        sf$navigate(link)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        num <- html_nodes(doc, xpath = "/html/body/table[3]/tbody/tr[3]/td/table[3]/tbody/tr")
        nodes1 <- html_nodes(doc, xpath = paste("/html/body/table[3]/tbody/tr[3]/td/table[3]/tbody/tr[", as.character(length(num)),
                                                "]/td/nobr/a", sep = ""))
        if (length(nodes1) == 0) {
                while (length(nodes1) == 0){
                        Sys.sleep(as.integer(runif(n = 1, min = 5, max = 10)))
                        res <- sf$getPageSource()
                        doc <- read_html(res[[1]])
                        num <- html_nodes(doc, xpath = "/html/body/table[3]/tbody/tr[3]/td/table[3]/tbody/tr")
                        nodes1 <- html_nodes(doc, xpath = paste("/html/body/table[3]/tbody/tr[3]/td/table[3]/tbody/tr[", as.character(length(num)),
                                                                "]/td/nobr/a", sep = ""))
                }
                Sys.sleep(as.integer(runif(n = 1, min = 5, max = 10)))
                table <- secondpage(link = html_attr(nodes1, name = "href"), sf=sf)
                table
        } else {
                Sys.sleep(as.integer(runif(n = 1, min = 5, max = 10)))
                table <- secondpage(link = html_attr(nodes1, name = "href"), sf = sf)
                table
        }
}
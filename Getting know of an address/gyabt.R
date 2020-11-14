gyabt <- function(sf, path) {
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        nodes <- html_nodes(doc, xpath = path)
        text <- html_text(nodes)
        if (length(grep("поселени", text)) != 0) {
                text <- text[-grep("поселени", text)]
        }
        if (length(grep("район", text)) != 0) {
                text <- text[-grep("район", text)]   
        }
        if (length(grep("Город ", text)) != 0) {
                text[grep("Город ", text)] <- str_sub(text[grep("Город ", text)], start = as.vector(str_locate(text[grep("Город ", text)][1], "Город"))[2] + 2)
        }
        if (length(grep("Поселок ", text)) != 0) {
                text[grep("Поселок ", text)] <- str_sub(text[grep("Поселок ", text)], start = as.vector(str_locate(text[grep("Поселок ", text)][1], "Поселок"))[2] + 2)
        }
        text
}
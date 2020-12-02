passport_click <- function(link, sf) {
        sf$navigate(link)
        res <- sf$getPageSource()
        doc <- read_html(res[[1]])
        element <- remDr$findElement(using = "xpath", "/html/body/div/table/tbody/tr[2]/td/table/tbody/tr/td[6]")
        remDr$mouseMoveToLocation(webElement = element)
        remDr$click()
}
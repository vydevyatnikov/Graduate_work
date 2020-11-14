click <- function(sf, path) {
        element <- remDr$findElement(using = "xpath", path)
        remDr$mouseMoveToLocation(webElement = element)
        remDr$click()
}
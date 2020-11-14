is_it_so_hard_to_establish_universal_standards <- function(cities) {
        locations <- str_locate(cities, "\"город ")
        for (i in seq_len(length(cities))) {
                cities[i] <- str_sub(cities[i], start = (locations[i,2] + 1), end = (str_length(cities[i]) - 1))
        }
        cities
}
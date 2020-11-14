find_real_name <- function(text) {
        mr_coordinates <- as.vector(str_locate(tolower(text), "муниципальный район"))
        if (grepl(" и ", text)) {
                i_coordinates <- as.vector(str_locate(text, " и "))
                gor_coordinates <- as.vector(str_locate(tolower(text), "город"))
                if (gor_coordinates[1] < i_coordinates[1]) {
                        string <- str_sub(text, start = i_coordinates[2] + 1)
                        rai_coordinates <- as.vector(str_locate(string, "район"))
                        if (rai_coordinates[2] == str_length(string)) {
                                white_space_coord <- str_locate(string, " ")
                                name <- str_sub(string, end = as.vector(white_space_coord[1] - 1))
                        } else {
                                white_space_coord <- str_locate_all(string, " ")
                                name <- str_sub(string, start = (white_space_coord[length(white_space_coord[,1]),][2] + 1))
                        }
                } else {
                        string <- str_sub(text, end = i_coordinates[1] - 1)
                        rai_coordinates <- as.vector(str_locate(string, "район"))
                        if (rai_coordinates[2] == str_length(string)) {
                                white_space_coord <- str_locate(string, " ")
                                name <- str_sub(string, end = as.vector(white_space_coord[1] - 1))
                        } else {
                                white_space_coord <- str_locate_all(string, " ")
                                name <- str_sub(string, start = (white_space_coord[length(white_space_coord[,1]),][2] + 1))
                        }
                }
        } else {
                if (mr_coordinates[1] == 1) {
                        name <- str_sub(text, start = mr_coordinates[2] + 2, end = str_length(text))
                } else {
                        name <- str_sub(text, start = 1, end = mr_coordinates[1] - 2)
                }
        }
        name
}
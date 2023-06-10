bridge_gtd <- function(prep_data, data) {
  skip <- ncol(prep_data) - 1
  for (j in 2:ncol(prep_data)) {
    for (ii in 1:nrow(data)) {
      if (month(data$Month[ii]) == 1 |
          month(data$Month[ii]) == 4 |
          month(data$Month[ii]) == 7 |
          month(data$Month[ii]) == 10) {
        data[ii, j + skip] = data[ii, j]
      } else if (month(data$Month[ii]) == 2 |
                 month(data$Month[ii]) == 5 |
                 month(data$Month[ii]) == 8 |
                 month(data$Month[ii]) == 11) {
        data[ii, j + skip] = (data[ii, j] + data[ii - 1, j]) / 2
      } else{
        data[ii, j + skip] = (data[ii, j] + data[ii - 1, j] + data[ii - 2, j]) / 3
      }
    }
  }
  data <- data %>%
    select(c(Month, ends_with("_b")))
}
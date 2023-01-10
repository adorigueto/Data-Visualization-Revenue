# Generate random income figures
#random_figures <- data.frame(sample(1000:20000000, 260))
#random_figures
norm_random_figures <- data.frame(rnorm(60, 400000, 20000))
norm_random_figures

# Generate random income dates
## 1st Option
date <- sample(seq.Date(as.Date('2022-01-01'), as.Date('2023-12-31'), by="day"), 260)
paste(sort(date))

## 2nd Option
rdate <- function(n,
                  min = paste0(format(Sys.Date(), '%Y'), '-01-01'),
                  max = paste0(format(Sys.Date(), '%Y'), '-12-31'),
                  sort = TRUE) {
  
  dates <- sample(seq(as.Date(min), as.Date(max), by = "day"), n, replace = TRUE)
  if (sort == TRUE) {
    sort(dates)
  } else {
    dates
  }
  
}

dates <- data.frame(rdate(260, '2022-01-01', '2023-12-31'))
paste(dates)

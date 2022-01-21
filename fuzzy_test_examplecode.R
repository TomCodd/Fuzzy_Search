a <- data.frame(name = c('Ace Co', 'Bayes', 'asd', 'Bcy', 'Baes', 'Bays'),
                price = c(10, 13, 2, 1, 15, 1))
b <- data.frame(name = c('Ace Co.', 'Bayes Inc.', 'asdf'),
                qty = c(9, 99, 10))

library(fuzzyjoin); library(dplyr);

start_time <- Sys.time()

stringdist_join(a, b, 
                by = "name",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") %>%
  group_by(name.x) %>%
  slice_min(order_by = dist, n = 3)

end_time <- Sys.time()
print(end_time)
end_time - start_time
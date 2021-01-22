############################################################
# Trying to automate and replace NA Boroughs for crossings 
#
# This code is trying to automate the replacement of the NA
# in the Borough column for the Crossings dataset with the 
# 'correct' Borough based on the st_length work and visual
# confirmation
# 
# 8/1/20
############################################################

#select small number of rows from crossings CID - THIS WORKS
# identify missing Borough details
a = f_crossings %>%
  filter(is.na(BOROUGH)) # 28 observations ie 28 crossings have no Borough
b = crossings_borough_NA_final # correct list
a = a %>%
  filter(FEATURE_ID == "RWG073152") # ? need to factor 8
b = b %>%
  filter(FEATURE_ID == "RWG073152")
a$BOROUGH[is.na(a$BOROUGH)] = 0 # replaces na in a with 0
a$BOROUGH <- b$BOROUGH # this then replaces the 0 with the borough from b

# Try to scale this up now
e = f_crossings %>%
  filter(is.na(BOROUGH)) # 28 observations ie 28 crossings have no Borough
e$BOROUGH[is.na(e$BOROUGH)] = 0 # replaces na in a with 0 
e$BOROUGH

# unfactor...
f = crossings_borough_NA_length
f$BOROUGH = as.character(f$BOROUGH)

# take e and drop borough
g = e %>%
  select(-"BOROUGH")
h = f %>%
  st_drop_geometry() %>%
  select(c("BOROUGH", "FEATURE_ID"))  #ACTUALLY DONT NEED TO UNFACTOR FOR THIS
i = left_join(g,h)  #' THIS WORKS

###################################
# WORKING AND SIMPLIFIED CODE IS: #
###################################
x = f_crossings %>%
  filter(is.na(BOROUGH)) %>% # select obs with no Boroughs
  select(-"BOROUGH")  # drop the borough column
y = crossings_borough_NA_length %>% 
  st_drop_geometry() %>%
  select(c("BOROUGH", "FEATURE_ID"))  # keep correct Borough with the feature id
z = left_join(x, y) # join using featureid and now has new column with correct borough
# Now need to add above to the correct_borough_NAs.R code file


# # notes from end friday 8/1/20 - look at
# # https://stackoverflow.com/questions/36433757/conditionally-replace-values-in-data-frame-from-separate-data-frame-in-r
# # and also this:
# # https://stackoverflow.com/questions/15310140/transfer-values-from-one-dataframe-to-another  BUT THIS ISNT CONDITIONAL
# 
# # another idea - replace contents of 3:5 columns in df with matched (location) 
# # contents of second data frmae then pull the data from another column in the 2nd df into the first
# 
# # We can select the columns of interest, loop through the columns with lapply, 
# # match with the 'location' column of 'counties' to get the numeric index of 
# #matches, then based on that get the corresponding 'fips' values, and update 
# # the dataset columns ([])
# df[3:5] <- lapply(df[3:5], function(x) counties$fips[match(x, counties$location)])
# # see https://stackoverflow.com/questions/55909850/map-values-of-a-data-frame-to-another-in-r-possibly-without-plyr?noredirect=1&lq=1
# 
# # or use a lookup table
# df <- data.frame(id = rep(c("one", "two", "three"), each = 10))
# lkp <- data.frame(id=c("one","two","three"), week.born=c(23,19,24))
# merge(df, lkp, by="id")
# # https://stackoverflow.com/questions/29807032/replace-values-in-a-data-frame-column-based-on-values-in-a-different-column?noredirect=1&lq=1
# # but would this work when I already have data in the Borough column...



# # attempts to run loops -COULDNT GET ANY OF THESE TO WORK - USED A JOIN INSTEAD (see above)
# # run loop to replace 0 in e with Boroughs from f
# for (i in seq_along(e$BOROUGH)) {
#   e$BOROUGH[[i]] = f$BOROUGH[[i]]
# } # output is that '0' borough names are replace with values but dont match the feature ID ? need to select feature id and match then change
# 
# # run loop to replace 0 in e with Boroughs from f
# for (i in seq_along(e$FEATURE_ID)) {
#   match(e$FEATURE_ID, f$FEATURE_ID)
#   e$BOROUGH[[i]] = f$BOROUGH[[i]]
# } # this also doesnt work, it doesnt match them 
# 
# if (e$FEATURE_ID == f$FEATURE_ID) {
#   for (i in seq_along(e$BOROUGH)) {
#     e$BOROUGH[[i]] = f$BOROUGH[[i]]
#   }
# } # doesnt work - BOROUGH IS JUT O
# 
# if (e$FEATURE_ID == f$FEATURE_ID) {
#   for (i in seq_along(e$FEATURE_ID)) {
#     e$BOROUGH[[i]] = f$BOROUGH[[i]]
#   }
# } # doesnt work - BOROUGH IS JUT O
# 
# for (i in seq_along(e$BOROUGH)) {
#   if (e$FEATURE_ID == f$FEATURE_ID) {
#     e$BOROUGH[[i]] = f$BOROUGH[[i]] # 1x=
#   }
# } # => warnings the condition has length > 1 and only the first element will be used
# 
# for (i in seq_along(e$BOROUGH)) {
#   if (e$FEATURE_ID == f$FEATURE_ID) {
#     e$BOROUGH[[i]] == f$BOROUGH[[i]] # 2x =
#   }
# } # => warnings the condition has length > 1 and only the first element will be used
# 
# 
# for (i in seq_along(e$BOROUGH)) {
#   if (e$FEATURE_ID == f$FEATURE_ID) {
#     replace(e$BOROUGH[[i]], f$BOROUGH[[i]])
#     }
# } # => warnings the condition has length > 1 and only the first element will be used
#   
# 
# # try using %in%
# for (i in seq_along(e$FEATURE_ID)) {
#   if (e$FEATURE_ID %in% f$FEATURE_ID == TRUE) {
#     replace(e$BOROUGH[[i]], f$BOROUGH[[i]])
#   }
# } # => warnings the condition has length > 1 and only the first element will be used



































# 
# p = function(f) {
#   ifelse(e$FEATURE_ID == f$FEATURE_ID, e$BOROUGH == f$BOROUGH)
# }  
# # https://bstaton1.github.io/au-r-workshop/ch1.html#if-else-and-ifelse
# 
# 
# 
# 
# 
# 
# 
# 
# # using coalesce - if NA value takes the value from the other dataset.  
# c = a %>%
#   select(BOROUGH) %>%
#   st_drop_geometry() # need to drop geometry on one of them as doesnt know which to keep
# d = b %>%
#   select(BOROUGH)
# 
# e = coalesce(c,d)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ####
# # Notes from NHS R Purr workshop
# 
# my_fn <- function(x, y) {
#   x + y
# }
# my_fn(3, 2) # -> 5
# 
# 
# 
# my_fn <- function(x) {
#   if (x > 10) {
#     stop ("x is too big!")
#   } else if (x > 5) {
#     return (x*2)
#   }
#   y <- x - 1
#   for (i in 1:5) {
#     y <- y*2
#   }
#   y
# }
# 
# my_fn(2) #-> 32
# my_fn(6) # -> 12
# my_fn(11) # => error
# 
# # using functions as variables
# average <- function(values, fn) {
#   fn(values)
# }
# average(c(1, 2, 5, 8), mean)
# average(c(1, 2, 5, 8), median)
# 
# # naming vectors
# named_vector <- c(a = 1, b = 2, c = 3)
# named_vector
# 
# 
# # applying a function to a set of values
# my_values <- c(1,4,5,3,2) # create values
# 
# my_fn <- function(x) {
#   3 * x + 2
# } # create function
#   
# my_fn(my_values) # run function over my_values vector
# # [1]  5 14 17 11  8
# 
# # Doing the above in a loop
# results <- numeric(length(my_values)) # create output
#   
# for (i in 1:length(my_values)) {
#   results[[i]] <- my_fn(my_values[[i]])
# } # create loop to iterate over values
# results # [1]  5 14 17 11  8
# 
# 
# # Doing above with map
# library(purrr)
# results <- map(my_values, my_fn)  # but gives it as a list
# 
# # instead use relevant purr function for the vector type
# results <- map_dbl(my_values, my_fn)
# 
# # can have an anonymous function in the map if you dont need it elsewhere
# map_dbl(my_values, function(x) {
#   3 * x + 2
# })
# map_dbl(my_values, ~ 3 * .x + 2) # same writtin in the purr formula syntax
# 
# # map over a column, run a function and create new column of result
# tibble(files = dir(pattern = "\\.png$")) %>%  # this is 2 pngs
#   mutate(filesize = map_dbl(files, file.size))
# 
# #using map to read in a set of csv files
# map(files, read.csv)
# #where files is a vector of the file names
# # e.g. "ae_attendances/2016-04-01.csv" found in dir obtained using below code
# files <- dir("ae_attendances/",
#              "^\\d{4}-\\d{2}-\\d{2}\\.csv$",
#              full.names = TRUE)
# 
# # can actually do it so it pulls all these into one df
# map_dfr(files, read_csv, col_types = "ccddd") # but this doesnt add the 
# #filename so it is missing the A&E attendance and date information
# 
# # use set_names to add names then .id in map_dfr to add this as a column
# files %>%
#   set_names() %>%
#   map_dfr(read_csv, col_types = "ccddd", .id = "filename") %>%
#   mutate(period = str_extract(filename,
#                               r"(\d{4}-\d{2}-\d{2}(?=\.csv$))") %>%
#            lubridate::ymd())
# 
# 
# # map2 takes two vectors as an argument
# map2_dbl(1:3, 4:6, ~ .x * .y) # takes 1 x4, 2 x5, 3 *6
# # [1]  4 10 18
# 
# 
# # pmap can use any number of vectors
# list(1:3, 4:6, 7:9) %>%
#   pmap_dbl(function(x, y, z) x * y + z)  # vectors need to be supplied as a list
# 
# list(a = 1:3, b = 4:6, c = 7:9) %>%
#   pmap_dbl(function(c, b, a) a * b + c) # can also use named vectors
# # this is usefu lif working with a dataframe where named vectors are columns
# 
# 
# 
# # iteratively map over outpust
# 1:3 %>%
#   map(~.x *2) # 1st map
# 
# 1:3 %>%
#   map(~.x *2) %>%
#   map_dbl(~ .x + 3) # -> [1] 5 7 9
# 
# 
# 
# # flatten in purr can flatten lists
# #e.g. flatten(list(list("abc", "d"), list("d", "e")))
# 
# flatten(list(list("abc", "d"), list("d", "e"))) %>%
#   flatten_chr() # -> flatten(list(list("abc", "d"), list("d", "e")))
# 
# 
# 
# # modify in purr
# df <- data.frame(
#   x = 1:3,
#   y = 6:4
# )
# #  x y
# #1 1 6
# #2 2 5
# #3 3 4
# 
# map(df, ~ .x * 2)
# # $x
# # [1] 2 4 6
# # $y
# #[1] 12 10  8
# 
# modify(df, ~ .x * 2)
# #>   x  y
# #> 1 2 12
# #> 2 4 10
# #> 3 6  8
# 
# df2 <- modify(df, ~ .x * 2) # need to assign to keep modified df
# 
# simple_modify <- function(x, f, ...) {
#   for (i in seq_along(x)) {
#     x[[i]] <- f(x[[i]], ...)
#   }
#   x
# }
# 
# simple_modify(df)

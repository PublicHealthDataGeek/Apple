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


# notes from end friday 8/1/20 - look at
# https://stackoverflow.com/questions/36433757/conditionally-replace-values-in-data-frame-from-separate-data-frame-in-r
# and also this:
# https://stackoverflow.com/questions/15310140/transfer-values-from-one-dataframe-to-another  BUT THIS ISNT CONDITIONAL



#select small number of rows from crossings CID
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

# run loop to replace 0 in e with Boroughs from f
for (i in seq_along(e$BOROUGH)) {
  e$BOROUGH[[i]] = f$BOROUGH[[i]]
} # output is that '0' borough names are replace with values but dont match the feature ID ? need to select feature id and match then change




if (e$FEATURE_ID == f$FEATURE_ID) {
  for (i in seq_along(e$BOROUGH)) {
    e$BOROUGH[[i]] = f$BOROUGH[[i]]
  }
} # doesnt work



for (i in seq_along(e$BOROUGH)) {
  if (e$FEATURE_ID == f$FEATURE_ID) {
    e$BOROUGH[[i]] = f$BOROUGH[[i]] # 1x=
  }
} # => warnings the condition has length > 1 and only the first element will be used

for (i in seq_along(e$BOROUGH)) {
  if (e$FEATURE_ID == f$FEATURE_ID) {
    e$BOROUGH[[i]] == f$BOROUGH[[i]] # 2x =
  }
} # => warnings the condition has length > 1 and only the first element will be used


for (i in seq_along(e$BOROUGH)) {
  if (e$FEATURE_ID == f$FEATURE_ID) {
    replace(e$BOROUGH[[i]], f$BOROUGH[[i]])
    }
} # => warnings the condition has length > 1 and only the first element will be used
  







p = function(f) {
  ifelse(e$FEATURE_ID == f$FEATURE_ID, e$BOROUGH == f$BOROUGH)
}  
# https://bstaton1.github.io/au-r-workshop/ch1.html#if-else-and-ifelse








# using coalesce - if NA value takes the value from the other dataset.  
c = a %>%
  select(BOROUGH) %>%
  st_drop_geometry() # need to drop geometry on one of them as doesnt know which to keep
d = b %>%
  select(BOROUGH)

e = coalesce(c,d)

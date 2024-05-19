library(haven)
library(tidyverse)
setwd("path_to_save")
path <- "path_where_the_file_is"

## load the attribute data first 
# attribute from wave 1  
data <- haven::read_sav(paste0(path, "1h",".sav"))
# we only need id, classroom code(osztalykod), gender(neme) and ethnicity(k16_1_1h)
data <- data %>% 
  select(idcode, osztalykod_1h, neme_1h, k16_1_1h) 

# wave 2
data_2 <- haven::read_sav(paste0(path, "2h",".sav"))
data_2 <- data_2 %>% 
  select(idcode, osztalykod_2h, k3_2h, k18_2h)

# wave 3
data_3 <- haven::read_sav(paste0(path, "3h",".sav"))
data_3 <- data_3 %>% 
  select(idcode, osztalykod_3h, k3_3h, k20_3h)

# wave 4
data_4 <- haven::read_sav(paste0(path, "4h",".sav"))
data_4 <- data_4 %>% 
  select(idcode, osztalykod_4h, k3_4h, k22_4h)


###########################################################33
# each wave have different numbers of observations now
# find the students that stay in all four waves
# common_ids <- Reduce(intersect, list(data$idcode, data_2$idcode, data_3$idcode, data_4$idcode))
# 938 observations

# create a data frame to only contain those 938 students
data_all <- inner_join(data, data_2, by = "idcode") %>%
  inner_join(data_3, by = "idcode") %>%
  inner_join(data_4, by = "idcode")


## CLASSROOM
# is there any Na in classroom code related columns?
sum(is.na(c(data_all$osztalykod_1h, data_all$osztalykod_2h, data_all$osztalykod_3h, data_all$osztalykod_4h)))
# good, no NA

# But what if they changed the classroom?
# Let's check if the students' classrooms stay the same through the four waves
# Check if classroom IDs are the same across all four columns
data_all <- data_all %>%
  rowwise() %>%
  mutate(all_classroom_same = if_else(osztalykod_1h == osztalykod_2h &
                                        osztalykod_2h == osztalykod_3h &
                                        osztalykod_3h == osztalykod_4h, TRUE, FALSE))

# Check if there are any observations where classroom IDs are not the same
any_different_classrooms <- any(!data_all$all_classroom_same)

if (any_different_classrooms) {
  cat("There are observations where classroom IDs are not the same in all columns.\n")
} else {
  cat("All observations have the same classroom IDs in all columns.\n")
}

# There are observations where classroom IDs are not the same in all columns.
# We want to keep only the students who stayed in the same classroom
data_all <- data_all %>% 
  filter(osztalykod_1h == osztalykod_2h &
         osztalykod_2h == osztalykod_3h &
         osztalykod_3h == osztalykod_4h)
# we now only have 890 students
# we only need one column to contain classroom code
data_all <- data_all %>% 
  mutate(classroom = osztalykod_1h) %>% 
  select(-osztalykod_1h,-osztalykod_2h,-osztalykod_3h,-osztalykod_4h,-all_classroom_same)


## Gender
# wait, the gender column in wave 1 is coded as 1&0, but 1&2 in later waves
# based on observation, all 0s in wave 1 should be 2s
data_all <- data_all %>% 
  mutate(neme_1h = ifelse(neme_1h == 0,2,neme_1h))
## gender: 1 for boys(Fiú), 2 for girls(Lány)

# however, it is easier to view it as 0 and 1
# so we change the 2s back to 0s
data_all <- data_all %>% 
  mutate(neme_1h = ifelse(neme_1h == 2,0,neme_1h),
         k3_2h = ifelse(k3_2h == 2,0,k3_2h),
         k3_3h = ifelse(k3_3h == 2,0,k3_3h),
         k3_4h = ifelse(k3_4h == 2,0,k3_4h))
# now, remember, 1 for boys and 0 for girls


# any student with 4 NAs in gender?
any_4gender_na <- any(rowSums(is.na(data_all[, c("neme_1h", "k3_2h", "k3_3h", "k3_4h")])) == 4)

if (any_4gender_na) {
  cat("There is at least one student who has 4 NAs in all four gender columns.\n")
} else {
  cat("There is no student who has 4 NAs in all four gender columns.\n")
}
# nice, all student has at least revealed their identified gender once.

# is gender constant?
# if there is only one non-Na value, we view it as constant here
data_all <- data_all %>%
  rowwise() %>%
  mutate(
    gender_na_count = sum(is.na(c(neme_1h, k3_2h, k3_3h, k3_4h))),
    all_gender_same = ifelse(
      gender_na_count >= 3, TRUE, ifelse(
      gender_na_count < 3 & length(unique(na.omit(c(neme_1h, k3_2h, k3_3h, k3_4h)))) == 1, TRUE, FALSE)
    )
  )

# Check if there are any observations where gender is not the same in all columns
any_different_gender <- any(!data_all$all_gender_same)

if (any_different_gender) {
  cat("There are observations where gender are not the same in all columns.\n")
} else {
  cat("All observations have the same gender in all columns.\n")
}
# nice, gender is constant
# so we only need to have one column for gender
# the gender column for wave 1 seems complete, have a check
if (any(is.na(data_all$neme_1h))) {
  cat("The 'neme_1h' column contains NA values.\n")
} else {
  cat("The 'neme_1h' column does not contain NA values.\n")
}
# yes, it is complete
# create a new column simply named "gender" and drop the four gender columns
data_all <- data_all %>% 
  mutate(gender = neme_1h) %>% 
  select(-neme_1h, -k3_2h, -k3_3h, -k3_4h, -gender_na_count, -all_gender_same)


## ETHNICITY
# any student with 4 NAs in ethnicity?
any_4eth_na <- any(rowSums(is.na(data_all[, c("k16_1_1h", "k18_2h", "k20_3h", "k22_4h")])) == 4)
# oh, there are students who never gave answer to ethnicity related questions

# How many
eth_with_all_na <- sum(rowSums(is.na(data_all[c("k16_1_1h", "k18_2h", "k20_3h", "k22_4h")])) == 4)

cat("Number of students who never revealed ethnicity:", eth_with_all_na, "\n")
# 24 students
data_all[rowSums(is.na(data_all[c("k16_1_1h", "k18_2h", "k20_3h", "k22_4h")])) == 4, ]$idcode

# is ethnicity constant?
# if there is only one non-Na value, we view it as constant here
data_all <- data_all %>%
  rowwise() %>%
  mutate(
    eth_na_count = sum(is.na(c(k16_1_1h, k18_2h, k20_3h, k22_4h))),
    all_eth_same = ifelse(
      eth_na_count == 4, TRUE, ifelse(
      eth_na_count < 4 & length(unique(na.omit(c(k16_1_1h, k18_2h, k20_3h, k22_4h)))) == 1, TRUE, FALSE)
    )
  )

# Check if there are any observations where gender is not the same in all columns
any_different_eth <- any(!data_all$all_eth_same)

if (any_different_eth) {
  cat("There are observations where ethnicty are not the same in all columns.\n")
} else {
  cat("All observations have the same ethnicty in all columns.\n")
}
# Ethnicity could be changed from wave 1 to 4

# group studnets based on whether they self-identified as member of the Roma ethnicity
# create a new column called "roma"
# as long as one student identify himself/herself as member of the Roma ethnicity once in 4 waves(value 2 or 3)
# the student will be coded as 1 in column "roma" and 0 other wise 
data_all <- data_all %>% 
  mutate(roma = case_when(
    eth_na_count != 4 & (
      any(k16_1_1h %in% c(2, 3)) | 
        any(k18_2h %in% c(2, 3)) | 
        any(k20_3h %in% c(2, 3)) | 
        any(k22_4h %in% c(2, 3))
    ) ~ 1,
    eth_na_count != 4 & (
      all(!k16_1_1h %in% c(2, 3)) & 
        all(!k18_2h %in% c(2, 3)) & 
        all(!k20_3h %in% c(2, 3)) & 
        all(!k22_4h %in% c(2, 3))
    ) ~ 0,
    TRUE ~ NA_real_
  ))%>% 
  select(idcode, classroom, gender, roma)

# save the data frame
write.csv(data_all,"attribute_all.csv", row.names = FALSE) # we don't need an extra column containing row numbers when saving


# Load packages
library("dplyr")
library("tidyr")
library("gbp")
library("purrr")

# total number of nodes
n <- 10000
# mean degree
mean.deg <- 2.72 # rural network

india_mix <- 
readRDS("./R/old_scripts/india_mix_oct2024.Rds")%>% 
  filter(study_site == "Rural")

# Calculate proportions of age groups
## merging age categories into 3 and retrieving contacts with household members
hh_age <- 
  india_mix  %>% 
  filter(hh_membership == "Member") %>% # Considering each contact is with household members (hh_membership=="Member"), no matter the contact is unique/repeat. 
  select(rec_id, study_day,participant_age,contact_age) %>% 
  mutate(participant_age = case_when(participant_age %in% c("0-9y", "10-19y") ~ "0-19y",
                                     participant_age %in% c("20-29y", "30-39y", "40-59y") ~ "20-59y",
                                     participant_age %in% c("60+y"  ) ~ "60+y",
                                     T ~ participant_age
  ),
  contact_age = case_when(contact_age %in% c("0-9y", "10-19y") ~ "0-19y",
                          contact_age %in% c("20-29y", "30-39y", "40-59y") ~ "20-59y",
                          contact_age %in% c("60+y"  ) ~ "60+y",
                          T ~ contact_age
  )
  )


# Characterize the proportion of household members in each age group
## Step 1: Create the hh_member_age dataframe for each rec_id by study_day
hh_member_age <- hh_age %>%
  group_by(rec_id, study_day) %>%
  summarise(hh_member_age = list(c(contact_age, unique(participant_age))), .groups = "drop")  # the hh_member_age variable list the age group of contacts/participant, each age group is considered to be of a household member


## Step 2: Calculate the relative proportion of hh_member_age by study day
hh_member_proportion <- hh_member_age %>%
  unnest(hh_member_age) %>%
  group_by(rec_id, study_day, hh_member_age) %>% 
  # The age distribution of a household can be approximated by the frequency of the age of the participant and his/her contacts on a single day (study day == "1"/"2")
  summarize(count = n(), .groups = "drop") %>% # summarize frequencies by the 3 variables in group_by, and drop grouping in dplyr
  group_by(rec_id, study_day) %>%
  mutate(proportion = count / sum(count)) %>% # proportion of hh_member_age by day and household
  select(-count)%>%
  ungroup()

## Step 3: Calculate the average proportions at the household level over the two days, yielding single-day proportions
average_proportion_2days <- hh_member_proportion %>%
  # complete() is to ensure that all age groups are represented, filling in unobserved age groups of a household with a proportion of 0.
  # However, for a household, if the data for a study_day is not available (i.e., only having data for day 1 or day 2), we use the proportion of the available study day as-is
  complete(rec_id, hh_member_age, fill = list(proportion = 0)) %>% 
  group_by(rec_id, hh_member_age) %>%
  # We observe for a small amount of households, the sum of average proportions across the hh_member_age categories is >1. 
  # These households are those the contacts of contact age group of a study day which was unobserved. E.g., 129-1, 630-1 in the rural population
  summarize(avg_proportion_2days = mean(proportion), .groups = "drop") # calculate the average proportion over two study days

## Step 4: Calculate the average proportions of each age category across all households
average_proportion <- average_proportion_2days %>% 
  group_by(hh_member_age) %>% 
  summarize(average_proportion = mean(avg_proportion_2days)
  )

# # Characterize the proportion of household having both children and non-elderly adults
# ## Step 1: Determine if hh_member_age contains both "0-19y" and "20-59y" on a day
# hh_member_age <- hh_member_age %>%
#   # we use map_lgl() from "purrr" to used to iterate over each element in the list in the hh_member_age variable by row.
#   # all(c("0-19y", "20-59y") %in% .) in map_lgl ("lgl" stands for logic) checks if both "0-19y" and "20-59y" are present in each hh_member_age vector.
#   mutate(child_n_adult = map_lgl(hh_member_age, ~ all(c("0-19y", "20-59y") %in% .)))
# 
# ## Step 2: Calculate the proportion of TRUE (having both child and adult)  in child_n_adult by study_day
# proportion_child_n_adult <- hh_member_age %>%
#   group_by(study_day) %>%
#   summarise(proportion_true = mean(child_n_adult), .groups = "drop")
# 
# ## Step 3: Average the proportions over 2 days
# average_proportion <- 
#   average_proportion %>% rbind(c("0-19y_and_20-59y", proportion_child_n_adult %>% pull(proportion_true) %>% mean )
#   ) %>% 
#   mutate(average_proportion = as.numeric(average_proportion))

## given the meaning of (1-prop.children.with.adult), it may be more straightforward to calculate the proportion of households only with children than proportion_child_n_adult

hh_member_age <- 
hh_member_age %>%
  mutate(child_only = map_lgl(hh_member_age, ~ all(. == "0-19y")))

proportion_child_only <- hh_member_age %>%
  group_by(study_day) %>%
  summarise(proportion_true = mean(child_only), .groups = "drop")

## Step 3: Average the proportions over 2 days
average_proportion <-
  average_proportion %>% rbind(c("0-19y_only", proportion_child_only %>% pull(proportion_true) %>% mean )
  ) %>%
  mutate(average_proportion = as.numeric(average_proportion))

average_proportion

# age distribution of target population
dss_pop_age_grp =
  data.frame(
    tar_pop_size=
  c(# rural
                 c(199+750+933+ 1017+958, # children (0-19)
                    1196+1195+1309+1272+1215+1088+1067+876, # adult (20-59)
                    777+626+499+321+437), #  eldery (60+)
                  # urban
                  c(309+1144+1458+1474+1814, # children (0-19)
                    1805+1731+1740+1669+1471+1395+1206+975,# adult (20-59)
                    891+572+412+236+245)  #  eldery (60+)
),
age.grp = rep(c("child", "adult", "elderly"), times= 2),
network = rep(c("rural", "urban"), each = 3)
) %>% group_by(network) %>%
  mutate(prop = tar_pop_size / sum(tar_pop_size)
         ) %>%
  ungroup()
## The household-level distribution of age appears to be similar to the target population



# Generate age groups
set.seed(20240930) # For reproducibility
age.grp <- sample(c("0-19y", "20-59y", "60+y"), # corresponds to 3 age group, children (0-19), adult (20-59), elderly (60+)
                  size = n,
                  replace = TRUE,
                  prob = dss_pop_age_grp %>% filter(network == "rural") %>% pull(prop)
                  )
#age.grp <- sample(c("0-19y", "20-59y", "60+y"), size = n, replace = TRUE, prob = c(0.25, 0.55, 0.20))


# Setup -------------------------------------------------------------------
# Set number of households based on household size
hh.size <- mean.deg +1 
#hh.size <- 2.7
n.hh <- round(n / hh.size) 
ids <- 1:n 
hh.ids <- 1:n.hh

# Create empty data frames to track hh assignment, the mem.u19, mem.20t59, and mem.60p are logic variables indicating whether there is >=1 hh member belong to that age group in a household
hh.by.age <- data.frame(hh.ids = hh.ids, mem.u19 = NA, mem.20t59 = NA, mem.60p = NA)
# Create data frame of nodal attribute, hh is to record the houshold id a node is assigned
persons.by.hh <- data.frame(ids = ids, age.grp = age.grp, hh = NA) 


# Set proportions of household-level age groups
# prop.hh.with.child <- average_proportion %>% filter(hh_member_age == "0-19y") %>% pull(average_proportion)
# prop.hh.with.adult <- average_proportion %>% filter(hh_member_age == "20-59y") %>% pull(average_proportion)
# prop.hh.with.elderly <- average_proportion %>% filter(hh_member_age == "60+y") %>% pull(average_proportion)
# prop.children.with.adult <- average_proportion %>% filter(hh_member_age == "0-19y_and_20-59y") %>% pull(average_proportion)
# prop.children.only <- average_proportion %>% filter(hh_member_age == "0-19y_only") %>% pull(average_proportion)

prop.hh.with.child <- 0.292
prop.hh.with.adult <- 0.314
prop.hh.with.elderly <- 0.791
#prop.children.with.adult <- average_proportion %>% filter(hh_member_age == "0-19y_and_20-59y") %>% pull(average_proportion)
prop.children.only <- 0.00182

# Household Assignment ----------------------------------------------------

# Determine which households will have a member under 19 
hh.u19 <- sample(x = hh.ids, size = round(prop.hh.with.child * n.hh)) # randomly select a set of household id, equals to the number of hh having kids<19, to consider them to have children under 19
hh.by.age$mem.u19 = ifelse(hh.by.age$hh.ids %in% hh.u19, TRUE, FALSE) # have the selection result in the dataframe (df) tracking household assignment

# Assign children under 19 to the selected households
persons.by.hh[persons.by.hh$ids %in% which(age.grp == "0-19y")[1:length(hh.u19)], ]$hh <- hh.u19 # in the nodal attribute df, we assign the household ids of hh.u19 to nodes under 19y
# there may still be children left who haven't been assigned to a household because there are more children than the number of households with children (hh.u19) - given a household can have >=2 kids. 
# the following assignment distributes the remaining children by sampling from the existing households (hh.u19),
children.hh.assign <- sample(x = hh.u19, size = length(which(age.grp == "0-19y")) - length(hh.u19), replace = TRUE)  # select a subset of household ids of 0-19y for the rest of kids
persons.by.hh[persons.by.hh$ids %in% which(age.grp == "0-19y")[(length(hh.u19) + 1):length(which(age.grp == "0-19y"))], ]$hh <- children.hh.assign # assign the household ids to the rest of nodes in 0-19y

# Determine which hh with children will have at least one 19 - 59 member
#num.children.wo.adult <- round((1-prop.children.with.adult) * (sum(age.grp == "0-19y"))) # number of households only with children
num.children.wo.adult <- round(prop.children.only * (sum(age.grp == "0-19y")))
num.children <- persons.by.hh[persons.by.hh$age.grp == "0-19y", ] %>% group_by(hh) %>% summarize(num = n()) # number of 0-19y children per household
## this optimization is to select housholds ids have children but without adults
hh.select <- gbp1d_solver_dpp(p = num.children$num, # count of children in each hh as weight for the optimization
                              w = num.children$num,
                              c = num.children.wo.adult) # constraint is the number of children live in households without an adult
hh.wo.adult <- num.children$hh[as.logical(hh.select$k)]
hh.with.adult <- setdiff(num.children$hh, hh.wo.adult) # hh w/ adults
hh.by.age$mem.20t59 = ifelse(hh.by.age$hh.ids %in% hh.with.adult, TRUE, NA) 

# Determine which hh without children will have at least one 19 - 59 member
num.hh.add.adult <- round(n.hh * prop.hh.with.adult - sum(hh.by.age$mem.20t59 == TRUE, na.rm = TRUE) # those mem.20t59 == TRUE are households with children and with adult, so the output is the # of households without children but with adult
                          )
hh.add.adult <- sample(x = hh.by.age[is.na(hh.by.age$mem.20t59) & hh.by.age$mem.u19 == FALSE, ]$hh.ids, # for households haven't been characterized and without children, randomly select # of households without children but with adult
                       size = num.hh.add.adult)
hh.by.age[hh.by.age$hh.ids %in% hh.add.adult, ]$mem.20t59 <- TRUE # record the select in the dataframe tracking hh assignment
hh.by.age[is.na(hh.by.age$mem.20t59), ]$mem.20t59 <- FALSE # for the rest of households, we consider them to not have adult (20-59)
hh.20t59 <- hh.by.age[hh.by.age$mem.20t59 == TRUE, ]$hh.ids # retrieve all household id with adults (20-59) 

# Assign houshold ids with adults to the nodal attribute data frame
persons.by.hh[persons.by.hh$ids %in% which(age.grp == "20-59y")[1:length(hh.20t59)], ]$hh <- hh.20t59 
adults.hh.assign <- sample(x = hh.20t59, size = length(which(age.grp == "20-59y")) - length(hh.20t59), replace = TRUE) # this assignment distributes the remaining household id with adults by sampling from the existing households - each household can have 2 adults
persons.by.hh[persons.by.hh$ids %in% which(age.grp == "20-59y")[(length(hh.20t59) + 1):length(which(age.grp == "20-59y"))], ]$hh <- adults.hh.assign # assign the remaining household id with adults to the nodal attribute data

# Determine which hh will have a 65+ member
hh.must.elderly <- hh.by.age[hh.by.age$mem.20t59 == FALSE, ]$hh.ids # we consider households don't have adult as those must have elderly
hh.by.age[hh.by.age$mem.20t59 == FALSE, ]$mem.60p <- TRUE # same as the logic above, we consider households don't have adult as those must have elderly
num.hh.add.elderly <- round(n.hh * prop.hh.with.elderly - length(hh.must.elderly)) # In the rural data, the proportion of households with elderly is smaller than the proportion of household without adults, making this line problematic
hh.add.elderly <- sample(x = hh.by.age[is.na(hh.by.age$mem.60p), ]$hh.ids, size = num.hh.add.elderly)
hh.by.age[hh.by.age$hh.ids %in% hh.add.elderly, ]$mem.60p <- TRUE
hh.by.age[is.na(hh.by.age$mem.60p), ]$mem.60p <- FALSE
hh.60p <- hh.by.age[hh.by.age$mem.60p == TRUE, ]$hh.ids

## Adaptation to the above - we consider the elderly assignment is independent from adult
# num.hh.add.elderly <- round(n.hh * prop.hh.with.elderly) # number of household with any elderly
# hh.add.elderly <- sample(x = hh.by.age$hh.ids, size = num.hh.add.elderly) # randomly select num.hh.add.elderly household IDs with elderly
# hh.by.age[hh.by.age$hh.ids %in% hh.add.elderly, ]$mem.60p <- TRUE # in the household dataframe, assign T to household with elderly
# hh.by.age[is.na(hh.by.age$mem.60p), ]$mem.60p <- FALSE # in the household dataframe, for other households, assign F to household with elderly
# hh.60p <- hh.by.age[hh.by.age$mem.60p == TRUE, ]$hh.ids # retrieve the household ids with elderly. after the adaptation hh.60p is the same as hh.add.elderly %>% sort

# Assign elderly to selected households
## select rows in the nodal attribute data whose nodes is 60+y and in the first set of rows equal to the number of households having 60+y
persons.by.hh[persons.by.hh$ids %in% which(age.grp == "60+y")[1:length(hh.60p)], ]$hh <- hh.60p
## for the rest of nodes of the 3rd age group, we randomly select housholds ID with 60+y to them
elderly.hh.assign <- sample(x = hh.60p, size = length(which(age.grp == "60+y")) - length(hh.60p), replace = TRUE)
## assign elderly.hh.assign to the rest of nodes 60+
persons.by.hh[persons.by.hh$ids %in% which(age.grp == "60+y")[(length(hh.60p) + 1):length(which(age.grp == "60+y"))], ]$hh <- elderly.hh.assign 

# Check Household Assignment ----------------------------------------------

# Rules 1 - 3: Proportions of households with at least one child/adult/elderly person 
hh.check1 <- persons.by.hh %>% group_by(age.grp) %>% summarize(num.hh = n_distinct(hh))
hh.check1$pct.hh <- round(hh.check1$num.hh / length(unique(persons.by.hh$hh)), 3) * 100
 hh.check1

# Rule 4: Average household size 
hh.check2 <- persons.by.hh %>% group_by(hh) %>% summarize(num.person = n())
mean.hh.check2 <- round(mean(hh.check2$num.person), 1)
hh.size

# 5. Every household with a child must also have at least one adult
hh.check3 <- persons.by.hh %>% group_by(hh) %>% summarize(min.age.grp = min(age.grp), max.age.grp = max(age.grp))
orphans <- nrow(hh.check3[hh.check3$min.age.grp == "0-19y" & hh.check3$max.age.grp == "0-19y", ])
orphans
## compared to 5 households, this is still a little off.


# 6. 97.9% of children live with an adult in the 19-59 age range 
hh.check4 <- persons.by.hh %>% group_by(hh, age.grp) %>% summarize(num.person = n())
hh.check4 <- spread(hh.check4, key = age.grp, value = num.person)
hh.check4 <- hh.check4[!is.na(hh.check4$`0-19y`) & !is.na(hh.check4$`20-59y`), ]
children.in.hh.w.adult <-sum(hh.check4$`0-19y`) / length(which(age.grp == "0-19y"))
1- children.in.hh.w.adult;  prop.children.only


# Prepare household network ----------------------------------------------

# # Set household attribute
# est[[1]]$newnetwork <- set.vertex.attribute(est[[1]]$newnetwork, "household", persons.by.hh$hh)
# 
# # Save household edge list
# hhPairs <- merge(persons.by.hh, persons.by.hh, by = "hh")
# hhPairs <- subset(hhPairs, (ids.x < ids.y))
# hhPairs <- hhPairs[, c(2, 4)]
# names(hhPairs) <- c(".head", ".tail")
# rm(list=setdiff(ls(), c("est", "hhPairs", "vax_targets", "mr_vec")))

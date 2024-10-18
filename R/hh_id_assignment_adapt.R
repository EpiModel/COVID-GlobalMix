# Load packages
library(dplyr)
library(tidyr)
library(gbp)
library(purrr)
library(tibble)

hh_age <- 
  readRDS("./R/old_scripts/india_mix_oct2024.Rds")

# mean degree
node_attribute_target_stats <- 
  readRDS(paste0("./data/network_stats_attributes/node_attribute_target_stats", "__", 0.001, ".Rds"))

# rural network
mean.deg <- 
node_attribute_target_stats$targetstats_age.grp$formation_stats_rural$edge_ct_matrix$Home %>% sum()*2/node_attribute_target_stats$attr$rural %>% nrow

hh_age_rural <- 
hh_age %>% 
  filter(study_site == "Rural")%>% 
  filter(hh_membership == "Member") %>% # Considering each contact is with household members (hh_membership=="Member"), no matter the contact is unique/repeat. 
  select(rec_id, study_day, fromdayone, participant_age,contact_age)%>% 
  ## merging age categories into 3 and retrieving contacts with household members
  mutate(participant_age = case_when(participant_age %in% c("0-9y", "10-19y") ~ "0-19y",
                                     participant_age %in% c("20-29y", "30-39y", "40-59y") ~ "20-59y",
                                     participant_age %in% c("60+y"  ) ~ "60-100y",
                                     T ~ participant_age
  ),
  contact_age = case_when(contact_age %in% c("0-9y", "10-19y") ~ "0-19y",
                          contact_age %in% c("20-29y", "30-39y", "40-59y") ~ "20-59y",
                          contact_age %in% c("60+y"  ) ~ "60-100y",
                          T ~ contact_age
  )
  ) %>% # if a contact on day 2 is a repeated contact, we filter it out
  filter(!(study_day == 2 & fromdayone == "Both days")
         )



hh_age_urban <- 
  hh_age %>% 
  filter(study_site == "Urban")%>% 
  filter(hh_membership == "Member") %>% # Considering each contact is with household members (hh_membership=="Member"), no matter the contact is unique/repeat. 
  select(rec_id, study_day, fromdayone, participant_age,contact_age)%>% 
  ## merging age categories into 3 and retrieving contacts with household members
  mutate(participant_age = case_when(participant_age %in% c("0-9y", "10-19y") ~ "0-19y",
                                     participant_age %in% c("20-29y", "30-39y", "40-59y") ~ "20-59y",
                                     participant_age %in% c("60+y"  ) ~ "60-100y",
                                     T ~ participant_age
  ),
  contact_age = case_when(contact_age %in% c("0-9y", "10-19y") ~ "0-19y",
                          contact_age %in% c("20-29y", "30-39y", "40-59y") ~ "20-59y",
                          contact_age %in% c("60+y"  ) ~ "60-100y",
                          T ~ contact_age
  )
  )   %>% # if a contact on day 2 is a repeated contact, we filter it out
  filter(!(study_day == 2 & fromdayone == "Both days")
  )

table(hh_age_rural$fromdayone == "Both days"); table(hh_age_urban$fromdayone == "Both days")

# Calculate proportions of age groups
## Calculate the required proportions for running the script
### define function for calculating the proportions
proportion_hh_members <- function(hh_age){
  
# Characterize the proportion of household members in each age group
## Step 1: Create the hh_member_age dataframe for each rec_id 
hh_member_age <- hh_age %>%
  group_by(rec_id) %>%
  summarise(hh_member_age = list(c(contact_age, unique(participant_age))), .groups = "drop")  # the hh_member_age variable list the age group of a participant and its de-duplicated contacts , each age group is considered to be of a household member


## Step 2: Calculate the relative proportion of hh_member_age by study day
### prop.hh.with.child, prop.hh.with.adult, prop.hh.with.elderly are the proportions of households with children, adults, and elderlies, respectively. 
### E.g., prop.hh.with.child is calculated as (the number of households with ≥1 children)/(the total number of households).

hh_member_age <- hh_member_age %>%
  mutate(
    child = map_lgl(hh_member_age, ~ any(grepl("0-19y", .))),
    adult = map_lgl(hh_member_age, ~ any(grepl("20-59y", .))),
    elderly = map_lgl(hh_member_age, ~ any(grepl("60-100y", .)))
  )

proportions <- hh_member_age %>%
  summarize(
    prop_hh_w_child = mean(child), 
    prop_hh_w_adult = mean(adult),
    prop_hh_w_elderly = mean(elderly),
    .groups = "drop") %>% t() %>% 
  as.data.frame()  %>% rename(proportion=V1) %>% 
  rownames_to_column(., "prop_type")


## Step 3: Calculate the number of children living with adults / total number of children
# Define the function to calculate the proportion of children with an adult (20-59y) in the household

# Define a function to check if a household has an adult (20-59y)
has_adult <- function(age_list) {
    return("20-59y" %in% age_list)
  }
  
# Count the total number of children (0-19y) in the household
count_children <- function(age_list) {
    return(sum(age_list == "0-19y"))
  }
  
# For each household (row), we use "has_adult" to record whether that household has ≥1 adults 
hh_member_age$has_adult <- sapply(hh_member_age$hh_member_age, has_adult)

# For each household (row), we use "count_children" to record the number of children (those aged "0-19y")
hh_member_age$children_count <- sapply(hh_member_age$hh_member_age, count_children)
  
# Filter only households with ≥ adult (20-59y) and calculate the total number of children in these households
children_with_adult <- sum(hh_member_age$children_count[hh_member_age$has_adult])
  
# Total number of children in all households
total_children <- sum(hh_member_age$children_count)
  
# Calculate the proportion of children with ≥ adult among all children
prop_children_with_adult <- children_with_adult / total_children
  

proportions <- 
  proportions %>% 
  rbind(., c("prop_child_w_adult", prop_children_with_adult)) %>% 
  mutate(proportion = as.numeric(proportion))

# the number of household only with children in the observed data
freq_hh_only_w_child <- 
hh_member_age %>%
  mutate(
    child_only = map_lgl(hh_member_age, ~ all(. == "0-19y"))
  ) %>% pull(child_only ) %>% as.numeric() %>% sum()

output <- list()
output$proportions <- proportions
output$freq_hh_only_w_child <- freq_hh_only_w_child
output$hh_member_age <- hh_member_age

return(output)
}

### run the function
prop_hh_members_rural <- 
proportion_hh_members(hh_age = hh_age_rural) # contains the proportions and the household members' age groups over all study days by household


prop_hh_members_rural$hh_member_age %>% filter(rec_id=="942-1") %>% pull(hh_member_age)


prop_hh_members_urban <- 
  proportion_hh_members(hh_age = hh_age_urban)

## define function
node_hh_assign <- 
  function(
    #observed proportions of households with children, adults, and elderly, respectively. 
    prop.hh.with.child,
    prop.hh.with.adult,
    prop.hh.with.elderly,
    #observed proportions of children with adults. 
    prop.children.with.adult, 
    #observed frequency if household only with children 
    freq.hh.child.only,
    # mean degree calculated from the age mixing matrix of edge count
    mean.deg,
    # age group of each node in the modeled population
    age.grp # 3-category age groups consist of "0-19y", 20-59y", and "60-100y"
    ){
    
# note the age category of "60-100y" is also labeled as "60p" in the following script
        
# total number of nodes
n = length(age.grp)
    
# Set number of households based on household size
## household size
hh.size <- mean.deg +1 
## number of households
n.hh <- round(n / hh.size) 
## node's ID
ids <- 1:n 
## household ID
hh.ids <- 1:n.hh

# Create empty data frames to track hh assignment, the mem.u19, mem.20t59, and mem.60p are logic variables indicating whether there is >=1 hh member belong to that age group in a household
hh.by.age <- data.frame(hh.ids = hh.ids, mem.u19 = NA, mem.20t59 = NA, mem.60p = NA)
# Create data frame of nodal attribute, hh is to record the houshold id a node is assigned
persons.by.hh <- data.frame(ids = ids, age.grp = age.grp, hh = NA) 

set.seed(2024)
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
num.children.wo.adult <- round((1-prop.children.with.adult) * (sum(age.grp == "0-19y"))) # number of households only with children
num.children <- persons.by.hh[persons.by.hh$age.grp == "0-19y", ] %>% group_by(hh) %>% summarize(num = n()) # number of 0-19y children per household
## this optimization is to select households ids have children but without adults
hh.select <- gbp1d_solver_dpp(p = num.children$num, # count of children in each hh as weight for the optimization
                              w = num.children$num,
                              c = num.children.wo.adult) # constraint is the number of children live in households without an adult
hh.wo.adult <- num.children$hh[as.logical(hh.select$k)]
hh.with.adult <- setdiff(num.children$hh, hh.wo.adult) # hh w/ adults
hh.by.age$mem.20t59 = ifelse(hh.by.age$hh.ids %in% hh.with.adult, TRUE, NA) 

# Determine which hh without children will have at least one 20 - 59 member
num.hh.add.adult <- round(n.hh * prop.hh.with.adult - sum(hh.by.age$mem.20t59 == TRUE, na.rm = TRUE) # those mem.20t59 == TRUE are households with children and with adult, so the output is the # of households without children but with adult
                          )
hh.add.adult <- sample(x = hh.by.age[is.na(hh.by.age$mem.20t59) & hh.by.age$mem.u19 == FALSE, ]$hh.ids, # for households haven't been characterized and without children, randomly select # of households without children but with adult
                       size = num.hh.add.adult)
hh.by.age[hh.by.age$hh.ids %in% hh.add.adult, ]$mem.20t59 <- TRUE # record the select households in the dataframe tracking hh assignment
hh.by.age[is.na(hh.by.age$mem.20t59), ]$mem.20t59 <- FALSE # for the rest of households, we consider them to not have adult (20-59)
hh.20t59 <- hh.by.age[hh.by.age$mem.20t59 == TRUE, ]$hh.ids # retrieve all household id with adults (20-59) 

# Assign houshold ids with adults to the nodal attribute data frame
persons.by.hh[persons.by.hh$ids %in% which(age.grp == "20-59y")[1:length(hh.20t59)], ]$hh <- hh.20t59 
adults.hh.assign <- sample(x = hh.20t59, size = length(which(age.grp == "20-59y")) - length(hh.20t59), replace = TRUE) # this assignment distributes the remaining household id with adults by sampling from the existing households - each household can have 2 adults
persons.by.hh[persons.by.hh$ids %in% which(age.grp == "20-59y")[(length(hh.20t59) + 1):length(which(age.grp == "20-59y"))], ]$hh <- adults.hh.assign # assign the remaining household id with adults to the nodal attribute data

# Determine which hh will have a 60+ member
hh.must.elderly <- hh.by.age[hh.by.age$mem.20t59 == FALSE, ]$hh.ids # we consider households don't have adult as those must have elderly
hh.by.age[hh.by.age$mem.20t59 == FALSE, ]$mem.60p <- TRUE # same as the logic above, we consider households don't have adult as those must have elderly
num.hh.add.elderly <- round(n.hh * prop.hh.with.elderly - length(hh.must.elderly)) # the number of households >=1 elderly - the number of household without adults = number of households needs to have elderly 
hh.add.elderly <- sample(x = hh.by.age[is.na(hh.by.age$mem.60p), ]$hh.ids, size = num.hh.add.elderly)
hh.by.age[hh.by.age$hh.ids %in% hh.add.elderly, ]$mem.60p <- TRUE
hh.by.age[is.na(hh.by.age$mem.60p), ]$mem.60p <- FALSE
hh.60p <- hh.by.age[hh.by.age$mem.60p == TRUE, ]$hh.ids

# Assign elderly to selected households
## select rows in the nodal attribute data whose nodes is 60-100y and in a set of rows equal to the number of households having 60-100y
persons.by.hh[persons.by.hh$ids %in% which(age.grp == "60+y")[1:length(hh.60p)], ]$hh <- hh.60p
## for the rest of nodes of the 3rd age group, we randomly select housholds ID with 60-100y to them
elderly.hh.assign <- sample(x = hh.60p, size = length(which(age.grp == "60+y")) - length(hh.60p), replace = TRUE)
## assign elderly.hh.assign to the rest of nodes 60+
persons.by.hh[persons.by.hh$ids %in% which(age.grp == "60+y")[(length(hh.60p) + 1):length(which(age.grp == "60+y"))], ]$hh <- elderly.hh.assign 

# Save results
output$assignments <- output$validation<- output <- list()
output$assignments <- persons.by.hh

# Check Household Assignment ----------------------------------------------
# Rules 1 - 3: Proportions of households with at least one child/adult/elderly person 
hh.check1 <- persons.by.hh %>% group_by(age.grp) %>% summarize(num.hh = n_distinct(hh))
hh.check1$pct.hh.simulated <- round(hh.check1$num.hh / length(unique(persons.by.hh$hh)), 3) 

sim_v_obs_props <- 
hh.check1 %>% 
  select(age.grp, pct.hh.simulated) %>% 
  mutate(prop_type = 
           recode(age.grp,
                  "0-19y" = "prop_hh_w_child",
                  "20-59y" = "prop_hh_w_adult",
                  "60+y" = "prop_hh_w_elderly",
                  )
         ) %>% select(prop_type, pct.hh.simulated)
 
## percentage of children who live with an adult in the 19-59 age range 
hh.check4 <- persons.by.hh %>% group_by(hh, age.grp) %>% summarize(num.person = n())
hh.check4 <- spread(hh.check4, key = age.grp, value = num.person)
hh.check4 <- hh.check4[!is.na(hh.check4$`0-19y`) & !is.na(hh.check4$`20-59y`), ]
children.in.hh.w.adult <- round(
  sum(hh.check4$`0-19y`) / length(which(age.grp == "0-19y")), 3
)

sim_v_obs_dta <- 
sim_v_obs_props %>% 
  rbind(., c("prop_child_w_adult", children.in.hh.w.adult)) %>%  #simulated data
  cbind(., pct.hh.observed = c(prop.hh.with.child, prop.hh.with.adult,prop.hh.with.elderly,prop.children.with.adult) #observed data
        )

# rename variable to save the rest of validation results
sim_v_obs_dta <- 
sim_v_obs_dta %>% rename(data_type = prop_type, simulated = pct.hh.simulated, observed = pct.hh.observed)

# Rule 4: Average household size 
hh.check2 <- persons.by.hh %>% group_by(hh) %>% summarize(num.person = n())
mean.hh.check2 <- round(mean(hh.check2$num.person), 2)

sim_v_obs_dta <- 
sim_v_obs_dta %>% rbind(c("hh_size", 
                            mean.hh.check2, 
                            round(hh.size,2)
                            )
                          )


# 5. Every household with a child must also have at least one adult for the simulated data
hh.check3 <- persons.by.hh %>% group_by(hh) %>% summarize(min.age.grp = min(age.grp), max.age.grp = max(age.grp))
orphans_simulated <- nrow(hh.check3[hh.check3$min.age.grp == "0-19y" & hh.check3$max.age.grp == "0-19y", ]) # the number of houeholds having only children

orphans_simulated

sim_v_obs_dta <- 
  sim_v_obs_dta %>% rbind(c("freq_hh_child_only", 
                            orphans_simulated, freq.hh.child.only
                           
  )
  )

# Save household edge list
hhPairs <- merge(persons.by.hh, persons.by.hh, by = "hh") # getting all combinations of nodes that belong to the same household - a cartesian product of node ids within each houshold
hhPairs <- subset(hhPairs, (ids.x < ids.y)) # remove duplicate pairs
hhPairs <- hhPairs %>% select(hh, ids.x, ids.y) %>% rename(.head = ids.x, .tail = ids.y)

output$edgelist <- hhPairs


# save validation data to "output"
output$validation <- sim_v_obs_dta

output
}

node_hh_assign_rural <- 
node_hh_assign(
  #observed proportions of households with children, adults, and elderly, respectively. 
  prop.hh.with.child = prop_hh_members_rural$proportions %>% filter(prop_type == "prop_hh_w_child") %>% pull(proportion),
  prop.hh.with.adult = prop_hh_members_rural$proportions %>% filter(prop_type == "prop_hh_w_adult") %>% pull(proportion),
  prop.hh.with.elderly = prop_hh_members_rural$proportions %>% filter(prop_type == "prop_hh_w_elderly") %>% pull(proportion),
  #observed proportions of children with adults. 
  prop.children.with.adult = prop_hh_members_rural$proportions %>% filter(prop_type == "prop_child_w_adult") %>% pull(proportion), 
  #observed frequency if household only with children 
  freq.hh.child.only = prop_hh_members_rural$freq_hh_only_w_child,
  # mean degree calculated from the age mixing matrix of edge count of the home layer of a network
  mean.deg = mean.deg,
  # age group of each node in the modeled population
  age.grp =  # 3-category age groups consist of "0-19y", 20-59y", and "60-100y"
    recode(as.character(node_attribute_target_stats$attr$rural$node.age.grp), 
           `0-9y` = "0-19y", 
           `10-19y` = "0-19y", 
           `20-29y` = "20-59y", 
           `30-39y` = "20-59y", 
           `40-59y` = "20-59y", 
           `60+y` = "60+y")
)

node_hh_assign_rural$validation

node_hh_assign_rural$edgelist$hh %>% unique %>% length # 29 hh, hh id 27,29,31 missed because the household only has one one-kid adults
node_hh_assign_rural$assignments$hh %>% unique %>% sort %>% length # 32 hh
 
node_hh_assign_rural$assignments %>% view





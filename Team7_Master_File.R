library(tidyverse)
library(dplyr)
library(stringr)
library(caret)
library(ROCR)
library(text2vec)
library(SnowballC)
library(glmnet)
library(vip)
library(tm)
library(randomForest)
library(gbm)
library(tidyverse)


train_x <- read_csv("airbnb_train_x_2024.csv")
train_y <- read_csv("airbnb_train_y_2024.csv")
test_x <- read_csv("airbnb_test_x_2024.csv")
cities <- read_csv("uscities.csv")
city_split <- separate_rows(cities, zipcode, sep = " ") %>%
  group_by(zipcode) %>%
  mutate(order = row_number()) %>%
  filter(order == 1)


set.seed(1)

#join the training y to the training x file
#also turn the target variables into factors
train <- cbind(train_x, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate)) 
summary(train)
train <- left_join(train, city_split, by = "zipcode")
test <- left_join(test_x, city_split, by = "zipcode")
# For numerical values: 
train <- train %>%
  mutate(accomodates = ifelse(is.na(accommodates), guests_included, accommodates),
         price = ifelse(is.na(price), 0 , price), #TL
         guests_included = ifelse(guests_included == 0, 1, guests_included),
         price_per_person = price/guests_included,
         is_occupied_in_maxOccupancy = ifelse(accommodates>guests_included, "NO", "YES"),
         is_occupied_in_maxOccupancy = as.factor(ifelse(is.na(is_occupied_in_maxOccupancy), "YES", is_occupied_in_maxOccupancy)),
         bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm=TRUE), bedrooms),
         bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm=TRUE), bathrooms),
         bathrooms_per_person = bathrooms/guests_included,
         beds = ifelse(is.na(beds), mean(beds, na.rm=TRUE), beds), #AL
         has_morebeds_than_bedrooms = ifelse((beds > bedrooms), 1, 0), #AL
         cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
         has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0 , "YES", "NO")),
         charges_for_extra = as.factor(ifelse(extra_people>0, "YES", "NO")),
         percent_cleaning_fee = ifelse(price == 0, 0, cleaning_fee/price),
         host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm=TRUE), host_total_listings_count),
         host_response_rate = ifelse(is.na(host_response_rate), mean(host_response_rate, na.rm=TRUE), host_response_rate),
         has_min_nights = as.factor(ifelse(minimum_nights > 1, "YES", "NO")),
         security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),
         has_security_deposit = as.factor(ifelse(security_deposit > 0 , "YES", "NO")),
         host_acceptance = as.factor(ifelse(is.na(host_acceptance_rate),"MISSING", ifelse(parse_number(host_acceptance_rate) == 100,"ALL","SOME"))) ,
         host_response_category = as.factor(ifelse(is.na(host_response_rate),"MISSING", ifelse(host_response_rate == 100,"ALL","SOME"))))

#Data Featuring for Categorical values

train <- train %>%
  mutate(bed_type = as.factor(bed_type),
         bed_category = ifelse(bed_type == "Real Bed", "bed", "other"),
         bed_category = as.factor(bed_category),
         cancellation_policy = str_replace (cancellation_policy,"super_strict_30", "strict"),
         cancellation_policy = str_replace (cancellation_policy,"super_strict_60", "strict"),
         cancellation_policy = str_replace (cancellation_policy,"no_refunds", "strict"),
         cancellation_policy = as.factor(cancellation_policy),
         host_response_time = as.factor(host_response_time),
         property_category = case_when(
           property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ 'apartment',
           property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ 'hotel',
           property_type %in% c("Townhouse", "Condominium") ~ 'condo',
           property_type %in% c("Bungalow", "House", "Villa", "Guesthouse" ) ~ 'house',
           TRUE ~ 'other'),
         property_category = as.factor(property_category),
         room_type = as.factor(room_type),
         state = as.factor(toupper(state)),
         amenities = tolower(amenities),
         number_of_platforms_host_verifications =  str_count(host_verifications, ",") + 1,
         host_response_time = ifelse(is.na(host_response_time),"MISSING", host_response_time),
         license = as.factor(ifelse(is.na(license), "NO", license)),
         license = tolower(license),
         license = ifelse(str_detect(license, "pending"), "pending", license),
         has_license = as.factor(case_when(
           license == "no" ~ "NO",
           license == "pending" ~ "pending",
           TRUE ~ "YES"
         )),
         has_email_verified = as.factor(ifelse(str_detect(tolower(host_verifications), "email"), "YES", "NO")),
         has_phone_verified = as.factor(ifelse(str_detect(tolower(host_verifications), "phone"), "YES", "NO")),
         weekly_price = ifelse(is.na(weekly_price), 0, weekly_price),
         monthly_price = ifelse(is.na(monthly_price),0, monthly_price),
         week_perc_disc = (price-(weekly_price/7))/price*100,
         month_perc_disc = (price-(monthly_price/30))/price*100,
         week_perc_disc = ifelse(is.na(week_perc_disc), 0, week_perc_disc),
         month_perc_disc = ifelse(is.na(month_perc_disc),0, month_perc_disc),
         week_disc= as.factor(ifelse(week_perc_disc>0, "YES", "NO")),
         month_disc= as.factor(ifelse(month_perc_disc>0, "YES", "NO")),
         neighborhood_group = as.factor(ifelse(is.na(neighborhood_group), "missing", tolower(neighborhood_group))),
         category = as.factor(ifelse(host_response_rate==100, "100",
                                                      ifelse(host_response_rate>=95 & host_response_rate<100, "More than 95, but less than 100",
                                                             ifelse(host_response_rate>=90 & host_response_rate<95, "B/w 90 & 95",
                                                                    ifelse(host_response_rate>75 & host_response_rate<90, "B/w 75 & 90",
                                                                           "Below 75")))))
  ) %>%
  group_by(property_category)  %>%
  mutate(
    med_price_per_person= median(price_per_person),
    price_higherThan_median= ifelse(price_per_person> med_price_per_person, 1, 0)) %>%
  ungroup() %>%
  group_by(property_category, room_type) %>%
  mutate(price = ifelse(is.na(price), mean(price, na.rm = TRUE), price)) %>%
  ungroup()


train <- train %>% 
  mutate(city_name = ifelse(!is.na(city_ascii),toupper(city_ascii), toupper(city.x)),
         city_name = ifelse(!is.na(city_name),city_name, "Other"),
         city_name_factor = factor(city_name, levels = levels(as.factor(toupper(city_split$city_ascii)))),
         state = as.factor(state))
  
# Data Featuring for Date variables

current_date <- Sys.Date()
train <- train %>%
  mutate(host_age = as.numeric(difftime(year(current_date), year(host_since))),
         host_age = ifelse(is.na(host_age), mean(host_age, na.rm=TRUE), host_age))



# Define words to be replaced
replacements <- list(
  "TV" = c("tv", "cable tv", "TV"),
  "internet" = c("internet", "wireless internet", "ethernet connection", "pocket wifi", "Internet"),
  "climate control" = c("air conditioning", "heating", "Air conditioning"),
  "pets allowed" = c("pets live on this property", "cat(s)","dog(s)", "pets allowed", "other pet(s)", " cat(s)",	" dog(s)", "other pet(s)"),
  "Free Parking Available" = c("free parking on premises", "free parking on street", "Free Parking Available"),
  "No parking available" = c("paid parking off premises", "No parking available"),
  "Kitchen" = c("cooking basics", "kitchen", "dishes and silverware", "stove", "coffee maker", "oven", "microwave", "refrigerator", "dishwasher", "kitchen"),
  "Breakfast included" = c("breakfast", "Breakfast included"),
  "Outdoor amenities" = c("garden or backyard", "patio or balcony", "bbq grill", "gym", "pool", "waterfront", "Outdoor amenities"),
  "shower bench" = c("tub with shower bench", "roll-in shower with shower bench or chair"),
  "wheelchair accessible" = c("accessible-height bed", "accessible-height toilet", "wide clearance to shower and toilet", "wide doorway", "flat smooth pathway to front door", "grab-rails for shower and toilet", "wide hallway clearance", "wide clearance to bed", "step-free access", "disabled parking spot", "wheelchair accessible"),
  "essentials" = c("beach essentials", "shampoo", "essentials", "essentials"),
  "Safety" = c("first aid kit", "carbon monoxide detector", "fire extinguisher", "safety card", "smoke detector", "window guards", "fireplace guards", "stair gates", "outlet covers", "Safety"),
  "Family Friendly" = c("family/kid friendly", "children’s books and toys", "children’s dinnerware", "table corner guards", "Family Friendly"),
  "Baby Friendly" = c("crib", "high chair", "babysitter recommendations", "pack ’n play/travel crib", "baby monitor", "changing table", "baby bath", "Baby safe"),
  "24-hour check-in" = c("24-hour check-in"),
  "others" = c("firm matress", "firm mattress", "bed linens", "extra pillows and blankets", "laptop friendly workspace", "room-darkening shades", "iron", "hair dryer", "hangers", "doorman entry", "doorman", "private living room", "private entrance", "lockbox", "smoking allowed", "suitable for events","hot water",	"private bathroom",	"luggage dropoff allowed",	"cleaning before checkout",	"single level home",	"long term stays allowed"),
  "self check-in" = c("self check-in", "smartlock", "smart lock", "keypad", "self check-in"),
  "bath tub" = c("bathtub", "hot tub", "tub"),
  "washer / dryer" = c("washer", "dryer", "washer / dryer", "washer / dryer"),
  "intercom" = c("buzzer/wireless interco"),
  "indoor fireplace" = c("elevator in building", "elevator"),
  "bedroom locks" = c("lock on bedroom door", "bedroom locks"),
  "luxury" = c("game console",	"ev charger",	"path to entrance lit at night")
)

# Loop through replacements and apply them using case_when
new_sentence <- train$amenities
for (key in names(replacements)) {
  new_sentence <- gsub(paste(replacements[[key]], collapse = "|"), key, new_sentence)
}

# Define function to clean amenities
clean_amenities <- function(amenities) {
  # Split amenities by comma
  amenities_list <- str_split(amenities, ",\\s*")[[1]]
  # Remove duplicates and sort
  unique_amenities <- sort(unique(amenities_list))
  # Join unique amenities with comma
  cleaned_amenities <- paste(unique_amenities, collapse = ", ")
  return(cleaned_amenities)
}

# Add new_sentence to airbnb and clean amenities
train <- train %>%
  mutate(new_sentence = map_chr(new_sentence, clean_amenities)) %>%
  separate(new_sentence, into = paste0("word", 1:50), sep = ",\\s*", fill = "right") %>%
  mutate(across(starts_with("word"), ~ifelse(grepl("^translation missing:", .), NA_character_, .))) %>%
  unite(new_sentence, starts_with("word"), sep = ", ", na.rm = TRUE) %>%
  mutate(amenities = new_sentence)

airbnb_split <- train %>% select(name, amenities) %>%
  separate_rows(amenities, sep = ", ")


airbnb_pivoted <- airbnb_split %>%
  group_by(name, amenities) %>%
  summarise(count = n()) %>%
  mutate(count = ifelse(count > 0, 1, 0)) %>%
  pivot_wider(names_from = amenities, values_from = count, values_fill = 0, names_prefix = "amenity_")

train <- left_join(train, airbnb_pivoted, by = "name")
train <- train %>%
  mutate_at(vars(starts_with("amenity_")), as.factor)




# Create separate fields for each category
train <- train %>%
  mutate(
    has_food_shopping = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("restaurant", "dine", "pizza", "eat", "cafe", "coffe", "breakfast", "supermarket", "groceri", "quick", "mall", "store", "bar", "club", "nightlife"), collapse = "|")) ~ "YES",
     TRUE ~ "NO"
    )),
    has_food = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("restaurant", "dine", "pizza", "eat", "cafe", "coffe", "breakfast", "bar", "club", "nightlife"), collapse = "|"))~ "YES",
      TRUE ~ "NO"
    )),
    has_shopping = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("supermarket", "groceri", "quick", "mall","store"), collapse = "|"))~ "YES",
      TRUE ~ "NO"
    )),
    has_food_shopping = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("restaurant", "dine", "pizza", "eat", "cafe", "coffe", "breakfast", "supermarket", "groceri", "quick", "mall", "store", "bar", "club", "nightlife"), collapse = "|")) ~ "YES",
      TRUE ~ "NO"
    )),
    has_transportation = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("station", "car", "transport", "metro", "airport", "bus", "train", "subway"), collapse = "|")) ~ "YES",
      TRUE ~ "NO"
    )),
    has_entertainment_activities = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("trail", "hike", "movie", "entertain", "museum", "galleri", "theater", "cultur", "art", "park", "river", "sunset", "beach", "market"), collapse = "|"))~ "YES",
      TRUE ~ "NO"
    )),
    has_location_specific = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("york", "austin", "san", "diego", "williamsburg", "nyc", "francisco", "portland", "seattl", "nashvill", "washington", "chicago"), collapse = "|")) ~ "YES",
      TRUE ~ "NO"
    ))
  )


train <- train %>%
  mutate(
    city_new = as.factor(case_when(
      city_name == "WASHINGTON" ~ "WASHINGTON",
      city_name == "NEW ORLEANS" ~ "NEW ORLEANS",
      city_name == "NASHVILLE" ~ "NASHVILLE",
      city_name == "NEW YORK" ~ "NEW YORK",
      city_name == "AUSTIN" ~ "AUSTIN",
      city_name == "BOSTON" ~ "BOSTON",
      city_name == "PORTLAND " ~ "PORTLAND ",
      city_name == "SAN FRANCISCO" ~ "SAN FRANCISCO",
      city_name == "SEATTLE" ~ "SEATTLE",
      city_name == "SAN DIEGO" ~ "SAN DIEGO",
      city_name == "CHICAGO" ~ "CHICAGO",
      city_name == "LOS ANGELES" ~ "LOS ANGELES",
      TRUE ~ "OTHER"
    ))
  )

train <- train %>%
  group_by(city_name) %>%
  mutate(count_city = n())%>%
  ungroup()

top_cities <- train %>%
  filter(count_city > 2000)  %>%
  select(city_name)

train <- train %>%
  mutate(
    securitydep_per = ifelse(security_deposit>0, security_deposit/ price, 0),
    population = ifelse(is.na(population), mean(population, na.rm=TRUE), population)
  )

train <- train %>%
  mutate(
    car = as.factor(case_when(
      str_detect(tolower(transit), "car|uber|lyft|taxi") |
        str_detect(tolower(access), "car|uber|lyft|taxi") ~ "YES",
      TRUE ~ "NO"
    )),
    walk = as.factor(case_when(
      str_detect(tolower(transit), "walk|walkable|street|block") |
        str_detect(tolower(access), "walk|walkable|street|block") ~ "YES",
      TRUE ~ "NO"
    )),
    public_transport = as.factor(case_when(
      str_detect(tolower(transit), "bus|train|subway|station|metro|transport") |
        str_detect(tolower(access), "bus|train|subway|station|metro|transport") ~ "YES",
      TRUE ~ "NO"
    )),
    bike = as.factor(case_when(
      str_detect(tolower(transit), "bike") |
        str_detect(tolower(access), "bike") ~ "YES",
      TRUE ~ "NO"
    )),
    airport = as.factor(case_when(
      str_detect(tolower(transit), "airport") |
        str_detect(tolower(access), "airport") ~ "YES",
      TRUE ~ "NO"
    ))
  )

train <- train %>%
  mutate(
        features = ifelse(is.na(features), "", features),
        is_superhost = as.factor(ifelse(str_detect(tolower(features), "superhost"), 1, 0)),
         is_instant_bookable = as.factor(ifelse(str_detect(tolower(features), "instant "), 1, 0)),
         is_guest_req = as.factor(ifelse(str_detect(tolower(features), "guest"), 1, 0)),
         is_license_req = as.factor(ifelse(str_detect(tolower(features), "license"), 1, 0)))

#Test Cleaning

#test <- left_join(train, city_split, by = "zipcode")
# For numerical values: 
test <- test %>%
  mutate(accomodates = ifelse(is.na(accommodates), guests_included, accommodates),
         price = ifelse(is.na(price), 0 , price), #TL
         guests_included = ifelse(guests_included == 0, 1, guests_included),
         price_per_person = price/guests_included,
         is_occupied_in_maxOccupancy = ifelse(accommodates>guests_included, "NO", "YES"),
         is_occupied_in_maxOccupancy = ifelse(is.na(is_occupied_in_maxOccupancy), "NO", is_occupied_in_maxOccupancy),
         bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm=TRUE), bedrooms),
         bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm=TRUE), bathrooms),
         bathrooms_per_person = bathrooms/guests_included,
         beds = ifelse(is.na(beds), mean(beds, na.rm=TRUE), beds), #AL
         has_morebeds_than_bedrooms = ifelse((beds > bedrooms), 1, 0), #AL
         cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
         has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0 , "YES", "NO")),
         charges_for_extra = as.factor(ifelse(extra_people>0, "YES", "NO")),
         percent_cleaning_fee = ifelse(price == 0, 0, cleaning_fee/price),
         host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm=TRUE), host_total_listings_count),
         host_response_rate = ifelse(is.na(host_response_rate), mean(host_response_rate, na.rm=TRUE), host_response_rate),
         has_min_nights = as.factor(ifelse(minimum_nights > 1, "YES", "NO")),
         security_deposit = ifelse(is.na(security_deposit), 0, security_deposit),
         has_security_deposit = as.factor(ifelse(security_deposit > 0 , "YES", "NO")),
         host_acceptance = as.factor(ifelse(is.na(host_acceptance_rate),"MISSING", ifelse(parse_number(host_acceptance_rate) == 100,"ALL","SOME"))) ,
         host_response_category = as.factor(ifelse(is.na(host_response_rate),"MISSING", ifelse(host_response_rate == 100,"ALL","SOME"))))

#Data Featuring for Categorical values

test <- test %>%
  # filter(!is.na(zipcode)) %>%
  mutate(bed_type = as.factor(bed_type),
         bed_category = ifelse(bed_type == "Real Bed", "bed", "other"),
         bed_category = as.factor(bed_category),
         cancellation_policy = str_replace (cancellation_policy,"super_strict_30", "strict"),
         cancellation_policy = str_replace (cancellation_policy,"super_strict_60", "strict"),
         cancellation_policy = str_replace (cancellation_policy,"no_refunds", "strict"),
         cancellation_policy = as.factor(cancellation_policy),
         #cityName = ifelse(is.na(zipcode), toupper(city), toupper(city_split$city_ascii[parse_number(city_split$zipcode) == parse_number(zipcode)])),
         #cityName_factor = factor(cityName, levels = levels(as.factor(toupper(city_split$city_ascii)))),
         host_response_time = as.factor(host_response_time),
         property_category = case_when(
           property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ 'apartment',
           property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ 'hotel',
           property_type %in% c("Townhouse", "Condominium") ~ 'condo',
           property_type %in% c("Bungalow", "House", "Villa", "Guesthouse" ) ~ 'house',
           TRUE ~ 'other'),
         property_category = as.factor(property_category),
         room_type = as.factor(room_type),
         state = as.factor(toupper(state)),
         amenities = tolower(amenities),
         number_of_platforms_host_verifications =  str_count(host_verifications, ",") + 1,
         host_response_time = ifelse(is.na(host_response_time),"MISSING", host_response_time),
         license = as.factor(ifelse(is.na(license), "NO", license)),
         license = tolower(license),
         license = ifelse(str_detect(license, "pending"), "pending", license),
         has_license = as.factor(case_when(
           license == "no" ~ "NO",
           license == "pending" ~ "pending",
           TRUE ~ "YES"
         )),
         has_email_verified = as.factor(ifelse(str_detect(tolower(host_verifications), "email"), "YES", "NO")),
         has_phone_verified = as.factor(ifelse(str_detect(tolower(host_verifications), "phone"), "YES", "NO")),
         weekly_price = ifelse(is.na(weekly_price), 0, weekly_price),
         monthly_price = ifelse(is.na(monthly_price),0, monthly_price),
         week_perc_disc = (price-(weekly_price/7))/price*100,
         month_perc_disc = (price-(monthly_price/30))/price*100,
         week_perc_disc = ifelse(is.na(week_perc_disc), 0, week_perc_disc),
         month_perc_disc = ifelse(is.na(month_perc_disc),0, month_perc_disc),
         week_disc= as.factor(ifelse(week_perc_disc>0, "YES", "NO")),
         month_disc= as.factor(ifelse(month_perc_disc>0, "YES", "NO")),
         category = as.factor(ifelse(host_response_rate==100, "100",
                                     ifelse(host_response_rate>=95 & host_response_rate<100, "More than 95, but less than 100",
                                            ifelse(host_response_rate>=90 & host_response_rate<95, "B/w 90 & 95",
                                                   ifelse(host_response_rate>75 & host_response_rate<90, "B/w 75 & 90",
                                                          "Below 75")))))
  ) %>%
  group_by(property_category)  %>%
  mutate(
    med_price_per_person= median(price_per_person),
    price_higherThan_median= ifelse(price_per_person> med_price_per_person, 1, 0)) %>%
  ungroup() %>%
  group_by(property_category, room_type) %>%
  mutate(price = ifelse(is.na(price), mean(price, na.rm = TRUE), price)) %>%
  ungroup()


test <- test %>% 
  mutate(city_name = ifelse(!is.na(city_ascii),toupper(city_ascii), toupper(city.x)),
         city_name = ifelse(!is.na(city_name),city_name, "Other"),
         city_name_factor = factor(city_name, levels = levels(as.factor(toupper(city_split$city_ascii)))),
         state = as.factor(state))

# Data Featuring for Date variables

current_date <- Sys.Date()
test <- test %>%
  mutate(host_age = as.numeric(difftime(year(current_date), year(host_since))),
         host_age = ifelse(is.na(host_age), mean(host_age, na.rm=TRUE), host_age))




# Loop through replacements and apply them using case_when
new_sentence <- test$amenities
for (key in names(replacements)) {
  new_sentence <- gsub(paste(replacements[[key]], collapse = "|"), key, new_sentence)
}


# Add new_sentence to airbnb and clean amenities
test <- test %>%
  mutate(new_sentence = map_chr(new_sentence, clean_amenities)) %>%
  separate(new_sentence, into = paste0("word", 1:50), sep = ",\\s*", fill = "right") %>%
  mutate(across(starts_with("word"), ~ifelse(grepl("^translation missing:", .), NA_character_, .))) %>%
  unite(new_sentence, starts_with("word"), sep = ", ", na.rm = TRUE) %>%
  mutate(amenities = new_sentence)

airbnb_split <- test %>% select(name, amenities) %>%
  separate_rows(amenities, sep = ", ")


airbnb_pivoted <- airbnb_split %>%
  group_by(name, amenities) %>%
  summarise(count = n()) %>%
  mutate(count = ifelse(count > 0, 1, 0)) %>%
  pivot_wider(names_from = amenities, values_from = count, values_fill = 0, names_prefix = "amenity_")

test <- left_join(train, airbnb_pivoted, by = "name")
test <- test %>%
  mutate_at(vars(starts_with("amenity_")), as.factor)



# Create separate fields for each category
test <- test %>%
  mutate(
    has_food_shopping = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("restaurant", "dine", "pizza", "eat", "cafe", "coffe", "breakfast", "supermarket", "groceri", "quick", "mall", "store", "bar", "club", "nightlife"), collapse = "|")) ~ "YES",
      TRUE ~ "NO"
    )),
    has_food = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("restaurant", "dine", "pizza", "eat", "cafe", "coffe", "breakfast", "bar", "club", "nightlife"), collapse = "|"))~ "YES",
      TRUE ~ "NO"
    )),
    has_shopping = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("supermarket", "groceri", "quick", "mall","store"), collapse = "|"))~ "YES",
      TRUE ~ "NO"
    )),
    has_food_shopping = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("restaurant", "dine", "pizza", "eat", "cafe", "coffe", "breakfast", "supermarket", "groceri", "quick", "mall", "store", "bar", "club", "nightlife"), collapse = "|")) ~ "YES",
      TRUE ~ "NO"
    )),
    has_transportation = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("station", "car", "transport", "metro", "airport", "bus", "train", "subway"), collapse = "|")) ~ "YES",
      TRUE ~ "NO"
    )),
    has_entertainment_activities = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("trail", "hike", "movie", "entertain", "museum", "galleri", "theater", "cultur", "art", "park", "river", "sunset", "beach", "market"), collapse = "|"))~ "YES",
      TRUE ~ "NO"
    )),
    has_location_specific = as.factor(case_when(
      str_detect(tolower(neighborhood_overview), paste(c("york", "austin", "san", "diego", "williamsburg", "nyc", "francisco", "portland", "seattl", "nashvill", "washington", "chicago"), collapse = "|")) ~ "YES",
      TRUE ~ "NO"
    ))
  )


test <- test %>%
  mutate(
    city_new = as.factor(case_when(
      city_name == "WASHINGTON" ~ "WASHINGTON",
      city_name == "NEW ORLEANS" ~ "NEW ORLEANS",
      city_name == "NASHVILLE" ~ "NASHVILLE",
      city_name == "NEW YORK" ~ "NEW YORK",
      city_name == "AUSTIN" ~ "AUSTIN",
      city_name == "BOSTON" ~ "BOSTON",
      city_name == "PORTLAND " ~ "PORTLAND ",
      city_name == "SAN FRANCISCO" ~ "SAN FRANCISCO",
      city_name == "SEATTLE" ~ "SEATTLE",
      city_name == "SAN DIEGO" ~ "SAN DIEGO",
      city_name == "CHICAGO" ~ "CHICAGO",
      city_name == "LOS ANGELES" ~ "LOS ANGELES",
      TRUE ~ "OTHER"
    ))
  )

test <- test %>%
  mutate(
    car = as.factor(case_when(
      str_detect(tolower(transit), "car|uber|lyft|taxi") |
        str_detect(tolower(access), "car|uber|lyft|taxi") ~ "YES",
      TRUE ~ "NO"
    )),
    walk = as.factor(case_when(
      str_detect(tolower(transit), "walk|walkable|street|block") |
        str_detect(tolower(access), "walk|walkable|street|block") ~ "YES",
      TRUE ~ "NO"
    )),
    public_transport = as.factor(case_when(
      str_detect(tolower(transit), "bus|train|subway|station|metro|transport") |
        str_detect(tolower(access), "bus|train|subway|station|metro|transport") ~ "YES",
      TRUE ~ "NO"
    )),
    bike = as.factor(case_when(
      str_detect(tolower(transit), "bike") |
        str_detect(tolower(access), "bike") ~ "YES",
      TRUE ~ "NO"
    )),
    airport = as.factor(case_when(
      str_detect(tolower(transit), "airport") |
        str_detect(tolower(access), "airport") ~ "YES",
      TRUE ~ "NO"
    ))
  )


# Logistic Model - Model 1

train_rows <- sample(nrow(train), 0.7*nrow(train))
tr <- train[train_rows,]
va <- train[-train_rows,]

# Get the y values
tr_y <- train[train_rows,]$perfect_rating_score
va_y <- train[-train_rows,]$perfect_rating_score

train_perfect <- tr %>%
  select(guests_included, price_per_person, bathrooms,bedrooms, beds,percent_cleaning_fee ,
         extra_people, host_acceptance, host_response_category, host_listings_count,
         has_min_nights,has_security_deposit, bed_category,cancellation_policy,
         property_category, room_type,host_response_time, host_age,number_of_platforms_host_verifications,
         has_morebeds_than_bedrooms, state,
         perfect_rating_score, starts_with('amenity_'))

va_perfect <- va %>%
  select(guests_included, price_per_person, bathrooms,bedrooms, beds,percent_cleaning_fee ,
         extra_people, host_acceptance, host_response_category, host_listings_count,
         has_min_nights,has_security_deposit, bed_category,cancellation_policy,
         property_category, room_type,host_response_time, host_age,number_of_platforms_host_verifications,
         has_morebeds_than_bedrooms, state,
         perfect_rating_score, starts_with('amenity_'))




logistic_perfect <- glm(perfect_rating_score~., data = train_perfect, family = "binomial")
summary(logistic_perfect)


probs_perfect <- predict(logistic_perfect, newdata = va, type = "response")

classifications_perfect <- ifelse(probs_perfect > .45, "YES", "NO")
assertthat::assert_that(sum(is.na(classifications_perfect))==0)
table(classifications_perfect)
summary(logistic_perfect)

correct_classifications <- ifelse(classifications_perfect == va_y, 1, 0) 

CM <- confusionMatrix(data = as.factor(classifications_perfect), #predictions
                      reference = va_y)

as.numeric(CM$overall["Accuracy"])
TNR <- CM$table[1,1]/(CM$table[1,1]+CM$table[2,1])
FPR <- 1-TNR
FPR
TPR <- CM$table[1,1] / (CM$table[1,1] + CM$table[1,2])
TPR


# Cross- Validation for Model-1

tr_pred_eval <- function(train_data, valid_data){
  logistic_perfect <- glm(perfect_rating_score~., data = train_data, family = "binomial")
  probs_perfect <- predict(logistic_perfect, newdata = valid_data, type = "response")
  classifications_perfect <- ifelse(probs_perfect > .45, "YES", "NO")
  CM <- confusionMatrix(data = as.factor(classifications_perfect), #predictions
                        reference = va_y)
  return(as.numeric(CM$overall["Accuracy"]))
}

k <- 10
folds <- cut(seq(1,nrow(train)),breaks=k,labels=FALSE)
logistic_folds = rep(0, k)
for(i in 1:k){
  #Segment your data by fold using the which() function 
  valid_inds <- which(folds==i,arr.ind=TRUE)
  valid_fold <- train_perfect[valid_inds, ]
  train_fold <- train_perfect[-valid_inds, ]
  logistic_folds[i] <- tr_pred_eval(train_fold, valid_fold)
}

paste0('Average ', k,  '-fold cross-valided accuracy of Model 1: ', round(mean(logistic_folds)*100, 2))

# For learning curve for Model 1

tr_pred_eval <- function(train_data, valid_data){
  logistic_perfect <- glm(perfect_rating_score~., data = train_data, family = "binomial")
  probs_perfect <- predict(logistic_perfect, newdata = valid_data, type = "response")
  classifications_perfect <- ifelse(probs_perfect > .45, "YES", "NO")
  CM <- confusionMatrix(data = as.factor(classifications_perfect), #predictions
                        reference = va_y)
  return(as.numeric(CM$overall["Accuracy"]))
}

train_sizes = seq(from = 1000, to = 65000, by = 500)
accuracies_lc <- rep(0, length(train_sizes))

for (i in c(1:length(train_sizes))){
  
  # Get the training set size
  train_size <- train_sizes[i]
  
  # Randomly select train_size instances
  train_inds <- sample(nrow(train), train_size)
  train_lc_size <- train_perfect[train_inds,]
  
  # Train, predict, classify as usual
  
  # Store the accuracy
  accuracies_lc[i] <- tr_pred_eval(train_lc_size, va_perfect)
  
}

# Make a scatterplot of training size vs. accuracy
plot(train_sizes, accuracies_lc)

# Create a data frame from train_sizes and accuracies_lc
learning_curve_data <- data.frame(Train_Size = train_sizes, Accuracy = accuracies_lc)


ggplot(learning_curve_data, aes(x = Train_Size, y = Accuracy)) +
  geom_line(color = "blue") +  # Plot a line connecting the points
  labs(x = "Training Size", y = "Accuracy", title = "Learning Curve for Model 1 - Logistic Regression")

# fitting curve
ggplot(boost_data, aes(x = boost_data$perfect_rating_score, y = predicted_probs, color = perfect_rating_score)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  geom_point() + +
  scale_color_manual(values = c("NO" = "blue", "YES" = "red")) +
  theme_minimal()

# Lasso Model - Model 2

lasso_data <- train %>%
  select(
        guests_included, price_per_person, bathrooms,bedrooms, beds,
         extra_people,
        securitydep_per,
        availability_30,
        availability_60,
        availability_90,
        availability_365,
         price_higherThan_median,
         price,
         host_acceptance, host_response_category, host_listings_count,
         has_min_nights,has_security_deposit, bed_category,cancellation_policy,
         property_category, room_type,
         host_age,
         bathrooms_per_person,
         has_morebeds_than_bedrooms,
         category,
         city_name_factor,
         state,
         city_new,
        neighborhood_group,
         population,
         is_occupied_in_maxOccupancy,
         has_cleaning_fee,
         percent_cleaning_fee,
         number_of_platforms_host_verifications,
         has_morebeds_than_bedrooms,
         maximum_nights,
         `amenity_24-hour check-in`, `amenity_self check-in`,             
         `amenity_Kitchen`,`amenity_Outdoor amenities`,`amenity_Safety`,`amenity_TV`,`amenity_bath tub`,`amenity_climate control`, `amenity_indoor fireplace`, `amenity_intercomm`,
         `amenity_internet`,`amenity_washer / dryer`, `amenity_wheelchair accessible`,`amenity_Family Friendly`,`amenity_bedroom locks`,                 
         `amenity_essentials`, `amenity_Breakfast included`,
         `amenity_Free Parking Available`, `amenity_cat(s)`, `amenity_dog(s)`,`amenity_pets allowed`, `amenity_Baby Friendly`, `amenity_luxury`,`amenity_other pet(s)`,                   
         `amenity_No parking available`,          
         has_license,has_email_verified, has_phone_verified,
         has_shopping,
         has_food,
         has_transportation,
         has_entertainment_activities,
         month_perc_disc, week_perc_disc,
         month_disc, week_disc,
        host_total_listings_count,
        car, bike, walk, public_transport, airport,
        price,
        accomodates,
        is_superhost,
        is_instant_bookable,
        is_guest_req,
        is_license_req,
         perfect_rating_score
  )



# glmnet requires a matrix of dummy variables rather than factors
boost_lasso_x <- lasso_data %>% select(-perfect_rating_score)
dummy <- dummyVars( ~ . , data=boost_lasso_x, fullRank = TRUE)
one_hot <- predict(dummy, newdata =boost_lasso_x)

# remove the target variable from the matrix of features
X <- one_hot 
Y <- boost_data$perfect_rating_score

#Nested holdout here
# Save the "test" data for the end to evaluate the chosen value of lambda
# set seed, load data, and do train/validation split
set.seed(1)
train_n <- sample(nrow(train),.6*nrow(train))
x_train <- X[train_n,]
y_train <- Y[train_n]

x_heldout <- X[-train_n,]
y_heldout <- Y[-train_n]

#split the heldout data into 70% valid, 30% test
valid <- sample(nrow(x_heldout), .5*nrow(x_heldout))
x_valid <- x_heldout[valid,]
y_valid <- y_heldout[valid]

x_test <- x_heldout[-valid,]
y_test <- y_heldout[-valid]

grid <- 10^seq(-1,-10,length=100)
accs <- rep(0, length(grid))
for(i in c(1:length(grid))){
  lam <- grid[i] #current value of lambda
  glmout <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = lam)
  preds <- predict(glmout, newx = x_valid, type = "response")
  boost_class <- ifelse(preds>0.475, "YES", "NO")
  boost_acc <- mean(ifelse(boost_class==y_valid,1,0))
  #classifications <- ifelse(preds > .475, "YES", "NO")
  #inner_acc <- accuracy(classifications, y_valid)
  accs[i] <- boost_acc
}
plot(log10(grid), accs)
# get best-performing lambda
best_validation_index <- which.max(accs)
best_lambda <- grid[best_validation_index]
cat('Best lambda for the lasso model from validation is:', best_lambda)


x_train_valid <- rbind(x_train, x_valid)
y_train_valid <- c(y_train, y_valid)
lasso_model <- glmnet(x_train_valid, y_train_valid, family = "binomial", alpha = 1, lambda = best_lambda)
preds <- predict(lasso_model, newx = x_test, type = "response")
boost_class <- ifelse(preds>0.475, "YES", "NO")
boost_acc <- mean(ifelse(boost_class==y_test,1,0))
cat('Accuracy of the lasso model with best lambda on test data is:', boost_acc)

# coefficients of the lasso model
coef(lasso_model)

CM <- confusionMatrix(data = factor(boost_class), #predictions
                      reference = factor(y_test) )

as.numeric(CM$overall["Accuracy"])
TP <- CM$table[2,2]
TN <- CM$table[1,1]
FP <- CM$table[2,1]
FN <- CM$table[1,2]

CM$overall["Accuracy"]
TPR <- TP/(TP+FN)
TNR <- CM$table[1,1]/(CM$table[1,1]+CM$table[2,1])
FPR <- 1-TNR
TPR
FPR

# For learning curve for Model 2
tr_pred_eval <- function(x,y){
  lasso_model <- glmnet(x,y, family = "binomial", alpha = 1, lambda = best_lambda)
  preds <- predict(lasso_model, newx = x_test, type = "response")
  boost_class <- ifelse(preds>0.475, "YES", "NO")
  boost_acc <- mean(ifelse(boost_class==y_test,1,0))
  return(boost_acc)
}

train_sizes = seq(from = 500, to = 65000, by = 1000)
accuracies_lc <- rep(0, length(train_sizes))

for (i in c(1:length(train_sizes))){
  
  # Get the training set size
  train_size <- train_sizes[i]
  
  # Randomly select train_size instances
  train_inds <- sample(nrow(train), train_size)
  train_lc_size <- boost_lasso_x[train_inds,]
  y<- Y[train_inds,]
  
  # Train, predict, classify as usual
  
  # Store the accuracy
  accuracies_lc[i] <- tr_pred_eval(train_lc_size, y)
  
}

# Create a data frame from train_sizes and accuracies_lc
learning_curve_data <- data.frame(Train_Size = train_sizes, Accuracy = accuracies_lc)

# Plot the learning curve with a trend line
ggplot(learning_curve_data, aes(x = Train_Size, y = Accuracy)) +
  geom_line(color = "blue") +  # Plot a line connecting the points
  labs(x = "Training Size", y = "Accuracy", title = "Learning Curve for Model 1 - Logistic Model")



# Random Forest - Model 3

train_inst <- sample(nrow(a), .7*nrow(a))
train_ <- a[train_inst,]
train_X <- train_ %>% select(guests_included, price_per_person, bathrooms,bedrooms, beds,percent_cleaning_fee ,
                             extra_people,
                             host_acceptance, host_response_category, host_listings_count,
                             has_min_nights,has_security_deposit, bed_category,cancellation_policy,
                             property_category, room_type,
                             host_response_time,
                             host_age,
                             number_of_platforms_host_verifications,
                             has_morebeds_than_bedrooms,
                             `amenity_24-hour check-in`, `amenity_self check-in`, `amenity_Baby Friendly`, `amenity_Breakfast included`, `amenity_Free Parking Available`, `amenity_Outdoor amenities`, `amenity_bath tub`, `amenity_wheelchair accessible`,
                             has_license,has_email_verified, has_phone_verified, host_response_newcategory, weekly_price, monthly_price)
train_Y <- train_$perfect_rating_score
valid <- a[-train_inst,]
valid_X <- valid %>% select(guests_included, price_per_person, bathrooms,bedrooms, beds,percent_cleaning_fee ,
                            extra_people,
                            host_acceptance, host_response_category, host_listings_count,
                            has_min_nights,has_security_deposit, bed_category,cancellation_policy,
                            property_category, room_type,
                            host_response_time,
                            host_age,
                            number_of_platforms_host_verifications,
                            has_morebeds_than_bedrooms,
                            `amenity_24-hour check-in`, `amenity_self check-in`, `amenity_Baby Friendly`, `amenity_Breakfast included`, `amenity_Free Parking Available`, `amenity_Outdoor amenities`, `amenity_bath tub`, `amenity_wheelchair accessible`,
                            has_license,has_email_verified, has_phone_verified, host_response_newcategory, weekly_price, monthly_price)
valid_Y <- valid %>% select(perfect_rating_score)

# Model


rf.mod <- randomForest(x=train_X,
                       y=train_Y,
                       mtry=4, ntree=1000,
                       importance=TRUE)



classifications_perfect <- predict(rf.mod, newdata=train_X)


summary(classifications_perfect)
summary(train_Y)
length(rf_acc)



conf_matrix <- table(Actual = train_Y, Predicted = classifications_perfect)

# Calculate TP, FP, TN, FN
TP <- conf_matrix[2, 2]
FP <- conf_matrix[1, 2]
TN <- conf_matrix[1, 1]
FN <- conf_matrix[2, 1]

# Calculate TPR and FPR
TPR <- TP / (TP + FN)
FPR <- FP / (FP + TN)

# Print TPR and FPR
print(paste("True Positive Rate (TPR):", TPR))
print(paste("False Positive Rate (FPR):", FPR))

# Tree - Model 4

valid_instn <- sample(nrow(boost_data), 0.30*nrow(boost_data))
tree_valid <- boost_data[valid_instn,]
tree_train <- boost_data[-valid_instn,]

library(tree)

## Train a classification tree
default_tree <- tree(tree_train$perfect_rating_score ~ guests_included + price_per_person + bathrooms + bedrooms + beds + extra_people + securitydep_per + availability_30 + availability_60 + availability_90 + availability_365 + price_higherThan_median + price + host_acceptance + host_response_category + host_listings_count + has_min_nights + has_security_deposit + bed_category
                     + cancellation_policy + property_category + room_type + host_age+ bathrooms_per_person + has_morebeds_than_bedrooms + category
                     + state + city_new + neighborhood_group + is_occupied_in_maxOccupancy + has_cleaning_fee + percent_cleaning_fee
                     + number_of_platforms_host_verifications + has_morebeds_than_bedrooms + maximum_nights
                     + has_license + has_email_verified + has_phone_verified + has_food_shopping + has_shopping + has_food + has_transportation + has_entertainment_activities + month_perc_disc + week_perc_disc + month_disc + week_disc + host_total_listings_count + car + bike + walk + public_transport + airport + price + accomodates + is_superhost + is_instant_bookable + is_guest_req + is_license_req, data = tree_train)

tree_preds <- predict(default_tree, newdata = tree_train)

classifications_perfect <- as.factor(ifelse(tree_preds[,2] > 0.35, "YES", "NO"))

summary(classifications_perfect)
confusion_matrix <- confusionMatrix(data = classifications_perfect, #predictions
                                    reference = tree_train$perfect_rating_score)

# Extracting values from confusion matrix
TP <- confusion_matrix$table[2,2]  # True Positives
FN <- confusion_matrix$table[2,1]  # False Negatives
FP <- confusion_matrix$table[1,2]  # False Positives
TN <- confusion_matrix$table[1,1]  # True Negatives

# True Positive Rate (TPR) or Sensitivity or Recall
TPR <- TP / (TP + FN)

# False Positive Rate (FPR)
FPR <- FP / (FP + TN)

# Print TPR and FPR
cat("True Positive Rate (TPR):", TPR, "\n")
cat("False Positive Rate (FPR):", FPR, "\n")


library(caret)

# Define the formula
formula <- perfect_rating_score ~ guests_included + price_per_person + bathrooms + bedrooms + beds + extra_people + securitydep_per + availability_30 + availability_60 + availability_90 + availability_365 + price_higherThan_median + price + host_acceptance + host_response_category + host_listings_count + has_min_nights + has_security_deposit + bed_category + cancellation_policy + property_category + room_type + host_age + bathrooms_per_person + has_morebeds_than_bedrooms + category + state + city_new + neighborhood_group + is_occupied_in_maxOccupancy + has_cleaning_fee + percent_cleaning_fee + number_of_platforms_host_verifications + has_morebeds_than_bedrooms + maximum_nights + has_license + has_email_verified + has_phone_verified + has_food_shopping + has_shopping + has_food + has_transportation + has_entertainment_activities + month_perc_disc + week_perc_disc + month_disc + week_disc + host_total_listings_count + car + bike + walk + public_transport + airport + price + accomodates + is_superhost + is_instant_bookable + is_guest_req + is_license_req

# Define the control for cross-validation
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train the model using cross-validation
default_tree_cv <- train(formula, data = tree_train, method = "tree", trControl = ctrl)

# Make predictions on the training set
tree_preds <- predict(default_tree_cv, newdata = tree_train)

# Classify predictions based on a threshold
classifications_perfect <- as.factor(ifelse(tree_preds[,2] > 0.35, "YES", "NO"))

# Create confusion matrix
confusion_matrix <- confusionMatrix(data = classifications_perfect, reference = tree_train$perfect_rating_score)

# Extract True Positives, False Negatives, False Positives, and True Negatives
TP <- confusion_matrix$table[2, 2]
FN <- confusion_matrix$table[2, 1]
FP <- confusion_matrix$table[1, 2]
TN <- confusion_matrix$table[1, 1]

# True Positive Rate (TPR) or Sensitivity or Recall
TPR <- TP / (TP + FN)

# False Positive Rate (FPR)
FPR <- FP / (FP + TN)

# Print TPR and FPR
cat("True Positive Rate (TPR):", TPR, "\n")
cat("False Positive Rate (FPR):", FPR, "\n")

#make every tree size between 2 and 100 nodes
tree_sizes <- c(2:100)
num_sizes <- length(tree_sizes)

tr_accs <- rep(0, num_sizes)
va_accs <- rep(0, num_sizes)

# repeat for every item in the list
for (i in c(1:num_sizes)) {
  
  #retrieve the tree size and prune the tree
  inner_size <- tree_sizes[i]
  inner_tree <- prune.tree(full_tree, best = inner_size)
  
  # predict, classify, evaluate in the training and validation data
  inner_training_accuracy <- tree_predict_classify(tree_train, inner_tree, 0.5)
  inner_validation_accuracy <- tree_predict_classify(tree_valid, inner_tree, 0.5)
  
  #replace the appropriate spot in each vector
  tr_accs[i] <- inner_training_accuracy
  va_accs[i] <- inner_validation_accuracy
}

# Set the y-axis limits for better visualization
plot(tree_sizes, tr_accs, col = "blue", type = 'l', ylim = c(0.7, 0.72))

# Add the validation accuracy curve
lines(tree_sizes, va_accs, col = "red")

# Add title and legend
title("Fitting Curve for Tree")
legend("bottomright", legend = c("Training", "Validation"), col = c("blue", "red"), lty = 1)

# Boosting Model - Model 5

boost <- train %>%
  select(
    accomodates,
    population,
    guests_included,
    price_per_person, 
    bathrooms,bedrooms, beds,
    extra_people,
    price_higherThan_median,
    host_acceptance, 
    host_response_category,
    host_listings_count,
    has_min_nights,has_security_deposit,
    bed_category,
    cancellation_policy,
    property_category,
    room_type,
    host_age,
    bathrooms_per_person,
    has_morebeds_than_bedrooms,
    category,
    state,
    is_occupied_in_maxOccupancy,
         has_cleaning_fee,
         percent_cleaning_fee,
         number_of_platforms_host_verifications,
         maximum_nights,
    `amenity_24-hour check-in`, `amenity_self check-in`,             
    `amenity_Kitchen`,`amenity_Outdoor amenities`,`amenity_Safety`,`amenity_TV`,
    `amenity_bath tub`,
    `amenity_climate control`, `amenity_indoor fireplace`, `amenity_intercomm`,
    `amenity_internet`,`amenity_washer / dryer`, `amenity_wheelchair accessible`,`amenity_Family Friendly`,`amenity_bedroom locks`,                 
    `amenity_essentials`, `amenity_Breakfast included`,
    `amenity_Free Parking Available`, `amenity_cat(s)`, 
    `amenity_dog(s)`,
    `amenity_pets allowed`, `amenity_Baby Friendly`, `amenity_luxury`,`amenity_other pet(s)`,                   
    `amenity_No parking available`,          
         `amenity_shower bench`,
         `amenity_others`,
         #has_license,
    has_email_verified, has_phone_verified,
    neighborhood_group,
    has_transportation,
    has_entertainment_activities,
    car,
    availability_30,
    availability_60,
    availability_90,
    availability_365,
    bike, walk, 
    public_transport, airport,
    is_superhost,
    is_instant_bookable,
    is_guest_req,
    is_license_req,
    has_location_specific,
    month_perc_disc, week_perc_disc,
    month_disc, 
    ##
    week_disc,
         perfect_rating_score
  )


boost$perfect_rating_score <- ifelse(boost_data$perfect_rating_score == "YES", 1, 0)


#needs a numerical target variable
train_inst <- sample(nrow(train), .7*nrow(train))

boost_train <- boost[train_inst,]
boost_valid <- boost[-train_inst,]

#boost_train <- boost
#boost_valid <- boost_test
boost.mod <- gbm(perfect_rating_score~.,data=boost_train,
                 distribution="bernoulli",
                 n.trees=1000,
                 interaction.depth=10)


#classify with a cutoff and compute accuracy
boost_preds <- predict(boost.mod,newdata=boost_valid,type='response',n.trees=1000)
pred <- prediction(boost_preds, boost_valid$perfect_rating_score)
boost_class <- ifelse(boost_preds>0.486, 1, 0)


boost_acc <- mean(ifelse(boost_class==boost_valid$perfect_rating_score,1,0))
boost_acc


CM <- confusionMatrix(data = factor(boost_class), #predictions
                      reference = factor(boost_valid$perfect_rating_score) )

as.numeric(CM$overall["Accuracy"])
TP <- CM$table[2,2]
TN <- CM$table[1,1]
FP <- CM$table[2,1]
FN <- CM$table[1,2]

CM$overall["Accuracy"]
TPR <- TP/(TP+FN)
TNR <- CM$table[1,1]/(CM$table[1,1]+CM$table[2,1])
FPR <- 1-TNR
TPR
FPR

# Hypertunning parameters

cutoff_values <- seq(0.45, 0.52, by = 0.001)

# Initialize vectors to store TPR and FPR for each cutoff
tpr_values <- numeric(length(cutoff_values))
fpr_values <- numeric(length(cutoff_values))

# Loop over each cutoff value
for (i in seq_along(cutoff_values)) {
  # Apply cutoff to boost_preds
  boost_class <- ifelse(boost_preds > cutoff_values[i], 1, 0)
  
  # Calculate confusion matrix
  CM <- confusionMatrix(data = factor(boost_class),
                        reference = factor(boost_valid$perfect_rating_score))
  
  # Calculate True Positive Rate (TPR)
  TP <- CM$table[2, 2]
  FN <- CM$table[1, 2]
  tpr_values[i] <- TP / (TP + FN)
  
  # Calculate False Positive Rate (FPR)
  FP <- CM$table[2, 1]
  TN <- CM$table[1, 1]
  fpr_values[i] <- FP / (FP + TN)
}

# Output the TPR and FPR values
result <- data.frame(cutoff = cutoff_values, TPR = tpr_values, FPR = fpr_values)
  
result



## Best cutoff --------------------------------------------------------------------------
library(caret)  # for confusionMatrix

# Initialize variables to track the best cutoff
best_cutoff <- NULL
max_tpr <- 0

# Iterate over possible cutoffs from 0 to 1 with a small step
for (cutoff in seq(0, 1, by = 0.001)) {
  # Classify predictions with the current cutoff
  boost_class <- ifelse(boost_preds > cutoff, 1, 0)
  
  # Calculate confusion matrix
  CM <- confusionMatrix(data = factor(boost_class),
                        reference = factor(boost_valid$perfect_rating_score))
  
  # Extract accuracy and FPR
  current_accuracy <- as.numeric(CM$overall["Accuracy"])
  TP <- CM$table[2, 2]
  TN <- CM$table[1, 1]
  FP <- CM$table[2, 1]
  FN <- CM$table[1, 2]
  
  # Calculate TPR and FPR
  TPR <- TP / (TP + FN)
  TNR <- TN / (TN + FP)
  FPR <- 1 - TNR
  
  # Check if this cutoff meets the FPR criterion and has a higher TPR
  if (FPR < 0.1 && TPR > max_tpr) {
    max_tpr <- TPR
    best_cutoff <- cutoff
  }
}

# Output the best cutoff found and its corresponding TPR
print(paste("Best cutoff: ", best_cutoff, " with TPR: ", max_tpr))

# Final ----------------------------------

classifications_perfect <- ifelse(boost_preds > 0.486, "YES", "NO")
#classifications_perfect <- ifelse(is.na(classifications_perfect), "NO", classifications_perfect)
assertthat::assert_that(sum(is.na(classifications_perfect))==0)
table(classifications_perfect)

#this code creates sample outputs in the correct format
write.table(classifications_perfect, "perfect_rating_score_group7.csv", row.names = FALSE)



# Boosting Model with features identified using Lasso regularization -  Final Model 

boost_data <- train %>%
  select(
    guests_included, price_per_person, bathrooms,bedrooms, beds,
    extra_people,
    securitydep_per,
    availability_30,
    availability_60,
    availability_90,
    availability_365,
    price_higherThan_median,
    price,
    host_acceptance, host_response_category, host_listings_count,
    has_min_nights,has_security_deposit, bed_category,cancellation_policy,
    property_category, room_type,
    #host_response_time, 
    host_age,
    bathrooms_per_person,
    has_morebeds_than_bedrooms,
    category,
    # city_name_factor,
    state,
    city_new,
    neighborhood_group,
    #population,
    is_occupied_in_maxOccupancy,
    has_cleaning_fee,
    percent_cleaning_fee,
    number_of_platforms_host_verifications,
    has_morebeds_than_bedrooms,
    maximum_nights,
    #`amenity_24-hour check-in`, `amenity_self check-in`, `amenity_Baby Friendly`, `amenity_Breakfast included`, `amenity_Free Parking Available`, `amenity_Outdoor amenities`, `amenity_bath tub`, `amenity_wheelchair accessible`, 
    `amenity_24-hour check-in`, `amenity_self check-in`,             
    `amenity_Kitchen`,`amenity_Outdoor amenities`,`amenity_Safety`,`amenity_TV`,`amenity_bath tub`,`amenity_climate control`, `amenity_indoor fireplace`, `amenity_intercomm`,
    `amenity_internet`,`amenity_washer / dryer`, `amenity_wheelchair accessible`,`amenity_Family Friendly`,`amenity_bedroom locks`,                 
    `amenity_essentials`, `amenity_Breakfast included`,
    `amenity_Free Parking Available`, `amenity_cat(s)`, `amenity_dog(s)`,`amenity_pets allowed`, `amenity_Baby Friendly`, `amenity_luxury`,`amenity_other pet(s)`,                   
    `amenity_No parking available`,          
    `amenity_shower bench`,
    #`amenity_others`,
    has_license,has_email_verified, has_phone_verified,
    has_food_shopping,
    has_shopping,
    has_food,
    has_transportation,
    has_entertainment_activities,
    # has_location_specific,
    month_perc_disc, week_perc_disc,
    month_disc, week_disc,
    #host_name,
    #host_since,
    #host_neighbourhood,
    host_total_listings_count,
    car, bike, walk, public_transport, airport,
    #zipcode,
    #population,
    #density,
    price,
    accomodates,
    #market,
    is_superhost,
    is_instant_bookable,
    is_guest_req,
    is_license_req,
    perfect_rating_score
  )




boost <- train %>%
  select(
    accomodates,
    #guests_included,
    price_per_person, 
    bathrooms,bedrooms, beds,
    # extra_people,
    price_higherThan_median,
    # host_acceptance, 
    #host_response_category,
    ## 
    host_listings_count,
    ## 
    has_min_nights,has_security_deposit,
    #bed_category,
    cancellation_policy,
    property_category,
    ##room_type,
    #host_response_time, 
    ##
    host_age,
    #bathrooms_per_person,
    ##
    has_morebeds_than_bedrooms,
    category,
    # city_name_factor,
    #state,
    city_new,
    #population,
    ##
    is_occupied_in_maxOccupancy,
    has_cleaning_fee,
    percent_cleaning_fee,
    ##number_of_platforms_host_verifications,
    #maximum_nights,
    #`amenity_24-hour check-in`, `amenity_self check-in`, `amenity_Baby Friendly`, `amenity_Breakfast included`, `amenity_Free Parking Available`, `amenity_Outdoor amenities`, `amenity_bath tub`, `amenity_wheelchair accessible`, 
    `amenity_24-hour check-in`, `amenity_self check-in`,             
    `amenity_Kitchen`,`amenity_Outdoor amenities`,`amenity_Safety`,`amenity_TV`,
    #`amenity_bath tub`,
    `amenity_climate control`, `amenity_indoor fireplace`, `amenity_intercomm`,
    `amenity_internet`,`amenity_washer / dryer`, `amenity_wheelchair accessible`,`amenity_Family Friendly`,`amenity_bedroom locks`,                 
    `amenity_essentials`, `amenity_Breakfast included`,
    `amenity_Free Parking Available`, `amenity_cat(s)`, 
    #`amenity_dog(s)`,
    `amenity_pets allowed`, `amenity_Baby Friendly`, `amenity_luxury`,`amenity_other pet(s)`,                   
    #`amenity_No parking available`,          
    #`amenity_shower bench`,
    `amenity_others`,
    #has_license,
    has_email_verified, has_phone_verified,
    neighborhood_group,
    #has_food_shopping,
    ##has_shopping,
    ##has_food,
    ##
    has_transportation,
    ##
    has_entertainment_activities,
    #car,
    availability_30,
    #availability_60,
    availability_90,
    availability_365,
    bike, walk, 
    public_transport, airport,
    # has_location_specific,
    #month_perc_disc, week_perc_disc,
    #month_disc, 
    ##
    week_disc,
    is_superhost,
    is_instant_bookable,
    is_guest_req,
    is_license_req,
    perfect_rating_score
  )

boost_test <- test %>%
  select(
    accomodates,
    #guests_included,
    price_per_person, 
    bathrooms,bedrooms, beds,
    # extra_people,
    price_higherThan_median,
    # host_acceptance, 
    #host_response_category,
    ## 
    host_listings_count,
    ## 
    has_min_nights,has_security_deposit,
    #bed_category,
    cancellation_policy,
    property_category,
    ##room_type,
    #host_response_time, 
    ##
    host_age,
    #bathrooms_per_person,
    ##
    has_morebeds_than_bedrooms,
    category,
    # city_name_factor,
    #state,
    city_new,
    #population,
    ##
    is_occupied_in_maxOccupancy,
    has_cleaning_fee,
    percent_cleaning_fee,
    ##number_of_platforms_host_verifications,
    #maximum_nights,
    #`amenity_24-hour check-in`, `amenity_self check-in`, `amenity_Baby Friendly`, `amenity_Breakfast included`, `amenity_Free Parking Available`, `amenity_Outdoor amenities`, `amenity_bath tub`, `amenity_wheelchair accessible`, 
    `amenity_24-hour check-in`, `amenity_self check-in`,             
    `amenity_Kitchen`,`amenity_Outdoor amenities`,`amenity_Safety`,`amenity_TV`,
    #`amenity_bath tub`,
    `amenity_climate control`, `amenity_indoor fireplace`, `amenity_intercomm`,
    `amenity_internet`,`amenity_washer / dryer`, `amenity_wheelchair accessible`,`amenity_Family Friendly`,`amenity_bedroom locks`,                 
    `amenity_essentials`, `amenity_Breakfast included`,
    `amenity_Free Parking Available`, `amenity_cat(s)`, 
    #`amenity_dog(s)`,
    `amenity_pets allowed`, `amenity_Baby Friendly`, `amenity_luxury`,`amenity_other pet(s)`,                   
    #`amenity_No parking available`,          
    #`amenity_shower bench`,
    `amenity_others`,
    #has_license,
    has_email_verified, has_phone_verified,
    neighborhood_group,
    #has_food_shopping,
    ##has_shopping,
    ##has_food,
    ##
    has_transportation,
    ##
    has_entertainment_activities,
    #car,
    availability_30,
    #availability_60,
    availability_90,
    availability_365,
    bike, walk, 
    public_transport, airport,
    # has_location_specific,
    #month_perc_disc, week_perc_disc,
    #month_disc, 
    ##
    week_disc,
    is_superhost,
    is_instant_bookable,
    is_guest_req,
    is_license_req
  )


boost$perfect_rating_score <- ifelse(boost_data$perfect_rating_score == "YES", 1, 0)


## Best cutoff --------------------------------------------------------------------------
library(caret)  # for confusionMatrix

# Initialize variables to track the best cutoff
best_cutoff <- NULL
max_tpr <- 0

# Iterate over possible cutoffs from 0 to 1 with a small step
for (cutoff in seq(0, 1, by = 0.001)) {
  # Classify predictions with the current cutoff
  boost_class <- ifelse(boost_preds > cutoff, 1, 0)
  
  # Calculate confusion matrix
  CM <- confusionMatrix(data = factor(boost_class),
                        reference = factor(boost_valid$perfect_rating_score))
  
  # Extract accuracy and FPR
  current_accuracy <- as.numeric(CM$overall["Accuracy"])
  TP <- CM$table[2, 2]
  TN <- CM$table[1, 1]
  FP <- CM$table[2, 1]
  FN <- CM$table[1, 2]
  
  # Calculate TPR and FPR
  TPR <- TP / (TP + FN)
  TNR <- TN / (TN + FP)
  FPR <- 1 - TNR
  
  # Check if this cutoff meets the FPR criterion and has a higher TPR
  if (FPR < 0.1 && TPR > max_tpr) {
    max_tpr <- TPR
    best_cutoff <- cutoff
  }
}

# Output the best cutoff found and its corresponding TPR
print(paste("Best cutoff: ", best_cutoff, " with TPR: ", max_tpr))

boost_train <- boost
boost_valid <- boost_test
boost.mod <- gbm(perfect_rating_score~.,data=boost_train,
                 distribution="bernoulli",
                 n.trees=1000,
                 interaction.depth=10)
boost_preds <- predict(boost.mod,newdata=boost_valid,type='response',n.trees=1000)


boost_class <- ifelse(boost_preds>0.499, 1, 0)

classifications_perfect <- ifelse(boost_preds > 0.499, "YES", "NO")
#classifications_perfect <- ifelse(is.na(classifications_perfect), "NO", classifications_perfect)
assertthat::assert_that(sum(is.na(classifications_perfect))==0)
table(classifications_perfect)

#this code creates sample outputs in the correct format
write.table(classifications_perfect, "perfect_rating_score_group7.csv", row.names = FALSE)



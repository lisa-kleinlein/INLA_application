london_weekdays <- read.csv("archive/london_weekdays.csv")
london_weekdays$weekend_listing <- 0
london_weekdays$room_type_entireHomeApt <- as.numeric(london_weekdays$room_type == "Entire home/apt")
london_weekdays$room_type_privateRoom <- as.numeric(london_weekdays$room_type == "Private room")
london_weekdays$room_type_sharedRoom <- as.numeric(london_weekdays$room_type == "Shared room")
london_weekdays$room_shared <- as.numeric(as.logical(london_weekdays$room_shared))
london_weekdays$room_private <- as.numeric(as.logical(london_weekdays$room_private))
london_weekdays$host_is_superhost <- as.numeric(as.logical(london_weekdays$host_is_superhost))
london_weekdays$guest_satisfaction_overall_10 <- london_weekdays$guest_satisfaction_overall / 10
london_weekdays$attr_index_norm_10 <- london_weekdays$attr_index_norm / 10 
london_weekdays$offers <- as.character(NA)
for(i in 1:nrow(london_weekdays)) {
  if (london_weekdays[i, ]$multi == 0 & london_weekdays[i, ]$biz == 0) {
    london_weekdays$offers[i] <- "1 offer"
  } else if (london_weekdays[i, ]$multi == 1 & london_weekdays[i, ]$biz == 0) {
    london_weekdays$offers[i] <- "2-4 offers"
  } else if (london_weekdays[i, ]$multi == 0 & london_weekdays[i, ]$biz == 1) {
    london_weekdays$offers[i] <- "more than 4 offers"
  }
}
london_weekdays$offers <- as.factor(london_weekdays$offers)



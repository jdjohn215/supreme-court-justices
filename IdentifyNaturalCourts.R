library(tidyverse)

justices <- read_csv("data/Justices.csv")

# define justice service dates
# i define last date of service as the day BEFORE termination
# this way justices don't incorrectly overlap when they resign
# on the same day their replacement is sworn in

justice.service <- justices %>%
  mutate(service_start = judicial_oath_taken,
         service_end = date_service_terminated - 1) %>%
  select(index, service_start, service_end) %>%
  # if date_service_missing is NA, replace service_end with today
  mutate(service_end = replace(service_end, is.na(service_end), Sys.Date()))

# find days on which the court's composition changes
# these are (1) dates of appoint or (2) dates when a judge leaves
court.change.dates <- justices %>%
  select(judicial_oath_taken, date_service_terminated) %>%
  mutate(date_service_terminated = replace(date_service_terminated,
                                           is.na(date_service_terminated),
                                           Sys.Date())) %>%
  pivot_longer(cols = everything(), values_to = "date") %>%
  group_by(date) %>%
  summarise() %>% # remove duplicates
  arrange() %>% # sort by date (ascending)
  pull(date) # pull vector of dates


# identify the composition of the court on each day that the
# composition changed

# this function returns the court composition on a given date
build_court <- function(date){
  print(date)
  this.court <- justice.service %>%
    rowwise() %>%
    mutate(on_court = ifelse(as.Date(date) %in% seq.Date(service_start,
                                                         service_end,
                                                         "day"),
                             TRUE, FALSE)) %>%
    ungroup() %>%
    filter(on_court == TRUE)
  tibble(date = date,
         justices = list(sort(this.court$index)))
}

# test the function
# the output is a tibble with a date column and a list column...
# containing the numeric identifiers of each justice on the court
build_court(Sys.Date())

# run the function on every date when the court's composition changed
court.compositions <- map_df(court.change.dates, build_court)

# define starting and ending dates for each combination of justices
natural.courts <- court.compositions %>%
  rename(start_date = date) %>%
  # each court ends the day before the next court begins
  mutate(end_date = (lead(start_date, 1) - 1),
         # replace the last value, which is missing, with today
         end_date = replace(end_date, is.na(end_date), Sys.Date())) %>%
  select(start_date, end_date, justices)

natural.courts2 <- natural.courts %>%
  unnest(cols = justices) %>%
  group_by(start_date, end_date) %>%
  summarise(justices = paste(justices, collapse = "-"),
            total_justices = n()) %>%
  ungroup()

# sanity test, how many courts have more than 9 justices
# just the Lincoln courts: https://en.wikipedia.org/wiki/Tenth_Circuit_Act_of_1863
natural.courts2 %>% filter(total_justices > 9)

write.csv(natural.courts2, "data/NaturalCourts.csv")

library(tidyverse)
library(rvest) # package for simple web scraping

# retrieve the content from this webpage
orig.justices <- read_html("https://www.supremecourt.gov/about/members_text.aspx") %>%
  # use SelectorGadget to identify the css selector you want: https://rvest.tidyverse.org/articles/articles/selectorgadget.html
  # in this case, i want to retrieve both the chief justice and associate justice
  # tables, so i use `html_nodes` rather than `html_node`
  html_nodes(".justicetable") %>%
  # this creates a list of two tables
  html_table() %>%
  # combines the two tables into one, no issues because column names are identical
  bind_rows() %>%
  # standardize the column names
  janitor::clean_names() %>%
  # remove any empty rows or columns that can occur when scraping tables
  janitor::remove_empty()

glimpse(orig.justices)

# clean up the data
clean.justices <- orig.justices %>%
  # remove the annotations from the date fields
  mutate(judicial_oath_taken = str_remove(judicial_oath_taken, coll("(a) ")),
         judicial_oath_taken = str_remove(judicial_oath_taken, coll("(b ")),
         judicial_oath_taken = str_remove(judicial_oath_taken, coll("(c") ),
         date_service_terminated = str_remove(date_service_terminated, coll("*"))) %>%
  # remove the commas from dates
  mutate(judicial_oath_taken = str_remove(judicial_oath_taken, ","),
         date_service_terminated = str_remove(date_service_terminated, ",")) %>%
  # convert date columns from character class to date class
  # see here for the date format codes: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime
  mutate(judicial_oath_taken = as.Date(judicial_oath_taken,
                                       format = "%B %d %Y"),
         date_service_terminated = as.Date(date_service_terminated,
                                           format = "%B %d %Y")) %>%
  # add a numeric identifier for each appointment
  arrange(judicial_oath_taken) %>% # arrange by date appointed
  mutate(index = 1:n())

glimpse(clean.justices)
write_csv(clean.justices, "data/Justices.csv")

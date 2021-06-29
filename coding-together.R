# ------------------
# DATA VISUALIZATION
# ------------------

# load the tidyverse collection of packages and other packages we will use

library(tidyverse)
library(here)
library(janitor)
library(tidykids)

# loading data
d <- read_csv(here("data", "sci-online-classes.csv"))

# making the names easier to read and type 
d <- clean_names(d)

# inspecting the data
glimpse(d)
View(d)

# These lines did not run as I expected - is str_detect() working differently?
# d %>%
#   filter(str_detect("frsc", subject))

# selecting particular variables
d <- d %>% 
  select(student_id:gender, time_spent_hours, int, pc, uv)

# inspecting the data again
glimpse(d)
View(d)

# visualizing a distribution

d %>% 
  ggplot(aes(x = final_grade_cems)) +
  geom_histogram(bins = 100)

# visualizing a scatter plot and fitted model

d %>% 
  ggplot(aes(x = time_spent_hours, y = final_grade_cems)) +
  geom_point()

d %>% 
  ggplot(aes(x = time_spent_hours, y = final_grade_cems)) +
  geom_smooth(method = "lm")

d %>% 
  ggplot(aes(x = time_spent_hours, y = final_grade_cems, color = subject)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# visualizing facets (small multiples) of a plot

d %>% 
  ggplot(aes(x = final_grade_cems)) +
  geom_histogram() + 
  facet_wrap(~subject)

# visualizing time series data

tidykids %>% # different data set
  filter(variable %in% c("PK12ed"),
         state %in% c("South Carolina", "Tennessee", "Michigan")) %>%
  ggplot(aes(x = year, y = inf_adj_perchild, color = state, group = state)) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_line()

# visualizing bars/columns

d %>% 
  group_by(subject) %>% 
  summarize(mean_time_spent_hours = mean(time_spent_hours, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(subject, mean_time_spent_hours), y = mean_time_spent_hours)) +
  geom_col()

# ---------------------
# QUALITATIVE ANALYSIS
# ---------------------

library(tidytext)

# load the text data
# these are responses to the question 
# what do you love about teaching/why did you enter the profession
td <- read_csv(here("data", "qual-data.csv"))

# making the names easier to read and type 
td <- clean_names(td)

# inspect the data

glimpse(td)
View(td)

# creating tokens

td <- td %>% 
  unnest_tokens(word, response)

# inspecting the data 

glimpse(td)
View(td)

# remove stop words
glimpse(stop_words)
View(stop_words)

# this is if we wanted to use only a single set of stop words
stop_word_snowball <- stop_words %>% 
  filter(lexicon == "snowball")

# below, we could replace stop_words with stop_words_snowball
td <- td %>% 
  anti_join(stop_words)

glimpse(td)

# examine the most frequent terms

td %>% 
  count(word, sort = TRUE) %>% 
  mutate(proportion = n / 122)

# detect specific words

glimpse(td)

td %>% 
  mutate(mention_love = str_detect("love", word))

# carry out a sentiment analysis

bing_sentiment <- get_sentiments("bing")

td %>% 
  left_join(bing_sentiment) %>% 
  count(sentiment)

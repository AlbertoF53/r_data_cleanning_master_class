#***************  AI Course Lesson 2 ************************
#*
#* David Keyes
#* March 6, 2025
#* 
#*
#* David demonstrates how to add copilot into R Studio
#* 
#*
#****************************************************************************

# Loading packages --------------------------------------------------------


library(tidyverse)
library(readr)
library(ellmer)

# Importing data ----------------------------------------------------------

survey_english <- 
  read_tsv("2020-english-survey-final.tsv") |> 
  janitor::clean_names()


  
# Data analysis -----------------------------------------------------------

#* Counting: We want to examine the Qr_experience variable. 
#* Want to know how many people rated their expertise

#* Counting survey responses on the qr_experience variable

survey_english |> 
  count(qr_experience, sort = TRUE)


survey_english |> 
  count(qr_experience, sort = TRUE)

#* Count the responses on the qr_experience variable, grouped by 
#* qr_how_often_used. Keep the result in a tidy format
#* Use the native pipe (|>) not the tidyverse pipe (%>%)

survey_english |> 
  count(qr_how_often_used, qr_experience, sort = TRUE)



#* Create a bar plot of the qr_experience variable


survey_english |> 
  count(qr_experience, sort = TRUE) |> 
  ggplot(aes(x = fct_reorder(qr_experience, n), y = n)) +
  geom_col() +
  labs(title = "QR Code Experience",
       x = "Experience",
       y = "Count") +
  theme_minimal()

# Create a bar plot of qr_how_often_used by qr_experience

#Create a bar plot of qr_how_often_used by qr_experience

survey_english |> 
  count(qr_how_often_used, qr_experience, sort = TRUE) |> 
  ggplot(aes(x = qr_how_often_used, y = n, fill = qr_experience)) +
  geom_col(position = "dodge") +
  labs(title = "QR Code Experience by Frequency of Use",
       x = "Experience of Use",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1")



survey_english |> 
  count(qr_how_often_used, qr_experience, sort = TRUE) |> 
  ggplot(aes(x = qr_how_often_used, y = n, fill = qr_experience)) +
  geom_col(position = "dodge") +
  labs(title = "QR Code Experience by Frequency of Use",
       x = "Experience of Use",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1")



# survey_english |> 
#   count(qr_how_often_used, qr_experience, sort = TRUE) |> 
#   ggplot(aes(x = qr_how_often_used, y = n, fill = qr_experience)) +
#   geom_col(position = "dodge") +
#   labs(title = "QR Code Experience by Frequency of Use",
#        x = "Experience of Use",
#        y = "Count") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   scale_fill_brewer(palette = "Set1")

#* Using {ellmer} as an alternative AI package

library(ellmer)
library(tidyverse)


chat <- chat_openai()

#What is the easiest way to create a histogram in r?




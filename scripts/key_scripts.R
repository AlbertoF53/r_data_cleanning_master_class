


# When using group_by() add groups = 'drop'  ---------------------------------
# group = 'drop' allows one to restore the original data structure
penguins_wo_NAs |> 
  group_by(species, island) |> 
  summarise(
    mean_flipper_length = mean(flipper_length_mm),
    sd_flipper_length = sd(flipper_length_mm),
    flipper_lengths = list(flipper_length_mm),
    .groups = 'drop')



# The unique() function ---------------------------------------------------
#* In order to avoid a long list of 344 means by species, 
#* use the unique() function. 


palmerpenguins::penguins |> 
  mutate(
    mean_column = mean(bill_length_mm, na.rm = TRUE),
    .by = species
  ) |>
  select(species,mean_column) |>
  unique()


# Centering a variable by its mean using scale() function ----------------------
#* Using the function scale()

penguins |> 
  filter(!is.na(sex)) |> 
  mutate(bill_length_mean_centered = scale(bill_length_mm), 
         .by = species) |> 
  ggplot() +
  geom_density(aes(x = bill_length_mean_centered, 
                   fill = species,
                   alpha = 0.5))


# complete() function to display empty combinations -----------------------
# Notice the assignment of Zeroes to empty combinations using list

penguins_wo_NAs |> 
  summarise(
    mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE),
    sd_flipper_length = sd(flipper_length_mm, na.rm = TRUE),
    flipper_lengths = list(flipper_length_mm),
    .by = c(species,island) ) |> 
  complete(
    species, island,
    fill = list(mean_flipper_length = 0,
                sd_flipper_length = 0)
  )


# across() function along .names ------------------------------------------

#Short form

penguins_wo_NAs |> 
  summarise(across(.cols = 3:6, 
                   function(x) mean(x, na.rm = TRUE),
                   .names = 'mean_{.col}'))

#Long form
penguins_wo_NAs |> 
  summarise(
    across(.cols = c('bill_length_mm',
                     'bill_depth_mm',
                     'flipper_length_mm'),
           function(x) mean(x, na.rm = TRUE),
           .names = 'Mean_{.col}')
  )

#where(is.numeric)
penguins_wo_NAs |> 
  summarise(across(where(is.numeric), 
                   function(x) mean(x, na.rm = TRUE),
                   .names = 'mean_{.col}') )


# Removing unwanted strings using across() --------------------------------

penguins_wo_NAs |> 
  summarise(across(
    .cols = c("bill_length_mm",
              "bill_depth_mm",
              "flipper_length_mm",
              "body_mass_g"),
    .fns = mean,
    .names = 'mean_{.col |> 
    str_remove("_mm") |> 
    str_remove("_g")}'
  ))

# Estimating skewnes and other descriptive using across

palmerpenguins::penguins |> 
  na.omit() |> 
  summarise(
    across(
      .cols = bill_length_mm,
      .fns = list(
        min = min,
        max = max,
        median = median,
        standard_dev = sd,
        skewness = e1071::skewness),
      .names = '{.fn}'
    )
  )


# Extracting numbers parse_numbers() -----------------------------------------

tibble(
  text1 = c("I have 10 apples", "I have 20 appples"),
  text2 = c("I have 30 apples", "I have 40 apples")
) |> 
  mutate(
    across(
      .cols = everything(),
      .fns = parse_number
    )
  )


# Selecting variables made up of counts -----------------------------------

library(modeldata)

ames_counts <- modeldata::ames |> 
  select(where(function(x) all(x %in% 0:10))) |> 
  skimr::skim()
  

# Selecting variables made up of continuous variables ---------------------


ames_numeric <- modeldata::ames |> 
  select(where(function(x) !all(x %in% 0:10))) |> 
  skimr::skim()


# Reframe, range and type -------------------------------------------------
penguins_wo_NAs |> 
  reframe(
    type = c("min", "max"),
    range_bill_lenght_mm = range(bill_length_mm),
    .by = c(species,island))


ames::ames |> 
  na.omit() |> 
  summarise(
    across(
      .cols = sale_price,
      .fns = list(
        min = min,
        max = max,
        median = median,
        standard_dev = sd,
        skewness = e1071::skewness),
      .names = '{.fn}'
    )
  )


# Lumping neighborhood column into five groups

ames_top_five <- modeldata::ames |> 
  na.omit() |> 
  janitor::clean_names() |> 
  mutate(
    top_five_neighborhood = fct_lump_n(
      neighborhood, n = 5)) |> 
  count(top_five_neighborhood) |> 
  print(n = Inf)


#* Reframe function
#* It works like a mutate() function.
#* The variable "type" is made up of a range of minimum and maximum values
#* the sale_price variable captures the values produced by the function range()
#* the .by = function groups the price ranges by neighborhood

modeldata::ames |> 
  janitor::clean_names() |> 
  reframe(
    type = c("min", "max"),
    sale_price = range(sale_price),
    .by = neighborhood
  )


# Range & across ----------------------------------------------------------
#Combining reframe with across


penguins_wo_NAs |> 
  reframe(
    type = c("min", "max"),
    across(.cols = c("bill_length_mm",
                     "bill_depth_mm",
                     "flipper_length_mm",
                     "body_mass_g"),
           .fns = range
    ),
    .by = species)


# Table with range --------------------------------------------------------
#* Example of creating a summary statistics table with ranges.
#* First we use reframe() function to specify ranges 
#* Next we use across
#* Next we pivot longer and then pivot wider


penguins_wo_NAs |> 
  reframe(
    type = c("min", "max"),
    across(.cols = c("bill_length_mm",
                     "bill_depth_mm",
                     "flipper_length_mm",
                     "body_mass_g"),
           .fns = list(range)
    ),
    .by = species) |> 
  pivot_longer(
    cols = 3:6,
    names_to = "quantity",
    values_to = 'value'
  ) |> 
  pivot_wider(
    id_cols = c(quantity),
    names_from = c('species', 'type')
  ) |> 
  gt()


# Glueing summary stats labels and column names ---------------------------

penguins_wo_NAs |> 
  summarise(
    across(
      .cols = c("bill_length_mm",
                "bill_depth_mm",
                "flipper_length_mm",
                "body_mass_g"),
      #Adding labels to the summary statistics
      .fns = list(avg = mean, std = sd),
      #Glueing summary stat labels & column names 
      .names = '{.fn}_{.col |> str_remove("_mm")}'),
    .by = c(species,island))


# Another glueing approach using function(x) ------------------------------

palmerpenguins::penguins |> 
  summarise(across(
    .cols = c(where(is.numeric), -year),
    .fns = list(avg = function(x) mean(x, na.rm = TRUE)),
    .names = '{.fn}_{.col |> str_remove("_mm")}'
  ),
  .by = c(species,island))






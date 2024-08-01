# Create the 'correct' column
look <- look %>%
  mutate(
    correct = response == correct_response
  )

# Create the 'shifted_correct' column by shifting the 'correct' column up by one
look <- look %>%
  mutate(
    shifted_correct = lead(correct, default = TRUE)
  )


# Initialize the last_correct_rule column
look <- look %>%
  mutate(perseverative_rule = NA_character_)



# Initialize the perseverative_rule column and a variable to store the current value
look$perseverative_rule <- NA_character_
current_value <- NA_character_

# Loop through each row to update perseverative_rule
for (i in 1:nrow(look)) {
  if (look$shifted_correct[i]) {
    current_value <- look$matching_rule[i]
  }
  look$perseverative_rule[i] <- current_value
}



# Determine the attributes of the chosen card based on the response
look <- look %>%
  mutate(
    chosen_color = case_when(
      response == "a" ~ color1,
      response == "b" ~ color2,
      response == "c" ~ color3,
      response == "d" ~ color4,
      TRUE ~ NA_character_
    ),
    chosen_shape = case_when(
      response == "a" ~ shape1,
      response == "b" ~ shape2,
      response == "c" ~ shape3,
      response == "d" ~ shape4,
      TRUE ~ NA_character_
    ),
    chosen_number = case_when(
      response == "a" ~ number1,
      response == "b" ~ number2,
      response == "c" ~ number3,
      response == "d" ~ number4,
      TRUE ~ NA_integer_
    )
  )


# Calculate perseverative errors with updated conditions using the shifted_correct
look <- look %>%
  mutate(
    is_incorrect = !shifted_correct,
    rule_persisted = !is.na(perseverative_rule),
    color_condition = if_else(rule_persisted, perseverative_rule == "color" & matching_rule != "color" & chosen_color == response_color, FALSE),
    shape_condition = if_else(rule_persisted, perseverative_rule == "shape" & matching_rule != "shape" & chosen_shape == response_shape, FALSE),
    number_condition = if_else(rule_persisted, perseverative_rule == "number" & matching_rule != "number" & chosen_number == response_number, FALSE),
    perseverative_error = rule_persisted & (color_condition | shape_condition | number_condition)
  )


library(dplyr)

# Add a column to count the cumulative number of perseverative errors for each participant
look <- look %>%
  group_by(jatosStudyResultId) %>%  # Use 'jatosStudyResultId' to group by participant
  mutate(
    cum_perseverative_error = cumsum(perseverative_error)
  ) %>%
  ungroup()

# Merge cum_perseverative_error into merged_data
merged_data <- merged_data %>%
  left_join(exp_result %>% select(jatosStudyResultId, cum_perseverative_error), by = "jatosStudyResultId")

# View the updated dataset
print(look)

# View the updated dataset
print(look)

# View the updated dataset
print(look)

### Libraries ##################################################################
packages <- c("magick", "stringr", "tidyverse", "readxl", "writexl")
# Install packages if they are not already installed
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}
# Load the libraries
library(magick)
library(stringr)
library(tidyverse)
library(readxl)
library(writexl)

### Functions ##################################################################
# Function to wrap text to a specified width
wrap_text <- function(text, width) {
  lines_for_curr_text <- 0
  words <- unlist(str_split(text, " "))
  wrapped_lines <- c()
  current_line <- ""
  for (word in words) {
    if (nchar(current_line) + nchar(word) + 1 > width) {
      wrapped_lines <- c(wrapped_lines, current_line)
      current_line <- word
      lines_for_curr_text <- lines_for_curr_text + 1  # Count new line
    } else {
      current_line <- ifelse(current_line == "", word, paste(current_line, word))
    }
  }
  if (current_line != "") {
    wrapped_lines <- c(wrapped_lines, current_line)
    lines_for_curr_text <- lines_for_curr_text + 1  # Count the last line
  }
  return(list(wrapped_text = paste(wrapped_lines, collapse = "\n"), line_count = lines_for_curr_text))
}

# Function to load data from an Excel file
load_data <- function(file_path) {
  data <- read_excel(file_path)
  return(data)
}

# Function to prepare sleep options from selected row
prepare_sleep_options <- function(selected_row) {
  sleepopties <- list(
    sleepoptie_A = selected_row$sleepoptie_A,
    sleepoptie_B = selected_row$sleepoptie_B,
    sleepoptie_C = selected_row$sleepoptie_C,
    sleepoptie_D = selected_row$sleepoptie_D,
    sleepoptie_E = selected_row$sleepoptie_E,
    sleepoptie_F = selected_row$sleepoptie_F,
    sleepoptie_G = selected_row$sleepoptie_G,
    sleepoptie_H = selected_row$sleepoptie_H
  )
  return(sleepopties[sapply(sleepopties, function(x) !is.na(x) && nchar(x) > 0)])
}

# Function to sanitize strings for file/directory names*(special chars produced errors)
sanitize_string <- function(string) {
  string <- gsub("[<>:\"\\/|?*]", "_", string)  # Replace invalid characters with underscores :)
  return(string)
}

# Create output directory if it doesn't exist
create_output_dir <- function(full_output_directory) {
  if (dir.exists(full_output_directory)) {
    cat("\n####################################################\n",
        "Er bestaat al een map voor:", full_output_directory, "\n --> Dit is gek omdat de kolom 'gegenereerd' leeg is.\n",
        "!!!!!", "Maak een sub-map in deze map en zet daar de plaatjes in die er nu al instaan, anders worden deze plaatjes vervangen door de nieuwe\n",
        "Wanneer je dit gedaan hebt, type dan een willekeurige letter en druk op enter.\n")
    user_input <- readline("Geef hier een willekeurige letter/willekeruig woord op: ")
    cat("\n####################################################\n")
    dir.create(full_output_directory)
    if (nchar(user_input) == 0) {
      stop("De code stopt.")
    }
  } else {
    dir.create(full_output_directory)
  }
}

# Function to create images for sleepopties, type based on data in Excel
create_images <- function(non_empty_sleepopties, full_output_directory, tekst_itemnummer, max_chars_per_line, image_width) {
  max_number_of_lines <- 0  
  # Iterate over each sleep option
  for (var_name in names(non_empty_sleepopties)) {
    text_to_wrap <- non_empty_sleepopties[[var_name]]
    result <- wrap_text(text_to_wrap, max_chars_per_line)
    wrapped_text <- str_trim(result$wrapped_text)
    lines_needed <- result$line_count
    # Update maximum number of lines based on actual needed lines
    if (lines_needed > max_number_of_lines) {
      max_number_of_lines <- lines_needed
    }
  }
  # Berekening van hoogte voor plaatjes
  calculated_height <- (max_number_of_lines * 15) + 5
  for (var_name in names(non_empty_sleepopties)) {
    text_to_wrap <- non_empty_sleepopties[[var_name]]
    result <- wrap_text(text_to_wrap, max_chars_per_line)
    wrapped_text <- str_trim(result$wrapped_text)
    # Plaatje maken met de berekende hoogte
    sleepoptie_doos_img <- image_read("500x500_template.png") %>%
      image_resize(paste0(image_width, "x", calculated_height, "!")) %>% # Use image width variable
      image_background("white")
    sleepoptie_doos <- image_annotate(sleepoptie_doos_img,
                                      wrapped_text,
                                      size = 11,
                                      gravity = "northwest",
                                      color = "black",
                                      font = "Verdana")
    output_filename <- file.path(full_output_directory,
                                 paste0(sanitize_string(tekst_titel), "_",
                                        tekst_itemnummer, "_",
                                        sanitize_string(var_name), ".jpg"))  # Use sanitized file name here
    image_write(sleepoptie_doos, output_filename)
  }
  cat("Sleepopties gegenereerd voor: ", tekst_titel, tekst_itemnummer, "\n")
}

### Main Script ################################################################
file_path <- "Sleepvraag_Items.xlsx"
data <- load_data(file_path)
user_name <- readline(prompt = "Geef a.u.b. jouw naam: ")
today_date <- format(Sys.Date(), "%Y-%m-%d")

# Loop through each row
for (i in seq_len(nrow(data))) {
  if ((is.na(data$gegenereerd[i]) || nchar(data$gegenereerd[i]) == 0) &&
      (data$type_sleepvraag[i] == 2 || 
       (data$type_sleepvraag[i] == 1 && data$aantal_kolommen[i] == 2))) {
    selected_row <- data[i, ]
    vak <- selected_row$vak
    tekst_titel <- sanitize_string(selected_row$tekst_titel)  # Sanitize here
    tekst_itemnummer <- as.integer(selected_row$vraag)
    max_chars_per_line <- 33
    image_width <- 210  
  } else if (data$type_sleepvraag[i] == 1 && data$aantal_kolommen[i] == 3) {
    selected_row <- data[i, ]
    vak <- selected_row$vak
    tekst_titel <- sanitize_string(selected_row$tekst_titel)  # Sanitize here
    tekst_itemnummer <- as.integer(selected_row$vraag)
    max_chars_per_line <- 22
    image_width <- 145  
  } else if (data$type_sleepvraag[i] == 1 && data$aantal_kolommen[i] == 4) {
    selected_row <- data[i, ]
    vak <- selected_row$vak
    tekst_titel <- sanitize_string(selected_row$tekst_titel)  # Sanitize here
    tekst_itemnummer <- as.integer(selected_row$vraag)
    max_chars_per_line <- 15
    image_width <- 109  
  } else {
    next  # Skip to the next row if none of the conditions are met
  }
  
  # Prepare the sleep options
  non_empty_sleepopties <- prepare_sleep_options(selected_row)
  
  # Create the main output directory if it doesn't exist
  main_output_directory <- "Sleepvragen_plaatjes"
  if (!dir.exists(main_output_directory)) {
    dir.create(main_output_directory)
  }
  
  # Create the output directory name and full output directory path
  output_directory_name <- sanitize_string(paste0(vak, "_", tekst_titel, "_item_", tekst_itemnummer))
  full_output_directory <- file.path(main_output_directory, output_directory_name)
  
  create_output_dir(full_output_directory)  # Check and create output directory
  create_images(non_empty_sleepopties, full_output_directory, tekst_itemnummer, max_chars_per_line, image_width)  
  
  # Update 'gegenereerd' column to prevent overwriting issues
  data$gegenereerd[i] <- paste(today_date, "door", user_name)
}

# Updaten 'gegenereerd' column in the Excel file
write_xlsx(data, file_path)
cat("Alle sleepopties zijn gegenereerd, dankjewel", user_name, "! :)")
cat("\n !!!!! Warning messages kun je negeren !!!!!")
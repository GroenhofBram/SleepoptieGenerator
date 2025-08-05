### Libraries ##################################################################
library(magick)
library(stringr)
library(tidyverse)
library(readxl)
library(writexl)

### Functions ##################################################################

# Function to wrap text to a specified width (remains unchanged)
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
  return(list(wrapped_text = paste(wrapped_lines, collapse = "\n"),
              line_count = lines_for_curr_text))
}

# Function to load data from an Excel file (remains unchanged)
load_data <- function(file_path) {
  data <- read_excel(file_path)
  return(data)
}

# Function to prepare sleep options from selected row (remains unchanged)
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

# Function to create output directory with a pre-existing check
create_output_dir <- function(directory_name) {
  if (dir.exists(directory_name)) {
    cat("\n####################################################\n",
        "Er bestaat al een map voor:", directory_name, "\n --> Dit is gek omdat de kolom 'gegenereerd leeg is.\n",
        "!!!!!", "Zorg ervoor dat de map leeg is, of dat de plaatjes/bestanden die daar al instaan weer in een map daarbinnen staan.\n",
        "Wanneer je door wil gaan, geef een willekeurige letter of willekeurig woord op.\n")
    user_input <- readline("Geef hier een willekeurige letter/willekeruig woord op: ")
    cat("\n####################################################\n")
    if (nchar(user_input) == 0) {
      stop("De code stopt.")
    }
  } else {
    dir.create(directory_name)
  }
}

# Function to create images for sleep options (remains unchanged)
create_images <- function(non_empty_sleepopties, tekst_titel, tekst_itemnummer, max_chars_per_line) {
  max_number_of_lines <- 0
  for (var_name in names(non_empty_sleepopties)) {
    text_to_wrap <- non_empty_sleepopties[[var_name]]
    result <- wrap_text(text_to_wrap, max_chars_per_line)
    wrapped_text <- str_trim(result$wrapped_text)
    lines_needed <- result$line_count
    
    # Update maximum number of lines
    if (lines_needed > max_number_of_lines) {
      max_number_of_lines <- lines_needed + 1
    }
    calculated_height <- (max_number_of_lines * 15) + 5
    
    # Plaatje maken met berekende hoogte.
    sleepoptie_doos_img <- image_read("500x500_template.png") %>%
      image_resize(paste0("210x", calculated_height, "!")) %>%
      image_background("white")
    sleepoptie_doos <- image_annotate(sleepoptie_doos_img,
                                      wrapped_text,
                                      size = 11,
                                      gravity = "northwest",
                                      color = "black",
                                      font = "Verdana")
    output_filename <- file.path(tekst_titel, paste0(tekst_titel, "_", var_name, ".jpg"))
    image_write(sleepoptie_doos, output_filename)
  }
  cat("Sleepopties gegenereerd voor: ", tekst_titel,"\n")
}

### Main Script ################################################################
file_path <- "Sleepvraag_Items.xlsx"
data <- load_data(file_path)

# Ask the user for the name to include in the "gegenereerd" column
user_name <- readline(prompt = "Geef a.u.b. jouw naam, voor het bijhouden wie wat gegenereerd heeft: ")
today_date <- format(Sys.Date(), "%Y-%m-%d")

# Process each row where the 'gegenereerd' column is empty
for (i in seq_len(nrow(data))) {
  if (is.na(data$gegenereerd[i]) || nchar(data$gegenereerd[i]) == 0) {
    selected_row <- data[i, ]
    vak <- selected_row$vak
    tekst_titel <- selected_row$tekst_titel
    tekst_itemnummer <- as.integer(selected_row$vraag)
    max_chars_per_line <- 33
    non_empty_sleepopties <- prepare_sleep_options(selected_row)
    
    # New output directory name
    output_directory_name <- paste0(vak, "_", tekst_titel, "_item_", tekst_itemnummer)
    
    create_output_dir(output_directory_name)  # Updated directory name
    create_images(non_empty_sleepopties, output_directory_name, tekst_itemnummer, max_chars_per_line)
    
    # Update 'gegenereerd' column for the current row
    data$gegenereerd[i] <- paste(today_date, "door", user_name)
  }
}

# Save the updated data back to Excel
write_xlsx(data, file_path)
cat("Alle sleepopties zijn gegenereerd, dankjewel", user_name, "! :)")
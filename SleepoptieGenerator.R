### Libraries ##################################################################
library(magick)
library(stringr)

### Code #######################################################################
## Functions ##
# Function to wrap text based on the specified character limit
wrap_text <- function(text, width) {
  words <- unlist(str_split(text, " "))
  wrapped_lines <- c()
  current_line <- ""
  
  for(word in words) {
    if (nchar(current_line) + nchar(word) + 1 > width) {
      wrapped_lines <- c(wrapped_lines, current_line)
      current_line <- word
    } else {
      current_line <- ifelse(current_line == "", word, paste(current_line, word))
    }
  }
  
  if (current_line != "") {
    wrapped_lines <- c(wrapped_lines, current_line)
  }
  
  return(paste(wrapped_lines, collapse = "\n"))
}


# Idee: ALS de tekst te lang is ("lengtetest9.png", "lengtetest10.png", "lengtetest13.png"), uitzondering programmeren waardoor de optie langer uit wordt geschreven?
# !!! Dubbele aanhalingstekens regexen

## Executable Code ##
# afleider_test = "In een nieuwsbericht over een klimaatdemonstratie wordt over Daniëlle vermeld dat zij anderen de toegang tot een luchthaven heeft belemmerd en hiervoor is aangehouden. en wat speling. :)"
# afleider_test = "Er is een kritischartikel geschreven overdenauwebanden die ziekenhuisbestuurder Paula onderhoudt met een farmaceutisch bedrijf acbdb andask."
# afleider_test = "Zowel de krant als Google passen berichten aan als de persoonlijke schade groter wordt gevonden dan het publieke belang."
# afleider_test = "Een kleine groep minimalisten loopt voorop."
# afleider_test = "Kijk rond in je huis, leg vast wat je allemaal hebt, of het rondslingert of op een eigen plek ligt en wat je er (niet) mee doet."
# afleider_test = "KPMG heeft een onderzoek verricht waaruit blijkt dat de bestuurders van Nederlandse bedrijven klagen over het normbesef van de startende jongeren."
# afleider_test = "De verschillende generaties moeten meer kennis hebben van elkaars motivatie en achtergronden. Dan kunnen ze van elkaars mogelijkheden gebruik maken."
# afleider_test = "Een klant heeft een nota ingediend bij Proteq. Vorige week heeft Proteq echter een brief verstuurd met de aankondiging dat de verzekering zal worden stopgezet, omdat de premie niet betaald is. Tijdens telefonisch contact blijkt dat de klant deze brief niet heeft ontvangen. Proteq moet de kosten van de dierenarts vergoeden."
# afleider_test = "Protocollen voor het gebruik van sociale media vormen een goede basis voor regulering binnen het bedrijf, maar het is ook belangrijk dat medewerkers zelf opletten wat ze op het internet zetten."
# afleider_test = "is aangeboren"
# afleider_test = "Een complicatie is dat je als sollicitant, wanneer je niet bent uitgenodigd, lastig kunt bewijzen dat dat door een negatief zoekresultaat komt."
afleider_test = "Elvire Blommers: \"Mijn reisagent is aangesloten bij de ANVR. Ik ben niet tevreden over mijn reis naar Egypte en ik wil een klacht indienen. De reisagent heeft gezegd dat ik deze klacht moet indienen bij de buitenlandse touroperator. Klopt dit?"

max_chars_per_line <- 33

wrapped_text <- wrap_text(afleider_test, max_chars_per_line)

# Read and resize the image, using '!' to ignore the aspect ratio if required
# Example: "100x100!" will force the size to 100x100 pixels
sleepoptie_doos_img <- image_read("500x500_template.png")

# For explicit dimension setting without aspect ratio preservation, use '!' at the end
sleepoptie_doos_img <- image_resize(sleepoptie_doos_img, "215x90!")  # Change to your desired width and height

# Ensure a white background if transparency is a concern
sleepoptie_doos_img <- image_background(sleepoptie_doos_img, "white")

# Annotate the image with the wrapped text
sleepoptie_doos <- image_annotate(sleepoptie_doos_img,
                                  wrapped_text,
                                  size = 11,
                                  gravity = "northwest",
                                  color = "black",
                                  font = "Verdana")

# Print image size for confirmation
cat("Image size (width x height):", image_info(sleepoptie_doos)$width, "x", image_info(sleepoptie_doos)$height, "\n")

# Save the image
print(sleepoptie_doos)
# image_write(sleepoptie_doos, "lengtetest.png")
# SleepoptieGenerator
Code that converts text into draggable images for Dutch MBO 2F/3F exams. It will always produce images of the same size.

- 2025-09-04: (...).R, is geüploadet. Deze versie werkt ook met Graphic Gapmatch vragen.

- 2025-08-07: Zelfde als vorige update, maar er is nu een handleiding beschikbaar. Ook mist in deze repo nog een tabel_templates.xlsx bestand. Deze bevatten beide prive informatie dus zijn niet hier geüploadet.

- 2025-08-04 Thirdcommit (NL): Werkt met Excelbestand. Er zijn nog wel wat mogelijke uitzonderingen die handmatig gefixt moeten worden. **WERKT VOOR NU ALLEEN VOOR GRAPHIC GAPMATCH CATEGORIZE VRAGEN**

- 2025-08-04: Second commit. Version that works by reading in an Excel file "Sleepvraag_Items.xlsx". Ensure that this file exists and is strucutred properly. It should have the following columns: vak	tekst_titel	vraag	sleepoptie_A	sleepoptie_B	sleepoptie_C	sleepoptie_D	sleepoptie_E	sleepoptie_F	sleepoptie_G	sleepoptie_H	gegenereerd
--> "gegenereerd" should be empty.

- 2025-07-16: First commit. Initial piece of code that works for hardcoded text.

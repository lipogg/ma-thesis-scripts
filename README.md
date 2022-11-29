# Scripts for my MA Thesis

This Repository contains scripts I have written to scrape, process and analyze Soviet ethnographic journals for my master's thesis _Socialist Visions and Imperial Categories. A Digital History of Soviet Ethnographies of the Tajik SSR, 1953-1991_. The repository is being updated continuously and does not document the final state of my thesis.

## Contents

`qde-parser.R` - Parse REFI-QDA XML files and extract code co-occurrence edgelists for import in [R Shiny App](https://github.com/lipogg/soviet-ethnography)
`project.qde` - REFI-QDA XML project file exported from MAXQDA. Contains code co-occurrence information
`mae_ocr.py` - OCR for _Sborniki muzej antropologii i etnografii_ (MAE) with PyMuPDF and Tesseract engine \
`mae_ocr-env.yml` - Anaconda environment file for MAE OCR \
`se_ner.ipynb` - Named Entitiy Recognition for _Sovetskaya etnografiya_ (SE) with SpaCy \
`se_ner-env.yml` - Anaconda environment file for SE NER \
`se_ner-requirements.txt` - Requirements for SE NER \
`se_pdftotxt.py` - Convert machine readable PDFs to plain text files \
`se_scraper.R` - Scrape all issues of SE from [https://www.booksite.ru/etnogr/index.htm](https://www.booksite.ru/etnogr/index.htm)

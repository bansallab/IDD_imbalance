# Data and code for "The extent of gender and race/ethnicity imbalance in infectious disease dynamics research."

This repository provides the source code for the following study: Juliana C Taube*, Alexes Merritt*, Shweta Bansal. "The extent of gender and race/ethnicity imbalance in infectious disease dynamics research." In press at Proceedings of the Royal Society: B. Anonymized data to reproduce the figures in the text are provided in compliance with our data-sharing agreement with Clarivate.


## Data (`coreidd/`)
This folder contains the anonymized data for our analyses. Data dictionaries for the columns for each file are provided in the folder. The different `input_jif` files correspond to different sensitivity analyses of the main text impact factor analysis. 

`cited-reference-loop` and `citing-articles-loop` folders contain code used to extract data from the Web of Science Core Collection API. They are housed in this folder as running the files would deposit the data into these respective folders.

## Code (`pull_data/`, `process_data/`, and `make_figures/`)
Scripts explaining how data were pulled from Web of Science, processed, and used to make figures. Some of the code is in the `coreidd/cited-reference-loop` and `coreidd/citing-articles-loop` folders. Files were run in numerical order and are named descriptively. 

Some files are pulled directly from [Dworkin et al.](https://github.com/jdwor/gendercitation) including `HelperFunctions.R`, `nickname.gends.csv`, and `nicknames.csv`. 


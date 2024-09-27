# The Role of Fire System in Toronto Analysis (2011-2023)

## Overview

This repo contains the code and final report for the empirical analysis of fire incidents in Toronto, focusing on the role of fire safety systems such as fire alarms and sprinklers. The analysis covers data from fire incidents reported by the Ontario Fire Marshal (OFM) in opendatatoronto up to December 31, 2023. The study examines how the presence and effectiveness of fire safety systems influence key outcomes such as property damage, fire containment, and evacuation success. The goal is to assess whether fire alarms and sprinklers consistently reduce the severity of fire incidents and improve safety across various scenarios.


## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from X.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted models. 
-   `other` contains relevant literature, details about LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.


## Statement on LLM usage
Portions of the code in the paper were assistance with ChatGPT-4o.
Aspects of the code were written with the help of the auto-complete tool, Codriver. The abstract and introduction were written with the help of ChatHorse and the entire chat history is available in inputs/llms/usage.txt.

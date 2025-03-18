# Data Directory Overview

This directory contains various datasets used throughout the study. Each subdirectory is organized to hold specific types of data. Below is a description of what each subdirectory contains:

## Subdirectories and Their Contents

### Data_raw
- **Contents:** Raw, unfiltered Google Trends data with overlapping timestamps.
- **Purpose:** This folder contains the initial raw data fetched directly from Google Trends, which includes multiple data points with overlapping timestamps. It serves as the baseline data for further cleaning and processing.

### Data_GT
- **Contents:** Cleaned and unique Google Trends data.
- **Purpose:** This directory holds the Google Trends data that has been processed to select only unique timestamps, eliminating overlaps. The data here has been further cleaned and prepared for analysis, ensuring that each data point is distinct and ready for accurate modeling and comparison.

### Data_polls
- **Contents:** Polling data for the German federal elections from various institutes.
- **Purpose:** Contains data collected from different polling companies. This data is used to compare and contrast with Google Trends predictions and to perform combined analyses.

### Data_WC
- **Contents:** Data without the usage of a category in Google Trends, just for comparison.
- **Purpose:** This folder includes datasets that were collected without specifying a category in Google Trends. It is used for comparative analyses to evaluate the impact of categorization on the accuracy of trend data.

## General Usage
The data stored in these directories are utilized in various scripts for processing, analysis, and model building. Refer to the `Scripts` directory for detailed information on how these datasets are employed in the study.


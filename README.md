# Reproducing and replicating this study

One of our objectives with this study was to provide other researchers with a transparent workflow that involves all the steps from collecting the raw GT data, obtaining the predictions, to producing the analyses and the study itself. The replication files contain the following code files that correspond to the steps we followed to conduct this study:

## Project Steps

### Step 1: Collect polling data
- **File:** `1_Step_1_collect_polling_data.R`
- **Description:** Contains the code to collect polling data from different polling companies in Germany including Infratest Dimap.
- **Output:** Creates files `data_polls_*.csv` for data from the different polling companies.

### Step 2: Collect GT data
- **File:** `2_Step_2_collect_GT_data.R`
- **Description:** Contains the code to collect the GT data and can be scheduled to run every hour over a certain time period.
- **Output:** Creates `.RData` files in a folder called `Data_raw` that contain the search volume data for the respective searches.

### Step 3: Subsetting GT data
- **File:** `3_Step_3_subset_GT_data.R`
- **Description:** This file is used to compare the collected GT datasets that are stored within `.RData` files within the `Data_raw` folder.
- **Process:** Subsequently, only `.Rdata` files are used (1 per day) where all the GT datasets are different from previous ones. These files are copied to the `Data` folder.

### Step 4: Generate predictions
- **File:** `4_Step_4_predictive_modelling.R`
- **Description:** Contains the code to yield the predictions based on the GT data.
- **Function:** This file loads the GT datasets stored in the `Data` folder and the polling data and builds the different predictive models that are analyzed in the paper.

### Step 5: Conduct analyses and generate paper
- **File:** `5_Step_5_generate_paper.qmd`
- **Description:** Contains the code to analyse and visualize our predictions as well as generate the paper.

---
title: "Reproducible Research Project 1"
author: "Victor .O. Oseji"
date: "12/01/2025"
---

Loading and preprocessing the data
----------------------------------

```r
knitr::opts_chunk$set(echo = TRUE)

if (!require("ggplot2")) install.packages("ggplots")
if (!require("dplyr")) install.packages("dplyr")
if (!require("scales")) install.packages("scales")
if (!require("kableExtra")) install.packages("kableExtra")

# Load required libraries
library(dplyr)
library(ggplot2)

# Function to download, unzip, and read CSV
download_and_read_csv <- function(url) {
  # Create a temporary file to store the zip
  temp_zip <- tempfile(fileext = ".zip")
  
  # Download the file
  download.file(url, temp_zip, mode = "wb")
  
  # Unzip the contents to a temporary directory
  temp_dir <- tempdir()
  unzip(temp_zip, exdir = temp_dir)
  
  # Find the first CSV file in the unzipped contents
  csv_file <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Read the CSV file into a data.table
  if (length(csv_file) > 0) {
    data <- read.csv(csv_file[1])
    return(data)
  } else {
    stop("No CSV file found in the zip archive.")
  }
}

# Load dataset
url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
data <- download_and_read_csv(url) %>% 
  # format the date 
  mutate(date = as.Date(date))

```

## What is mean total number of steps taken per day?

```r
# Summarize daily steps by date
# - `across()` is used to apply the sum function to the "steps" column
# - `sum(x, na.rm = TRUE)` calculates the total steps for each date, ignoring NA values
# - `.by = date` groups the data by the "date" column
daily_steps <- data %>% 
  summarise(across(steps, \(x) sum(x, na.rm = TRUE)), .by = date) 

# Visualize the distribution of daily steps using a histogram
daily_steps %>% 
  ggplot(aes(steps)) + 
  geom_histogram(
    bins = 20,                # Set the number of bins for the histogram
    na.rm = TRUE,             # Exclude NA values from the plot
    fill = '#374859',         # Set the fill color of the bars
    color = 'white'           # Set the color of the bar borders
  ) +
  # Add labels and a title to the plot
  labs(
    title = 'Distribution of Daily Steps Taken',  # Title of the plot
    y = 'Frequency of Steps',                     # Label for the y-axis
    x = 'Daily Steps'                             # Label for the x-axis
  ) +
  theme_minimal() +         # Use a minimal theme for a clean appearance
  scale_x_continuous(
    n.breaks = 8,                              # Set the number of ticks on the x-axis
    labels = scales::label_number(big.mark = ',') # Format large numbers with commas
  ) +
  coord_cartesian(expand = FALSE) +  # Disable axis expansion for a tighter fit to the data
  theme(
    panel.grid = element_blank(),        # Remove the background grid
    axis.line = element_line(            # Customize the axis lines
      linetype = 'solid',                # Use a solid line type
      linewidth = 0.5,                   # Set the line width
      color = '#373737'                  # Set the line color
    )
  )

```

![]{figure/RCplot1.png}


## What is the average daily activity pattern?

```r
#| echo: true

# Summarize the average steps for each 5-minute interval
# - `mean(steps, na.rm = TRUE)` calculates the average steps for each interval, ignoring NA values
# - `.by = interval` groups the data by the "interval" column
AvgStep_5min_int <- data %>% 
  summarise(Steps = mean(steps, na.rm = TRUE), .by = interval)

# Identify the interval with the highest average steps
# - `which.max(Steps)` finds the row index of the maximum "Steps" value
# - Extract the row corresponding to this maximum value
highest_pt <- with(AvgStep_5min_int, AvgStep_5min_int[which.max(Steps), ])

# Create a line graph showing the trend of steps across 5-minute intervals
AvgStep_5min_int %>% 
  ggplot(aes(x = interval, y = Steps)) + 
  geom_line(color = 'chocolate') +    # Add a line representing the trend, colored chocolate
  geom_point(                         # Highlight the highest point on the graph
    data = highest_pt, 
    aes(x = interval, y = Steps), 
    color = 'forestgreen',            # Use forest green color for emphasis
    size = 4,                         # Set point size
    shape = 21                        # Use a circle shape for the point
  ) +
  # Add title and axis labels
  labs(
    title = 'Trend of Steps Taken within 5min Interval', # Title of the graph
    y = 'Steps',                                         # Y-axis label
    x = '5-Minutes Interval'                             # X-axis label
  ) +
  theme_minimal() +                   # Apply a minimal theme for clean styling
  scale_x_continuous(
    n.breaks = 8,                     # Set the number of ticks on the x-axis
    labels = scales::label_number(big.mark = ',') # Format x-axis values with commas
  ) +
  scale_y_continuous(
    n.breaks = 8,                     # Set the number of ticks on the y-axis
    limits = c(0, 230),               # Set the y-axis range
    labels = scales::label_number(big.mark = ',') # Format y-axis values with commas
  ) +
  # Add a label for the highest point
  geom_text(
    data = highest_pt, 
    aes(
      x = interval, 
      y = Steps, 
      label = paste0("Highest: ", scales::comma(Steps)) # Format the label with a comma
    ),
    vjust = -1,                        # Position the text slightly above the point
    color = "darkgreen",               # Use dark green color for the text
    fontface = "bold"                  # Use bold font for emphasis
  ) +
  coord_cartesian(expand = FALSE) +    # Disable axis expansion for a tighter fit
  theme(
    panel.grid = element_blank(),      # Remove grid lines for a cleaner look
    axis.line = element_line(          # Customize the axis lines
      linetype = 'solid',              # Use a solid line type
      linewidth = 0.5,                 # Set the line width
      color = '#373737'                # Set the line color
    )
  )
```
![](figure/RCplot2.png)

```r
# Summarize the daily steps data
# - Create a summary table with the mean and median of daily steps
# - `scales::comma()` formats the numbers with commas for readability
daily_steps %>% 
  summarise(
    Activity = 'Steps',                                   # Add a column to label the activity
    Mean = mean(steps, na.rm = TRUE) %>% scales::comma(), # Calculate the mean, ignoring NA, and format it
    Median = median(steps, na.rm = TRUE) %>% scales::comma() # Calculate the median, ignoring NA, and format it
  ) %>% 
  # Create a styled table using kableExtra
  kableExtra::kbl(
    caption = 'Summary of Daily Steps',  # Add a caption to describe the table
    digits = 0,                         # Round all numeric values to 0 decimal places
    row.names = FALSE                   # Exclude row names from the table
  ) %>% 
  kableExtra::kable_classic_2()         # Apply a classic table style using kableExtra
```

| Activity | Mean | Median |
|----------|------|--------|
| Steps    | 9354 | 10395  |


## What is the average daily activity pattern?

```r
# Summarize the average steps for each 5-minute interval
# - `mean(steps, na.rm = TRUE)` calculates the average steps for each interval, ignoring NA values
# - `.by = interval` groups the data by the "interval" column
AvgStep_5min_int <- data %>% 
  summarise(Steps = mean(steps, na.rm = TRUE), .by = interval)

# Identify the interval with the highest average steps
# - `which.max(Steps)` finds the row index of the maximum "Steps" value
# - Extract the row corresponding to this maximum value
highest_pt <- with(AvgStep_5min_int, AvgStep_5min_int[which.max(Steps), ])

# Create a line graph showing the trend of steps across 5-minute intervals
AvgStep_5min_int %>% 
  ggplot(aes(x = interval, y = Steps)) + 
  geom_line(color = 'chocolate') +    # Add a line representing the trend, colored chocolate
  geom_point(                         # Highlight the highest point on the graph
    data = highest_pt, 
    aes(x = interval, y = Steps), 
    color = 'forestgreen',            # Use forest green color for emphasis
    size = 4,                         # Set point size
    shape = 21                        # Use a circle shape for the point
  ) +
  # Add title and axis labels
  labs(
    title = 'Trend of Steps Taken within 5min Interval', # Title of the graph
    y = 'Steps',                                         # Y-axis label
    x = '5-Minutes Interval'                             # X-axis label
  ) +
  theme_minimal() +                   # Apply a minimal theme for clean styling
  scale_x_continuous(
    n.breaks = 8,                     # Set the number of ticks on the x-axis
    labels = scales::label_number(big.mark = ',') # Format x-axis values with commas
  ) +
  scale_y_continuous(
    n.breaks = 8,                     # Set the number of ticks on the y-axis
    limits = c(0, 230),               # Set the y-axis range
    labels = scales::label_number(big.mark = ',') # Format y-axis values with commas
  ) +
  # Add a label for the highest point
  geom_text(
    data = highest_pt, 
    aes(
      x = interval, 
      y = Steps, 
      label = paste0("Highest: ", scales::comma(Steps)) # Format the label with a comma
    ),
    vjust = -1,                        # Position the text slightly above the point
    color = "darkgreen",               # Use dark green color for the text
    fontface = "bold"                  # Use bold font for emphasis
  ) +
  coord_cartesian(expand = FALSE) +    # Disable axis expansion for a tighter fit
  theme(
    panel.grid = element_blank(),      # Remove grid lines for a cleaner look
    axis.line = element_line(          # Customize the axis lines
      linetype = 'solid',              # Use a solid line type
      linewidth = 0.5,                 # Set the line width
      color = '#373737'                # Set the line color
    )
  )
```

![](figure/RCplot2.png)

```r
# Modify the `highest_pt` data to include a descriptive time interval
highest_pt %>% 
  mutate(
    interval = paste0(interval - 5, ' - ', interval) # Create a new time interval label
    # Combines the start and end of the interval as a string (e.g., "10 - 15")
  ) %>% 
  rename(
    `Time Interval` = interval,      # Rename the "interval" column for clarity
    `Avg_Steps Taken` = Steps        # Rename the "Steps" column for clarity
  ) %>% 
  # Create a styled table using kableExtra
  kableExtra::kbl(
    caption = 'Highest Point',       # Add a caption to describe the table
    digits = 0,                      # Round all numeric values to 0 decimal places
    row.names = FALSE                # Exclude row names from the table
  ) %>% 
  kableExtra::kable_classic_2()      # Apply a classic style to the table
```

| Time Interval       | Avg_Steps Taken |
|---------------------|-----------------|
| 830 - 835           | 206             |


## Imputing missing values

```{r}

# Calculate total number of rows with missing data
missing <- sum(is.na(data))
percent <- missing/nrow(data)

# missing data imputation with median value for 5mins interval
filled_data <- data %>% 
  mutate(steps = ifelse(date == as.Date('2012-10-01'), 0, steps) ) %>% 
  mutate(steps = tidyr::replace_na(steps, median(steps,na.rm=T)), .by = interval)
```

The total number of missing data is `r scales::comma(missing)` which is about `r scales::percent(percent,1)` of the dataset.


```{r}

# Summarize daily steps for the dataset with missing data filled
# - `sum(x, na.rm = TRUE)` calculates the total steps for each date, ignoring NA values
# - `.by = date` groups the data by the "date" column
filleddaily_steps <- filled_data %>% 
  summarise(across(steps, \(x) sum(x, na.rm = TRUE)), .by = date)

# Create a histogram to visualize the distribution of daily steps
filleddaily_steps %>% 
  ggplot(aes(steps)) + 
  # Add histogram bars
  geom_histogram(
    bins = 20,            # Divide the data into 20 bins
    na.rm = TRUE,         # Ignore missing values
    fill = '#374859',     # Set bar color to a dark shade
    color = 'white'       # Outline each bar with white
  ) +
  # Add title and axis labels
  labs(
    title = 'Distribution of Daily Steps Taken {Missing Data Filled}', # Chart title
    y = 'Frequency of Steps',                                          # Y-axis label
    x = 'Daily Steps'                                                  # X-axis label
  ) +
  theme_minimal() +                 # Apply a clean, minimal theme
  scale_x_continuous(
    n.breaks = 8,                   # Define the number of breaks on the x-axis
    labels = scales::label_number(big.mark = ',') # Format numbers with commas
  ) +
  coord_cartesian(expand = FALSE) + # Remove padding around the chart
  # Customize the chart theme
  theme(
    panel.grid = element_blank(),   # Remove grid lines for a cleaner look
    axis.line = element_line(       # Customize axis lines
      linetype = 'solid',           # Use solid lines for axes
      linewidth = 0.5,              # Set line width
      color = '#373737'             # Set line color to dark gray
    )
  )
```

![](figure/RCplot3.png)

```{r}

# Summarize the daily steps data and create a styled table

filleddaily_steps %>% 
  # Summarize the data to calculate mean and median daily steps
  summarise(
    Activity = 'Steps',                        # Add a column indicating the activity type
    Mean = mean(steps, na.rm = TRUE) %>%       # Calculate the mean of daily steps
      scales::comma(),                         # Format the mean with commas for readability
    Median = median(steps, na.rm = TRUE) %>%   # Calculate the median of daily steps
      scales::comma()                          # Format the median with commas for readability
  ) %>% 
  # Create a styled table with kableExtra
  kableExtra::kbl(
    caption = 'Summary of Daily Steps',        # Add a title to the table
    digits = 0,                                # Round all numeric values to the nearest integer
    row.names = FALSE                          # Exclude row names from the table
  ) %>% 
  kableExtra::kable_classic_2()                # Apply a clean, classic style to the table
```

| Activity | Mean | Median |
|----------|------|--------|
| Steps    | 9473 | 10395  |

After imputing the missing data with the median values of the respective time interval, we noticed difference in the calculated mean of the raw data and the imputed dateset. We observed that the average daily steps increased in the imputed dataset while the median remained the same.

## Are there differences in activity patterns between weekdays and weekends?

```{r}

# Step 1: Create a dataset summarizing steps by interval and week status
# - Add a new column 'Week_Status' to categorize days as 'Weekdays' or 'Weekends'
# - Summarize the mean steps for each 5-minute interval (`interval`) grouped by 'Week_Status'
filled_int_data <- filled_data %>% 
  mutate(
    Week_Status = case_when( 
      weekdays(date, abbreviate = TRUE) %in% c('Sat', 'Sun') ~ 'Weekends', # Mark weekends
      weekdays(date, abbreviate = TRUE) %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'Weekdays' # Mark weekdays
    )
  ) %>% 
  summarise(
    across(steps, \(x) mean(x, na.rm = TRUE)), # Calculate mean steps per group
    .by = c(interval, Week_Status)            # Group by interval and week status
  )

# Step 2: Identify the highest point (maximum steps) within each week status group
highest_pt <- filled_int_data %>% 
  filter(steps == max(steps, na.rm = TRUE), .by = Week_Status) # Find the max steps for each 'Week_Status'

# Step 3: Visualize the trends in steps for weekdays and weekends
filled_int_data %>% 
  ggplot(aes(x = interval, y = steps)) + 
  # Add a line to represent the trend of steps across intervals
  geom_line(color = 'darkblue') +
  # Highlight the points with the highest steps
  geom_point(data = highest_pt, aes(x = interval, y = steps), 
             color = 'forestgreen', size = 4, shape = 21) +
  # Add title and axis labels
  labs(
    title = 'Trend of Steps Taken within 5min Interval', # Chart title
    y = 'Steps',                                         # Y-axis label
    x = '5-Minutes Interval'                             # X-axis label
  ) + 
  theme_minimal() + # Apply a clean theme
  # Customize x-axis scale
  scale_x_continuous(
    n.breaks = 8, 
    labels = scales::label_number(big.mark = ',')
  ) +
  # Customize y-axis scale
  scale_y_continuous(
    n.breaks = 8, 
    limits = c(0, 250), 
    labels = scales::label_number(big.mark = ',')
  ) +
  # Add text annotations for the highest points
  geom_text(
    data = highest_pt, 
    aes(x = interval, y = steps, 
        label = paste0("Highest: ", scales::comma(steps))),
    vjust = -1, color = "darkgreen", fontface = "bold"
  ) +
  # Create separate plots for 'Weekdays' and 'Weekends'
  facet_wrap(
    ~Week_Status,               # Facet by 'Week_Status' (Weekdays and Weekends)
    nrow = 1, ncol = 2,         # Arrange facets in one row with two columns
    scales = 'free'             # Allow scales to adjust independently
  ) +
  coord_cartesian(expand = FALSE) + # Remove padding around the chart
  # Customize chart aesthetics
  theme(
    panel.grid = element_blank(),            # Remove grid lines
    plot.title = element_text(               # Style the title
      hjust = 0.5, face = 'italic', color = '#373737'
    ),
    axis.line = element_line(                # Customize axis lines
      linetype = 'solid', linewidth = 0.5, color = '#373737'
    ),
    strip.background = element_rect(         # Style the facet strip background
      fill = '#673737', color = 'white'
    ),
    strip.text = element_text(               # Style the facet strip text
      color = 'white', face = 'bold.italic'
    )
  )
```
![](figure/RCplot4.png)
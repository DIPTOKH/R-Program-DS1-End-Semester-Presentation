
rm(list=ls())

library(rvest)
library(httr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

#scrapping
url <- "https://www.gov.uk/government/publications/ai-skills-for-life-and-work-labour-market-and-skills-projections/ai-skills-for-life-and-work-labour-market-and-skills-projections"

html <- read_html(
  httr::content(
    httr::GET(url),
    "text"
  )
)

# All the tables are present inside Appendix B
# id = "appendix-b-detailed-results"

appendix_b <- html %>%
  html_element(
    xpath = "//*[@id='appendix-b-detailed-results']"
  )


# Extract all tables
appendix_b_tables <- appendix_b %>%
  html_elements(
    xpath = "following::table"
  )


# Should give 6 tables:
# 8.1, 8.2, 8.3, 8.4, 8.5, 8.6

length(appendix_b_tables)

#get table names

table_captions <- sapply(
  appendix_b_tables,
  function(table){
    
    table %>%
      html_elements(
        xpath = "preceding-sibling::p[1]"
      ) %>%
      html_text()
    
  }
)

table_captions

#select tables 8.1-8.4

tbl_8.1_node <- appendix_b_tables[[grep("Table 8\\.1", table_captions)]]
tbl_8.2_node <- appendix_b_tables[[grep("Table 8\\.2", table_captions)]]
tbl_8.3_node <- appendix_b_tables[[grep("Table 8\\.3", table_captions)]]
tbl_8.4_node <- appendix_b_tables[[grep("Table 8\\.4", table_captions)]]


#raw table extraction

# header = FALSE because the tables have multiple header rows

table_8.1_raw <- tbl_8.1_node %>%
  html_table(
    fill = TRUE,
    header = FALSE
  )

table_8.2_raw <- tbl_8.2_node %>%
  html_table(
    fill = TRUE,
    header = FALSE
  )

table_8.3_raw <- tbl_8.3_node %>%
  html_table(
    fill = TRUE,
    header = FALSE
  )

table_8.4_raw <- tbl_8.4_node %>%
  html_table(
    fill = TRUE,
    header = FALSE
  )


# Inspect raw tables

head(table_8.1_raw, 3)
head(table_8.2_raw, 3)
head(table_8.3_raw, 3)
head(table_8.4_raw, 3)


#cleaning table 8.1

# Drop the 2 header rows

table_8.1 <- table_8.1_raw[-c(1,2), ]


# Set column names

names(table_8.1) <- c(
  "occupation",
  
  "BS_2024",
  "BS_2030",
  "BS_2035",
  
  "AS_2024",
  "AS_2030",
  "AS_2035",
  
  "TS_2024",
  "TS_2030",
  "TS_2035",
  
  "HC_2024",
  "HC_2030",
  "HC_2035"
)


# Clean numeric columns

table_8.1[, -1] <- lapply(
  table_8.1[, -1],
  function(x)
    as.numeric(
      gsub("[,*]", "", x)
    )
)


# Check

head(table_8.1)
str(table_8.1)

#clening table 8.2

table_8.2 <- table_8.2_raw[-c(1,2), ]


names(table_8.2) <- c(
  "occupation",
  
  "BS_net_change",
  "BS_replacement_demand",
  
  "AS_net_change",
  "AS_replacement_demand",
  
  "TS_net_change",
  "TS_replacement_demand",
  
  "HC_net_change",
  "HC_replacement_demand"
)


# Clean numeric columns

table_8.2[, -1] <- lapply(
  table_8.2[, -1],
  function(x)
    as.numeric(
      gsub("[,*]", "", x)
    )
)


# Check

head(table_8.2)
str(table_8.2)


#table 8.3

table_8.3 <- table_8.3_raw[-c(1,2), ]
names(table_8.3) <- c("All AI-related occupations",
                      "TS_unadjusted_2024","TS_unadjusted_2030","TS_unadjusted_2035",
                      "TS_adjusted_2024","TS_adjusted_2030","TS_adjusted_2035"
)


# Clean numeric columns

table_8.3[, -1] <- lapply(
  table_8.3[, -1],
  function(x)
    as.numeric(
      gsub("[,*]", "", x)
    )
)


# Check

head(table_8.3)
str(table_8.3)


#table 8.4

table_8.4 <- table_8.4_raw[-1, ]


if (ncol(table_8.4) == 3) {
  
  names(table_8.4) <- c(
    "occupation",
    "net_change",
    "replacement_demand"
  )
  
} else {
  
  names(table_8.4) <- c(
    "occupation",
    paste0(
      "col_",
      seq_len(ncol(table_8.4) - 1)
    )
  )
}


# Clean numeric columns

table_8.4[, -1] <- lapply(
  table_8.4[, -1],
  function(x)
    as.numeric(
      gsub("[,*]", "", x)
    )
)


# Check

head(table_8.4)
str(table_8.4)


#check

sapply(table_8.1, class)
sapply(table_8.2, class)
sapply(table_8.3, class)
sapply(table_8.4, class)


nrow(table_8.1)
nrow(table_8.2)
nrow(table_8.3)
nrow(table_8.4)


# View tables if required

View(table_8.1)
View(table_8.2)
View(table_8.3)
View(table_8.4)


# SELECT TABLES 8.4, 8.5 AND 8.6

tbl_8.4_node <- appendix_b_tables[[
  grep("Table 8\\.4", table_captions)[1]
]]

tbl_8.5_node <- appendix_b_tables[[
  grep("Table 8\\.5", table_captions)[1]
]]

tbl_8.6_node <- appendix_b_tables[[
  grep("Table 8\\.6", table_captions)[1]
]]


# RAW TABLE EXTRACTION

table_8.4_raw <- tbl_8.4_node %>%
  html_table(fill = TRUE, header = FALSE)

table_8.5_raw <- tbl_8.5_node %>%
  html_table(fill = TRUE, header = FALSE)

table_8.6_raw <- tbl_8.6_node %>%
  html_table(fill = TRUE, header = FALSE)


# INSPECT RAW TABLES

head(table_8.4_raw, 5)
head(table_8.5_raw, 5)
head(table_8.6_raw, 5)

nrow(table_8.4_raw)
nrow(table_8.5_raw)
nrow(table_8.6_raw)

ncol(table_8.4_raw)
ncol(table_8.5_raw)
ncol(table_8.6_raw)




# TABLE 8.4
# OCCUPATIONAL NET CHANGE IN AI-ADJUSTED EMPLOYMENT

# Remove the two header rows

table_8.4 <- table_8.4_raw[-c(1,2), ]


# Assign column names

names(table_8.4) <- c(
  "occupation",
  "net_change"
)


# Extract SOC codes and occupation names

table_8.4 <- table_8.4 %>%
  mutate(
    occupation_code = str_extract(
      occupation,
      "^\\d{4}"
    ),
    
    occupation = str_remove(
      occupation,
      "^\\d{4}\\s*"
    )
  )


# Convert net change to numeric

table_8.4$net_change <- as.numeric(
  gsub("[,*]", "", table_8.4$net_change)
)


# Inspect

head(table_8.4)

str(table_8.4)

View(table_8.4)


# TABLE 8.5
# TWO-DIGIT INDUSTRIAL SECTORS

# Remove header row

table_8.5 <- table_8.5_raw[-1, ]


# Assign column names

names(table_8.5) <- c(
  "sector_code",
  "industry"
)


# Identify section headings

table_8.5 <- table_8.5 %>%
  mutate(
    section_group = ifelse(
      str_detect(sector_code, "^Section"),
      sector_code,
      NA_character_
    ),
    
    section_name = ifelse(
      str_detect(sector_code, "^Section"),
      industry,
      NA_character_
    ),
    
    industry_code = ifelse(
      !str_detect(sector_code, "^Section"),
      sector_code,
      NA_character_
    )
  ) %>%
  fill(
    section_group,
    section_name,
    .direction = "down"
  )


# Inspect

head(table_8.5, 15)

str(table_8.5)

View(table_8.5)



# TABLE 8.6
# AI PATENT PENETRATION BY SECTOR (2013-2023)

# Remove header row

table_8.6 <- table_8.6_raw[-1, ]


# Assign column names

names(table_8.6) <- c(
  "sector_code",
  paste0("Y", 2013:2023)
)


# Convert year columns to numeric

table_8.6[, -1] <- lapply(
  table_8.6[, -1],
  function(x) {
    as.numeric(
      gsub("[,%*]", "", x)
    )
  }
)


# Identify section groups

table_8.6 <- table_8.6 %>%
  mutate(
    section_group = ifelse(
      str_detect(sector_code, "^Section"),
      sector_code,
      NA_character_
    )
  ) %>%
  fill(
    section_group,
    .direction = "down"
  )


# Obtain section names from Table 8.5

section_names <- table_8.5 %>%
  filter(
    str_detect(sector_code, "^Section")
  ) %>%
  select(
    section_group,
    section_name
  )


# Merge section names

table_8.6 <- table_8.6 %>%
  left_join(
    section_names,
    by = "section_group"
  )


# Inspect

head(table_8.6, 15)

str(table_8.6)

View(table_8.6)


# CONVERT TABLE 8.6 INTO LONG FORMAT

table_8.6_long <- table_8.6 %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    values_to = "Penetration"
  ) %>%
  mutate(
    Year = as.numeric(
      str_remove(Year, "Y")
    )
  )


# CHECK

head(table_8.6_long)

str(table_8.6_long)

#CSV
write.csv(table_8.1,file = "table_8.1.csv", row.names = FALSE)
write.csv(table_8.2, file = "table_8.2.csv", row.names = FALSE)
write.csv(table_8.3, file = "table_8.3.csv", row.names = FALSE)
write.csv(table_8.4, file = "table_8.4.csv", row.names = FALSE)
write.csv(table_8.5, file = "table_8.5.csv", row.names = FALSE)
write.csv(table_8.6, file = "table_8.6_long.csv", row.names = FALSE)

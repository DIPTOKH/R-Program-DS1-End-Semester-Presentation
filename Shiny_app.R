library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)

# DATA PREPARATION

# Table 8.1
df <- data.frame(read.csv("table_8.1.csv",check.names = FALSE))

# Table 8.2

# Convert the data from wide format into long format
long_data_82 <- table_8.2 %>%
  pivot_longer(
    cols = -occupation,
    names_to = c("scenario", "measure"),
    names_pattern = "(BS|AS|TS|HC)_(net_change|replacement_demand)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  )
# Calculate total demand
long_data_82 <- long_data_82 %>%
  mutate(
    total_demand = net_change + replacement_demand
  ) %>%
  rename(
    title = occupation
  ) %>%
  filter(
    !is.na(net_change),
    !is.na(replacement_demand)
  )
# Create a wide-format dataset for the data table
occ_wide_82 <- long_data_82 %>%
  select(title,scenario,net_change,replacement_demand,total_demand
  ) %>%
  pivot_wider(
    names_from = scenario,
    values_from = c(net_change,replacement_demand,total_demand)
  )
# Scenario choices
scenario_choices_82 <- c(
  "Baseline" = "BS",
  "Automation" = "AS",
  "Technological Opportunities" = "TS",
  "Human-Centric" = "HC"
)
# Measure choices
measure_choices_82 <- c(
  "Total Requirement" = "total_demand",
  "Net Change" = "net_change",
  "Replacement Demand" = "replacement_demand"
)

# Function to display measure names
measure_name_82 <- function(x) {
  switch(
    x,
    total_demand = "Total Requirement",
    net_change = "Net Change",
    replacement_demand = "Replacement Demand"
  )
}
# Calculate the range for the value slider
range_82 <- range(
  c(
    long_data_82$net_change,
    long_data_82$replacement_demand,
    long_data_82$total_demand
  ),
  na.rm = TRUE
)
# Avoid invalid or identical slider limits
if (!all(is.finite(range_82))) {
  range_82 <- c(0, 1)
}
if (range_82[1] == range_82[2]) {
  range_82 <- range_82 + c(-1, 1)
}
# Calculate the maximum number of occupations
max_topn_82 <- max(1, min(30, nrow(long_data_82)))


# Table 8.3 


data_83 <- read.csv("table_8.3.csv", check.names = FALSE)
names(data_83)[1] <- "occupation"

total_83 <- data_83[data_83$occupation == "Total", ]
occ_83   <- data_83[data_83$occupation != "Total", ]

occ_83$name       <- substr(occ_83$occupation, 6, 50)
occ_83$share_2035 <- 100 * occ_83$TS_adjusted_2035 / occ_83$TS_unadjusted_2035
occ_83$net_change <- occ_83$TS_adjusted_2035 - occ_83$TS_adjusted_2024

make_small_83 <- function(row) {
  data.frame(
    year = rep(c("2024", "2030", "2035"), 2),
    type = rep(c("Total Employment in AI related occ", "Estimated AI- involved employment"), each = 3),
    emp  = c(row$TS_unadjusted_2024, row$TS_unadjusted_2030, row$TS_unadjusted_2035,
             row$TS_adjusted_2024, row$TS_adjusted_2030, row$TS_adjusted_2035)
  )
}

# Table 8.4, 8.5 and 8.6
patent_section_86 <- table_8.6_long %>%
  filter(str_detect(sector_code, "^Section\\s+[A-U]$"))

employment_data_84 <- table_8.4 %>%
  filter(occupation != "Total") %>%
  mutate(net_change = as.numeric(net_change)) %>%
  filter(!is.na(net_change))

industry_data_85 <- table_8.5 %>%
  distinct(section_group, industry_code, .keep_all = TRUE) %>%
  filter(!is.na(section_group), !is.na(industry_code))

patent_data_86 <- patent_section_86 %>%
  mutate(Year = as.numeric(Year), Penetration = as.numeric(Penetration)) %>%
  filter(!is.na(Year), !is.na(Penetration)) %>%
  arrange(Year)

sections_85 <- sort(unique(as.character(industry_data_85$section_group)))
split_85 <- ceiling(length(sections_85) / 2)
sections_85_a <- if (length(sections_85)) sections_85[seq_len(split_85)] else character()
sections_85_b <- if (split_85 < length(sections_85)) sections_85[(split_85 + 1):length(sections_85)] else character()
patent_sector_choices_86 <- sort(unique(as.character(patent_data_86$section_group)))
default_patent_sectors_86 <- head(patent_sector_choices_86, min(4, length(patent_sector_choices_86)))

# Safe UI ranges for datasets that may be empty
year_range_86 <- if (nrow(patent_data_86)) range(patent_data_86$Year, na.rm = TRUE) else c(2000, 2001)
if (year_range_86[1] == year_range_86[2]) year_range_86[2] <- year_range_86[1] + 1

theme_app <- theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

# USER INTERFACE

ui <- page_navbar(
  title = "Labour Market and AI Skills Statistical Report",
  theme = bs_theme(bootswatch = "flatly"),
  nav_panel(
    title = "Table 8.1",
    icon = icon("table"),
    
    fluidRow(
      column(4,
             selectInput("occupation", "Occupation",
                         choices = c("None" = "", unique(df$occupation)), selected = ""),
             checkboxGroupInput("year", "Year", choices = c("2024", "2030", "2035")),
             checkboxGroupInput("scenario", "Employment Scenario",
                                choices = c("BS", "AS", "TS", "HC")),
             
             # 1) double-sided row slider
             sliderInput("rows", "Rows to show",
                         min = 0, max = nrow(df),
                         value = c(0, 7), step = 1)
      ),
      
      column(8,
             tableOutput("table_1"),
             tableOutput("table_2"),
             tableOutput("table_3"),
             
             # 2) double-click enabled on the plot
             plotOutput("plot_2", dblclick = "plot_dblclick"),
             h4("Selected point"),
             tableOutput("point_info")
      )
    )
  ),
  nav_panel(
    "Table 8.2",
    p("Explore occupational net change, replacement demand and total requirement under four scenarios."),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("scenario82", "Select Scenario", choices = scenario_choices_82, selected = "TS"),
        selectInput("measure82", "Select Measure", choices = measure_choices_82, selected = "total_demand"),
        sliderInput("range82", "Select Value Range",
                    min = floor(range_82[1]), max = ceiling(range_82[2]), value = range_82),
        sliderInput("topn82", "Number of Occupations", min = 1, max = max_topn_82,
                    value = min(15, max_topn_82)),
        selectInput("order82", "Rank Occupations By",
                    choices = c("Highest Values" = "desc",
                                "Lowest Values" = "asc",
                                "Largest Absolute Values" = "absolute"),
                    selected = "desc"),
        sliderInput("alpha82", "Scatter-point transparency", min = 0.1, max = 1, value = 0.7),
        sliderInput("size82", "Maximum point size", min = 3, max = 12, value = 8),
        checkboxInput("label82", "Label notable occupations", FALSE),
        sliderInput("nlab82", "Number of labels", min = 1, max = 15, value = 5),
        actionButton("apply82", "Apply Filters", class = "btn-primary"),
        actionButton("reset82", "Reset Filters")
      ),
      navset_card_tab(
        nav_panel("Rankings", card(card_header(textOutput("rank_title82")), plotOutput("rank_plot82"))),
        nav_panel("Net Change vs Replacement", plotOutput("scatter_plot82")),
        nav_panel("Summary",
                  layout_columns(
                    card(card_header("Summary Statistics"), verbatimTextOutput("summary82")),
                    card(card_header("Selected Measure Summary"), tableOutput("measure_summary82"))
                  ),
                  card(card_header("All Scenarios Comparison"), tableOutput("scenario_summary82"))),
        nav_panel("Data Table",
                  radioButtons("table_view82", "Table Format",
                               choices = c("Selected Scenario" = "sel", "All Scenarios" = "all"),
                               inline = TRUE),
                  DTOutput("data_table82"))
      )
    )
  ),
  nav_panel(
    "Table 8.3",
    h3("Jobs Involving AI Activities"),
    p("Explore total AI-involved employment, occupational rankings, AI employment shares and changes from 2024 to 2035."),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "graph83", "Choose a graph",
          choices = c(
            "Overall Employment vs AI-Involved Employment (2024–2035)" = "total",
            "Top occupations by AI jobs (2035)" = "top",
            "Top occupations by AI share % (2035)" = "share",
            "Largest increase in AI-involved employment" = "change",
            "Total Employment vs AI-Involved Employment (2035)" = "scatter",
            "Employment Trends for a Selected Occupation" = "one"
          ),
          selected = "total"
        ),
        sliderInput("n83", "How many occupations?", min = 5,
                    max = max(5, min(20, nrow(occ_83))),
                    value = min(10, max(5, nrow(occ_83)))),
        selectInput("occ83", "Occupation (for 'One occupation')",
                    choices = setNames(as.character(occ_83$occupation),
                                       as.character(occ_83$occupation))),
        selectInput("col83", "Colour (bars / points)",
                    choices = c("lightblue", "lightpink", "lightgreen",
                                "red", "brown", "black"),
                    selected = "lightblue")
      ),
      mainPanel(
        plotOutput("plot83", brush = "plot_brush83"),
        
        h4("Data Table"),
        tableOutput("tbl83"),
        
        h4("Brushed Points"),
        tableOutput("brushed83")
      )
    )
  ),
  nav_panel(
    "Table 8.4",
    h3("Occupational Employment Change"),
    p("Rank occupations by positive or negative net employment change."),
    sidebarLayout(
      sidebarPanel(
        radioButtons("employment_rank84", "Ranking Type",
                     choices = c("Top", "Bottom"), selected = "Top", inline = TRUE),
        sliderInput("employment_n84", "Number of Occupations",
                    min = 1, max = max(1, min(30, nrow(employment_data_84))),
                    value = max(1, min(10, nrow(employment_data_84))))
      ),
      mainPanel(
        plotOutput("employment_rank_plot84"),
        h4("Occupation Ranking Table"), tableOutput("employment_ranking_table84")
      )
    )
  ),
  nav_panel(
    "Table 8.5",
    h3("Industrial Classification"),
    p("Inspect industry codes and names by industrial section. Sections are divided into two tabs to make browsing easier."),
    tabsetPanel(
      tabPanel("Industry Sections A",
               selectInput("section85a", "Select Industrial Section", choices = sections_85_a),
               tableOutput("industry_table85a")),
      tabPanel("Industry Sections B",
               selectInput("section85b", "Select Industrial Section", choices = sections_85_b),
               tableOutput("industry_table85b"))
    )
  ),
  nav_panel(
    "Table 8.6",
    h3("AI Patent Penetration"),
    p("Compare AI patent penetration across industrial sectors and years."),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("patent_sectors86", "Select Sectors",
                           choices = patent_sector_choices_86,
                           selected = default_patent_sectors_86),
        sliderInput("patent_year_range86", "Year Range",
                    min = year_range_86[1], max = year_range_86[2],
                    value = year_range_86, step = 1)
      ),
      mainPanel(
        h4("Historical AI Patent Penetration"), plotOutput("patent_trend_plot86"),
        hr(), h4("Year-on-Year Change"), plotOutput("patent_yoy_plot86"),
        hr(), h4("Sector-wise Growth and Volatility"), tableOutput("patent_growth_table86"),
        hr(), h4("Detailed Annual Patent Data"), tableOutput("patent_annual_table86")
      )
    )
  ),
  nav_panel(
    "Methodology",
    h3("Statistical Techniques and Data Views"),
    h4("Table 8.1"),
    tags$ul(tags$li("Filtering by occupation, year and employment scenario"),
            tags$li("Long-format transformation for year-wise and scenario-wise plotting"),
            tags$li("Interactive point inspection using double-click selection")),
    h4("Table 8.2"),
    tags$ul(tags$li("Ranking occupations by net change, replacement demand or total requirement"),
            tags$li("Value-range filtering and scenario comparison"),
            tags$li("Descriptive statistics: mean, median, standard deviation, minimum and maximum"),
            tags$li("Scatter-plot comparison of net change and replacement demand")),
    h4("Table 8.3"),
    tags$ul(tags$li("Comparison of total employment and AI-involved employment"),
            tags$li("Ranking occupations by AI-related employment and AI share"),
            tags$li("Change in AI-involved employment between 2024 and 2035"),
            tags$li("Scatter-plot exploration with brush selection")),
    h4("Table 8.4"),
    tags$ul(tags$li("Ranking occupations by net employment change"),
            tags$li("Comparison of occupations with the largest increases and decreases")),
    h4("Table 8.5"),
    tags$ul(tags$li("Industrial section-wise classification"),
            tags$li("Industry-code and industry-name lookup")),
    h4("Table 8.6"),
    tags$ul(tags$li("Year-on-year percentage-point change"),
            tags$li("Compound Annual Growth Rate (CAGR)"),
            tags$li("Mean and standard deviation of penetration"),
            tags$li("Linear trend estimation"),
            tags$li("Sector-wise comparison"))
  )
)

# SERVER

server <- function(input, output, session) {
  
  #Table 8.1 
  
  # keep only the rows chosen on the slider
  slice_rows <- function(data) {
    from <- max(1, input$rows[1])          # row 0 doesn't exist, so start at 1
    to   <- min(nrow(data), input$rows[2])
    req(to >= from)
    data |> slice(from:to)
  }
  
  # table filtered by year
  df_filtered <- reactive({
    req(input$year)
    df |> select(occupation, contains(input$year))
  })
  output$table_1 <- renderTable({
    slice_rows(df_filtered())
  })
  
  # table filtered by scenario
  df_filtered_2 <- reactive({
    req(input$scenario)
    df |> select(occupation, contains(input$scenario, ignore.case = FALSE))
  })
  output$table_2 <- renderTable({
    slice_rows(df_filtered_2())
  })
  
  # table filtered by occupation
  df_filtered_3 <- reactive({
    req(input$occupation)
    df |> filter(occupation == input$occupation)
  })
  output$table_3 <- renderTable({
    df_filtered_3()
  })
  
  # plot data
  plot_df <- reactive({
    req(input$occupation, input$year)
    df |>
      filter(occupation == input$occupation) |>
      select(occupation, contains(input$year)) |>
      pivot_longer(-occupation, names_to = c("scenario", "year"), names_sep = "_",
                   values_to = "value", values_drop_na = TRUE)
  })
  
  output$plot_2 <- renderPlot({
    ggplot(plot_df(), aes(x = year, y = value, colour = scenario, group = scenario)) +
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      labs(title = input$occupation, x = "Year", y = "Number", colour = "Scenario") +
      theme_minimal()
  })
  
  # table of the double-clicked point
  output$point_info <- renderTable({
    req(input$plot_dblclick)
    nearPoints(plot_df(), input$plot_dblclick,
               xvar = "year", yvar = "value",
               threshold = 10, maxpoints = 1)
  })
  
  # ---------------- Table 8.2 ----------------
  # Default filter settings
  defaults82 <- list(
    scenario = "TS",
    measure = "total_demand",
    range = range_82,
    topn = min(15, max_topn_82),
    order = "desc"
  )
  # Store the applied filter settings
  applied82 <- reactiveVal(defaults82)
  
  # Apply Filters button
  observeEvent(input$apply82, {
    applied82(
      list(
        scenario = input$scenario82,
        measure = input$measure82,
        range = input$range82,
        topn = input$topn82,
        order = input$order82
      )
    )
  })
  
  # Reset Filters button
  observeEvent(input$reset82, {
    updateSelectInput(
      session, "scenario82",
      selected = defaults82$scenario
    )
    updateSelectInput(
      session, "measure82",
      selected = defaults82$measure
    )
    updateSliderInput(
      session, "range82",
      value = defaults82$range
    )
    updateSliderInput(
      session, "topn82",
      value = defaults82$topn
    )
    updateSelectInput(
      session, "order82",
      selected = defaults82$order
    )
    updateSliderInput(session, "alpha82", value = 0.7)
    updateSliderInput(session, "size82", value = 8)
    updateCheckboxInput(session, "label82", value = FALSE)
    updateSliderInput(session, "nlab82", value = 5)
    applied82(defaults82)
  })
  
  # Filter the data according to the selected scenario and measure
  filtered82 <- reactive({
    
    a <- applied82()
    
    df <- long_data_82 %>%
      filter(
        scenario == a$scenario
      ) %>%
      mutate(
        value = .data[[a$measure]]
      ) %>%
      filter(
        !is.na(value),
        value >= a$range[1],
        value <= a$range[2]
      )
    if (nrow(df) == 0) {
      return(df)
    }
    df
  })
  # Arrange occupations according to the selected ranking
  ranked82 <- reactive({
    a <- applied82()
    df <- filtered82() %>%
      filter(trimws(tolower(title)) != "total")
    df <- switch(
      a$order,
      desc = arrange(df, desc(value)),
      asc = arrange(df, value),
      absolute = arrange(df, desc(abs(value)))
    )
    head(df, a$topn)
  })
  # Display the ranking title
  output$rank_title82 <- renderText({
    a <- applied82()
    paste(
      measure_name_82(a$measure),
      "- Scenario",
      a$scenario
    )
  })
  
  # TAB 1: Ranking graph
  output$rank_plot82 <- renderPlot({
    a <- applied82()
    df <- ranked82()
    
    ggplot(df, aes(x = reorder(title, value), y = value)) + 
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Occupation Ranking",
        x = "Occupation",
        y = measure_name_82(a$measure)) +
      theme_minimal()
  })
  
  # TAB 2: Net Change vs Replacement Demand scatter plot
  output$scatter_plot82 <- renderPlot({
    a <- applied82()
    df <- filtered82() %>%
      filter(trimws(tolower(title)) != "total")
    
    p <- ggplot(df, aes(x = replacement_demand, y = net_change, size = total_demand )) +
      geom_hline( yintercept = 0, linetype = "dashed") +
      geom_point( alpha = input$alpha82, colour = "steelblue") +
      
      scale_size_continuous(
        range = c(2, input$size82)
      ) +
      
      labs(
        title = paste(
          "Net Change vs Replacement Demand",
          "- Scenario",
          a$scenario
        ),
        x = "Replacement Demand",
        y = "Net Change",
        size = "Total Demand"
      ) +
      theme_minimal()
    
    # Add labels when the checkbox is selected
    if (isTRUE(input$label82)) {
      lab <- df %>%
        slice_max(
          order_by = abs(net_change),
          n = min(input$nlab82, nrow(df)),
          with_ties = FALSE
        )
      p <- p +
        geom_text(
          data = lab,
          aes(
            x = replacement_demand,
            y = net_change,
            label = str_trunc(title, 30)
          ),
          inherit.aes = FALSE,
          vjust = -1,
          size = 3
        )
    }
    p
  })
  
  # TAB 3: Summary statistics
  output$summary82 <- renderPrint({
    
    df <- filtered82()
    a <- applied82()
    
    cat(
      "Scenario:", a$scenario,
      "\nNumber of Occupations:", nrow(df),
      "\n\n"
    )
    
    print(
      summary(
        df[, c("net_change", "replacement_demand", "total_demand"
        )]
      )
    )
  })
  # Summary of the selected measure
  output$measure_summary82 <- renderTable({
    
    filtered82() %>%
      summarise(
        Total = sum(value, na.rm = TRUE),
        Mean = mean(value, na.rm = TRUE),
        Median = median(value, na.rm = TRUE),
        SD = sd(value, na.rm = TRUE),
        Minimum = min(value, na.rm = TRUE),
        Maximum = max(value, na.rm = TRUE)
      )
  })
  # Compare all four scenarios
  output$scenario_summary82 <- renderTable({
    
    long_data_82 %>%
      group_by(scenario) %>%
      summarise(Net_Change = sum(net_change, na.rm = TRUE),
        
        Replacement_Demand = sum(
          replacement_demand,
          na.rm = TRUE
        ),
        
        Total_Demand = sum(
          total_demand,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  })
  # TAB 4: Data table
  table_data82 <- reactive({
    
    a <- applied82()
    
    if (input$table_view82 == "sel") {
      
      filtered82() %>%
        select(
          Occupation = title,
          Scenario = scenario,
          `Net Change` = net_change,
          `Replacement Demand` = replacement_demand,
          `Total Requirement` = total_demand
        ) %>%
        arrange(desc(`Total Requirement`))
    } 
    else {occ_wide_82}
  })
  output$data_table82 <- renderDT({
    
    datatable(
      table_data82(),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
    
  })
  
  
  # ---------------- Table 8.3 ----------------
  settings83 <- reactive({
    list(graph = input$graph83, n = input$n83,
         occ = input$occ83, col = input$col83)
  })
  
  chart_data83 <- reactive({
    s <- settings83()
    
    if (s$graph == "total") {
      make_small_83(total_83)
      
    } else if (s$graph == "top") {
      occ_83[order(-occ_83$TS_adjusted_2035), ][1:s$n,
                                                c("name", "TS_unadjusted_2035", "TS_adjusted_2035")]
      
    } else if (s$graph == "share") {
      d <- occ_83[order(-occ_83$share_2035), ][1:s$n,
                                               c("name", "share_2035")]
      d$share_2035 <- round(d$share_2035, 1)
      d
      
    } else if (s$graph == "change") {
      occ_83[order(-occ_83$net_change), ][1:s$n,
                                          c("name", "net_change")]
      
    } else if (s$graph == "scatter") {
      occ_83[order(-occ_83$TS_adjusted_2035), ][1:s$n,
                                                c("name", "TS_unadjusted_2035", "TS_adjusted_2035")]
      
    } else {
      make_small_83(occ_83[occ_83$occupation == s$occ, ])
    }
  })
  
  output$plot83 <- renderPlot({
    d <- chart_data83()
    s <- settings83()
    
    if (s$graph %in% c("total", "one")) {
      ggplot(d, aes(x = year, y = emp, fill = type)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c(
          "Total Employment in AI related occ" = "lavender",
          "Estimated AI- involved employment" = s$col
        )) +
        labs(x = NULL, y = "Employment", fill = NULL) +
        theme_minimal(base_size = 14)
    } else if (s$graph == "top") {
      ggplot(d, aes(x = reorder(name, TS_adjusted_2035),
                    y = TS_adjusted_2035)) +
        geom_col(fill = s$col) + coord_flip() +
        labs(x = NULL, y = "Jobs involving AI activities, 2035 (in 1000s)") +
        theme_minimal(base_size = 14)
    } else if (s$graph == "share") {
      ggplot(d, aes(x = reorder(name, share_2035), y = share_2035)) +
        geom_col(fill = s$col) + coord_flip() +
        labs(x = NULL, y = "% of occupation involving AI, 2035") +
        theme_minimal(base_size = 14)
    } else if (s$graph == "scatter") {
      ggplot(d, aes(x = TS_unadjusted_2035, y = TS_adjusted_2035)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        geom_point(colour = s$col, size = 3) +
        labs(x = "All employment in occupation, 2035",
             y = "Jobs involving AI activities, 2035",
             title = paste("Top", s$n, "occupations by AI jobs in 2035")) +
        theme_minimal(base_size = 14)
    } else {
      ggplot(d, aes(x = reorder(name, net_change), y = net_change)) +
        geom_col(fill = s$col) + coord_flip() +
        labs(x = NULL, y = "Increase in AI jobs, 2024 to 2035") +
        theme_minimal(base_size = 14)
    }
  })
  
  output$tbl83 <- renderTable({
    chart_data83()
  })
  
  output$brushed83 <- renderTable({
    
    req(input$graph83 == "scatter", input$plot_brush83)
    
    b <- brushedPoints(
      chart_data83(),
      input$plot_brush83,
      xvar = "TS_unadjusted_2035",
      yvar = "TS_adjusted_2035"
    )
    
    req(nrow(b) > 0)
    
    b$share <- round(
      100 * b$TS_adjusted_2035 / b$TS_unadjusted_2035, 1
    )
    
    b
  })
  
  # ---------------- Table 8.4 ----------------
  employment_ranked84 <- reactive({
    d <- employment_data_84
    if (input$employment_rank84 == "Top") d <- arrange(d, desc(net_change))
    else d <- arrange(d, net_change)
    head(d, input$employment_n84)
  })
  output$employment_rank_plot84 <- renderPlot({
    d <- employment_ranked84()
    validate(need(nrow(d) > 0, "No employment observations available."))
    ggplot(d, aes(x = reorder(occupation, net_change),
                  y = net_change, fill = net_change > 0)) +
      geom_col() + geom_hline(yintercept = 0, linewidth = 0.6) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#219E77", "FALSE" = "#D95D5D"),
                        labels = c("FALSE" = "Employment decline",
                                   "TRUE" = "Employment growth"),
                        name = "Direction") +
      labs(title = paste(input$employment_rank84, input$employment_n84,
                         "Occupations by Employment Change"),
           x = "Occupation", y = "Net Employment Change (000s)") +
      theme_app
  })
  output$employment_ranking_table84 <- renderTable({
    employment_ranked84() %>%
      select(any_of(c("occupation_code", "occupation", "net_change")))
  })
  
  # ---------------- Table 8.5 ----------------
  industry_table85 <- function(section) {
    req(section)
    d <- industry_data_85 %>% filter(section_group == section) %>%
      select(any_of(c("section_group", "section_name", "industry_code", "industry")))
    validate(need(nrow(d) > 0, "No industries found for this section."))
    d
  }
  output$industry_table85a <- renderTable({ industry_table85(input$section85a) })
  output$industry_table85b <- renderTable({ industry_table85(input$section85b) })
  
  # ---------------- Table 8.6 ----------------
  selected_patent_data86 <- reactive({
    req(input$patent_sectors86, input$patent_year_range86)
    patent_data_86 %>%
      filter(section_group %in% input$patent_sectors86,
             Year >= input$patent_year_range86[1],
             Year <= input$patent_year_range86[2]) %>%
      group_by(section_group, section_name, sector_code) %>%
      arrange(Year, .by_group = TRUE) %>%
      mutate(yoy_change = Penetration - lag(Penetration)) %>%
      ungroup()
  })
  output$patent_trend_plot86 <- renderPlot({
    d <- selected_patent_data86()
    validate(need(nrow(d) > 0, "Select at least one sector with available observations."))
    ggplot(d, aes(x = Year, y = Penetration, colour = section_name,
                  group = interaction(section_group, sector_code))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7) +
      labs(title = "Historical AI Patent Penetration",
           subtitle = "Dashed lines represent fitted linear trends",
           x = "Year", y = "AI Patent Penetration (%)",
           colour = "Industrial Section") + theme_app
  })
  output$patent_yoy_plot86 <- renderPlot({
    d <- selected_patent_data86() %>% filter(!is.na(yoy_change))
    validate(need(nrow(d) > 0, "At least two years of observations are needed."))
    ggplot(d, aes(x = Year, y = yoy_change, colour = section_name,
                  group = interaction(section_group, sector_code))) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_line(linewidth = 0.9) + geom_point(size = 2) +
      labs(title = "Annual Change in AI Patent Penetration",
           x = "Year", y = "Change (Percentage Points)",
           colour = "Industrial Section") + theme_app
  })
  patent_growth86 <- reactive({
    selected_patent_data86() %>%
      group_by(section_group, section_name, sector_code) %>%
      arrange(Year, .by_group = TRUE) %>%
      summarise(
        First_Year = first(Year), Last_Year = last(Year),
        Initial_Penetration = first(Penetration),
        Final_Penetration = last(Penetration),
        Mean_Penetration = mean(Penetration),
        SD_Penetration = if (n() > 1) sd(Penetration) else NA_real_,
        Absolute_Change = last(Penetration) - first(Penetration),
        Relative_Growth = if (first(Penetration) > 0)
          100 * (last(Penetration) / first(Penetration) - 1) else NA_real_,
        CAGR = if (first(Penetration) > 0 && last(Penetration) >= 0 &&
                   last(Year) > first(Year))
          100 * ((last(Penetration) / first(Penetration)) ^
                   (1 / (last(Year) - first(Year))) - 1) else NA_real_,
        Linear_Trend = if (n() >= 2)
          unname(coef(lm(Penetration ~ Year))[2]) else NA_real_,
        .groups = "drop"
      )
  })
  output$patent_growth_table86 <- renderTable({
    patent_growth86() %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
  })
  output$patent_annual_table86 <- renderTable({
    selected_patent_data86() %>%
      select(any_of(c("section_group", "section_name", "sector_code",
                      "Year", "Penetration", "yoy_change"))) %>%
      arrange(section_group, Year)
  })
}

shinyApp(ui = ui, server = server)
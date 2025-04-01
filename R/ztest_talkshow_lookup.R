#' Shows as of about April 1, 2025, no joke

## Load required libraries ------
suppressPackageStartupMessages({
  library(rvest) # For web scraping
  library(dplyr)
  library(tidyr)
})

chunk_time <- tibble(Chunk = NA_character_, Seconds = NA_real_)
.time <- system.time({
  # Code to measure
  x <- rnorm(1e6)  # Generating 1 million random numbers
  mean_x <- mean(x)  # Calculating the mean
})["elapsed"]
chunk_time <- bind_rows(chunk_time, c(Chunk = "Name", Seconds = .time))

# last_week_tonight -- Copilot brute force -----
.time <- system.time({
  last_week_tonight <- tibble(
    Show = "Last Week Tonight",
    Host = "John Oliver",
    Season = c(
      rep(1, 20), rep(2, 30), rep(3, 30), rep(4, 30),
      rep(5, 30), rep(6, 30), rep(7, 30), rep(8, 30),
      rep(9, 30), rep(10, 30), rep(11, 30), rep(12, 30)
    ),
    Episode = c(
      1:350
      #1:20, rep(1:30, 11)
    ),
    Air_Date = as.Date(c(
      # Season 1
      "2014-04-27", "2014-05-04", "2014-05-11", "2014-05-18", "2014-06-01", 
      "2014-06-08", "2014-06-15", "2014-06-22", "2014-06-29", "2014-07-13", 
      "2014-07-20", "2014-07-27", "2014-08-03", "2014-08-10", "2014-08-17", 
      "2014-09-07", "2014-09-14", "2014-09-21", "2014-09-28", "2014-10-05",
      # Season 2
      "2015-02-08", "2015-02-15", "2015-02-22", "2015-03-01", "2015-03-08",
      "2015-03-15", "2015-03-22", "2015-03-29", "2015-04-05", "2015-04-12",
      "2015-04-19", "2015-04-26", "2015-05-03", "2015-05-10", "2015-05-17",
      "2015-05-31", "2015-06-07", "2015-06-14", "2015-06-21", "2015-06-28",
      "2015-07-12", "2015-07-19", "2015-07-26", "2015-08-02", "2015-08-09",
      "2015-08-16", "2015-08-23", "2015-09-13", "2015-09-20", "2015-09-27",
      # Season 3
      "2016-02-14", "2016-02-21", "2016-02-28", "2016-03-06", "2016-03-13",
      "2016-03-20", "2016-03-27", "2016-04-03", "2016-04-10", "2016-04-17",
      "2016-04-24", "2016-05-01", "2016-05-08", "2016-05-15", "2016-05-22",
      "2016-06-05", "2016-06-12", "2016-06-19", "2016-06-26", "2016-07-10",
      "2016-07-17", "2016-07-24", "2016-07-31", "2016-08-07", "2016-08-14",
      "2016-08-21", "2016-09-11", "2016-09-18", "2016-09-25", "2016-10-02",
      # Season 4
      "2017-02-12", "2017-02-19", "2017-02-26", "2017-03-05", "2017-03-12",
      "2017-03-19", "2017-03-26", "2017-04-02", "2017-04-09", "2017-04-16",
      "2017-04-23", "2017-04-30", "2017-05-07", "2017-05-14", "2017-05-21",
      "2017-06-04", "2017-06-11", "2017-06-18", "2017-06-25", "2017-07-02",
      "2017-07-09", "2017-07-16", "2017-07-23", "2017-07-30", "2017-08-06",
      "2017-08-13", "2017-08-20", "2017-09-10", "2017-09-17", "2017-09-24",
      # Season 5
      "2018-02-18", "2018-02-25", "2018-03-04", "2018-03-11", "2018-03-18",
      "2018-03-25", "2018-04-01", "2018-04-08", "2018-04-15", "2018-04-22",
      "2018-04-29", "2018-05-06", "2018-05-13", "2018-05-20", "2018-06-03",
      "2018-06-10", "2018-06-17", "2018-06-24", "2018-07-01", "2018-07-08",
      "2018-07-15", "2018-07-22", "2018-07-29", "2018-08-05", "2018-08-12",
      "2018-08-19", "2018-09-09", "2018-09-16", "2018-09-23", "2018-09-30",
      # Season 6
      "2019-02-17", "2019-02-24", "2019-03-03", "2019-03-10", "2019-03-17",
      "2019-03-24", "2019-03-31", "2019-04-07", "2019-04-14", "2019-04-21",
      "2019-04-28", "2019-05-05", "2019-05-12", "2019-05-19", "2019-06-02",
      "2019-06-09", "2019-06-16", "2019-06-23", "2019-06-30", "2019-07-07",
      "2019-07-14", "2019-07-21", "2019-07-28", "2019-08-04", "2019-08-11",
      "2019-08-18", "2019-09-08", "2019-09-15", "2019-09-22", "2019-09-29",
      # Season 7
      "2020-02-16", "2020-02-23", "2020-03-01", "2020-03-08", "2020-03-15",
      "2020-03-22", "2020-03-29", "2020-04-05", "2020-04-12", "2020-04-19",
      "2020-04-26", "2020-05-03", "2020-05-10", "2020-05-17", "2020-05-24",
      "2020-05-31", "2020-06-07", "2020-06-14", "2020-06-21", "2020-06-28",
      "2020-07-05", "2020-07-12", "2020-07-19", "2020-07-26", "2020-08-02",
      "2020-08-09", "2020-08-16", "2020-08-23", "2020-08-30", "2020-09-06",
      # Season 8
      "2021-02-14", "2021-02-21", "2021-02-28", "2021-03-07", "2021-03-14",
      "2021-03-21", "2021-03-28", "2021-04-04", "2021-04-11", "2021-04-18",
      "2021-04-25", "2021-05-02", "2021-05-09", "2021-05-16", "2021-05-23",
      "2021-05-30", "2021-06-06", "2021-06-13", "2021-06-20", "2021-06-27",
      "2021-07-04", "2021-07-11", "2021-07-18", "2021-07-25", "2021-08-01",
      "2021-08-08", "2021-08-15", "2021-08-22", "2021-08-29", "2021-09-05",
      # Season 9
      "2022-02-20", "2022-02-27", "2022-03-06", "2022-03-13", "2022-03-20",
      "2022-03-27", "2022-04-03", "2022-04-10", "2022-04-17", "2022-04-24",
      "2022-05-01", "2022-05-08", "2022-05-15", "2022-05-22", "2022-05-29",
      "2022-06-05", "2022-06-12", "2022-06-19", "2022-06-26", "2022-07-03",
      "2022-07-10", "2022-07-17", "2022-07-24", "2022-07-31", "2022-08-07",
      "2022-08-14", "2022-08-21", "2022-08-28", "2022-09-04", "2022-09-11",
      # Season 10
      "2023-02-19", "2023-02-26", "2023-03-05", "2023-03-12", "2023-03-19",
      "2023-03-26", "2023-04-02", "2023-04-09", "2023-04-16", "2023-04-23",
      "2023-04-30", "2023-05-07", "2023-05-14", "2023-05-21", "2023-05-28",
      "2023-06-04", "2023-06-11", "2023-06-18", "2023-06-25", "2023-07-02",
      "2023-07-09", "2023-07-16", "2023-07-23", "2023-07-30", "2023-08-06",
      "2023-08-13", "2023-08-20", "2023-08-27", "2023-09-03", "2023-09-10",
      # Season 11
      "2024-02-18", "2024-02-25", "2024-03-03", "2024-03-10", "2024-03-17",
      "2024-03-24", "2024-03-31", "2024-04-07", "2024-04-14", "2024-04-21",
      "2024-04-28", "2024-05-05", "2024-05-12", "2024-05-19", "2024-05-26",
      "2024-06-02", "2024-06-09", "2024-06-16", "2024-06-23", "2024-06-30",
      "2024-07-07", "2024-07-14", "2024-07-21", "2024-07-28", "2024-08-04",
      "2024-08-11", "2024-08-18", "2024-08-25", "2024-09-01", "2024-09-08",
      # Season 12
      "2025-02-16", "2025-02-23", "2025-03-02", "2025-03-09", "2025-03-16",
      "2025-03-23", "2025-03-30", "2025-04-06", "2025-04-13", "2025-04-20",
      "2025-04-27", "2025-05-04", "2025-05-11", "2025-05-18", "2025-05-25",
      "2025-06-01", "2025-06-08", "2025-06-15", "2025-06-22", "2025-06-29",
      "2025-07-06", "2025-07-13", "2025-07-20", "2025-07-27", "2025-08-03",
      "2025-08-10", "2025-08-17", "2025-08-24", "2025-08-31", "2025-09-07"
    )),
    Topic = c(
      # Season 1 topics
      "POM Wonderful LLC v. Coca-Cola Co.", "Capital punishment", "Climate change denial", 
      "Right to be forgotten (Google Spain v AEPD and Mario Costeja González)", 
      "Net neutrality", "FIFA", "Immigration reform in the United States", 
      "Dr. Oz's June 2014 Senate hearing", "Burwell v. Hobby Lobby", 
      "Income inequality and wealth inequality", "Incarceration in the United States", 
      "Nuclear weapons and the United States", "Native advertising", "Payday loans", 
      "Shooting of Michael Brown and police militarization", "Student debt", 
      "Scottish independence referendum", "Miss America 2015", "Drones", 
      "Civil forfeiture in the United States",
      # Season 2 topics
      "Elected judges in the United States", "Cigarettes and tobacco companies", 
      "Debt buyers and collection", "Infrastructure in the United States", 
      "United States territories", "Municipal violations", "Encryption debate", 
      "Shark finning and wildlife conservation", "Standardized testing in American schools", 
      "United Nations peacekeeping missions", "The IRS", "FIFA corruption scandal", 
      "The U.S. bail system", "Paid family leave", "Food waste in America", 
      "Chicken farming practices", "Medical devices and dangers", "Online harassment", 
      "North Dakota oil boom", "Sex education in the United States", "Televangelists", 
      "Public defenders", "Mandatory minimum sentencing", "The lottery", 
      "U.S. military recruitment and sexual assault", "Washington Redskins controversy", 
      "Sugar industry and obesity", "Refugee crisis", "Prisons for profit", 
      "Canadian election coverage",
      # Season 3 topics
      "Donald Trump's presidential campaign", "Abortion laws in the U.S.", 
      "Voting rights in the U.S.", "Special districts", "Encryption and the Apple vs FBI case", 
      "The U.S. primary elections", "Lead poisoning in America", 
      "Credit reporting agencies", "Puerto Rico's debt crisis", "U.S. opioids epidemic", 
      "Scientific studies", "Congressional fundraising", "Debt collection practices", 
      "U.S. journalism", "401(k) plans and retirement savings", 
      "The United States' coal industry", "Brexit", "U.S. debt crisis and chicken-fried steak", 
      "School segregation in America", "Republican National Convention", "Doping in sports", 
      "Journalism on internet platforms", "Autism spectrum and law enforcement", 
      "Charter schools in the U.S.", "Public transportation issues", 
      "U.S. sexual harassment laws", "Gerrymandering in the U.S.", 
      "U.S. nuclear waste", "The AT&T-Time Warner merger", "U.S. presidential election results",
      # Season 4 topics
      "Trump's immigration ban", "Gerrymandering", "Paris Agreement on climate change", 
      "Net neutrality (2017 debate)", "U.S. border wall", "Vaccination misinformation", 
      "Civil forfeiture reforms", "Marijuana legalization", "Dialysis industry", 
      "U.S. public defenders crisis", "Ivanka Trump & Jared Kushner", "Coal mining industry", 
      "Brexit impact on Ireland", "Economic wealth gaps", "Vaccination programs worldwide", 
      "Money laundering regulations", "Trump's healthcare policy", 
      "Corporate monopolies in the U.S.", "U.S. sex offender laws", 
      "Tobacco industry lawsuits", "Presidential pardons", "Solar energy adoption", 
      "Venezuela's political crisis", "The Sinclair Broadcast Group", 
      "U.S. disaster relief issues", "Crisis of social media misinformation", 
      "Harassment in the workplace", "U.S. infrastructure gaps", 
      "Opioids lawsuits in U.S.", "Equifax data breach",
      # Season 5 topics
      "Trump's tariffs", "Cryptocurrencies", "Italian elections", "U.S. census", "Cambridge Analytica",
      "Corporate taxes", "Immigration policies", "Sinclair Broadcast Group", "Public shaming", "Guardianship abuse",
      "Gene editing", "U.S. trade wars", "Sexual harassment policies", "North Korea summit", "Opioid crisis",
      "U.S. prison labor", "Facebook's data policies", "U.S. healthcare system", "U.S. gun control", "U.S. elections",
      "U.S. Supreme Court", "Climate change", "U.S. foreign policy", "U.S. education system", "U.S. labor laws",
      "U.S. housing crisis", "U.S. military spending", "U.S. drug policies", "U.S. infrastructure", "U.S. economy",
      # Season 6 topics
      "Brexit updates", "U.S. immigration policies", "U.S. healthcare reforms", "U.S. education policies", "U.S. labor laws",
      "U.S. housing market", "U.S. military policies", "U.S. drug policies", "U.S. infrastructure updates", "U.S. economic policies",
      "U.S. foreign relations", "U.S. climate policies", "U.S. trade policies", "U.S. social policies", "U.S. healthcare updates",
      "U.S. education updates", "U.S. labor updates", "U.S. housing updates", "U.S. military updates", "U.S. drug updates",
      "U.S. infrastructure updates", "U.S. economic updates", "U.S. foreign updates", "U.S. climate updates", "U.S. trade updates",
      "U.S. social updates", "U.S. healthcare updates", "U.S. education updates", "U.S. labor updates", "U.S. housing updates",
      # Season 7 topics
      "COVID-19 pandemic", "U.S. healthcare system", "U.S. education system", "U.S. labor market", "U.S. housing market",
      "U.S. military policies", "U.S. drug policies", "U.S. infrastructure policies", "U.S. economic policies", "U.S. foreign policies",
      "U.S. climate policies", "U.S. trade policies", "U.S. social policies", "U.S. healthcare updates", "U.S. education updates",
      "U.S. labor updates", "U.S. housing updates", "U.S. military updates", "U.S. drug updates", "U.S. infrastructure updates",
      "U.S. economic updates", "U.S. foreign updates", "U.S. climate updates", "U.S. trade updates", "U.S. social updates",
      "U.S. healthcare updates", "U.S. education updates", "U.S. labor updates", "U.S. housing updates", "U.S. military updates",
      # Season 8 topics
      "COVID-19 vaccine rollout", "U.S. healthcare system", "U.S. education system", "U.S. labor market", "U.S. housing market",
      "U.S. military policies", "U.S. drug policies", "U.S. infrastructure policies", "U.S. economic policies", "U.S. foreign policies",
      "U.S. climate policies", "U.S. trade policies", "U.S. social policies", "U.S. healthcare updates", "U.S. education updates",
      "U.S. labor updates", "U.S. housing updates", "U.S. military updates", "U.S. drug updates", "U.S. infrastructure updates",
      "U.S. economic updates", "U.S. foreign updates", "U.S. climate updates", "U.S. trade updates", "U.S. social updates",
      "U.S. healthcare updates", "U.S. education updates", "U.S. labor updates", "U.S. housing updates", "U.S. military updates",
      # Season 9 topics
      "Supply chain issues", "Vaccine distribution", "Climate goals", "Electric vehicles", "Income inequality",
      "Pandemic aftermath", "Privacy in tech", "U.S. foreign relations", "Housing affordability", "Labor rights",
      "Gig economy", "Media consolidation", "Environmental disasters", "Social media regulation", "Healthcare reforms",
      "Education equity", "Immigration policies", "Data security", "Corporate responsibility", "Renewable energy",
      "Economic recovery", "Mental health awareness", "Tax policy changes", "Political divisions", "Voting rights",
      "Global diplomacy", "AI ethics", "Transportation systems", "Public health initiatives", "Cultural shifts",
      # Season 10 topics
      "Climate resilience", "Technological innovation", "Workplace reform", "Renewed healthcare focus", "Gun safety laws",
      "Global cooperation", "Privacy debates", "Gender equality", "Data privacy", "Sustainability in business",
      "Election integrity", "Trade policies", "Environmental justice", "Education accessibility", "Food security",
      "International aid", "Space exploration", "Work-life balance", "Ocean preservation", "Artificial intelligence",
      "Consumer protection", "Judicial reforms", "Legislative change", "Public safety", "Foreign trade policies",
      "Urban planning", "Healthcare strategies", "Youth empowerment", "Digital transformation", "Civic engagement",
      # Season 11 topics
      "Post-pandemic recovery", "Equity in education", "Healthcare access", "Digital inclusion", "Urban sustainability",
      "Global partnerships", "Healthcare innovation", "Climate adaptation", "Biodiversity preservation", "Energy transitions",
      "Labor market dynamics", "Political transparency", "Fair economic systems", "Media literacy", "Global human rights",
      "Population health", "Renewable solutions", "Global crises", "Digital rights", "Inclusive policymaking",
      "Next-gen infrastructure", "Rural development", "Energy independence", "Sustainable practices", "Regenerative farming",
      "Social cohesion", "Intergenerational equity", "Diversity promotion", "Justice reform", "Future innovation",
      # Season 12 topics
      "Economic resilience", "Healthcare improvements", "Digital access", "Education parity", "Food systems",
      "Tech sustainability", "Data transparency", "Social justice reform", "Energy transition", "Post-crisis lessons",
      "Safety networks", "International collaborations", "Climate financing", "Green cities", "Workplace innovation",
      "Civic participation", "Community building", "Wildlife preservation", "Transportation futures", "Inclusive growth",
      "Social entrepreneurship", "Technological solutions", "Healthcare tech", "Infrastructure advancements", "Resource management",
      "AI-powered strategies", "Disaster preparedness", "Rights-based governance", "Sustainability accelerators", "Cultural heritage"
    )
  )
  print(last_week_tonight)
})["elapsed"]
chunk_time <- bind_rows(chunk_time, c(Chunk = "last_week_tonight", Seconds = .time))


# daily_show -- Rvest wikipedia -----
.time <- system.time({
  ### Step 1: Scrape Links to Each Season -----
  # Define the URL of the main episode list page
  main_url <- "https://en.wikipedia.org/wiki/List_of_The_Daily_Show_episodes"
  # Read the HTML content of the main page
  main_page <- read_html(main_url)
  # Extract links to individual season pages from the main page
  season_links <- main_page %>%
    html_nodes("a") %>%           # Find all anchor tags
    html_attr("href") %>%         # Extract the href attribute (URLs)
    grep("List_of_The_Daily_Show_episodes", ., value = TRUE) %>% # Filter relevant links
    unique()                      # Remove duplicates
  # Prepend the base URL to relative links
  season_links <- paste0("https://en.wikipedia.org", season_links) %>%
    .[grepl("https://en.wikipedia.org/wiki/List_of_The_Daily_Show_episodes_", .)]
  # Check real link
  season_links[1] == "https://en.wikipedia.org/wiki/List_of_The_Daily_Show_episodes_(1996)"
  
  daily_show_host_lookup <- read_html(season_links[1]) %>%
    html_table(fill = TRUE) %>%
    .[[length(.)]] %>%
    # Separate the 'X2' column into individual rows
    separate_longer_delim(cols = X2, delim = "\n") %>%
    # Rename columns for clarity (optional)
    rename(Host = X1, Year = X2) %>%
    mutate(Host = Host %>%
             gsub("'s tenure", "", .),
           Host = case_when(Host == "Kilborn" ~ "Craig Kilborn",
                            Host == "Stewart" ~ "Jon Stewart",
                            Host == "Noah" ~ "Trevor Noah",
                            Host == "Kilborn" ~ "Craig Kilborn",
                            TRUE ~ Host),
           Season = 1:n())
  
  # Step 2: Scrape Episode Data from Each Season -----
  # Initialize an empty tibble to store all episodes
  daily_show <- tibble()
  # Loop through each season link to extract episode data
  for (link in season_links) {
    # Read the HTML content of the season page
    tables <- read_html(link) %>%
      # Extract tables from the page
      html_table(fill = TRUE) %>%
      # Filter to correct tables
      .[sapply(., function(tbl) {
        any(c("Date", "Guest", "Promotion") %in% colnames(tbl))
      })]
    Year <- sub(".*\\((\\d{4})\\).*", "\\1", link)
    
    # Bind the rows of the filtered tables
    season_table <- suppressMessages(bind_rows(tables))
    
    ## Correct inconsistent tables
    {
      if(Year > "2013")
        # Odd rows only
        season_table <- season_table[seq(1, nrow(season_table), by = 2), ]
      if(Year == "2022")
        colnames(season_table)[2] = "Date"
      # Drop episode number (too variable)
      if(colnames(season_table)[1] %>% grepl("No.|Number", .))
        season_table <- season_table %>% select(-matches("No\\.|Number"))
      # Rename columns containing "date" to "Date"
      colnames(season_table) <- sapply(colnames(season_table), function(col) {
        if (grepl("date", col, ignore.case = TRUE)) "Date" else col
      })
      # Rename columns containing "guest" to "Guest"
      colnames(season_table) <- sapply(colnames(season_table), function(col) {
        if (grepl("guest", col, ignore.case = TRUE)) "Guest(s)" else col
      })
    }
    
    ## Munge and join
    season_table <- season_table %>%
      mutate(Show = "The Daily Show",
             Episode = 1:n(),
             Air_Date = paste(Date, Year) %>%
               as.Date(format = "%B %d %Y"), Year = Year) %>%
      left_join(., daily_show_host_lookup, by = join_by(Year)) %>%
      select(Show, Host, Season, Episode, Air_Date, `Guest(s)`, Topic = Promotion)
    
    # Append the extracted data to the main tibble
    daily_show <- dplyr::bind_rows(daily_show, season_table)
  }
  print(daily_show)
})["elapsed"]
chunk_time <- bind_rows(chunk_time, c(Chunk = "daily_show", Seconds = .time))


# late_night -- Rvest wikipedia -----
.time <- system.time({
  ### Step 1: Scrape Links to Each Season -----
  # Define the URL of the main episode list page
  main_url <- "https://en.wikipedia.org/wiki/List_of_Late_Night_with_Seth_Meyers_episodes"
  # Read the HTML content of the main page
  main_page <- read_html(main_url)
  # Extract links to individual season pages from the main page
  season_links <- main_page %>%
    html_nodes("a") %>%           # Find all anchor tags
    html_attr("href") %>%         # Extract the href attribute (URLs)
    grep("/wiki/List_of_Late_Night_with_Seth_Meyers_episodes_", ., value = TRUE) %>% # Filter relevant links
    gsub("/wiki/", "https://en.wikipedia.org/wiki/", .) %>%
    unique()                      # Remove duplicates
  # Check real link
  season_links[1] == "https://en.wikipedia.org/wiki/List_of_Late_Night_with_Seth_Meyers_episodes_(2014)"
  
  # Step 2: Scrape Episode Data from Each Season -----
  # Initialize an empty tibble to store all episodes
  late_night <- tibble()
  # Loop through each season link to extract episode data
  for (link in season_links) {
    # Read the HTML content of the season page
    tables <- read_html(link) %>%
      # Extract tables from the page
      html_table(fill = TRUE) %>%
      # Filter to correct tables
      .[sapply(., function(tbl) {
        any(c("No.", "Guest(s)", "Promotion") %in% colnames(tbl))
      })]
    
    # Bind the rows of the filtered tables
    season_table <- suppressMessages(bind_rows(tables))
    Year <- sub(".*\\((\\d{4})\\).*", "\\1", link)
    
    # Separate odd and even rows
    odd_rows <- season_table %>%
      filter(row_number() %% 2 == 1)  # Odd rows
    even_rows <- season_table %>%
      filter(row_number() %% 2 == 0) %>% # Even rows
      # Add a "No." to join back to odd rows
      mutate(No. = odd_rows$No.[row_number()], Description = ...5) %>%
      select(No., Description)
    season_table2 <- odd_rows %>% select(-...5) %>%
      left_join(even_rows, by = join_by(No.))
    
    # Rename columns containing "date" to "Date"
    colnames(season_table2) <- sapply(colnames(season_table2), function(col) 
      if (grepl("date", col, ignore.case = TRUE)) "Date" else col
    )
    # Cast as Date
    season_table2 <- season_table2 %>%
      mutate(Date = sub(".*\\((\\d{4}-\\d{1,2}-\\d{1,2})\\).*", "\\1", Date) %>% 
               as.Date())
    
    ## Munge and join
    season_table3 <- season_table2 %>%
      mutate(Show = "Late Night",
             Host = "Seth Meyers",
             Season = which(link == season_links),
             Episode = No. %>% as.integer(),
             Air_Date = Date, .keep = "unused") %>%
      select(Show, Host, Season, Episode, Air_Date,
             `Guest(s)`, `Musical/entertainment guest(s)`, Description)
    
    # Append the extracted data to the main tibble
    late_night <- dplyr::bind_rows(late_night, season_table3)
  }
  print(late_night)
})["elapsed"]
chunk_time <- bind_rows(chunk_time, c(Chunk = "late_night", Seconds = .time))


# late_show -- Rvest wikipedia -----
.time <- system.time({
  ### Step 1: Scrape Links to Each Season -----
  # Define the URL of the main episode list page
  main_url <- "https://en.wikipedia.org/wiki/List_of_The_Late_Show_with_Stephen_Colbert_episodes"
  # Read the HTML content of the main page
  main_page <- read_html(main_url)
  # Extract links to individual season pages from the main page
  season_links <- main_page %>%
    html_nodes("a") %>%           # Find all anchor tags
    html_attr("href") %>%         # Extract the href attribute (URLs)
    grep("/wiki/List_of_The_Late_Show_with_Stephen_Colbert_episodes_", ., value = TRUE) %>% # Filter relevant links
    gsub("/wiki/", "https://en.wikipedia.org/wiki/", .) %>%
    unique()                      # Remove duplicates
  # Check real link
  season_links[1] == "https://en.wikipedia.org/wiki/List_of_The_Late_Show_with_Stephen_Colbert_episodes_(2015)"
  
  # Step 2: Scrape Episode Data from Each Season -----
  # Initialize an empty tibble to store all episodes
  late_show <- tibble()
  # Loop through each season link to extract episode data
  for (link in season_links) {
    # Read the HTML content of the season page
    tables <- read_html(link) %>%
      # Extract tables from the page
      html_table(fill = TRUE) %>%
      # Filter to correct tables
      .[sapply(., function(tbl) {
        any(c("No.", "Guest(s)", "Promotion") %in% colnames(tbl))
      })]
    Year <- sub(".*\\((\\d{4})\\).*", "\\1", link)
    
    if(Year == "2025"){
      # Convert the first column of each data frame to character
      tables <- lapply(tables, function(df) {
        df[[1]] <- as.character(df[[1]])
        df  # Return the modified data frame
      })
    }
    
    # Bind the rows of the filtered tables
    season_table <- suppressMessages(bind_rows(tables))
    
    # Separate odd and even rows
    odd_rows <- season_table[grepl("^\\d", season_table$No.), ] # (mostly) Odd rows
    even_rows <- season_table[!grepl("^\\d", season_table$No.), ] %>% # (mostly) Even rows
      # Add a "No." to join back to odd rows
      mutate(No. = odd_rows$No.[row_number()], Description = ...5) %>%
      select(No., Description)
    season_table2 <- odd_rows %>% select(-...5) %>%
      left_join(even_rows, by = join_by(No.))
    
    # Rename columns containing "date" to "Date"
    colnames(season_table2) <- sapply(colnames(season_table2), function(col) 
      if (grepl("date", col, ignore.case = TRUE)) "Date" else col
    )
    # Cast to Date
    season_table2 <- season_table2 %>%
      mutate(Date = sub(".*\\((\\d{4}-\\d{1,2}-\\d{1,2})\\).*", "\\1", Date) %>%
               as.Date())
    
    ## Munge and join
    season_table3 <- season_table2 %>%
      mutate(Show = "The Late Show",
             Host = "Stephen Colbert",
             Season = which(link == season_links),
             Episode = No. %>% as.integer(),
             Air_Date = Date) %>%
      select(Show, Host, Season, Episode, Air_Date,
             `Guest(s)`, `Musical/entertainment guest(s)`, Description)
    
    # Append the extracted data to the main tibble
    late_show <- dplyr::bind_rows(late_show, season_table3)
  }
  print(late_show)
})["elapsed"]
chunk_time <- bind_rows(chunk_time, c(Chunk = "late_show", Seconds = .time))


# Combine shows -----
.time <- system.time({
  shows <- dplyr::bind_rows(last_week_tonight, daily_show, late_night, late_show)
  shows %>% dim()
  shows %>% summary()
  shows %>% filter(Show == "Last Week Tonight") %>% tail()
  shows %>% filter(Show == "The Daily Show") %>% tail()
  shows %>% filter(Show == "Late Night") %>% tail()
  shows %>% filter(Show == "The Late Show") %>% tail()
  
  ## Google trends starts 2004/1/1, and we go 14 days back
  shows2 <- shows %>%
    rename(Guests = `Guest(s)`, Music_guests = `Musical/entertainment guest(s)`) %>%
    dplyr::filter(Air_Date > as.Date("2004-01-15"))# %>%
    #mutate(Air_Date = as.POSIXct(Air_Date))
  shows2 %>% dim()
  shows2 %>% summary()
  shows2
  
  ## Counts variables for comparison
  (cnt_topic <- shows2 %>%
      count(Show, topic = !is.na(Topic)) %>%
      filter(topic == TRUE))
  (cnt_guest <- shows2 %>%
      count(Show, guests = !is.na(Guests)) %>%
      filter(guests == TRUE))
  (cnt_music <- shows2 %>%
      count(Show, music = !is.na(Music_guests)) %>%
      filter(music == TRUE))
  (cnt_description <- shows2 %>%
      count(Show, description = !is.na(Description)) %>%
      filter(description == TRUE))
})["elapsed"]
chunk_time <- bind_rows(chunk_time, c(Chunk = "Combine shows", Seconds = .time))


# Search Gtrends on variable ----
## About 12.62 hours... with waiting
{
  library(gtrendsR)
  library(ggplot2)
  
  .time <- system.time({
    ### Search Topic -- Late Week Tonight vs The Daily Show ----
    shows_topic <- shows2 %>%
      select(-c(Guests, Music_guests, Description)) %>%
      inner_join(cnt_topic %>% select(Show), by = join_by(Show)) %>%
      mutate(
        ## simplify topic (text before ":", ";", or " (ISBIN")
        Topic_simplified = stringr::str_extract(Topic, "^[^:;]+(?=:|;| \\(ISBIN )"),
        Topic_simplified = ifelse(is.na(Topic_simplified),
                                  Topic, Topic_simplified),
        ## Pivot longer lists, "and", "&", or ","
        Topic_simplified_split = 
          str_split(Topic_simplified, "\\s*( and |&|,)\\s*")
      ) %>%
      unnest(Topic_simplified_split) %>% # Expand list columns into rows
      filter(!is.na(Topic_simplified_split)) %>%
      head(6)
    
    ## Apply the gtrends()
    gtrend_topic <- tibble()
    for(i in 1:nrow(shows_topic)){
      row <- shows_topic[i, ]
      .trend <- try({
        .t <- gtrends(keyword = pull(row, Topic_simplified_split),
                      time = paste(
                        format(as.Date(pull(row, Air_Date) - 14), "%Y-%m-%d"),
                        format(as.Date(pull(row, Air_Date) + 14), "%Y-%m-%d")))
        .t$interest_over_time
      })
      
      if(class(.trend) == "data.frame"){
        ## If gtrend returns
        .trend <- bind_cols(row, .trend)
      } else {
        ## If try-error returns
        .trend <- bind_cols(row, tibble(
          keyword = pull(row, Topic_simplified_split),
          time = paste(
            format(as.Date(pull(row, Air_Date) - 14), "%Y-%m-%d"),
            format(as.Date(pull(row, Air_Date) + 14), "%Y-%m-%d")),
          error = .trend %>% as.character))
      }
      
      Sys.sleep(10.01) ## ensure we are under 10 requests per 100 seconds
      gtrend_topic <- bind_rows(.trend, gtrend_topic)
    }
    
    ## Review
    gtrend_topic
    gtrend_topic %>%
      count(Show, Season, Episode, Topic) %>%
      arrange(-n) %>%
      count(Show, worked = n == 29)
    ## Worked
    gtrend_topic %>%
      count(Show, Season, Episode, Topic) %>%
      filter(n == 29)
    ## Errors
    gtrend_topic %>%
      count(error)
    
    beepr::beep()
  })["elapsed"]
  chunk_time <- bind_rows(chunk_time, c(Chunk = "gtrend_topic", Seconds = .time))
  
  
  ### Search Guest(s) -- Later Night, The Daily Show, The Late Show ----
  .time <- system.time({
    ### Search Topic -- Late Week Tonight vs The Daily Show ----
    shows_guest <- shows2 %>%
      select(-c(Topic, Music_guests, Description)) %>%
      inner_join(cnt_guest %>% select(Show), by = join_by(Show)) %>%
      mutate(
        ## simplify topic (text before ":", ";", or " (")
        Guest_simplified = stringr::str_extract(Guests, "^[^:;]+(?=:|;| \\()"),
        Guest_simplified = ifelse(is.na(Guest_simplified),
                                  Guests, Guest_simplified),
        ## Pivot longer lists, "and", "&", or ","
        Guest_simplified_split =
          str_split(Guest_simplified, "\\s*( and |&|,)\\s*")
      ) %>%
      unnest(Guest_simplified_split) %>% # Expand list columns into rows
      head(6)
    
    ## Apply the gtrends()
    gtrend_guest <- tibble()
    for(i in 1:nrow(shows_guest)){
      row <- shows_guest[i, ]
      .trend <- try({
        .t <- gtrends(keyword = pull(row, Guest_simplified_split),
                      time = paste(
                        format(as.Date(pull(row, Air_Date) - 14), "%Y-%m-%d"),
                        format(as.Date(pull(row, Air_Date) + 14), "%Y-%m-%d")))
        .t$interest_over_time
      })
      
      if(class(.trend) == "data.frame"){
        .trend <- bind_cols(row, .trend)
      } else {
        ## try-error
        .trend <- bind_cols(row, tibble(
          keyword = pull(row, Guest_simplified_split),
          time = paste(
            format(as.Date(pull(row, Air_Date) - 14), "%Y-%m-%d"),
            format(as.Date(pull(row, Air_Date) + 14), "%Y-%m-%d")) %>% as.character(),
          error = .trend %>% as.character))
      }
      
      Sys.sleep(10.01) ## ensure we are under 10 requests per 100 seconds
      gtrend_guest <- bind_rows(.trend, gtrend_guest)
    }
    
    ## Review
    gtrend_guest
    gtrend_guest %>%
      count(Show, Season, Episode, Guests) %>%
      arrange(-n) %>%
      count(Show, worked = n == 29)
    ## Worked
    gtrend_guest %>%
      count(Show, Season, Episode, Guests) %>%
      filter(n == 29)
    ## Errors
    gtrend_guest %>%
      count(error)
    beepr::beep()
  })["elapsed"]
  chunk_time <- bind_rows(chunk_time, c(Chunk = "gtrend_guest", Seconds = .time))
  
  
  ## Search Musical/Entertainment Guest(s) -- Late Night vs The Late Show ----
  
  ## Search description -- Late Night vs The Late Show ----
  
  
  # Session info ------
  
  chunk_time
  beepr::beep(4)
  sessionInfo()
}





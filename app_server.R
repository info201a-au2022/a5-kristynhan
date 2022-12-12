library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)

data <- read.csv("owid-co2-data.csv")
data <- data %>%
  filter(year >= 2000) %>%
  drop_na()

# Interactive 1: Comparative Bar Graph
countries <- c("World", "China", "United States", "Russia", "Japan", "Germany",
               "South Korea")
comparative_data <- data %>%
  filter(country %in% countries) %>%
  filter(year == 2000 | year == 2018) %>%
  select(country, year, co2, co2_per_capita, co2_per_gdp, co2_per_unit_energy,
         energy_per_capita, energy_per_gdp, total_ghg)

double_bar <- function(country_attribute) {
  if (country_attribute == "co2") {
    graph <- ggplot(comparative_data, aes(x = country, y = `co2`,
                                          group = year)) +
      geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75,
               aes(fill = as.factor(year)), na.rm = TRUE) +
      labs(x = "Country", y = "CO2 emissions (million tonnes)",
           fill = "Year") +
      ggtitle("Total CO2 emissions Produced by Country in million tonnes") +
      theme(plot.title = element_text(hjust = 0.5))
    return(graph)
  } else if (country_attribute == "co2_per_capita") {
    graph <- ggplot(comparative_data, aes(x = country, y = `co2_per_capita`,
                                          group = `year`)) +
      geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75,
               aes(fill = as.factor(year)), na.rm = TRUE) +
      labs(x = "Country", y = "CO2 emissions (million tonnes/person)",
           fill = "Year") +
      ggtitle("CO2 emissions Produced by Country Per Capita in tonnes per person") +
      theme(plot.title = element_text(hjust = 0.5))
    return(graph)
  } else if (country_attribute == "co2_per_gdp") {
    graph <- ggplot(comparative_data, aes(x = country, y = `co2_per_gdp`,
                                          group = `year`)) +
      geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75,
               aes(fill = as.factor(year)), na.rm = TRUE) +
      labs(x = "Country", y = "CO2 emissions (kg/$)",
           fill = "Year") +
      ggtitle("CO2 emissions Produced by Country Per GDP in kg/international dollar") +
      theme(plot.title = element_text(hjust = 0.5))
    return(graph)
  } else if (country_attribute == "co2_per_unit_energy") {
    graph <- ggplot(comparative_data, aes(x = country, y = `co2_per_unit_energy`,
                                          group = `year`)) +
      geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75,
               aes(fill = as.factor(year)), na.rm = TRUE) +
      labs(x = "Country", y = "CO2 emissions (kg/kwH)",
           fill = "Year") +
      ggtitle("CO2 emissions Produced Per Unit Energy by Country in kg/kwH") +
      theme(plot.title = element_text(hjust = 0.5))
    return(graph)
  } else if (country_attribute == "energy_per_capita") {
    graph <- ggplot(comparative_data, aes(x = country, y = `energy_per_capita`,
                                          group = `year`)) +
      geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75,
               aes(fill = as.factor(year)), na.rm = TRUE) +
      labs(x = "Country", y = "Energy Consumed (kwH/person)",
           fill = "Year") +
      ggtitle("Energy Consumed Per Capita by Country in kwH/person") +
      theme(plot.title = element_text(hjust = 0.5))
    return(graph)
  } else if (country_attribute == "energy_per_gdp") {
    graph <- ggplot(comparative_data, aes(x = country, y = `energy_per_gdp`,
                                          group = `year`)) +
      geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75,
               aes(fill = as.factor(year)), na.rm = TRUE) +
      labs(x = "Country", y = "Energy Consumed (kwH/$)",
           fill = "Year") +
      ggtitle("Energy Consumed per GDP by Country in kwh/international dollar") +
      theme(plot.title = element_text(hjust = 0.5))
    return(graph)
  } else {
    graph <- ggplot(comparative_data, aes(x = country, y = `total_ghg`,
                                          group = `year`)) +
      geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75,
               aes(fill = as.factor(year)), na.rm = TRUE) +
      labs(x = "Country", y = "CO2 emissions (million tonnes)",
           fill = "Year") +
      ggtitle("Total Greenhouse Gas emissions Produced by Country in million tonnes") +
      theme(plot.title = element_text(hjust = 0.5))
    return(graph)
  } 
}


# Interactive 2: Line Graph

world_data <- data %>%
  filter(country == "World")

same_years <- world_data$year

us_data <- data %>%
  filter(country == "United States") %>%
  subset(year %in%same_years)

canada_data <- data %>%
  filter(country == "Canada") %>%
  subset(year %in%same_years)

combined_data <- rbind(world_data, us_data, canada_data) 

line_graph <- function(plot1, year1, year2) {
  if (plot1 == "gas_co2") {
    graph <- ggplot(combined_data, aes(x = `year`,
                                       y = `gas_co2`,
                                       group = `country`)) +
      geom_line(mapping = aes(color = `country`)) +
      geom_point(aes(color = `country`)) +
      xlim(year1, year2) +
      labs(x = "Year", y = "Emissions (million tonnes)",
             title = "Annual CO2 production-based emissions from gas")
    return(graph)
  } else if (plot1 == "gas_co2_per_capita") {
    graph <- ggplot(combined_data, aes(x = `year`,
                                       y = `gas_co2_per_capita`,
                                       group = `country`)) +
      geom_line(mapping = aes(color = `country`)) +
      geom_point(aes(color = `country`)) +
      xlim(year1, year2) +
      labs(x = "Year", y = "Emissions (tonnes per person)",
             title ="Annual CO2 production-based emissions from gas, per capita")
    return(graph)
  } else if (plot1 == "oil_co2") {
    graph <- ggplot(combined_data, aes(x = `year`,
                                       y = `oil_co2`,
                                       group = `country`)) +
      geom_line(mapping = aes(color = `country`)) +
      geom_point(aes(color = `country`)) +
      xlim(year1, year2) +
      labs(x = "Year", y = "Emissions (million tonnes)",
             title = "Annual CO2 production-based emissions from oil")
    return(graph)
  } else {
    graph <- ggplot(combined_data, aes(x = `year`,
                                       y = `oil_co2_per_capita`,
                                       group = `country`)) +
      geom_line(mapping = aes(color = `country`)) +
      geom_point(aes(color = `country`)) +
      xlim(year1, year2) +
      labs(x = "Year", y = "Emissions (tonnes per person)",
             title = "Annual CO2 production-based emissions from oil, per capita")
    return(graph)
  } 
}

server <- function(input, output) {
  output$double_bar <- renderPlotly({
    return(double_bar(input$country_attribute))
  })
  
  output$line <- renderPlotly({
    return(line_graph(input$plot1, input$year[1], input$year[2]))
  })
}
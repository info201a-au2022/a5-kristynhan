# Load libraries so they are available
library("shiny")
library("plotly")
library("ggplot2")
library("tidyverse")


# Introduction

intro_content <- mainPanel( 
  p(strong(h3("Introduction"))),
  p("This project is focused on analyzing the change in CO2 and greenhouse gas
    emissions over the years 2000-2018. Climate change has been on the rise over
    the last century, and one big contributor to this is the production of CO2
    and greenhouse gases. These numbers largely differ by countries and
    measurements, so I chose to compare the production of these pollutants by
    country and by year to see how each country's contributions have changed
    over the last 20 years. From this analysis, I hope to gain a better
    understanding of who are the biggest contributors to CO2 and greenhouse gas
    emissions, in what ways they are producing CO2 and greenhouse gas emissions
    and consuming energy, and how they stand in comparison to other countries
    around the world."),
  p("For example, from our first interative visualization, we can observe that
    the total production of greenhouse gas emissions produced by the world in
    2000 was 35835 million tonnes vs 2018 was 49368 million tonnes, and the
    total production of greenhouse gas emissions produced by China in 2000 was
    4221 million tonnes vs 2018 was 11821 million tonnes. We can draw from this
    comparison that the amount of greenhouse gas emissions produced over the
    last 20 years has increased."),
  p("Additionally, from our second interactive visualization, we can observe
    that from the total production of CO2 emmisions from gas in 2018, the world
    cumulative stands at 7529.85 tonnes, and to that, the U.S. contributes
    1618.72 tonnes and Canada contributes 231.98 tonnes. We can draw from this
    comparison of North American countries how much each country contributes
    to the overall world's CO2 emissions.")
)

intro_panel <- tabPanel(
  class = "inner-content",
  "Introduction",
  intro_content
)


# Interactive 1
countries_sidebar <- sidebarPanel(
  selectInput(
    "country_attribute",
    label = "Select an attribute to display:",
    choices = list(
      "Total CO2 emissions" = "co2",
      "CO2 emissions per capita" = "co2_per_capita",
      "CO2 emissions per GDP" = "co2_per_gdp",
      "CO2 emissions per unit energy" = "co2_per_unit_energy",
      "Energy consumed per capita" = "energy_per_capita",
      "Energy consumed per GDP" = "energy_per_gdp",
      "Total greenhouse gas emissions" = "total_ghg"
    ),
    selected = "co2"
  )
)

countries_content <- mainPanel (
  plotlyOutput("double_bar"),
  p(em("Source: Data on CO2 and Greenhouse Gas Emissions, by Our World in Data"),
    align = "right", style = "font-size:14px;"),
  p("These graphs display different attributes of countries' ways of
    contributing to CO2 emissions and their amount of energy consumption,
    comparing the 10 highest contributors and the world overall to climate
    change (not pictured: India, Iran, Saudi Arabia, Indonesia) with the first
    and most recent years from the 21st century. Users can select which
    attribute to view using a dropdown selector."),
  p("These graphs tell and allow us to compare how the production of CO2 and
    consumption of energy has changed over the last 20 years in the 21st
    century, over a variety of ways and on comparison of different economic
    statistics of each country. For example, for the total production of
    greenhouse gas emissions produced by the world in 2000 was 35835 million
    tonnes vs 2018 was 49368 million tonnes, and the total production of
    greenhouse gas emissions produced by China in 2000 was 4221 million tonnes
    vs 2018 was 11821 million tonnes. We can draw from this comparison
    that the amount of greenhouse gas emissions produced over the last 20 years
    has increased.")
)

countries_panel <- tabPanel(
  "Overview of Countries",
  titlePanel("Interactive 1: Overview of Countries (2000 vs 2018)"),
  countries_sidebar,
  countries_content
)


# Interactive 2

co2_production_sidebar <- sidebarPanel(
  selectInput(
    "plot1",
    label = "View annual CO2 production based emmisions from:",
    choices = list(
      "Gas (million tonnes)" = "gas_co2",
      "Gas (tonnes per person)" = "gas_co2_per_capita",
      "Oil (million tonnes)" = "oil_co2",
      "Oil (tonnes per person)" = "oil_co2_per_capita"
    ),
    selected = "gas_co2"
  ),
  sliderInput(
    inputId = "year",
    label = "from the following years:",
    min = 2000, max = 2018,
    step = 3,
    value = c(2000, 2018),
    round = TRUE
  )
)

co2_production_content <- mainPanel(
  plotlyOutput("line"),
  p(em("Source: Data on CO2 and Greenhouse Gas Emissions, by Our World in Data"),
    align = "right", style = "font-size:14px;"),
  p("These graphs display the production of CO2 emissions depending on the source,
    and at what scale: per person, or overall as a country. It compares the
    North American countries of Canada and the United States, vs the world overall.
    Users can select a feature to view the different line graphs, and
    the slider input allows users to more closely view trends among different
    year ranges."),
  p("These graphs tell us who are the highest contributors of CO2 production based
    on oil and gas. The first and third graphs show general information with the
    world as the cumulative total of emissions, and how much Canada and the U.S.
    each contribute to that cumulative total. For example, for the total
    production of CO2 emmisions from gas in 2018, the world cumulative stands at
    7529.85 tonnes, and to that, the U.S. contributes 1618.72 tonnes and Canada
    contributes 231.98 tonnes. The second and fourth graphs show how the
    production of CO2 emissions changes with consideration and proportion
    of each country's population."),
  p("If I had more time to work on this assignment, I would look more closely
    at the per capita statistics and how the countries' populations tie into
    the proper proportions.")
)

co2_production_panel <- tabPanel(
  "CO2 Productions",
  titlePanel("Interactive 2: CO2 Productions (2000-2018)"),
  co2_production_sidebar,
  co2_production_content
)


ui <- fluidPage(
  navbarPage(
    "CO2 Emissions Around the World",
    theme = shinythemes::shinytheme("readable"),
    intro_panel,
    countries_panel,
    co2_production_panel, 
  )
)

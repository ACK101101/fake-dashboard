library(shiny)
library(semantic.dashboard)
library(shiny.semantic)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(ggbreak)
library(DT)
library(viridis)
library(formattable)
library(janitor)

options(dplyr.summarise.inform = FALSE)
options(DT.options = list(dom = "t"))

### Fake data

df <- read.csv("fakeData.csv", stringsAsFactors = F)
df["Judges"] <- "Judges"

judge_data <- df %>% filter(Judge_Name != NA | Judge_Name != "") %>% 
                mutate(Judge_Name = recode(Judge_Name, 
                "Agnostini" = "Agostini",
                "Mckenna" = "McKenna", "McKenna " = "McKenna", 
                "Roony" = "Rooney", "ROONIE" = "Rooney",
                "Santanello " = "Santanello")) %>% 
                group_by(Judge_Name)

### Dropdown Choices
court_choices <- c("Superior Court", "District Court")
judge_choices <- unique(filter(judge_data, Judge_Name != "" |  Judge_Name != NA)["Judge_Name"])
race_choices <- unique(filter(df, Def_Race != "" |  Def_Race != NA | Def_Race != "Unknown")["Def_Race"])
crime_choices <- c("Crimes against persons", "Violent crimes", "Property crimes", 
                  "Drug/narcotics crimes", "Motor vehicle offenses", "Financial crimes",
                  "Crimes involving firearms")

### Info boxes
# Judge
accord_num_cases <- list(list(
  title = "Info", content = p("Height of the bars indicates the number of cases in District Court/Superior Court/All Courts that each Judge has presided over")
))
accord_j_reject <- list(list(
  title = "Info", content = p("The full bar represents all cases presided over by the judge selected and the colors show the proportion for which the judge disagreed with the prosecutor’s recommendation in the tender of plea.")
))
accord_j_impose <- list(list(
  title = "Info", content = p("The full bar represents all cases presided over by the judge selected and colors show the proportion for which the judge imposed a sentence different than what was outlined in the tender of plea.")
))
accord_j_impose2 <- list(list(
  title = "Info", content = p("The full bar represents all cases in which a sentence was imposed by the judge selected and the colors represent how the imposed sentence compares to the recommended sentence in terms of severity and leniency.")
))
# Def
accord_race_x_crimetype <- list(list(
  title = "Info", content = p("Values in this table represent the number/percentage of people charged with different types of crimes (each column) and the race of those people charged with those crimes")
))
accord_race_x_charge <- list(list(
  title = "Info", content = p("Values in this table show the number/percentage of people who accepted a plea in which the first charge was associated with this type of disposition as a function of the race of the people charged with those crimes.")
))
accord_race_x_sentence <- list(list(
  title = "Info", content = p("Values in this table show the number/percentage of people who accepted a plea in which the first charge was associated with this type of disposition as a function of the race of the people charged with those crimes.")
))
accord_race_x_treatment <- list(list(
  title = "Info", content = p("Values in this table show the number/percentage of people charged with crimes within each racial group who were offered rehabilitation or treatment as part of their plea ('Yes'), or not ('No').")
))
# Charge
accord_disposition_x_court <- list(list(
  title = "Info", content = p("Each colored section of each bar shows the proportion of people charged who received each disposition type for the first charge against them, separated out according to whether their case was in District or Superior Court. \n
                              The data displayed on the graph can also be restricted to a particular type of case and/or race of the person charged using the drop-down menus at the top of the graph. If any of these sub categories are selected, the graph will only display data from people or cases from those categories, and will only ever show the dispositions for the first charge against the person.")
))

### UI
ui <- semantic.dashboard::dashboardPage(
  semantic.dashboard::dashboardHeader(),
  semantic.dashboard::dashboardSidebar(
    color = "black", inverted = TRUE, visible = FALSE,
    semantic.dashboard::sidebarMenu(
        semantic.dashboard::menuItem(tabName = "caseTab", "Cases"),
        semantic.dashboard::menuItem(tabName = "judgeTab", "Judges"),
        semantic.dashboard::menuItem(tabName = "defTab", "Defendants"),
        semantic.dashboard::menuItem(tabName = "chargeTab", "Charges"),
        semantic.dashboard::menuItem(tabName = "sentenceTab", "Sentencing")
        # semantic.dashboard::menuItem(tabName = "attorneyTab", "Attorney")
    )
  ),
  semantic.dashboard::dashboardBody(
      ### Temp Page
      semantic.dashboard::tabItem(
        tabName = "temp",
        fluidRow(
          column(
            width = 12,
            header(title = "View Dumbbell", description = ""),
            plotOutput("PleaTimelineDumbbell")
          )
        )
      ),
      ### Case Page
      semantic.dashboard::tabItem(
        tabName = "caseTab",
        fluidRow(
          column(
            width = 8, align = "left",
            header(title = "Cases", description = "")
          )
        ),
        fluidRow(
          box(width = 4, column(
            width = 4, align = "left",
            h4("Select Court Type: "),
            dropdown_input("court_type", default_text = "Court Type", 
              choices = c("All Cases", court_choices),
              value = "All Cases"),
          )),
          column(
            width = 12, align = "center",
            box(h4("Types of Cases"),
            plotOutput("CrimeTypeProp"))
          )
        ),
        fluidRow(
          box(width = 4, column(
            width = 4, align = "left",
            h4("Sort Data By: "),
            dropdown_input("case_stack_by", default_text = "All", 
              choices = c("None", "Race", "Gender", "History"),
              value = "None")
          )),
          column(
            width = 12, align = "center",
            box(h4("Proportion of Cases Sorted By Crime Type"),
            plotOutput("CrimeType"))
          )
        )
      ),
      ### Judge Page
      # group judge names
      semantic.dashboard::tabItem(
        tabName = "judgeTab",
        fluidRow(
          column(
            width = 8, align = "left",
            header(title = "Judges", description = "")
          )
        ),
        fluidRow(
          column(
            width = 4, align = "left",
            h4("Select Court Type: "),
            dropdown_input("court_type_judge", default_text = "Court Type", 
              choices = c("All Cases", court_choices),
              value = "All Cases")
          )       
        ),
        fluidRow(
          box(width = 16, plotOutput("NumCases"),
          accordion(accord_num_cases, fluid = F, 
            active_title = "", styled = FALSE))
        ),
        fluidRow(
          column(
            width = 4, align = "left",
            h4("Select a Specific Judge: "),
            dropdown_input("judge_choice", default_text = "Judge Choice", 
              choices = c("All Judges", judge_choices),
              value = "All Judges")
          )        
        ),
        fluidRow(
          box(width = 16, plotOutput("JudgeReject", height = "150px"),
          accordion(accord_j_reject, fluid = F, 
            active_title = "", styled = FALSE))
        ),
        fluidRow(
          box(width = 16, plotOutput("JudgeImpose", height = "150px"),
          accordion(accord_j_impose, fluid = F, 
            active_title = "", styled = FALSE))
        ),
        fluidRow(
          box(width = 16, plotOutput("JudgeImpose2", height = "150px"),
          accordion(accord_j_impose2, fluid = F, 
            active_title = "", styled = FALSE))
        )
      ),
      ### Def Page
      # search sentence type by substring
      semantic.dashboard::tabItem(
        tabName = "defTab",
        fluidRow(
          header(title = "Defendants", description = "")
        ),
        fluidRow(box(width = 16, align = "center",
          h4("Number of People Charged with Crimes in District or Superior Court and their Race"),
          plotOutput("def_race_x_court")
        )),
        fluidRow(
          column(width = 8, align = "center", 
          card(div(class = "content",
                div(class = "header", "Total Superior Court Cases: "),
                div(class = "description", textOutput("superior_total"))
            ))),
          column(width = 8, align = "center", 
          card(div(class = "content",
                div(class = "header", "Total District Court Cases: "),
                div(class = "description", textOutput("district_total"))
            ))),
        ),
        fluidRow(
          column(
            width = 8, align = "left",
            h4("Select Court Type: "),
            dropdown_input("court_type_def", default_text = "Court Type", 
              choices = c("All Cases", court_choices),
              value = "All Cases")
          ),
          column(
            width = 8, align = "left",
            h4("Display counts or percentages: "),
            dropdown_input("perc_or_num_def", 
              choices = c("Number", "Percentage"),
              value = "Number")
          )          
        ),
        fluidRow(
          semantic_DTOutput("def_race_x_crimetype"),
          # formattableOutput("def_race_x_crimetype"),
          accordion(accord_race_x_crimetype, fluid = F, 
            active_title = "", styled = FALSE)
        ),
        fluidRow(
          semantic_DTOutput("def_race_x_charge"),
          accordion(accord_race_x_charge, fluid = F, 
            active_title = "", styled = FALSE)
        ),
        fluidRow(
          semantic_DTOutput("def_race_x_sentence"),
          accordion(accord_race_x_sentence, fluid = F, 
            active_title = "", styled = FALSE)
        ),
        fluidRow(
          semantic_DTOutput("def_race_x_treatment"),
          accordion(accord_race_x_treatment, fluid = F, 
            active_title = "", styled = FALSE)
        )
      ),
      ### Charge Page
      semantic.dashboard::tabItem(
        tabName = "chargeTab",
        fluidRow(
          header(title = "Charges", description = "")
        ),
        fluidRow(
          column(width = 8, align = "left",
            h4("Select by Type of Crime: "),
            dropdown_input("crime_type_charge", default_text = "Crime Type", 
                choices = c("All Crimes", crime_choices),
                value = "All Crimes"),
          ),
          column(width = 8, align = "left",
            h4("Filter by Race: "),
            dropdown_input("race_type_charge", default_text = "Race", 
                choices = c("All Races", race_choices),
                value = "All Races")   
          ) 
        ),
        fluidRow(box(width = 16, align = "center",
          h4("Number of Cases at District and Superior Court"),
          plotOutput("charge_disposition_x_court"),
          accordion(accord_disposition_x_court, fluid = F, 
            active_title = "", styled = FALSE)
        ))
      ),
      ### Sentence Page
      semantic.dashboard::tabItem(
        tabName = "sentenceTab",
        fluidRow(
          header(title = "Sentencing", description = "")
        ),
        fluidRow(box(width = 16, align = "center",
          h4("Sentence Type Given by Race"),
          plotOutput("sentence_race_x_sentencetype"),
        )),
        fluidRow(column(width = 8, align = "left",
          h4("Filter by Prison or Probation Sentences: "),
          dropdown_input("sentence_choice", default_text = "Prison or Probation", 
              choices = c("Prison", "Probation"),
              value = "Prison")   
        )),
        fluidRow(
          semantic_DTOutput("sentence_race_x_sentencelength"),
        ),
        # fluidRow(
        #   semantic_DTOutput("sentence_crimetype_x_sentencelength"),
        # )
      ),
      ### Attorney Page
      semantic.dashboard::tab_item(
        tabName = "attorneyTab",
        fluidRow(
          dropdown_input("attorney_stack_by", default_text = "Race", 
               choices = c("Race", "Gender", "History"),
               value = "Race")
        ),
        fluidRow(
          plotOutput("attorney_by")
        )
      )
  )
)

### General Functions
court_filter <- function(dataframe, input) {
  filtered_data <- dataframe %>% filter(Court != "" | Court != NA)
  if (input$court_type == "District Court") {
    filtered_data <- filtered_data %>% filter(Court == "District Court")
  }
  else if (input$court_type == "Superior Court") {
    filtered_data <- filtered_data %>% filter(Court == "Superior Court")
  }
  return(filtered_data)
}

race_filter <- function(dataframe) {
  filtered_data <- filter(dataframe, Def_Race %in% c("White", "Black", "Hispanic")) %>%
                  group_by(Def_Race)
  return(filtered_data)
}

gender_filter <- function(dataframe) {
  filtered_data <- filter(dataframe, Def_Gender %in% c("Male", "Female")) %>%
                  group_by(Def_Gender)
  return(filtered_data)
}

history_filter <- function(dataframe) {
  filtered_data <- filter(dataframe, AnyCriminalHistory %in% c(0, 1)) %>%
                mutate(AnyCriminalHistory = recode(AnyCriminalHistory, 
                "0" = "No Criminal History",
                "1" = "Has Criminal History")) %>%
                group_by(AnyCriminalHistory)
  return(filtered_data)
}

crimetype_data_filter <- function(dataframe) {
  crime_data <- dataframe %>% 
                filter(!is.na(crimetype_people)) %>%  
                filter(!is.na(crimetype_violent)) %>%  
                filter(!is.na(crimetype_property)) %>%  
                filter(!is.na(crimetype_drug)) %>%  
                filter(!is.na(crimetype_mva)) %>%  
                filter(!is.na(crimetype_financial)) %>%  
                filter(!is.na(crimetype_firearms)) %>%  
                summarise("Crimes against persons" = sum(crimetype_people),
                          "Violent crimes" = sum(crimetype_violent),
                          "Property crimes" = sum(crimetype_property),
                          "Drug/narcotics crimes" = sum(crimetype_drug),
                          "Motor vehicle offenses" = sum(crimetype_mva),
                          "Financial crimes" = sum(crimetype_financial),
                          "Crimes involving firearms" = sum(crimetype_firearms)
                )
  return(crime_data)
}

crimetype_stack_datamaker <- function(dataframe, input) {
  if (input$case_stack_by == "Gender") {
    dataframe <- gender_filter(dataframe)
    data <- crimetype_stack_by(dataframe, Def_Gender)
  }
  else if (input$case_stack_by == "History") {
    dataframe <- history_filter(dataframe)
    data <- crimetype_stack_by(dataframe, AnyCriminalHistory)
  }
  else {
    dataframe <- race_filter(dataframe)
    data <- crimetype_stack_by(dataframe, Def_Race)
  }
  return(data)
}

crimetype_stack_by <- function(test_data, parse_by) {
  people_data <- test_data %>% 
                filter(crimetype_people == 1) %>%
                group_by(crimetype_people, {{parse_by}}) %>% 
                summarise(Percentage = n()) %>% 
                mutate(Percentage = Percentage/sum(Percentage)*100) 
  people_data$Crime <- "Crimes against persons"

  violent_data <- test_data %>% 
                filter(crimetype_violent == 1) %>%
                group_by(crimetype_violent, {{parse_by}}) %>% 
                summarise(Percentage = n()) %>% 
                mutate(Percentage = Percentage/sum(Percentage)*100) 
  violent_data$Crime <- "Violent crimes"

  property_data <- test_data %>% 
                filter(crimetype_property == 1) %>%
                group_by(crimetype_property, {{parse_by}}) %>% 
                summarise(Percentage = n()) %>% 
                mutate(Percentage = Percentage/sum(Percentage)*100) 
  property_data$Crime <- "Property crimes"

  drug_data <- test_data %>% 
                filter(crimetype_drug == 1) %>%
                group_by(crimetype_drug, {{parse_by}}) %>% 
                summarise(Percentage = n()) %>% 
                mutate(Percentage = Percentage/sum(Percentage)*100) 
  drug_data$Crime <- "Drug/narcotics crimes"
  
  mva_data <- test_data %>% 
                filter(crimetype_mva == 1) %>%
                group_by(crimetype_mva, {{parse_by}}) %>% 
                summarise(Percentage = n()) %>% 
                mutate(Percentage = Percentage/sum(Percentage)*100) 
  mva_data$Crime <- "Motor vehicle offenses"

  financial_data <- test_data %>% 
                filter(crimetype_financial == 1) %>%
                group_by(crimetype_financial, {{parse_by}}) %>% 
                summarise(Percentage = n()) %>% 
                mutate(Percentage = Percentage/sum(Percentage)*100) 
  financial_data$Crime <- "Financial crimes"

  firearms_data <- test_data %>% 
                filter(crimetype_firearms == 1) %>%
                group_by(crimetype_firearms, {{parse_by}}) %>% 
                summarise(Percentage = n()) %>% 
                mutate(Percentage = Percentage/sum(Percentage)*100) 
  firearms_data$Crime <- "Crimes involving firearms"

  big_table <- bind_rows(people_data, violent_data, property_data, drug_data, mva_data, financial_data, firearms_data)
  big_table <- rename(big_table, Stack_By = {{parse_by}})
  return(big_table)
}

crimetype_stack_plotter <- function(dataframe) {
    ggplot(dataframe, aes(x = Crime, y = Percentage, fill = Stack_By)) + 
    geom_bar(stat = "identity", position = "stack") + coord_flip() + 
    theme(legend.title = element_blank()) + 
    scale_fill_viridis(discrete = TRUE, option = "A")
}


### Specific Functions
case_total_prop_data <- function(dataframe, input) {
  case_data <- court_filter(dataframe, input)
  case_data <- crimetype_data_filter(case_data)
  case_data_prop <- prop.table(case_data) * 100
  case_data <- pivot_longer(case_data_prop, cols = "Crimes against persons":"Crimes involving firearms",
                            names_to = "Type_Crime", values_to = "Percentage")
  case_data["Crimes"] = 0
  return(case_data)
}

case_total_prop_plot <- function(dataframe) {
    ggplot(dataframe, aes(x = Crimes, y = Percentage, fill = Type_Crime)) + 
    geom_bar(stat = "identity", position = "stack") + 
    theme(legend.title = element_blank()) +
    scale_fill_viridis(discrete = TRUE, option = "A")
}

case_datamaker <- function(dataframe, input) {
  case_data <- court_filter(dataframe)
  if (input$case_stack_by == "Race") {
    case_data <- race_filter(case_data)
  }
  else if (input$case_stack_by == "Gender") {
    case_data <- gender_filter(case_data)
  }
  else if (input$case_stack_by == "History") {
    case_data <- history_filter(case_data)
  }
  case_data <- crimetype_data_filter(case_data)
  
  case_data_long <- pivot_longer(case_data, cols = "Crimes against persons":"Crimes involving firearms", 
                              names_to = "Type_Crime", values_to = "Percentage", 
                              values_drop_na = TRUE)
  return(case_data_long)
}

case_plotter <- function(dataframe, ylabel, input) {
  if (input$case_stack_by == "Race") {
    ggplot(dataframe, aes(y = "Type_Crime", x = Percentage, fill = Def_Race)) +
    theme(legend.position = "top", legend.title = element_blank()) +
    geom_col() + labs(y = "Race")
  }
  else if (input$case_stack_by == "Gender") {
    ggplot(dataframe, aes(y = "Type_Crime", x = Percentage, fill = Def_Gender)) +
    theme(legend.position = "top", legend.title = element_blank()) +
    geom_col() + labs(y = "Gender")
  }
  else if (input$case_stack_by == "History") {
    ggplot(dataframe, aes(y = "Type_Crime", x = Percentage, fill = AnyCriminalHistory)) +
    theme(legend.position = "top", legend.title = element_blank()) +
    geom_col() + labs(y = "History")
  }
  else {
    ggplot(dataframe, aes(y = "Type_Crime", x = Percentage)) +
    theme(legend.position = "top", legend.title = element_blank()) +
    geom_col() + labs(y = "None")
  }
}

timeline_datamaker <- function(dataframe, input) {
  timeline_data <- dataframe
  if (input$court_type == "District Court") {
    timeline_data <- timeline_data %>% 
                  filter(!is.na(Court)) %>%  
                  filter(Court == "District Court")
  }
  if (input$court_type == "Superior Court") {
    timeline_data <- timeline_data %>% 
                  filter(!is.na(Court)) %>%  
                  filter(Court == "Superior Court")
  } 
  timeline_data <- timeline_data %>%
    select("days_btwn_arrest_plea", "days_btwn_plea_tender") %>%  
    filter(!is.na(days_btwn_arrest_plea)) %>%  
    filter(!is.na(days_btwn_plea_tender)) %>%  
    mutate(idx = row_number())
    timeline_data_long <- pivot_longer(timeline_data, 
                                      cols = "days_btwn_arrest_plea":"days_btwn_plea_tender",
                                      names_to = "time_btwn", values_to = "days", values_drop_na = TRUE)
    return(timeline_data)
    # return(timeline_data_long)
}

timeline_plotter <- function(dataframe) {
  ggplot(dataframe, aes(x = idx, y = days, fill = time_btwn)) + 
    theme(legend.position = "top", legend.title = element_blank()) +
    geom_col() + labs(y = "Plea Timeline") + coord_flip()
}

judge_total_cases <- function(dataframe, input) {
  filtered_data <- dataframe %>% filter(Court != "" | Court != NA)
  if (input$court_type_judge == "District Court") {
    filtered_data <- filtered_data %>% filter(Court == "District Court")
  }
  else if (input$court_type_judge == "Superior Court") {
    filtered_data <- filtered_data %>% filter(Court == "Superior Court")
  }
  return(filtered_data)
}

judge_datamaker <- function(dataframe, parse_by, input) {
  judge_data <- dataframe
  if (input$court_type_judge == "District Court") {
    judge_data <- judge_data %>% filter(Court == "District Court")
  }
  else if (input$court_type_judge == "Superior Court") {
    judge_data <- judge_data %>% filter(Court == "Superior Court")
  }
  if (input$judge_choice != "All Judges") {
    judge_data <- judge_data %>% filter(Judge_Name == input$judge_choice)
  }
  judge_data <- judge_data %>%
                filter(!is.na({{parse_by}})) %>% 
                filter({{parse_by}} != "") %>%
                group_by({{parse_by}}) %>% 
                summarise(counts = n())
  return(judge_data)
}

judge_plotter <- function(dataframe, parse_by, input, xlabel, old_leg, new_leg) {
  ggplot(dataframe, aes(y = "Judges", x = counts, fill = {{parse_by}})) + 
    theme(legend.title = element_blank()) +
    geom_col() + labs(x = xlabel, y = "") +
    scale_fill_viridis(discrete = TRUE, option = "A", breaks = old_leg, labels = new_leg)
}            

def_datamaker <- function(dataframe, input, parse_by, parse_by_idx) {
  def_data <- dataframe %>%
                filter(!is.na(Court)) %>%  
                filter(!is.na(Def_Race)) %>% 
                filter(Def_Race != "") %>% 
                group_by(Def_Race)
  if (input$court_type_def == "District Court") {
    def_data <- def_data %>% filter(Court == "District Court")
  }
  else if (input$court_type_def == "Superior Court") {
    def_data <- def_data %>% filter(Court == "Superior Court")
  }
  if (parse_by_idx == -1) {
    def_data <- crimetype_data_filter(def_data)
    # def_data <- t(def_data)
    # def_data <- janitor::row_to_names(def_data, row_number = 1)
  }
  else {
    def_data <- def_data %>%
                filter(!is.na({{parse_by}})) %>% 
                filter({{parse_by}} != "") %>%
                group_by({{parse_by}})
    if (input$perc_or_num_def == "Number") {
      def_data <- tibble(select(def_data, parse_by_idx, which(colnames(df) == "Def_Race"))) %>%
              count(select(def_data, parse_by_idx, which(colnames(df) == "Def_Race"))) %>%
              pivot_wider(names_from = Def_Race, values_from = n, values_fill = 0) 
    }
    else if (input$perc_or_num_def == "Percentage") {
      def_data <- tibble(select(def_data, parse_by_idx, which(colnames(df) == "Def_Race"))) %>%
              count(select(def_data, parse_by_idx, which(colnames(df) == "Def_Race"))) %>%
              mutate(Percentage = round(n/sum(n)*100, 1)) %>% select(-n) %>%
              pivot_wider(names_from = Def_Race, values_from = Percentage, values_fill = 0) 
    }
  }
  return(def_data)
}

charge_datamaker <- function(dataframe, input) {
  charge_data <- dataframe
  if (input$crime_type_charge != "All Crimes") {
    if (input$crime_type_charge == "Crimes against persons") {
      charge_data <- charge_data %>% filter(crimetype_people == 1)
    }
    else if (input$crime_type_charge == "Violent crimes") {
      charge_data <- charge_data %>% filter(crimetype_violent == 1)
    }
    else if (input$crime_type_charge == "Property crimes") {
      charge_data <- charge_data %>% filter(crimetype_property == 1)
    }
    else if (input$crime_type_charge == "Drug/narcotics crimes") {
      charge_data <- charge_data %>% filter(crimetype_drug == 1)
    }
    else if (input$crime_type_charge == "Motor vehicle offenses") {
      charge_data <- charge_data %>% filter(crimetype_mva == 1)
    }
    else if (input$crime_type_charge == "Financial crimes") {
      charge_data <- charge_data %>% filter(crimetype_financial == 1)
    }
    else if (input$crime_type_charge == "Crimes involving firearms") {
      charge_data <- charge_data %>% filter(crimetype_firearms == 1)
    }
  }
  if (input$race_type_charge != "All Races") {
    if (input$race_type_charge == "White") {
      charge_data <- charge_data %>% filter(Def_Race == "White")
    }
    else if (input$race_type_charge == "Unknown") {
      charge_data <- charge_data %>% filter(Def_Race == "Unknown")
    }
    else if (input$race_type_charge == "Other") {
      charge_data <- charge_data %>% filter(Def_Race == "Other")
    }
    else if (input$race_type_charge == "Black") {
      charge_data <- charge_data %>% filter(Def_Race == "Black")
    }
    else if (input$race_type_charge == "Hispanic") {
      charge_data <- charge_data %>% filter(Def_Race == "Hispanic")
    }
  }
  charge_data <- charge_data %>% 
              filter(Court != "" | Court != NA) %>% 
              filter(X1stCharge_Status != "" | X1stCharge_Status != NA) %>% 
              count(Court, X1stCharge_Status)
  return(charge_data)
}

attorney_datamaker <- function(dataframe, input) {
  attorney_data <- dataframe %>% 
              filter(DefAttorneyType != "" | DefAttorneyType != NA)
  if (input$attorney_stack_by == "Race") {
    attorney_data <- attorney_data %>% 
              filter(Def_Race != "" | Def_Race != NA) %>% 
              group_by(Def_Race) %>% 
              count(DefAttorneyType, Def_Race)
  }
  else if (input$attorney_stack_by == "Gender") {
    attorney_data <- attorney_data %>% 
              filter(Def_Gender != "" | Def_Gender != NA) %>% 
              group_by(Def_Gender) %>% 
              count(DefAttorneyType, Def_Gender)
  }
  else if (input$attorney_stack_by == "History") {
    attorney_data <- attorney_data %>% 
              filter(AnyCriminalHistory != "" | AnyCriminalHistory != NA) %>% 
              group_by(AnyCriminalHistory) %>% 
              count(DefAttorneyType, AnyCriminalHistory)
  }
  return(attorney_data)
}

attorney_plotter <- function(dataframe, input) {
  if (input$attorney_stack_by == "Race") {
    ggplot(dataframe, aes(x = Def_Race, y = n, fill = DefAttorneyType)) +
    geom_bar(stat = "identity", position = "dodge")
  }
  else if (input$attorney_stack_by == "Gender") {
    ggplot(dataframe, aes(x = Def_Gender, y = n, fill = DefAttorneyType)) +
    geom_bar(stat = "identity", position = "dodge")
  }
  else if (input$attorney_stack_by == "History") {
    ggplot(dataframe, aes(x = AnyCriminalHistory, y = n, fill = DefAttorneyType)) +
    geom_bar(stat = "identity", position = "dodge")
  }
}

sentence0_datamaker <- function(dataframe, input) {
  sentence_data <- dataframe %>% 
    filter(Def_Race != "" | Def_Race != NA)
  
  if (input$sentence_choice == "Prison") {
    sentence_data <- sentence_data %>%
      filter(X1stPROS_SentLength != "" | X1stPROS_SentLength != NA) %>% 
      select(Def_Race, X1stPROS_SentLength)  
    
    white_stats <- sentence_data %>% filter(Def_Race == "White") %>%
    summarise(mean = round(mean(X1stPROS_SentLength), 1), 
              sd = round(sd(X1stPROS_SentLength), 1), 
              median = round(median(X1stPROS_SentLength), 1))
 
    black_stats <- sentence_data %>% filter(Def_Race == "Black") %>%
        summarise(mean = round(mean(X1stPROS_SentLength), 1), 
              sd = round(sd(X1stPROS_SentLength), 1), 
              median = round(median(X1stPROS_SentLength), 1))

    hispanic_stats <- sentence_data %>% filter(Def_Race == "Hispanic") %>%
        summarise(mean = round(mean(X1stPROS_SentLength), 1), 
              sd = round(sd(X1stPROS_SentLength), 1), 
              median = round(median(X1stPROS_SentLength), 1))

    unknown_stats <- sentence_data %>% filter(Def_Race == "Unknown") %>%
        summarise(mean = round(mean(X1stPROS_SentLength), 1), 
              sd = round(sd(X1stPROS_SentLength), 1), 
              median = round(median(X1stPROS_SentLength), 1))

    sent_data <- t(rbind(white_stats, black_stats, hispanic_stats, unknown_stats))
    colnames(sent_data) <- c("White", "Black", "Hispanic", "Unknown")
    rownames(sent_data) <- c("Mean", "SD", "Median")
  }
  
  else if (input$sentence_choice == "Probation") {
    sentence_data <- sentence_data %>%
      filter(X1stPROS_ProbLength != "" | X1stPROS_ProbLength != NA) %>% 
      select(Def_Race, X1stPROS_ProbLength) 
    
    white_stats <- sentence_data %>% filter(Def_Race == "White") %>%
    summarise(mean = round(mean(X1stPROS_ProbLength), 1), 
              sd = round(sd(X1stPROS_ProbLength), 1), 
              median = round(median(X1stPROS_ProbLength), 1))
 
    black_stats <- sentence_data %>% filter(Def_Race == "Black") %>%
        summarise(mean = round(mean(X1stPROS_ProbLength), 1), 
              sd = round(sd(X1stPROS_ProbLength), 1), 
              median = round(median(X1stPROS_ProbLength), 1))

    hispanic_stats <- sentence_data %>% filter(Def_Race == "Hispanic") %>%
        summarise(mean = round(mean(X1stPROS_ProbLength), 1), 
              sd = round(sd(X1stPROS_ProbLength), 1), 
              median = round(median(X1stPROS_ProbLength), 1))

    unknown_stats <- sentence_data %>% filter(Def_Race == "Unknown") %>%
        summarise(mean = round(mean(X1stPROS_ProbLength), 1), 
              sd = round(sd(X1stPROS_ProbLength), 1), 
              median = round(median(X1stPROS_ProbLength), 1))

    sent_data <- t(rbind(white_stats, black_stats, hispanic_stats, unknown_stats))
    colnames(sent_data) <- c("White", "Black", "Hispanic", "Unknown")
    rownames(sent_data) <- c("Mean", "SD", "Median")
  }

  return(sent_data)
}

sentence1_datamaker <- function(dataframe, input) {
  sentence_data <- dataframe %>% 
    filter(Def_Race != "" | Def_Race != NA)
  
  if (input$sentence_choice == "Prison") {
    sentence_data <- sentence_data %>%
      filter(X1stPROS_SentLength != "" | X1stPROS_SentLength != NA) %>% 
      select(Def_Race, X1stPROS_SentLength)  
    
    white_stats <- sentence_data %>% filter(Def_Race == "White") %>%
    summarise(mean = mean(X1stPROS_SentLength), 
              sd = sd(X1stPROS_SentLength), 
              median = median(X1stPROS_SentLength))
 
    black_stats <- sentence_data %>% filter(Def_Race == "Black") %>%
        summarise(mean = mean(X1stPROS_SentLength), 
                  sd = sd(X1stPROS_SentLength), 
                  median = median(X1stPROS_SentLength))

    hispanic_stats <- sentence_data %>% filter(Def_Race == "Hispanic") %>%
        summarise(mean = mean(X1stPROS_SentLength), 
                  sd = sd(X1stPROS_SentLength), 
                  median = median(X1stPROS_SentLength))

    unknown_stats <- sentence_data %>% filter(Def_Race == "Unknown") %>%
        summarise(mean = mean(X1stPROS_SentLength), 
                  sd = sd(X1stPROS_SentLength), 
                  median = median(X1stPROS_SentLength))

    sent_data <- t(rbind(white_stats, black_stats, hispanic_stats, unknown_stats))
    colnames(sent_data) <- c("White", "Black", "Hispanic", "Unknown")
    rownames(sent_data) <- c("Mean", "SD", "Median")
  }
  
  else if (input$sentence_choice == "Probation") {
    sentence_data <- sentence_data %>%
      filter(X1stPROS_ProbLength != "" | X1stPROS_ProbLength != NA) %>% 
      select(Def_Race, X1stPROS_ProbLength) 
    
    white_stats <- sentence_data %>% filter(Def_Race == "White") %>%
    summarise(white = mean(X1stPROS_ProbLength), 
              white = sd(X1stPROS_ProbLength), 
              white = median(X1stPROS_ProbLength))
 
    black_stats <- sentence_data %>% filter(Def_Race == "Black") %>%
        summarise(black = mean(X1stPROS_ProbLength), 
                  black = sd(X1stPROS_ProbLength), 
                  black = median(X1stPROS_ProbLength))

    hispanic_stats <- sentence_data %>% filter(Def_Race == "Hispanic") %>%
        summarise(hispanic = mean(X1stPROS_ProbLength), 
                  hispanic = sd(X1stPROS_ProbLength), 
                  hispanic = median(X1stPROS_ProbLength))

    unknown_stats <- sentence_data %>% filter(Def_Race == "Unknown") %>%
        summarise(unknown = mean(X1stPROS_ProbLength), 
                  unknown = sd(X1stPROS_ProbLength), 
                  unkown = median(X1stPROS_ProbLength))

    sent_data <- t(rbind(white_stats, black_stats, hispanic_stats, unknown_stats))
    colnames(sent_data) <- c("White", "Black", "Hispanic", "Unknown")
    rownames(sent_data) <- c("Mean", "SD", "Median")
  }

  return(sent_data)
}

# make median and mean comparisons
### Server
server <- function(input, output) {

  ### Case Page
  # Crime prop graph
  case_prop_table <- reactive(case_total_prop_data(df, input))
  output$CrimeTypeProp <- renderPlot(
    case_total_prop_plot(case_prop_table())
  )
  # Crime prop by stack_by
  case_summary_data <- reactive(crimetype_stack_datamaker(df, input))
  output$CrimeType <- renderPlot(
    crimetype_stack_plotter(case_summary_data())
  )

  ###
  timeline_data <- reactive(timeline_datamaker(df, input))
  output$PleaTimelineDumbbell <- renderPlot({
        ggplot(data = timeline_data()) +

        geom_segment(
                    aes(x = idx,
                        xend = idx,
                        y = 0,
                        yend = abs(days_btwn_arrest_plea),
                        linetype = "Arrest to Plea"),
                    color = "#E69F00",
                    size = 2) +

        geom_segment(
                    aes(x = idx,
                        xend = idx,
                        y = abs(days_btwn_arrest_plea),
                        yend = days_btwn_plea_tender,
                        linetype = "Plea to Tender"),
                    color = "#56B4E9",
                    size = 2) +

        scale_y_continuous(breaks = c(250, 500, 750, 1000, 1250, 14875)) +
        scale_y_break(c(1250, 14750)) +

        scale_linetype_manual("Time Between (in days):",
                              values = c("Arrest to Plea" = 1,
                                         "Plea to Tender" = 1
                              )) +

        labs(x = "Index",
             y = "Days",
             title = "Days Between Arrest, Plea, Tender") +

        theme_bw() +
        theme(axis.title = element_text(size = 16),
              plot.title = element_text(size = 28,
                                        hjust = 0.5),
              legend.position = "right")
  })
  
  ### Judge Page
  # Total Num Cases
  judge_total <- reactive(judge_total_cases(judge_data, input))
  output$NumCases <- renderPlot(
    ggplot(judge_total(), aes(x = Judge_Name)) + geom_bar(stat = "count") + 
    scale_fill_viridis_d() +
    labs(y = "Number of Cases", x = "Judge") 
  )
  # Plea Outcome : Judge_Reject
  judge_reject_data <- reactive(judge_datamaker(judge_data, Judge_Reject, input))
  output$JudgeReject <- renderPlot(
    judge_plotter(judge_reject_data(), Judge_Reject, 
                  input, "Number of Cases", c("No", "Yes"), 
                  c("Judge agreed with prosecutor’s recommendation", "Judge disagreed with prosecutor’s recommendation")), 
                  height = 150
  )
  # Imposed Sent : Judge_Impose
  judge_impose_data <- reactive(judge_datamaker(judge_data, Judge_Impose, input))
  output$JudgeImpose <- renderPlot(
    judge_plotter(judge_impose_data(), Judge_Impose, 
                  input, "Number of Cases", c("No", "Yes"), 
                  c("Judge did not impose a sentence", "Judge imposed a sentence")), 
                  height = 150
  )
  # Sent Severity : Judge_Impose2
  judge_impose2_data <- reactive(judge_datamaker(judge_data, Judge_Impose2, input))
  output$JudgeImpose2 <- renderPlot(
    judge_plotter(judge_impose2_data(), Judge_Impose2, 
                  input, "Number of Cases", c("Less", "More", "Unclear", "Unknown"), 
                  c("Imposed a more lenient sentence", "Imposed a harsher sentence", "A different sentence but unclear whether more or less severe", "The imposed sentence is unknown")), 
                  height = 150
  )

  ### Def Page
  # FILTER BY STRING
  # FIGURE OUT CELL COLORING
  # Race by Court
  race_x_court_data <- df %>% 
                filter(Court != "" | Court != NA) %>% 
                filter(Def_Race != "" | Def_Race != NA) %>% 
                count(Court, Def_Race)
  output$def_race_x_court <- renderPlot(
    ggplot(race_x_court_data, aes(x = Def_Race, y = n, fill = Court)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    labs(x = "Race of Person Charged", y = "Number of cases") + 
    scale_fill_viridis(discrete = TRUE, option = "A")
  )
  output$superior_total <- renderText(
    sum((race_x_court_data %>% filter(Court == "Superior Court"))["n"])
  )
  output$district_total <- renderText(
    sum((race_x_court_data %>% filter(Court == "District Court"))["n"])
  )

  # Race by Crimetype
  def_race_x_crimetype_data <- reactive(def_datamaker(df, input, "", -1))
  output$def_race_x_crimetype <- DT::renderDataTable({
                                  as.datatable(formattable(def_race_x_crimetype_data(), 
                                      list("Crimes against persons" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Violent crimes" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Property crimes" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Drug/narcotics crimes" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Motor vehicle offenses" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Financial crimes" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Crimes involving firearms" = color_tile("#FCFDBFFF", "#FD9567FF")
                                 )))
  })
  # Race by Charge
  def_race_x_charge_data <- reactive(def_datamaker(df, input, X1stCharge_Status, which(colnames(df) == "X1stCharge_Status")))
  output$def_race_x_charge <- DT::renderDataTable({
    as.datatable(formattable(def_race_x_charge_data(), list("Black" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Hispanic" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "White" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Unknown" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Other" = color_tile("#FCFDBFFF", "#FD9567FF"))
    ))
  })
  # Race by Sentence
  def_race_x_sentence_data <- reactive(def_datamaker(df, input, X1stPROS_SentenceType, which(colnames(df) == "X1stPROS_SentenceType")))
  output$def_race_x_sentence <- DT::renderDataTable({
    as.datatable(formattable(def_race_x_sentence_data(), list("Black" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Hispanic" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "White" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Unknown" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Other" = color_tile("#FCFDBFFF", "#FD9567FF"))
    ))
  })
  # Race by Treatment
  def_race_x_treatment_data <- reactive(def_datamaker(df, input, RehabTreatment, which(colnames(df) == "RehabTreatment")))
  output$def_race_x_treatment <- DT::renderDataTable({
    as.datatable(formattable(def_race_x_treatment_data(), list("Black" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Hispanic" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "White" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Unknown" = color_tile("#FCFDBFFF", "#FD9567FF"),
                                          "Other" = color_tile("#FCFDBFFF", "#FD9567FF"))
    ))
  })

  ### Charge Page
  # Disposition by Court
  disposition_x_court_data <- reactive(charge_datamaker(df, input))
  output$charge_disposition_x_court <- renderPlot(
    ggplot(disposition_x_court_data(), aes(x = Court, y = n, fill = X1stCharge_Status)) +
    geom_bar(stat = "identity", position = "stack") + 
    scale_fill_viridis(discrete = TRUE, option = "A", name = "Disposition associated with first charge")
  )

  ### Sentence Page
  # Race by Sentence Type
  # FILTER BY COURT TYPE
  race_x_sentencetype_data <- df %>% 
                    filter(Def_Race != "" | Def_Race != NA) %>% 
                    filter(X1stPROS_Prison. != "" | X1stPROS_Prison. != NA) %>% 
                    filter(X1stPROS_Probation != "" | X1stPROS_Probation != NA) %>% 
                    count(Def_Race, X1stPROS_Prison., X1stPROS_Probation) %>%
                    mutate(Prison_x_Probation = case_when(
                            X1stPROS_Prison. == "No" & X1stPROS_Probation == "No" ~ "Neither",
                            X1stPROS_Prison. == "Yes" & X1stPROS_Probation == "No" ~ "Prison",
                            X1stPROS_Prison. == "No" & X1stPROS_Probation == "Yes" ~ "Probation",
                            X1stPROS_Prison. == "Yes" & X1stPROS_Probation == "Yes" ~ "Both"))
  output$sentence_race_x_sentencetype <- renderPlot(
    ggplot(race_x_sentencetype_data, aes(x = Def_Race, y = n, fill = Prison_x_Probation)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_viridis(discrete = TRUE, option = "A", name = "Sentence Type")
  )
  # Race by Sentence Length
  race_x_sentencelength_data <- reactive(sentence0_datamaker(df, input))
  output$sentence_race_x_sentencelength <- DT::renderDT({
    datatable({race_x_sentencelength_data()}, rownames = TRUE)
  })
  # # Crime by Sentence Length
  # race_x_sentencelength_data <- reactive(sentence1_datamaker(df, input))
  # output$sentence_crimetype_x_sentencelength <- DT::renderDT({
  #   datatable({race_x_sentencelength_data()}, rownames = TRUE)
  # })


  ### Attorney Page
  attorney_by_data <- reactive(attorney_datamaker(df, input))
  output$attorney_by <- renderPlot(
    attorney_plotter(attorney_by_data(), input)
  )
}

shinyApp(ui, server)

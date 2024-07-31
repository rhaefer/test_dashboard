library(tidyverse)
library(tidycensus)
library(lehdr)
library(sf)
library(mapview)
library(tigris)
library(CTPPr)
library(shinydashboard)
library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(tmap)
census_api_key("680398dff0a2f4c566f10c95888da7f25e55147b")

## prep ##

bgs <- block_groups(state="VT") %>% filter(COUNTYFP=="007")

## ACS ## 2022

# telework 
telework_19<-get_acs(state="VT",variables='B08301_021', summary_var = "B08301_001", geography = "block group", county=c("Chittenden"), geometry = T, year=2019)%>% st_transform(crs=4326)  %>%
  mutate(percent_telework=estimate/summary_est)

telework_22<-get_acs(state="VT",variables='B08301_021', summary_var = "B08301_001", geography = "block group", county=c("Chittenden"), geometry = T, year=2022)%>% st_transform(crs=4326)  %>%
  mutate(percent_telework=estimate/summary_est)

telework_22 %>% mapview(zcol="percent_telework")

# mode share

var_19_all <- load_variables(year=2019, "acs5", cache = TRUE)

vars_19=data.frame(variable=c("B08301_003","B08301_004","B08301_010","B08301_016","B08301_017","B08301_018","B08301_019","B08301_020","B08301_021"),
                name=var_19_all %>% filter(name %in% c("B08301_003","B08301_004","B08301_010","B08301_016","B08301_017","B08301_018","B08301_019","B08301_020","B08301_021")) %>% pull(label)) 

bgs_19 <- get_acs(state="VT", variables=unique(vars_19$variable), geography = "block group", county=c("Chittenden"), summary_var="B08135_001", geometry = T, year=2019) %>%
  distinct(GEOID, geometry)

mode_19 <- get_acs(state="VT", variables=unique(vars_19$variable), geography = "block group", county=c("Chittenden"), summary_var="B08135_001", geometry = T, year=2019) %>%
  left_join(vars_19, by="variable") %>%
  group_by(GEOID) %>%
  mutate(total=sum(estimate),mode_share=round((estimate/total) * 100,2), year=2019,name = sub("^[^!]*!![^!]*!!", "", name)) %>% 
  st_transform(crs=4326) %>% data.frame() %>% select(-c(variable, NAME, moe, summary_est, summary_moe, geometry))

var_22_all <- load_variables(year=2022, "acs5", cache = TRUE)

vars_22=data.frame(variable=c("B08301_003","B08301_004","B08301_010","B08301_016","B08301_017","B08301_018","B08301_019","B08301_020","B08301_021"),
                   name=var_22_all %>% filter(name %in% c("B08301_003","B08301_004","B08301_010","B08301_016","B08301_017","B08301_018","B08301_019","B08301_020","B08301_021")) %>% pull(label)) 

bgs_22 <- get_acs(state="VT", variables=unique(vars_22$variable), geography = "block group", county=c("Chittenden"), summary_var="B08135_001", geometry = T, year=2022) %>%
  distinct(GEOID, geometry)

mode_22 <- get_acs(state="VT", variables=unique(vars_22$variable), geography = "block group", county=c("Chittenden"), summary_var="B08135_001", geometry = T, year=2022) %>%
  left_join(vars_22, by="variable") %>%
  group_by(GEOID) %>%
  mutate(total=sum(estimate),mode_share=round((estimate/total) * 100,2), year=2022,name = sub("^[^!]*!![^!]*!!", "", name)) %>%
  st_transform(crs=4326) %>% data.frame() %>% select(-c(variable, NAME, moe, summary_est, summary_moe, geometry))


# bgs_22 %>% left_join(
#   mode_22 %>%
#     filter(name=="Estimate!!Total:!!Worked from home" & GEOID=="500070026011"), by="GEOID"
# ) %>% filter(!is.na(mode_share)) %>% mapview(zcol="mode_share")
# 
# bgs_19 %>% left_join(
#   mode_19 %>%
#     filter(name=="Estimate!!Total:!!Worked from home" & GEOID=="500070026011"), by="GEOID"
# ) %>% filter(!is.na(mode_share)) %>% mapview(zcol="mode_share")
  # mode_22 %>%
  #   filter(name=="Estimate!!Total:!!Worked from home" & GEOID=="500070026011") %>% View()
  # 
  # mode_19 %>%
  #   filter(name=="Estimate!!Total:!!Worked from home" & GEOID=="500070026011") %>% View()




## LEHD ## 2019

# jobs_19<-grab_lodes(state = "vt", year = 2019, lodes_type = "od", job_type = "JT00", 
#                  segment = "S000", state_part = "main", agg_geo = "bg")
# 
# home_loc_in_chi_19 <- jobs_19 %>%
#   filter(w_bg %in% unique(bgs$GEOID))

# bgs %>% left_join(home_loc_in_chi_19, by=c("GEOID"="h_bg")) %>% mapview(zcol="S000")

# work_loc_in_chi <- jobs %>%
#   filter(h_bg %in% unique(bgs$GEOID))
# 
# bgs %>% left_join(work_loc_in_chi, by=c("GEOID"="w_bg")) %>% mapview(zcol="S000")

## LEHD ## 2021

# jobs_21<-grab_lodes(state = "vt", year = 2021, lodes_type = "od", job_type = "JT00", 
#                  segment = "S000", state_part = "main", agg_geo = "bg")
# 
# home_loc_in_chi_21 <- jobs_21 %>%
#   filter(w_bg %in% unique(bgs$GEOID))

# bgs %>% left_join(home_loc_in_chi_21, by=c("GEOID"="h_bg")) %>% mapview(zcol="S000")

# work_loc_in_chi <- jobs %>%
#   filter(h_bg %in% unique(bgs$GEOID))

# bgs %>% left_join(work_loc_in_chi, by=c("GEOID"="w_bg")) %>% mapview(zcol="S000")

breaks <- c(0, 20, 40, 60, 80, 100)

options <-vars_19 %>%
  mutate(name = sub("^[^!]*!![^!]*!!", "", name))

############ dashboard ##################

ui <- dashboardPage(skin="black", 
                    dashboardHeader(title="Chittenden County Employee Travel Behavior",titleWidth = 750),
                    dashboardSidebar(
                      selectInput("mode", "Select Mode", choices=unique(options$name))
                    ),
                    dashboardBody(
                      tabBox(width=12,
                             tabPanel("Mode Share (ACS)",
                                      fluidRow(
                                        column(width=6,
                                        box(title=uiOutput("mode1"), width=12, solidHeader =T, status = "primary",
                                          tmapOutput('map_1', height = 800)
                                        )
                                        ),
                                        column(width=6,
                                               box(title=uiOutput("mode2"), width=12,solidHeader =T,status = "primary",
                                               tmapOutput('map_2', height = 800)
                                               )
                                        )
                                      )
                                      )
                    )
                    ),
                    tags$head(tags$style(HTML("
    .skin-black .main-sidebar {
        background-color:  black;
                            }")))
)
server <- function(input, output, session) { 
output$mode1 <- renderText({
  paste("Mode Share (%) (2019)", input$mode, sep=" - ")
  })
output$mode2 <- renderText({
  paste("Mode Share (%) (2022)", input$mode, sep=" - ")
})
map1 <-reactive({
  bgs_19 %>% left_join(
    mode_19 %>%
      filter(name==input$mode), by="GEOID"
  )
  })
output$map_1  <-renderTmap({
  tm_shape(map1()) + tm_polygons("mode_share", legend.title = "Mode Share (%)", breaks = breaks,
                                 id = "mode_share",
                                 popup.vars = c("Block Group" = "GEOID",
                                                "Mode"= "name",
                                                "Mode Share" = "mode_share", 
                                                "Estimate" = "estimate", 
                                                "Year" = "year")) 
  })
map2 <-reactive({
  bgs_22 %>% left_join(
    mode_22 %>%
      filter(name==input$mode), by="GEOID"
  ) 
})
output$map_2  <-renderTmap({
  tm_shape(map2()) + tm_polygons("mode_share", legend.title = "Mode Share (%)", breaks = breaks,
                                 id = "mode_share",
                                 popup.vars = c("Block Group" = "GEOID",
                                                "Mode"= "name",
                                                "Mode Share" = "mode_share", 
                                                "Estimate" = "estimate", 
                                                "Year" = "year")) 
})
}
shinyApp(ui, server)


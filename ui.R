# library(shiny)
# library(semantic.dashboard)
library(shinydashboard)


# theme = "cruelon",
# dboard_header = dashboardHeader(disable = TRUE)
# dboard_sidebar = dashboardSidebar(disable = TRUE)
  


dboard_body = dashboardBody(
  # fluidRow(
  #   column(
  #     width = 3,
  #     selectizeInput("from_year", "From :", years_disp, selected =
  #                      min(years_disp))
  #   ),
  #   column(
  #     width = 3,
  #     selectizeInput("to_year", "To :", years_disp, selected = max(years_disp))
  #   ),
  #   column(
  #     width = 3,
  #     selectizeInput("period_selected", "Period :", period_disp, selected = period_disp[3])
  #   ),
  #   column(
  #     width = 4,
  #     radioButtons(
  #       "sales_inv",
  #       "Data:",
  #       choices = c("Sales" = "SM", "Inventory" = "IM"),
  #       selected = "SM",
  #       inline = TRUE
  #     ),
  #     radioButtons(
  #       "sea_adj",
  #       "Seasonal Adj:",
  #       choices = c("Yes" = 1, "No" = 0),
  #       selected = 1,
  #       inline = TRUE
  #     ),
  #     radioButtons(
  #       "act_pct",
  #       "Actuals/Growth:",
  #       choices = c("Actuals" = "actuals", "Growth" = "growth"),
  #       selected = "actuals",
  #       inline = TRUE
  #     )
  #   )
  # ),
  # fluidRow(
  #   box(
  #     width = 8,
  #     title = "Top 5 Categories",
  #     color = "green",
  #     ribbon = TRUE,
  #     title_side = "Top Categories",
  #     column(8,
  #            "Test" #plotlyOutput("top5_plot1")))
  #     ),
  #     box(
  #       width = 8,
  #       title = "Bottom 5 Categories",
  #       color = "green",
  #       ribbon = TRUE,
  #       title_side = "Bottom Categories",
  #       column(8,
  #              "Test" # plotlyOutput("bottom5_plot1"))))
  #       )
  #     )
  #   )
  # ),
  fluidRow(
    # box(
    #   width = 16,
    #   title = "Graph 1",
    #   color = "green",
    #   ribbon = TRUE,
    #   title_side = "Retail Sales",
    #   column(8,
    #          "Test" #plotlyOutput("ret_plot1"))))
    #   )
    # ),
    fluidRow(box(
      # width = 16,
      title = "Relative Size",
      # color = "green",
      status = "success",
      solidHeader = TRUE,
      # title_side = "Retail Sales",
      "Test" # plotOutput("tree_plot1")
    ))),
          
    fluidRow(
      box(
        width = 8,
        height = 450,
        title = "Growth vs Revenue - Leaders (Current Year)",
        # color = "green",
        status = "success",
        solidHeader = TRUE,
        # title_side = "xxx ",
        plotlyOutput("point_plot1")
      ),
      box(
        width = 8,
        height = 450,
        title = "Growth vs Revenue - Leaders (Current Quarter)",
        # color = "green",
        status = "success",
        solidHeader = TRUE,
        # title_side = "xxx ",
        plotlyOutput("point_plot2")
      )
    )
)

# ####
ecomm_frow_top = fluidRow(
  # valueBoxOutput("value1"),
  # valueBoxOutput("value2"),
  # valueBoxOutput("value3")
  column(
    width = 12,
    box(
      # valueBoxOutput("value1"),
      # valueBoxOutput("value2"),
      # valueBoxOutput("value3")
      width = 12,
      title = NULL,
      status = NULL,
      solidHeader = FALSE,
      valueBoxOutput("value1"),
      valueBoxOutput("value2"),
      valueBoxOutput("value3")
    )
  )
)

ecomm_frow1 = fluidRow( 
    column(width = 12,    box(width = 6,
      title = "Revenue - eComm vs Retail",
      status = "success",
      solidHeader = TRUE,
      plotlyOutput("ecom_sales")),
    box(width = 6,
        title = "eComm as percent of Retail",
        status = "success",
        solidHeader = TRUE,
        plotlyOutput("ecom_sales2")
    ))
)

ecomm_frow2 = fluidRow(
  column(width = 12,    box(width = 12,
                            title = "Revenue - eComm vs Retail",
                            status = "success",
                            solidHeader = TRUE,
                            plotlyOutput("ecom_pctGrowth"))
))

# ####
ret_frow_top = fluidRow(
  column(
    width = 12,
    box(width = 12, collapsible = TRUE,
        column(
          width = 2,
          selectizeInput("from_year", "From", years_disp, selected =
                           min(years_disp))
        ),
        column(
          width = 2,
          selectizeInput("to_year", "To", years_disp, selected = max(years_disp))
        ),
        column(
          width = 2,
          selectizeInput("period_selected", "Data Frequency", period_disp, selected = period_disp[3])
        ),
        column(
          width = 4,
          radioButtons(
            "sales_inv",
            "Data:",
            choices = c("Sales" = "SM", "Inventory" = "IM"),
            selected = "SM",
            inline = TRUE
          ),
          radioButtons(
            "sea_adj",
            "Seasonal Adj:",
            choices = c("Yes" = 1, "No" = 0),
            selected = 1,
            inline = TRUE
          ),
          radioButtons(
            "act_pct",
            "Actuals/Growth:",
            choices = c("Actuals" = "actuals", "Growth" = "growth"),
            selected = "actuals",
            inline = TRUE
          )
        )
    )
  )
)
# ####
ret_frow1 = fluidRow(
  column(
    width = 12
    # box(
    #   width = 6,
    #   title = "Revenue - eComm vs Retail",
    #   status = "success",
    #   solidHeader = TRUE,
    #   plotlyOutput("ecom_sales")
    # ),
    # box(
    #   width = 6,
    #   title = "eComm as percent of Retail",
    #   status = "success",
    #   solidHeader = TRUE,
    #   plotlyOutput("ecom_sales2")
    # )
  )
)

ret_frow2 = fluidRow(
  column(
    width = 12,
    box(
      width = 16,
      title = "Graph 1",
      color = "green",
      ribbon = TRUE,
      title_side = "Retail Sales",
      plotlyOutput("ret_plot1")
    )
  )
)
  
ecomm_tabs =  fluidRow(ecomm_frow_top, ecomm_frow1, ecomm_frow2)
ret_tabs =  fluidRow(ret_frow_top, ret_frow1, ret_frow2)


dboard_header = dashboardHeader(title = "Dashboard Demo")
dboard_sidebar = dashboardSidebar(
  menuItem(tabName = "RetailOverall", text = "Retail Overall"),
  menuItem(tabName = "SubSectorAnalysis", text ="Sub Sector Analysis"),
  menuItem(tabName = "eCommerce", text ="eCommerce")
  
)

menu_mapping = tabItems(
  tabItem(tabName = "RetailOverall",ret_tabs),
  tabItem(tabName = "SubSectorAnalysis","subsector_tabs"),
  tabItem(tabName = "eCommerce",ecomm_tabs)
)



dboard_body2 = dashboardBody(
  menu_mapping
)
db_page = dashboardPage(dboard_header, dboard_sidebar, dboard_body2)

shinyUI(db_page)



#   fluidRow(
#   tabBox(
#   width = 7,
#   color = "blue",
#   collapsible = FALSE,
#   tabs = list(
#     list(menu = div("First Tab"),content =  ecomm_frow2),
#     list(menu = "Second Tab", content = "Some Other Text")
#   )
#     
#   # tabPanel("Tab1", "ecomm_frow2"),
#   # tabPanel("Tab2", "Tab content 2")
# ))

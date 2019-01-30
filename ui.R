# library(semantic.dashboard)
library(shiny)
library(shinydashboard)

# eComm top row ####
ecomm_frow_top = fluidRow(
  column(
    width = 12,
    valueBoxOutput("ecom_value1"),
    valueBoxOutput("ecom_value2"),
    valueBoxOutput("ecom_value3")
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
  column(
    width = 12,
    box(
      width = 12,
      title = "Revenue - eComm vs Retail",
      status = "success",
      solidHeader = FALSE,
      plotlyOutput("ecom_pctGrowth", height = "600px")
    )
  )
)

# ####
ret_frow_top1 = fluidRow(
  column(
    width = 12,
    # box(width = 12, collapsible = TRUE, status = "success",
    column(
      width = 3,
      selectizeInput("period_selected", "Data Frequency", period_disp, selected = period_disp[1])
    ),
    # column(width=2),
    
        column(
          width = 3,
          selectizeInput("from_year", "From", years_disp, selected =
                           min(years_disp))
        ),
        # column(width=1),
        column(
          width = 3,
          selectizeInput("to_year", "To", years_disp, selected = max(years_disp))
        )

  )
)
ret_frow_top2 = fluidRow(
  column(
    width = 12,
    column(
      width = 3,
      radioButtons(
        "sales_inv",
        "Data:",
        choices = c("Sales" = "SM", "Inventory" = "IM"),
        selected = "SM",
        inline = TRUE
      )),
      column(width = 3,
      radioButtons(
        "sea_adj",
        "Seasonal Adj:",
        choices = c("Yes" = 1, "No" = 0),
        selected = 1,
        inline = TRUE
      )),
      column(width = 3,
      radioButtons(
        "act_pct",
        "Actuals/Growth:",
        choices = c("Actuals" = "actuals", "Growth" = "growth"),
        selected = "actuals",
        inline = TRUE
      ))
    )
  )
# ####
ret_frow1 = fluidRow(
  column(
    width = 12,
    box(
      width = 12,
      collapsible = TRUE,
      title = "Retail Level Sales/Growth",
      status = "success",
      color = "olive",
      solidHeader = FALSE,
      plotlyOutput("ret_plot1")
    )
  )
)


ret_frow2 = fluidRow( 
  column(
    width = 12,
    box(
      width = 6, 
      title = "Top 5 Sub Categories",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput("top5_plot1")
    ),
    box(
      width = 6, 
      title = "Bottom 5 Sub Categories",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput("bottom5_plot1")
    )
  )
)

ret_frow3 = fluidRow(
  column(
    width = 12,
    box(
      width = 12,
      title = "Subsector - Relative Size",
      status = "success",
      solidHeader = FALSE,
      collapsible = TRUE,
      plotOutput("tree_plot1", height = "500px")
    )
  )
)

ret_frow4 = fluidRow(
  column(
    width = 12,
    box(
      width = 12,
      title = "Sales + Growth Leaders - Last Year (2017)",
      status = "success",
      solidHeader = FALSE,
      plotlyOutput("salesgrwoth_leader_plot1")
    )
  )
)
# Category row 2 ####
cat_frow_top2 = fluidRow(
  column(
    width = 12,
        column(
          width = 3,
          selectizeInput("period_selected2", "Data Frequency", period_disp, selected = period_disp[1])
        ),
        column(width = 2),
               
        column(
          width = 3,
          radioButtons(
            "sales_inv2", "Data:",
            choices = c("Sales" = "SM", "Inventory" = "IM"),
            selected = "SM",
            inline = TRUE
          )),
        
        column(
          width = 3,
          radioButtons(
            "act_pct2",
            "Actuals/Growth:",
            choices = c("Actuals" = "actuals", "Growth" = "growth"),
            selected = "actuals",
            inline = TRUE
          )
        )
    )
  # )
) 
# Category top select row ####
cat_frow_top1 = fluidRow(
  column(width = 12,
         column(
           width = 5,
           selectizeInput(
             "category_selected",
             "Select a sector :",
             subsec_list,
             selected = subsec_list[1]
           )
         ),
         column(
           width = 3,
           selectizeInput("from_year2", "From", years_disp, selected =
                            min(years_disp))
         ),
         column(
           width = 3,
           selectizeInput("to_year2", "To", years_disp, selected = max(years_disp))
         )
  )
)  

cat_frow1 = fluidRow(
  column(
    width = 9,
    box(
      width = 12,
      title = "Category Sales/Growth",
      status = "success",
      solidHeader = TRUE,
      plotlyOutput("cat_plot1")
    )
  ),
  column(
    width = 3,
    
    valueBoxOutput("cat_value1", width = 12),
    valueBoxOutput("cat_value2", width = 12),
    valueBoxOutput("cat_value3", width = 12),
    valueBoxOutput("cat_value4", width = 12)
  )
  
)

cat_frow_topx = fluidRow(
  column(
    width = 12,
    box(
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
  
ecomm_tabs =  fluidRow(ecomm_frow_top, ecomm_frow1, ecomm_frow2 )
ret_tabs =  fluidRow(ret_frow_top1, ret_frow_top2, ret_frow1, ret_frow2, ret_frow3, ret_frow4)
cat_tabs =  fluidRow(cat_frow_top1, cat_frow_top2, br(), br(), cat_frow1)

dboard_header = dashboardHeader(title = "US Retail Sales Data Analyis")
dboard_sidebar = dashboardSidebar(
  sidebarMenu(
    # menuItem(tabName = "Home", text = "Home", icon = icon("home", lib="glyphicon")),
    menuItem(tabName = "RetailOverall", text = "Retail Overall", icon = icon("stats", lib="glyphicon")),
    menuItem(tabName = "SubSectorAnalysis", text ="Sub Sector Analysis", icon = icon("tag", lib="glyphicon")),
    menuItem(tabName = "eCommerce", text ="eCommerce", icon = icon("phone", lib="glyphicon"))
  )
)



dboard_body2 = dashboardBody(
  tabItems(
    tabItem(tabName = "RetailOverall",ret_tabs),
    tabItem(tabName = "SubSectorAnalysis",cat_tabs),
    tabItem(tabName = "eCommerce",ecomm_tabs)
  )
)
db_page = dashboardPage(dboard_header, dboard_sidebar, dboard_body2)

shinyUI(db_page)



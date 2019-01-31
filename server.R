library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(ggthemes)


# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {

  min_year <- reactive({
    input$from_year
  })
  
  min_year2 <- reactive({
    input$from_year2
  })

  max_year <- reactive({
    input$to_year
  })

  max_year2 <- reactive({
    input$to_year2
  })
  
  sea_adjusted_flag <- reactive({
    as.integer(input$sea_adj)
  })

  sales_inventory_flag = reactive({
    input$sales_inv
  })

  sales_inventory_flag2 = reactive({
    input$sales_inv2
  })

  period_selected = reactive({
    input$period_selected
  })
  
  period_selected2 = reactive({
    input$period_selected2
  })

  sector_selected = reactive({
    input$category_selected
  })
  
  cat_stats = reactive({
    df1 = filter(last_12mth_ret_sales, cat_desc2 == input$category_selected)
    df2 = filter(last_12mth_ret_inv, cat_desc2 == input$category_selected)
    c(df1$total_value, df1$avg_growth, df2$avg_value, df2$avg_growth)
  })
  
  growth_flag = reactive({
    if(input$act_pct == "actuals") x = "value"
    if(input$act_pct == "growth") x = "pct_diff"
    x
  })
  
  growth_flag2 = reactive({
    if(input$act_pct2 == "actuals") x = "value"
    if(input$act_pct2 == "growth") x = "pct_diff"
    x
  })

  observe({

  })

  output$ret_plot1 <- renderPlotly({
    ylable_text = paste0(ifelse(sales_inventory_flag()=="SM", "Sales ", "Inventory "),
                         ifelse(growth_flag() == "value", "(in $bln)", "Growth Percent")
                         )
      
    upd_df = applyFilters(df3,min_date = min_year(), max_date = max_year(), 
                          SA=sea_adjusted_flag(), data_type = sales_inventory_flag() )
    upd_df = applyRollups(upd_df, period = period_selected())
    g = build_primary_plot(upd_df, growth_flag(), xlabel = "", ylabel = ylable_text )

    ggplotly(g) %>% layout(legend = list(orientation = "h", x = 0.4, y =-0.2))
    
  })

  output$cat_plot1 <- renderPlotly({
    ylable_text = paste0(ifelse(sales_inventory_flag2()=="SM", "Sales ", "Inventory "),
                         ifelse(growth_flag2() == "value", "(in $bln)", "Growth Percent")
                         )
    upd_df = applyFilters(df3,min_date = min_year2(), max_date = max_year2(), 
                          data_type = sales_inventory_flag2(), 
                          master_cats = FALSE, sector = sector_selected())
    upd_df = applyRollups(upd_df, period = period_selected2())
    g = build_primary_plot(upd_df, growth_flag2(), xlabel = "", ylabel = ylable_text )
    
    validate(
      need(g, "Data not available for selected combination")
    ) 
    
    ggplotly(g) %>% layout(legend = list(orientation = "h", x = 0.4, y =-0.2))
    
  })

  output$top5_plot1 <- renderPlotly({
    upd_df = applyFilters(df3,min_date = min_year(), max_date = max_year(),
                          SA=sea_adjusted_flag(), data_type = sales_inventory_flag(),  master_cats = FALSE )
    upd_df = applyRollups(upd_df, period = period_selected())
    upd_df = get_top_n(upd_df, 5, growth_flag())
    
    g = build_top5_plot(upd_df, xlabel = "" ) + scale_fill_brewer(palette = "Paired")
    
    ggplotly(g) #%>% layout(legend = list(orientation = "h", x = 1, y =-0.2))
  })  
    
  output$bottom5_plot1 <- renderPlotly({
    upd_df = applyFilters(df3,min_date = min_year(), max_date = max_year(),
                          SA=sea_adjusted_flag(), data_type = sales_inventory_flag(),master_cats = FALSE )
    upd_df = applyRollups(upd_df, period = period_selected())
    upd_df = get_top_n(upd_df, -5, growth_flag())
    
    g = build_top5_plot(upd_df, xlabel = "" ) + scale_fill_brewer(palette = "Paired")
    ggplotly(g) %>% layout(legend = list(orientation = "h", x = 0.4, y =-0.2))
  })
  
  # output$top5_plot1 <- renderGvis({
  # 
  #   upd_df = applyFilters(df3,min_date = min_year(), max_date = max_year(),
  #                         SA=sea_adjusted_flag(), data_type = sales_inventory_flag(),  master_cats = FALSE )
  # 
  #   upd_df = applyRollups(upd_df, period = period_selected())
  #   print(dim(upd_df))
  #   
  #   upd_df = get_top_n(upd_df, 5, growth_flag())
  # 
  #   g = build_top5_plot(upd_df,type_of_data = growth_flag(), xlabel = "" )
  #   g
  # })
  # 
  # output$bottom5_plot1 <- renderGvis({
  #   upd_df = applyFilters(df3,min_date = min_year(), max_date = max_year(),
  #                         SA=sea_adjusted_flag(), data_type = sales_inventory_flag(),master_cats = FALSE )
  #   upd_df = applyRollups(upd_df, period = period_selected())
  #   upd_df = get_top_n(upd_df, -5, growth_flag())
  #   g = build_top5_plot(upd_df, type_of_data = growth_flag(), xlabel = "" )
  #   # g
  # })
  
  output$tree_plot1 <- renderPlot({
    upd_df = applyFilters(df3,min_date = min_year(), max_date = max_year(),
                          SA=sea_adjusted_flag(), data_type = sales_inventory_flag(), master_cats = FALSE )
    upd_df = applyRollups(upd_df, period = period_selected())

    g = treemap(upd_df,index = "cat_desc2", vSize = "value",
                title = "", palette="Set3")
  })
  
  output$salesgrwoth_leader_plot1 <- renderPlotly({

    upd_df = applyFilters(df3,min_date = global_min_year, max_date = global_max_year,  master_cats = FALSE)
    
    upd_df = applyRollups(upd_df, period = "year", YoY = TRUE)
    
    upd_df = upd_df %>% filter(year(date) == current_year) %>%
      mutate(Sales = value/1e3, GrowthRate = pct_diff/1e2, Sector = cat_desc2)
    # %>%   select(, everything())
    upd_df = na.omit(upd_df)
    
    median_value = median(upd_df$Sales)
    median_growth = median(upd_df$GrowthRate)
    
    
    g = ggplot(upd_df, aes(x=Sales, y = GrowthRate, label=Sector)) + 
      geom_point(size=2, color = "purple") + 
      # geom_text(aes(label=ifelse((pct_diff>median_growth & value > median_value),cat_desc2,""),hjust=0,vjust=0, size = 2, nudge_y = 0.5 ))+
      # geom_label()+  
      geom_hline(yintercept = median_growth, color="orange")+
      geom_vline(xintercept =  median_value, color="orange")+
      # theme(legend.position = "null") +
      scale_y_continuous(labels = percent) + 
      scale_x_continuous(labels = comma) +
      labs(x = "Sales (in $bln)", y = "Growth") +
      theme_gdocs()
      
    ggplotly(g)
  })

  output$point_plot2 <- renderPlotly({

    upd_df = applyFilters(df3,min_date = global_min_year, max_date = global_max_year)
    
    upd_df = applyRollups(upd_df, period = "quarter", YoY = TRUE)
    
    upd_df = filter(upd_df, year(date) == current_year,quarter(date) == current_quarter )
    
    
    
    median_value = median(upd_df$value)
    median_growth = median(upd_df$pct_diff)
    
    g = ggplot(upd_df, aes(x=value, y = pct_diff, label=cat_desc2)) + geom_point(size=2, color = "tomato") + 
      # geom_label_repel(aes(label = ifelse(pct_diff>=5,cat_desc2,"")),
      #                  box.padding   = 0.35, point.padding = 0.5, segment.color = 'grey50') +
      geom_text(aes(label=ifelse((pct_diff>median_growth & value > median_value),cat_desc2,''))
                ,hjust=0,vjust=1, size = 2, nudge_y = 0.5 )+
      
      geom_hline(yintercept = median_growth, color="orange")+
      geom_vline(xintercept =  median_value, color="orange")+
      theme(legend.position = "null") 
    ggplotly(g)
  })
  
  output$ecom_sales <- renderPlotly({
    g = geteComVsRetailSales() 
    ggplotly(g) %>% 
      layout(legend = list(orientation = "h", x = 0.4, y =-0.1))
  })

  output$ecom_sales2 <- renderPlotly({
    g = getEcommPctSales()

    ggplotly(g) %>% 
      layout(legend = list(orientation = "h", x = 0.4, y =-0.1))
  })
  
  output$ecom_pctGrowth <- renderPlotly({
    g = getEcomYOYGrowth()
    ggplotly(g) %>% 
      layout(legend = list(orientation = "h", x = 0.4, y =-0.1))
  })
  
  output$seasonality_plot1 <- renderPlotly({
    g = getSeasonalityChart(nsa_cat_df, sector_selected())
    validate(
      need(g, "Sales data not available for selected sector")
    )     
    ggplotly(g)
  })

  output$seasonality_plot2 <- renderPlotly({
    g = getSeasonalityChart(nsa_cat_df, sector_selected(), data_type = "IM")

    validate(
      need(g, "Inventory data not available for selected sector")
    )    
    ggplotly(g) 
  })
  
  
  output$ecom_value1 <- renderValueBox({
    value = ecomm_stats[ecomm_stats$cat_desc == "E-commerce Retail Sales",]$total_value
    value = paste0("$", round(value/1000,1), " bln")
    valueBox(
      value = formatC(value, format = "d", big.mark = ','),
      subtitle = 'Last 4 Quarter Sales',
      color = "black",
      width = 4)
  })    
  output$ecom_value2 <- renderValueBox({
    value = ecomm_stats[ecomm_stats$cat_desc == "E-commerce Retail Sales",]$avg_growth
    value = paste0(round(value,2),"%")
    valueBox(
      value = formatC(value, format = "d", big.mark = ','),
      subtitle = 'Avg Growth - last 4 quarters',
      # icon = icon("line chart"),
      color = "black",
      width = 4)
  })
  output$ecom_value3 <- renderValueBox({
    value = ecomm_stats[ecomm_stats$cat_desc == "Retail Trade",]$total_value
    value = paste0("$", round(value/1000,1), " bln")
    valueBox(
      value = formatC(value, format = "d", big.mark = ','),
      subtitle = 'Retail sales(includes Auto)',
      # icon = icon("line chart"),
      color = "black",
      width = 4)
  })

  output$cat_value1 <- renderValueBox({
    value = cat_stats()[1]
    value = paste0("$", round(value/1000,1), " bln")
    
    valueBox(
      value = formatC(value, format = "d", big.mark = ','),
      subtitle = 'Sales - last 12M',
      color = "black")
  })
  
  output$cat_value2 <- renderValueBox({
    value = cat_stats()[2]
    value = paste0(round(value,2),"%")
    
    valueBox(
      value = formatC(value, format = "d", big.mark = ','),
      subtitle = 'YoY Sales Growth',
      color = "black")
  })
  
  output$cat_value3 <- renderValueBox({
    value = cat_stats()[3]
    if (!is.na(value)) value = paste0("$", round(value/1000,1), " bln")
    
    
    valueBox(
      value = formatC(cat_stats()[3], format = "d", big.mark = ','),
      subtitle = 'Avg Inventory - last 12M',
      color = "black")
  })
  
  output$cat_value4 <- renderValueBox({
    value = cat_stats()[4]
    if (!is.na(value)) value = paste0(round(value,2),"%")
    
    valueBox(
      value = formatC(value, format = "d", big.mark = ','),
      subtitle = 'YoY Inventory Growth',
      color = "black")
  })
  
})



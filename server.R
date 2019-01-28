
# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {

  min_year <- reactive({
    input$from_year
  })

  max_year <- reactive({
    input$to_year
  })

  sea_adjusted_flag <- reactive({
    as.integer(input$sea_adj)
  })

  sales_inventory_flag = reactive({
    input$sales_inv
  })

  period_selected = reactive({
    input$period_selected
  })

  growth_flag = reactive({
    if(input$act_pct == "actuals") x = "value"
    if(input$act_pct == "growth") x = "pct_diff"
    x
  })

  observe({
    print(growth_flag())
    # print(typeof(period_selected()))
    # print(class(period_selected()))

  })

  # colscale <- c(semantic_palette[["red"]], semantic_palette[["green"]], semantic_palette[["blue"]])
  
  output$ret_plot1 <- renderPlotly({
    upd_df = applyFilters(df3,min_date = min_year(), max_date = max_year(), 
                          SA=sea_adjusted_flag(), data_type = sales_inventory_flag() )
    print(dim(upd_df))
    upd_df = applyRollups(upd_df, period = period_selected())
    print(dim(upd_df))
    g = build_primary_plot(upd_df, growth_flag(), xlabel = "Year" )
    # ggplotly(g + scale_colour_manual(values = colscale))
    ggplotly(g) 
    
  })

  output$top5_plot1 <- renderPlotly({
    upd_df = applyFilters(df3,min_date = min_year(), max_date = max_year(),
                          SA=sea_adjusted_flag(), data_type = sales_inventory_flag() )
    upd_df = applyRollups(upd_df, period = period_selected())
    upd_df = get_top_n(upd_df, 5)
    
    g = build_top5_plot(upd_df )
    ggplotly(g)
  })
  
  output$bottom5_plot1 <- renderPlotly({
    upd_df = applyFilters(df3,min_date = min_year(), max_date = max_year(),
                          SA=sea_adjusted_flag(), data_type = sales_inventory_flag() )
    upd_df = applyRollups(upd_df, period = period_selected())
    upd_df = get_top_n(upd_df, -5)
    
    g = build_top5_plot(upd_df )
    ggplotly(g)
  })
  
  output$tree_plot1 <- renderPlot({
    upd_df = applyFilters(df3,min_date = min_year(), max_date = max_year(),
                          SA=sea_adjusted_flag(), data_type = sales_inventory_flag() )
    upd_df = applyRollups(upd_df, period = period_selected())

    g = treemap(upd_df,index = "cat_desc2", vSize = "value")
    g
  })
  
  output$point_plot1 <- renderPlotly({
    print(min_year)
    print(max_year)
    
    upd_df = applyFilters(df3,min_date = global_min_year, max_date = global_max_year)

    upd_df = applyRollups(upd_df, period = "year", YoY = TRUE)

    upd_df = filter(upd_df, year(date) == current_year)
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

  output$point_plot2 <- renderPlotly({
    print(min_year)
    print(max_year)
    
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
  
  output$value2 <- renderValueBox({
    valueBox(
      value = formatC(20000, format = "d", big.mark = ','),
      subtitle = 'Total Expected Revenue',
      icon = icon("line chart"),
      color = "black",
      width = 5)
  })
  output$value3 <- renderValueBox({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
})


# output$boxplot1 <- renderPlotly({
#   ggplotly(ggplot(mtcars, aes(x = as.factor(am), y = mpg)) +
#              geom_boxplot(fill = semantic_palette[["green"]]) + 
#              xlab("gearbox") + ylab("Miles per gallon"))
# })
# 
# output$dotplot1 <- renderPlotly({
#   ggplotly(ggplot(mtcars, aes(wt, mpg))
#            + geom_point(aes(colour=factor(cyl), size = qsec))
#            + scale_colour_manual(values = colscale)
#   )
# })
# 
# output$carstable <- DT::renderDataTable(mtcars)
# 

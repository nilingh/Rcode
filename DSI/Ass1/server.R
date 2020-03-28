# The Server part of a Shiny app

# Course: DATA423-20S1
# Task: Assignment 1
# Student: Zhen Huang
# ID: 74093323

shinyServer(function(input, output, session) {
    # Categorical Data
    output$summary_my_tb1 <- renderPrint({
        skimr::skim(my_tb,ID,Author,Priority,Price,Speed,Duration,Temp,Location,Agreed,State,Class,Surface)
    })
    
    # Date
    output$summary_my_tb2 <- renderPrint({
        skimr::skim(my_tb,Date)
    })
    
    # Numeric Data
    output$summary_my_tb3 <- renderPrint({
        skimr::skim(my_tb,Y,sensor1,sensor2,sensor3,sensor4,sensor5,sensor6,sensor7,sensor8,sensor9,sensor10,sensor11,sensor12,sensor13,sensor14,sensor15,sensor16,sensor17,sensor18,sensor19,sensor20,sensor21,sensor22,sensor23,sensor24,sensor25,sensor26,sensor27,sensor28,sensor29,sensor30)
    })
    
    # Summary Data
    output$summary_my_tb4 <- renderPrint({
        summary(my_tb)
    })
    
    # Show Row Data
    output$my_tb_raw <- DT::renderDataTable({
        if (input$round_cols) {
            DT::datatable(data = my_tb,
                          rownames = FALSE,
                          filter = "top"
                          ) %>% formatRound(1, 3) %>% formatRound(15:44, 3)
        }
        else {
            DT::datatable(data = my_tb,
              rownames = FALSE,
              filter = "top"
              ) 
        }
    })
    
    output$vis_my_tb <- renderPlot({
        visdat::vis_dat(my_tb, sort_type = input$sort) +
            labs(title = "Visualise of Ass1Data")
  })
    
  output$Boxplot <- renderPlot({
    # data <- as.matrix(my_tb)
    data <- scale(my_numeric_data, center = input$standardise, scale = input$standardise)
    boxplot(x = data, use.cols = TRUE, notch = FALSE, varwidth = FALSE,
            horizontal = FALSE, outline = input$outliers,
            col = brewer.pal(n = dim(data)[2], name = "RdBu"),
            range = input$iqr_range, main = "Boxplots of Ass1 data", las = 2)
    # text(box_plt, par("usr")[3], labels = num_cols, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.8) 

  })
  
  x <- reactive({ na.omit(my_tb[[input$sel_col]]) })
  
  output$ui_range <- renderUI({
      min_val <- round(min(x()),2)
      max_val <- round(max(x()),2)
      sliderInput("ui_data_range", label = "Choose range of interest:", min = min_val, max = max_val, value = c(min_val, max_val))
  })
  
  output$ui_plot <- renderPlot({
      plot_data <- x()
      plot_data <- plot_data[plot_data>=input$ui_data_range[1] & plot_data<=input$ui_data_range[2]]
      plot(x = plot_data, xlab = "Observations", ylab = input$sel_col, main = paste("Data of", input$sel_col))
  })
  
  output$Corrgram <- renderPlot({
    corrgram::corrgram(my_numeric_data, order = input$order, abs = input$abs,
                       upper.panel = panel.pie, 
                       text.panel = panel.txt, cor.method = input$cor_method,
                       main = "Correlation of Ass1 data")
  })
  
  output$Corr_cate_plot <- renderPlot({
    corrgram::corrgram(my_cate_data, order = "OLO", # abs = TRUE,
                       upper.panel = panel.pie,
                       ext.panel = panel.txt,
                       cor.method = "kendall",
                       main = "Correlation of ordinal data")
  })
  
  output$GGpairs_s <- renderPlot({
      switch (input$spearman_group,
              "spear_group_col1" = data1 <- my_numeric_data[spear_group_col1],
              "spear_group_col2" = data1 <- my_numeric_data[spear_group_col2],
              "spear_group_col3" = data1 <- my_numeric_data[spear_group_col3]
              )
    GGally::ggpairs(data = data1, title = "Pairs of Ass1 data")
  })
  
  output$GGpairs_p <- renderPlot({
      switch (input$pearson_group,
              "sensor_group_col1" = data1 <- my_numeric_data[sensor_group_col1],
              "sensor_group_col2" = data1 <- my_numeric_data[sensor_group_col2],
              "sensor_group_col3" = data1 <- my_numeric_data[sensor_group_col3],
              "sensor_group_col4" = data1 <- my_numeric_data[sensor_group_col4]
              )
      GGally::ggpairs(data = data1, title = "Pairs of Ass1 data")
  })
  

  output$Mosaic <- renderPlot({
    formula <- as.formula(paste("~",paste(input$var_mosaic, collapse = " + ")))
    vcd::mosaic(formula, data = my_tb,
           main = "Mosaic of Ass1 data", shade = TRUE, legend = TRUE)
  })
  
  
  output$barplot_novelty <- renderPlot({
      col_type <- c()
      ratio <- c()
      for (col in all_cols) {
          col_type <- c(col_type,class(my_tb[[col]]))
          ratio <- c(ratio,round(length(unique(my_tb[[col]])) / nrow(my_tb),3))
      }
      my_novelty <- tibble(col_names = all_cols,col_type,ratio)
      my_novelty <- arrange(my_novelty, ratio)
      my_bar_color <- as.factor(my_novelty$col_type)
      bar_plt <- barplot(height = my_novelty$ratio, 
                         names.arg = all_cols, 
                         xaxt = "n", 
                         col = my_bar_color, 
                         xlab = "Variables", ylab = "Ratio of continuty", 
                         main = "Novelty of variables"
                         ) # end of barplot
      legend(legend = c("Numeric","Charactor","Date"), col = c(3,1,2), pch = 15, x = "topleft", y = "top")
      # for adjust the angle of x-axis, from stackoverflow.com
      text(bar_plt, par("usr")[3], labels = my_novelty$col_names, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.8) 
      
  })
  
  
  output$rising_order_plot <- renderPlot({
      # c <- input$col_rising_order
      d <- my_tb[input$col_rising_order]
      for (col in 1:ncol(d)) {
          d[,col] <- d[order(d[[col]]),col]
      }
      d <- scale(x = d, center = TRUE, scale = TRUE)
      my_color <- rainbow(ncol(d))
      matplot(y = d, type = "l", col = my_color, lty = 1, xlab ="Obbservations", ylab = "Values", main = "Continuity of selected variables")
      legend(legend = colnames(d), col = my_color, lty = 1, x = "topleft", y = "top")
  })
  

  # ui_col_tab <- reactive({ input$col_tab })
  
  # output$ui_col_sort_tab <- renderUI({
  #     col_sort <- ui_col_tab()
  #     # col_sort <- col_sort[col_sort!="Date"]
  #     selectizeInput(inputId = "col_sort_tab", label = "Select ordering variable:", choices = col_sort, multiple = FALSE, selected = "ID")
  # })
  
  output$ui_tab_plot <- renderPlot({
      data_in_tab1 <- my_tb %>% select(input$col_tab,c("ID","Date","Author"))
      data_in_tab1$Date <- as.character(my_tb$Date)
      data_in_tab1$rownum <- rownames(data_in_tab1)
      tabplot::tableplot(data_in_tab1, sortCol = input$col_sort_tab)
  })

  output$TimeSeries <- renderPlot({
      data_ts <- my_tb[c("Date",input$col_rising_order)] %>% gather(key = "sensor", value = "value", -Date)
      ggplot(data = data_ts, aes(x = Date, y = value)) +
          geom_line(aes(color = sensor)) +
          labs(y="Sensor value", title = "Sensor value by sort of Date") +
          theme(legend.position = "left")
  })
  
  ui_select_group <- reactive({ input$select_group })
  
  output$ui_group_sensor <- renderUI({
      switch (ui_select_group(),
              "1" = select_cols <- spear_group_col1,
              "2" = select_cols <- spear_group_col2,
              "3" = select_cols <- spear_group_col3
              )
      selectizeInput(inputId = "col_mix", label = "Select variables:", choices = select_cols, multiple = TRUE, selected = select_cols[1:3])
  })
  
  output$MixedPairs <- renderPlot({
    data_mix <- my_tb %>% select(input$col_mix, input$col_mix_cate)
    data_mix[input$col_mix_cate] <- factor(data_mix[[input$col_mix_cate]])
    GGally::ggpairs(data = data_mix,  mapping = ggplot2::aes(colour = data_mix[[input$col_mix_cate]]), title = "Mixed pairs of Ass1 data")
  })
  
})

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

function(input, output, clientData, session) {
  
  ##---------------------------------------
  ## Fill in 1st window inputs.
  ##---------------------------------------
  
  observe({
    cat("_ Observe 1st window _ \n")
    infile <- input$infile
    if( is.null(uploadeddata()) )
    {
      updateSelectInput(session, "varnames", choices = "Choose...")    
      updateSelectInput(session, "targetnames", choices = "Choose...")    
      cat("  updated selectinput 1st window => Choose... \n")
    } 
    else if( !is.null(uploadeddata()) && (input$varnames == "Choose...")  )
    { 
      # update list of variable names
      temp <- lapply(X = uploadeddata(), FUN = class)
      temp <- temp[ temp == "integer" | temp == "numeric"]
      updateSelectInput(session, "varnames", choices = c("Choose...", names(temp)))    
      
      # update list of target variable names
      temp <- apply(X = uploadeddata(), MARGIN = 2, FUN = function(x){return(length(unique(x)))} )
      updateSelectInput(session, "targetname", choices = c("Choose...", names(temp[temp == 2])))      
      cat("  updated selectinput 1st window. \n")
    }
    cat("[END] Observe 1st window\n")
  })#observe
  
  ##---------------------------------------
  ## Fill in 2nd window inputs.
  ## names of variables + max #of buckets
  ##---------------------------------------
  
  
  ## Reactive values to keep the max bucket for the slider.
  uniquevalues <- reactiveValues(nr = NULL)  
  
  
  ## Set window 2 drop-downs with chosen variables.
  observe({
    cat("_ Observe 2nd window _ \n")
    
    if((input$varnames != "Choose...") && (input$targetname != "Choose..."))
    {
      ## Update variable names list, on 2nd window.
      updateSelectInput(session, "preinterval1", choices = input$varnames)
      updateSelectInput(session, "preinterval2", choices = input$targetname)
      cat("  Observe 2nd window: updated selectinput. \n")
      
      uniquevalues$nr <- length(unique(uploadeddata()[,input$varnames]))
      cat("  Observe 2nd window: set reactive value uniquevalues$nr", uniquevalues$nr, "\n")
      
    }#if
    cat("[END] Observe 2nd window\n")
    })#observe
  

  ## Debug text
  output$text.1 <- renderPrint({
    #cat("_ output$text.1 _ \n")
    print("uniquevalues$nr")
    str(uniquevalues$nr)
    print("input$setbuckets")
    str(input$setbuckets)
    #cat("[END] output$text.1 \n")
  })
  

  ##-------------------------------------------
  ## Dynamically render UI, (slider in this case.)
  ## Only way around the non-updating problems I had.
  ##-------------------------------------------
  
  output$ui_slider <- renderUI(expr = {
    cat("_ output$ui_slider _ \n")
    if(input$varnames != "Choose..."){
      ## Save the lenght unique values in x
      uniquevalues$nr <- length(unique(uploadeddata()[,input$varnames]))
      ## Create/reset the slider, so it follows the data.
      cat("[END] output$ui_slider \n")
      sliderInput(inputId = "setbuckets",label = "Number of buckets", min = 5, max = uniquevalues$nr, value = uniquevalues$nr, step = floor(uniquevalues$nr / 10))
    }
  })

  
  ##---------------------------------------
  ## Uploaded data.
  ##---------------------------------------
  uploadeddata <- reactive({
    cat("_ uploadeddata() _ \n")
    infile <- input$infile
    if(is.null(infile)) 
    {
      cat("[END] uploadeddata()\n")
      return(NULL)
    }
      
    cat("[END] uploadeddata()\n")
    read.csv(infile$datapath, header = T, sep = "\t") 
  })#reactive
  
  
  
  
  ## -----------------------------
  ## bucketed [data frame]
  ## - re-bucked dataset to use.
  ## reduces data range, so loop goes faster.
  ##-------------------------------
  ## create buckets according to breaks
  
  bucketed <- reactive({
    cat("_ bucketed() _ \n")
    cat("  input$setbuckets = ", input$setbuckets, "\n")
    cat("  input$preinterval1 = ", input$preinterval1, "\n")
    cat("  input$preinterval2 = ", input$preinterval2, "\n")
    
    ## Get data chosen.    
    tempdata.1 <- uploadeddata()[, c(input$preinterval1, input$preinterval2)]
    ## Rename data.
    names(tempdata.1) <- c("x", "s")
    ## Set n = 1, for each observation.
    tempdata.1$n <- 1
    cat("  bucketed : 2 \n")    
    ## Cut to chosen #of buckets    
    ## If chosen buckets = max, then do no such cut.
    ## Else, cut into smaller buckets, fairly coarse such.
    if(input$setbuckets == uniquevalues$nr)
    {
      ## Keep each unique value
      tempdata.1$b <- paste(tempdata.1$x)
    } else
    {
      ## Cut the interval into smaller/larger buckets
      tempdata.1$b <- paste(cut(tempdata.1$x, breaks = input$setbuckets))
    }
    cat("  bucketed : 3 \n")
    ## Calculate all key numbers used.  
    tempbucketed2 <- tempdata.1 %>%
      group_by(b) %>%
      summarise(s = sum(s), n = sum(n), xmax = max(x), xmin = min(x)) %>%  
      mutate(  m = s/n
             , ss = n - s 
             , bx = ifelse(xmax == xmin, paste0("[",xmin,"]"), paste0("[",xmin,"-", xmax, "]") ) 
             , spct = s/sum(s), npct = n/sum(n), sspct = ss/sum(ss)
             , cum_s = cumsum(s)/sum(s), cum_ss = cumsum(ss)/sum(ss)) %>%
      ungroup() %>%
      arrange(xmin)
    cat("  bucketed : 4 \n")
    ## Add bucket number, consecutive number.
    tempbucketed2$bn <- 1:nrow(tempbucketed2)
    ## Return data.
    cat("[END] bucketed()\n")
    return(tempbucketed2)
  })#reactive
  
  ## -----------------------------
  ## bucketed2 [data frame]
  ## - re-bucked dataset to use.
  ## eliminates some buckets so loop goes faster.
  ##-------------------------------  
  ## special dataset, designed to be plotted
  bucketed2 <- reactive({
    cat("_ bucketed2() _")
    
    temp <- bucketed()
    tempbucketed2 <- rbind(
      temp %>% mutate(group = 1, c = order_by(bn, cumsum(s)), cum = c / sum(s))   %>% select(bn, cum, group)
      ,
      temp %>% mutate(group = 0, c = order_by(bn, cumsum(ss)), cum = c / sum(ss)) %>% select(bn, cum, group) 
    )
    cat("[END] bucketed2()\n")
    return(tempbucketed2)
  })#reactive
  
  
  
  ##---------------------------------------
  ## intervaldata()
  ## Calculate intervals. Takes time.
  ##
  ## + Update page 4 sliders!
  ##---------------------------------------
  
  
  intervaldata_loop <- reactiveValues(counter = NULL)
 
  
  #kick the calculation to start it.
  output$text3 <- renderText({ 
    if (input$calculate == 0 ) return()
    isolate( paste0(class(intervaldata()))) 
  })#rendertext 
  
  
  ##--------------------------------
  ## intervaldata()
  ##--------------------------------
  intervaldata <-  reactive({
    input$calculate
    cat("_ intervaldata() _ \n")
    data.2 <- isolate( bucketed() )
    cat("  intervaldata : 2\n")
    df.1 <- data.frame(row = 1:(nrow(data.2)*(nrow(data.2)-1)/2), s = NA, n = NA, m = NA,
                       lowerlimit = NA, upperlimit = NA)
    i <- j <- tmpcnt <- 1
    cat("  intervaldata : 3\n")
    ## Show a progress bar for the calculation.
    withProgress(message = "Calculating... ", value = 0, expr = {      
      for(i in 1:(nrow(data.2)-1))
      {
        run_N <- data.2[i, "n"]
        run_s <- data.2[i, "s"]  
        for(j in (i+1):nrow(data.2))
        {
          run_N <- run_N + data.2[j, "n"]
          run_s <- run_s + data.2[j, "s"]
          # write each 
          df.1[tmpcnt, "s"] <- run_s
          df.1[tmpcnt, "n"] <- run_N
          df.1[tmpcnt, "m"] <- run_s / run_N
          df.1[tmpcnt, "lowerlimit"] <- data.2[i, "xmin"]
          df.1[tmpcnt, "upperlimit"] <- data.2[j, "xmax"]
          tmpcnt <- tmpcnt + 1
          cat(tmpcnt, " ")
          ## Update the progress bar.
          incProgress(amount = 1/(nrow(data.2)*(nrow(data.2)-1)*0.5)
                      , detail = paste("interval", tmpcnt, " of ", (nrow(data.2)*(nrow(data.2)-1)*0.5))
                      )
        }#for i
      }#for j
    })#withprogress
    
    cat("  intervaldata : 3\n")
    
    ## Update sliders on page 4
    ## Filter for: N, mean, zoom : xlimit + ylimit.
    cat("  updated sliders window 4 : 3\n")
    cat("[END]\n")
    return(df.1) # print the resulting data to output
  })##reactive

  
  
  
  ##------------------------------
  ## Window nr 4
  ## Create sliders based on the data.
  ##------------------------------
  

  ##
  ## UI Slider: [nr of obs] (create dynamic from data)
  ##
  output$ui_n_slider <- renderUI(expr = {
    cat("_ output$ui_n_slider _ \n")
    temp <- intervaldata()
    cat("[END] output$ui_slider \n")
    
    sliderInput(  inputId = "n_slider"
                , label = "Number of observations"
                , sep = ""
                , min = round(min(temp$n), 0)
                , max = round(max(temp$n), 0)
                , value = c(min(temp$n), max(temp$n))
                , step = signif((max(temp$n) - min(temp$n)) / nrow(temp), 0)
                , ticks = F)
    })#renderUI

  ##
  ## UI Slider: [targetrate%] (create dynamic from data)
  ##
  output$ui_m_slider <- renderUI(expr = {
      cat("_ output$ui_m_slider _ \n")
      temp <- intervaldata()
      sliderInput(  inputId = "m_slider"
                  , label = "Targetrate (%)"
                  , sep = ""
                  , min = round(min(temp$m), 3)
                  , max = round(max(temp$m), 3)
                  , value = c(round(min(temp$m), 3), round(max(temp$m), 3))
                  , step = 0.001
                  , ticks = F)
    })#renderUI


  ##
  ## UI Slider: [zoom x-limit] (create dynamic from data)
  ##
  output$ui_xlimit <- renderUI(expr = {
    cat("_ output$ui_xlimit _ \n")
    temp <- intervaldata()
    sliderInput(    inputId = "xlimit"
                  , label = "X-axis limits"
                  , sep = ""
                  , min = signif(min(temp$lowerlimit), 3)
                  , max = signif(max(temp$lowerlimit), 3)
                  , value = c(min(temp$lowerlimit), max(temp$lowerlimit))
                  , step = signif((max(temp$lowerlimit) - min(temp$lowerlimit)) / length(unique(temp$lowerlimit)), 4)
                  , ticks = F)
  })#renderUI  
  
  ##
  ## UI Slider: [zoom y-limit] (create dynamic from data)
  ##
  output$ui_ylimit <- renderUI(expr = {
    cat("_ output$ui_ylimit _ \n")
    temp <- intervaldata()
    sliderInput(    inputId = "ylimit"
                  , label = "Y-axis limits"
                  , sep = ""
                  , min = signif(min(temp$upperlimit), 4)
                  , max = signif(max(temp$upperlimit), 4)
                  , value = c(min(temp$upperlimit), max(temp$upperlimit))
                  , step = signif((max(temp$upperlimit) - min(temp$upperlimit)) / length(unique(temp$lowerlimit)), 4)
                  , ticks = F)
  })#renderUI  
      
  
  
  ##------------------------------
  ## Plots window 1
  ##------------------------------
  
  ## Basic data plots
  output$plot.1.histogram <- renderPlot({
    cat("_ output$plot.1.histogram _ \n")
    cat("  input$varnames = ", input$varnames, "\n")
    cat("  input$targetnames = ", input$targetname, "\n")
    
    ## If no chosen variable yet, show nothing.
    if(input$varnames == "Choose..." || input$targetname == "Choose...") 
    {
      cat("[END] output$plot.1.histogram\n")
      return(NULL)
    }
    ## Plot targetrate for interval variable
    cat("[END] output$plot.1.histogram\n")
    plot(density(uploadeddata()[, input$varnames] ), main = "", xlab = "", ylab = "", yaxt = "n")
    hist(uploadeddata()[, input$varnames], add = T, probability = T)
    polygon(density(uploadeddata()[, input$varnames] ), density = 10)
  })
  
  
  output$plot.1.distributions <- renderPlot({
    cat("_ output$plot.1.distributions _ \n")
    if(input$varnames == "Choose..." || input$targetname == "Choose...") 
    {
      cat("[END] output$plot.1.distributions\n")
      return(NULL)
    }
    plot(ecdf(uploadeddata()[ uploadeddata()[, input$targetname]==1 , input$varnames])
         , verticals = T, pch = NA, col = "blue"
         , las = 1, ylab = "", xlab = "", main = "")
    lines(ecdf(uploadeddata()[ uploadeddata()[, input$targetname]==0 , input$varnames])
          , verticals = T, pch = NA, col = "red")
    cat("[END] output$plot.1.distributions\n")
  })#renderplot
  
  
  
  
  
  ##------------------------------
  ## Plots window 2
  ##------------------------------    
  ## plot the histogram
  output$plot.2.top <- renderPlot({
    cat("_ output$plot.2.top _ \n")
    
    temp <- bucketed() %>% ggplot +
      aes(x =  bn, y =  m ) +
      geom_line(stat = "identity" ) +
      scale_x_discrete(name = waiver(), labels = NULL) +
      scale_y_continuous(name = waiver(), labels = percent) +
      theme(axis.text.y = element_text(size = 12))
    cat("[END] output$plot.2.top\n")
    return(temp)
  })#renderplot
  
  ## plot the histogram
  output$plot.2.bottom <- renderPlot({
    cat("_ output$plot.2.bottom _ \n")
    tempdata <- bucketed2()
    temp <- tempdata %>% ggplot +
      aes(x = bn, y = cum, colour = factor(group), group = group) +
      geom_line(stat="identity", size = 1) +
      scale_x_continuous(name = waiver(), breaks = waiver(), labels = NULL) +
      scale_y_continuous(name = waiver(), labels = percent, limits = c(0,1)) +
      theme(axis.text.y = element_text(size = 12))
    cat("[END] output$plot.2.bottom\n")
    return(temp)
  })#renderplot
  

  ## Inform user of how many calculations to do.
  output$nr_of_buckets_to_calculate <- renderText({
    paste0(input$setbuckets * (input$setbuckets - 1) / 2  ,  " intervals total.")
  })#renderText
  
  
  
  
  ##------------------------------
  ## Plots window 4
  ##------------------------------   

  
  #-------------------------------
  # Main plot of this program!!
  # Plot the image of intervals 
  #-------------------------------
  output$plot.4.top <- renderPlot({
    cat("_ output$plot.4.top _ \n")
    
    temp <- intervaldata() 
    temp %>% 
      filter(    n > input$n_slider[1], n < input$n_slider[2]
               , m > input$m_slider[1], m < input$m_slider[2]
      ) -> temp2
    ##cat(glimpse(temp2))
    cat("  output$plot.4.top : 2", "\n")
    
    
    #get upper and lower limit
    minll <- min(input$xlimit)
    maxll <- max(input$xlimit)
    minul <- min(input$ylimit)
    maxul <- max(input$ylimit)
    cat("  output$plot.4.top : 3", "\n")
    
    temp2 %>% ggplot +
      aes(x = lowerlimit, y = upperlimit
          , color = m) +
      coord_cartesian(xlim = c(  minll - 0.1 * range(minll, maxll)
                                 , maxll + 0.1 * range(minll, maxll) )
                      , ylim = c(  minul - 0.1 * range(minul, maxul)
                                   , maxul + 0.1 * range(minul, maxul) )
      ) +
      scale_color_gradient(low = "green", high = "red")+
      geom_point(size = input$pointsize_slider) -> tmpplot
    cat("  output$plot.4.top : 4", "\n")
    cat("[END] output$plot.4.top\n")
    return(tmpplot)
    
  })
  
  ##----------------------------------
  ## reactivevalues, click on plot 4 
  ## (workaround)
  ##----------------------------------
  tmpclickvalues <- reactiveValues(cl = NULL)
  
  ## Save Click on plot 4  to reactive values.
  observeEvent(eventExpr = input$plot.4.click, handlerExpr = {
    cat("_ observeEvent:input$plot.4.click _ ", "\n")      
    tmpclickvalues$cl <- input$plot.4.click
    cat("[END] observeEvent:input$plot.4.click \n")
  })
   
  
  ##------------------------
  ## Table for clicked point
  ##------------------------
  output$plot.4.table <- renderTable({
    cat("_ output$plot.4.table _ ", "\n")
    return(NULL)
    ## Find the points closest to click.
    ## Recreate the data first, so we don't show something filtered.
    temp <- isolate(intervaldata()) 
    temp %>% 
      filter(  n > isolate(input$n_slider[1]), n < isolate(input$n_slider[2])
               , m > isolate(input$m_slider[1]), m < isolate(input$m_slider[2])
               #                , lowerlimit > 0.1, lowerlimit < 0.3
               #                , upperlimit > 0.1, upperlimit < 0.25
      ) -> temp2
    cat("  output$plot.4.table : 2", "\n")
    ## Get the closest points to the click.
    tmppoints <- nearPoints(df = temp2
                            , coordinfo = tmpclickvalues$cl
                            , xvar = "lowerlimit"
                            , yvar = "upperlimit"
                            , threshold = 10
                            , addDist = T
    )
    cat("  output$plot.4.table : 3", "\n")
    
    ## Put data in readable form for output.
    tmppoints <- tmppoints %>% select(lowerlimit, upperlimit, n, m)
    cat("[END] output$plot.4.table \n")
    return(tmppoints)
  })
  
  ## ------------------------------------
  ## Zoom function through double click.
  ## ------------------------------------

  
  ## Reactive value trick for zoom-values
  zoom <- reactiveValues(xlim = NULL, ylim = NULL)
  
  observeEvent(input$plot.4.dblclick, {
    cat("_ observeEvent:input$plot.4.dblclick _", "\n")
    brush <- input$plot.4.brush
    if (!is.null(brush)) {
      cat("  observeEvent input$plot.4.dblclick : 2", "\n")
      zoom$xlim <- c(brush$xmin, brush$xmax)
      zoom$ylim <- c(brush$ymin, brush$ymax)
      cat("  observeEvent input$plot.4.dblclick : 3", "\n")      
      updateSliderInput(session = session
                        , inputId = "xlimit"
                        , value = c(round(min(zoom$xlim))
                                    , floor(max(zoom$xlim))
                                    )
                        )
                
      cat("  observeEvent input$plot.4.dblclick : 4", "\n")
      updateSliderInput(session = session
                        , inputId = "ylimit"
                        , value = c(round(min(zoom$ylim))
                                    , floor(max(zoom$ylim))
                        )
      )
    } else {
      zoom$xlim <- NULL
      zoom$ylim <- NULL
      cat("  observeEvent input$plot.4.dblclick : 5", "\n")
      
    }
    cat("  observeEvent input$plot.4.dblclick : 6", "\n")    
    cat("[END] observeEvent:input$plot.4.dblclick \n")
  })
  
  
}#server function end
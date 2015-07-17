library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

function(input, output, clientData, session) {
  
  ##---------------------------------------
  ## Fill in 1st window inputs.
  ##---------------------------------------
  
  observe({
    cat("observe")
    infile <- input$infile
    if( is.null(uploadeddata()) )
    {
      updateSelectInput(session, "varnames", choices = "Choose...")    
      updateSelectInput(session, "targetnames", choices = "Choose...")    
      
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
      
      
    }
  })#observe
  
  ##---------------------------------------
  ## Fill in 2nd window inputs.
  ##---------------------------------------
  observe({
    updateSelectInput(session, "preinterval1", choices = input$varnames)
    updateSelectInput(session, "preinterval2", choices = input$targetname)
    ##output$uniquevalues <- renderText( length(unique(uploadeddata()[, input$varnames])))
  })#observe
  
  
  ##---------------------------------------
  ## Uploaded data.
  ##---------------------------------------
  uploadeddata <- reactive({
    infile <- input$infile
    if(is.null(infile)) 
      return(NULL)
    
    read.csv(infile$datapath, header = T, sep = "\t") 
  })#reactive
  
  
  
  
  ##------------------------------
  ## Update data to bucket
  ##------------------------------
  
  
  ## -----------------------------
  ## middledata [data frame]
  ## - all unique values per predictor
  ##-------------------------------
  ## create middle data per unique value.
  ## not dplyr since problem with strings as input to dplyr.
  middledata <- reactive({
    cat("middledata reactive \n")
    
    tempmiddledata <- data.frame(
      s = as.vector(unlist(tapply(X = uploadeddata()[, input$preinterval2]
                                  , FUN = sum
                                  , INDEX = list(uploadeddata()[, input$preinterval1])
      )))
      ,
      n = as.vector(unlist(tapply(X = uploadeddata()[, input$preinterval2]
                                  , FUN = length
                                  , INDEX = list(uploadeddata()[, input$preinterval1])
      )))
      , 
      x = sort(as.numeric(unique(uploadeddata()[, input$preinterval1]))
      )
    )#data.frame
    cat("middledata reactive :1 \n")
    tempmiddledata$m <- tempmiddledata$s / tempmiddledata$n
    cat("middledata reactive :2 \n")
    updateSliderInput(session, "setbuckets"
                      , max = nrow(tempmiddledata)
                      , min = 2
                      , value = nrow(tempmiddledata)
    )#updatesliderinput
    cat("middledata reactive :3 \n")
    
    return(tempmiddledata)
  })#reactive  
  
  
  ## -----------------------------
  ## bucketed [data frame]
  ## - re-bucked dataset to use.
  ## eliminates some buckets so loop goes faster.
  ##-------------------------------
  ## create buckets according to breaks
  
  bucketed <- reactive({
    cat("bucketed reactive: 1 \n")
    cat("nr of breaks input:", input$setbuckets, "\n")
    
    tempbucketed <- middledata() 
    tempbucketed$b <- cut(tempbucketed$x, breaks = input$setbuckets) 
    cat("bucketed reactive: 2 \n")
    
    tempbucketed2 <- tempbucketed %>%
      group_by(b) %>%
      summarise(s = sum(s), n = sum(n), xmax = max(x), xmin = min(x)) %>%  
      mutate(m = s/n, ss = n - s, bn = as.numeric(b)
             , bx = ifelse(xmax == xmin, paste0("[",xmin,"]"), paste0("[",xmin,"-", xmax, "]") ) 
             , spct = s/sum(s), npct = n/sum(n), sspct = ss/sum(ss)
             , cum_s = cumsum(s)/sum(s), cum_ss = cumsum(ss)/sum(ss))
    
    cat("bucketed reactive: 3 \n")
    tempbucketed2$bn <- 1:nrow(tempbucketed2)
    return(tempbucketed2)
  })#reactive
  
  ## -----------------------------
  ## bucketed2 [data frame]
  ## - re-bucked dataset to use.
  ## eliminates some buckets so loop goes faster.
  ##-------------------------------  
  ## special dataset, designed to be plotted
  bucketed2 <- reactive({
    print("bucketed2 reactive")
    tempbucketed2 <- rbind(
      bucketed() %>% select(cum = cum_ss, bn, bx, pct = sspct) %>% mutate(group = 0)
      ,
      bucketed() %>% select(cum = cum_s, bn, bx, pct = spct) %>% mutate(group = 1)
    )
  })#reactive
  
  
  
  ##---------------------------------------
  ## intervaldata()
  ## Calculate intervals. Takes time.
  ##
  ## + Update page 4 sliders!
  ##---------------------------------------
  
  
  intervaldata_loop <- reactiveValues(counter = NULL)

  
#   output$text31 <- renderText({
#     invalidateLater(millis = 100, session = session)
#     intervaldata_loop$counter
#   })#renderText
  
  
  #kick the calculation to start it.
  output$text3 <- renderText({ 
    if (input$calculate == 0 ) return()
    isolate( paste0(class(intervaldata()))) 
  })#rendertext 
  
  intervaldata <-  reactive({
    input$calculate
    cat("intervaldata : 1\n")
    data.2 <- isolate( bucketed() )
    cat("intervaldata : 2\n")
    df.1 <- data.frame(row = 1:(nrow(data.2)*(nrow(data.2)-1)/2), s = NA, N = NA, m = NA,
                       lowerlimit = NA, upperlimit = NA)
    i <- j <- tmpcnt <- 1
    cat("intervaldata : 3\n")
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
          #cat(tmpcnt, " ")
          ## Update the progress bar.
          incProgress(amount = 1/(nrow(data.2)*(nrow(data.2)-1)*0.5)
                      , detail = paste("interval", tmpcnt, " of ", (nrow(data.2)*(nrow(data.2)-1)*0.5))
                      )
        }#for i
      }#for j
    })#withprogress
    
    cat("intervaldata : 3\n")
    
    ## Update sliders on page 4
    updateSliderInput(session = session, inputId = "n_slider", min = min(df.1$n), max = max(df.1$n), value = c(min(df.1$n), max(df.1$n)))
    updateSliderInput(session = session, inputId = "m_slider"
                      , min = floor(min(df.1$m) * 1000) / 1000
                      , max = floor(max(df.1$m) * 1000) / 1000 
                      , value = c(min(df.1$m), max(df.1$m)))
    updateSliderInput(session = session, inputId = "xlimit", min = min(df.1$lowerlimit), max = max(df.1$lowerlimit), value = c(min(df.1$lowerlimit), max(df.1$lowerlimit)))
    updateSliderInput(session = session, inputId = "ylimit", min = min(df.1$upperlimit), max = max(df.1$upperlimit), value = c(min(df.1$upperlimit), max(df.1$upperlimit)))
    
    return(df.1) # print the resulting data to output
  })##reactive
  
  ##------------------------------
  ## Plots window 1
  ##------------------------------
  
  
  ## temporary plot to find problems
  #   phony <- reactive({
  #     return(data.frame(x = runif(n = 10)*20
  #                       , y = runif(n=10)*50
  #                       , z = factor(sample(x = 1:2, replace = T, size = 10))
  #                       )
  #            )
  #   })
  #   output$plot.1.test <- renderPlot({
  #     #mtcars %>% ggplot + aes(x = wt, y = mpg) + geom_point() WORKS FINE
  #     #phony() %>% ggplot + aes(x = x, y = y) + geom_point()    WORKS FINE
  # #     phony() %>% ggplot +
  # #       aes(x = x, y = y)+
  # #       geom_point(size = 3) -> tmpplot NOT WORKING!
  # #    plot(tmpplot)
  # 
  #         phony() %>% ggplot +
  #           aes(x = x, y = y)+
  #           geom_point() -> tmpplot 
  #        return(tmpplot)
  #   })
  #   # see plot
  #   output$plot.1.text <- renderPrint(str(input$plot.1.click))
  # 
  # 
  #   output$plot.1.table <- renderTable({
  #     ##return(nearPoints(phony(), coordinfo = input$plot.1.click, xvar = "x", yvar = "y")) WORKS w/o refresh
  #     
  #     
  #     tmppoints <- nearPoints(df = phony()
  #                             , coordinfo = input$plot.1.click
  #                             , xvar = "x"
  #                             , yvar = "y"
  #                             , threshold = 100
  #                             , addDist = T
  #     )
  #     tmppoints
  #   })
  
  ## end of temporary plots etc.
  output$plot.1.histogram <- renderPlot({
    cat("output$plot.1.histogram", "\n")
    cat("input$varnames :", input$varnames, "\n")
    cat("input$targetnames :", input$targetname, "\n")
    
    if(input$varnames == "Choose..." || input$targetname == "Choose...") 
      return(NULL)
    
    ## plot the histogram for the interval variable
    hist(uploadeddata()[, input$varnames ], freq = F, las = 1, main = "", ylab = "", xlab = "")
    polygon(density(uploadeddata()[, input$varnames ]), density = 10)
    
  })
  
  
  output$plot.1.distributions <- renderPlot({
    cat("output$plot.1.distributions", "\n")
    if(input$varnames == "Choose..." || input$targetname == "Choose...") 
      return(NULL)
    plot(ecdf(uploadeddata()[ uploadeddata()[, input$targetname]==1 , input$varnames])
         , verticals = T, pch = NA, col = "blue"
         , las = 1, ylab = "", xlab = "", main = "")
    lines(ecdf(uploadeddata()[ uploadeddata()[, input$targetname]==0 , input$varnames])
          , verticals = T, pch = NA, col = "red")
  })#renderplot
  
  
  
  
  
  ##------------------------------
  ## Plots window 2
  ##------------------------------    
  ## plot the histogram
  output$plot.2.top <- renderPlot({
    cat("output$plot.2.top", "\n")
    
    temp <- bucketed() %>% ggplot +
      aes(x =  bn, y =  m ) +
      geom_line(stat = "identity" ) +
      scale_x_discrete(name = waiver(), labels = bucketed()$bx) +
      scale_y_continuous(name = waiver(), labels = percent)
    
    return(temp)
  })#renderplot
  
  ## plot the histogram
  output$plot.2.bottom <- renderPlot({
    cat("output$plot.2.bottom", "\n")
    
    temp <- bucketed2() %>% ggplot +
      aes(x = bn, y = cum, colour = factor(group), group = group) +
      geom_line(stat="identity", size = 1) +
      scale_x_continuous(name = waiver(), breaks = bucketed2()$bn, labels = bucketed2()$bx) +
      scale_y_continuous(name = waiver(), labels = percent, limits = c(0,1)) 
    #      scale_fill_brewer(palette = "Set1") +
    #      theme(legend.title=element_blank()) 
    
    return(temp)
  })#renderplot
  

  ## Inform user of how many calculations to do.
  output$nr_of_buckets_to_calculate <- renderText({
    paste0(input$setbuckets * (input$setbuckets - 1) / 2  ,  " intervals possible.")
  })#renderText
  
  
  
  
  ##------------------------------
  ## Plots window 4
  ##------------------------------   

  
  
  # Main plot of this program!!
  # Plot the image of intervals 
  output$plot.4.top <- renderPlot({
    cat("output$plot.4.top : 1", "\n")
    
    temp <- intervaldata() 
    temp %>% 
      filter(  n > input$n_slider[1], n < input$n_slider[2]
               , m > input$m_slider[1], m < input$m_slider[2]
               #                , lowerlimit > 0.1, lowerlimit < 0.3
               #                , upperlimit > 0.1, upperlimit < 0.25
      ) -> temp2
    ##cat(glimpse(temp2))
    cat("output$plot.4.top : 2", "\n")
    
    
    #get upper and lower limit
    minll <- min(input$xlimit)
    maxll <- max(input$xlimit)
    minul <- min(input$ylimit)
    maxul <- max(input$ylimit)
    cat("output$plot.4.top : 3", "\n")
    
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
    cat("output$plot.4.top : 4", "\n")
    
    return(tmpplot)
    
  })
  
  
  ## Save click on plot 4, to reactivevalues
  ## (workaround)
  tmpclickvalues <- reactiveValues(cl = NULL)
  
  ## observe the event, click. Save it to reactive values.
  observeEvent(eventExpr = input$plot.4.click, handlerExpr = {
    cat("observeEvent  input$plot.4.click : 1", "\n")      
    tmpclickvalues$cl <- input$plot.4.click
  })
  
  ## Debug printout for clicks!
#   output$plot.4.text <- renderPrint({
#     cat("output$plot.4.text", "\n")
#     str(input$plot.4.brush)
#   })
  
  
  ## Table clicked point
  output$plot.4.table <- renderTable({
    cat("output$plot.4.table : 1", "\n")
    ## Find the points closest to click.
    ## Recreate the data first, so we don't show something filtered.
    temp <- isolate(intervaldata()) 
    temp %>% 
      filter(  n > isolate(input$n_slider[1]), n < isolate(input$n_slider[2])
               , m > isolate(input$m_slider[1]), m < isolate(input$m_slider[2])
               #                , lowerlimit > 0.1, lowerlimit < 0.3
               #                , upperlimit > 0.1, upperlimit < 0.25
      ) -> temp2
    cat("output$plot.4.table : 2", "\n")
    ## Get the closest points to the click.
    tmppoints <- nearPoints(df = temp2
                            , coordinfo = tmpclickvalues$cl
                            , xvar = "lowerlimit"
                            , yvar = "upperlimit"
                            , threshold = 10
                            , addDist = T
    )
    cat("output$plot.4.table : 3", "\n")
    
    ## Put data in readable form for output.
    tmppoints <- tmppoints %>% select(lowerlimit, upperlimit, n, m)
    return(tmppoints)
  })
  
  
  ## Zoom function through double click.
  
  
  ## Reactive value trick for zoom-values
  zoom <- reactiveValues(xlim = NULL, ylim = NULL)
  
  observeEvent(input$plot.4.dblclick, {
    cat("observeEvent input$plot.4.dblclick : 1", "\n")
    brush <- input$plot.4.brush
    if (!is.null(brush)) {
      cat("observeEvent input$plot.4.dblclick : 2", "\n")
      zoom$xlim <- c(brush$xmin, brush$xmax)
      zoom$ylim <- c(brush$ymin, brush$ymax)
      cat("observeEvent input$plot.4.dblclick : 3", "\n")      
      updateSliderInput(session = session
                        , inputId = "xlimit"
                        , min = min(intervaldata()$lowerlimit)
                        , max = max(intervaldata()$lowerlimit)
                        , value = c(round(min(zoom$xlim))
                                    , floor(max(zoom$xlim))
                                    )
                        )
                
      cat("observeEvent input$plot.4.dblclick : 4", "\n")
      updateSliderInput(session = session
                        , inputId = "ylimit"
                        , min = min(intervaldata()$upperlimit)
                        , max = max(intervaldata()$upperlimit)
                        , value = c(round(min(zoom$ylim))
                                    , floor(max(zoom$ylim))
                        )
      )
    } else {
      zoom$xlim <- NULL
      zoom$ylim <- NULL
      cat("observeEvent input$plot.4.dblclick : 5", "\n")
      
    }
    cat("observeEvent input$plot.4.dblclick : 6", "\n")    
    
  })
  
  
}#function end
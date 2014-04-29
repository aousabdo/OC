library(shiny)
library(ggplot2)
library(plyr)
library(reshape2)
library(grid)

source("./MaxFail.R")
source("./MaxX.R")

shinyServer(function(input, output){
  # Reactive data frame to store data
  data <- reactive({
    tolerance <- 0.005
    TRV   <- input$TRV
    PA    <- input$PA
    CL    <- input$CL
    addtest2 <- input$addtest2
    addtest3 <- input$addtest3
    
    test1 <- input$test1 # length of test 1
    AF1   <- MaxFailFun(TL=test1, RR = TRV, CL = CL) 
    test2 = test3 = AF2 = AF3 <- NA # initialize second and third test parameters to NA. 
    # These will be modified later if user selects to plot tests see below
    
    # Change test 2 pars to selected 
    if(addtest2){
      test2 <- input$test2
      AF2   <- MaxFailFun(TL=test2, RR = TRV, CL = CL) 
      if(addtest3){
        test3 <- input$test3
        AF3   <- MaxFailFun(TL=test3, RR = TRV, CL = CL)        
      }
    }

    if(input$adjustMTBF){
      MTBF <- seq(input$mtbfmin, input$mtbfmax, 0.1)
    }
    else{
      MTBFMax <- MaxX(AF=AF1, test=test1, tolerance=tolerance, MaxValue=1.0)
      if(addtest2){
        if(MaxX(AF=AF2, test=test2, tolerance=tolerance, MaxValue=1.0) > MTBFMax){
          MTBFMax <- MaxX(AF=AF2, test=test2, tolerance=tolerance, MaxValue=1.0)
        }
        if(addtest3){
          if(MaxX(AF=AF3, test=test3, tolerance=tolerance, MaxValue=1.0) > MTBFMax){
            MTBFMax <- MaxX(AF=AF3, test=test3, tolerance=tolerance, MaxValue=1.0)
          }
        }
      }
      MTBF <- seq(input$mtbfmin, MTBFMax, 0.1)
    }
    
    TL    <- c(test1, test2, test3)# Vector to store tese lengths
    AF    <- c(AF1, AF2, AF3)# Vector to store acceptable number of failures for all tests
    
    Prob  <- matrix(nrow=length(MTBF), ncol=length(TL)) 
    RR    <- input$TRV
    
    # Calculate probabilities
    Prob1 <- ppois(AF1, test1/MTBF)
  
    # Store in a dataframe
    df       <- data.frame(MTBF=MTBF, Test1=Prob1)
    df2      <- mutate(df, Prob1diff=abs(Prob1-PA))
    vert1    <- df2[df2$Prob1diff == min(df2$Prob1diff),]$MTBF
    df$vert1 <- vert1
    
    # do the same for test 2 and 3 if selected by user
    df$vert2 <- NA
    df$vert3 <- NA
    if(addtest2){
      Prob2    <- ppois(AF2, test2/MTBF)
      df$Test2 <- Prob2
      df2      <- mutate(df, Prob2diff=abs(Prob2-PA))
      vert2    <- df2[df2$Prob2diff == min(df2$Prob2diff),]$MTBF
      df$vert2 <- vert2
      if(addtest3 ){
        Prob3    <- ppois(AF3, test3/MTBF)
        df$Test3 <- Prob3
        df2      <- mutate(df, Prob3diff=abs(Prob3-PA))
        vert3    <- df2[df2$Prob3diff == min(df2$Prob3diff),]$MTBF
        df$vert3 <- vert3
      }
    }
    
    # Melt data frame one variable at a time
    df <- melt(df, id=c("MTBF", "vert1", "vert2", "vert3"), variable.name = "Test", 
               value.name = "Prob")
    df <- melt(df, id=c("MTBF", "Test", "Prob"), variable.name="Vert", value.name="vert", 
               na.rm=TRUE)
    # Round probability to 5 sig figures
    df$Prob <- round(df$Prob, 5)
    
    # Build a list and store all vectors and the dataframe in it
    List <- list(df=df, TL=TL, AF=AF)
    return(List)
  })
  
  # Make plot
  output$OCplot<- renderPlot({makePlot()})
  
  makePlot <- function(){
    # Read entries from data
    df <- data()$df
    TL <- data()$TL
    AF <- data()$AF
    
    # Function to do the dcast
    dcastfun <- "MTBF + Test1 ~ Vert"
    # Add Test 2 and Test 3 to the function if corresponding tests are selected
    if(input$addtest2){
      dcastfun <- "MTBF + Test1 + Test2 ~ Vert"
      if(input$addtest3){dcastfun <- "MTBF + Test1 + Test2 + Test3 ~ Vert"}
    } 
    
    # Dcast one variable at a time
    df <- dcast(df, MTBF + Vert + vert ~ Test , value.var="Prob")
    df <- dcast(df, dcastfun, value.var="vert")
    
    vert1 <- df$vert1
    dfv <- data.frame(line="vert1", value=vert1, stringsAsFactors=FALSE)
    
    if(input$addtest2){
      vert2 <- df$vert2
      if(input$addtest3){
        vert3 <- df$vert3
      }
    }
    
    df$vert4 = input$TRV
    df$horiz = input$PA
    
    # Using R base plot fuction since ggplot is driving me CRAZY    
    
    layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
    if(input$under){
      axessize  <- input$axessize
      linewidth <- input$linewidth
      legtext   <- input$legtext 
    }
    else{
      axessize  <- 1.5
      linewidth <- 2
      legtext   <- 1.2
    }
    
    par(mar=c(5, 6.5, 4, 2) + 0.1) 
    if(input$adjustMTBF){
      xmin <- input$mtbfmin
      xmax <- input$mtbfmax
    }
    else{
      xmin <- df$MTBF[1]
      xmax <- df$MTBF[length(df$MTBF)]
    }
    ## the xaxs="i", yaxs="i" options enforce the axes limits
    plot(df$MTBF, df$Test1, 'l', col="blue", lwd=linewidth,
         xlim=c(xmin, xmax), ylim=c(0,1), 
         xlab=NA, ylab=NA, cex.axis=axessize,
         xaxs="i")
    grid(lwd=2)
    
    par(new=TRUE)
    plot(df$MTBF, df$Test1, 'l', col="blue", lwd=linewidth,
         xlim=c(xmin, xmax), ylim=c(0,1), 
         xlab=NA, ylab=NA, cex.axis=axessize,
         xaxs="i")    
    
    if(input$under){
      Title     <- input$title
      xlab      <- input$xtitle
      ylab      <- input$ytitle
      title(Title, cex.main=input$titlesize,
            xlab=xlab, ylab=ylab, 
            cex.lab=input$axeslabels)
      test1name <- input$test1name
      if(input$addtest2){
        test2name <- input$test2name
        if(input$addtest3){
          test3name <- input$test3name
        }
      }
    }
    else{
      title(main="Operating Characteristic Curves",
            xlab="True MTBF (Hours)", 
            ylab="Probability of Acceptance", 
            cex.lab=1.6, cex.main=2
      )
      test1name="Test 1"
      if(input$addtest2){
        test2name <- "Test 2"
        if(input$addtest3){
          test3name <- "Test 3"
        }
      }
    }
    
    ## add lines for the other tests
    if(input$addtest2){
      lines(df$MTBF, df$Test2, col="red", lwd=linewidth)
      if(input$addtest3){
        lines(df$MTBF, df$Test3, col="darkgreen", lwd=linewidth)
      }
    }
    
    if(!input$toglines){
      # Add vertical lines for tests goals
      abline(v=vert1, col="blue", lty=2, lwd=linewidth)
      if(input$addtest2){
        abline(v=vert2, col="red", lty=2, lwd=linewidth)
        if(input$addtest3){
          abline(v=vert3, col="darkgreen", lty=2, lwd=linewidth)
        }
      }
      
      # Add vertical TRV and horizontal PA lines
      abline(input$PA, 0, col="black", lwd=linewidth)
      abline(v=input$TRV, col="black", lwd=linewidth, lty=2)
    }
    
    # Legend entries
    if(!input$toglines){
      leg1 <- sprintf("Probability of Acceptance Level")
      leg2 <- sprintf("%s length = %.0f hours, %.0f failures permitted", test1name, TL[1], AF[1])
      leg5 <- sprintf("Threshold Requirement")
      leg6 <- sprintf("%s Pre IOT&E Reliability Goal: %.2f Hours",test1name, vert1[1])
      
      # Customize legend
      nCol <- 2 
      legend_entries <- c(leg1, leg2, leg5, leg6)
      legend_colors  <- c("black", "blue", "black", "blue")
      legend_lty     <- c(1,1,2,2)
      if(input$addtest2){
        leg3 <- sprintf("%s length = %.0f hours, %.0f failures permitted",test2name, TL[2], AF[2])
        leg7 <- sprintf("%s Pre IOT&E Reliability Goal: %.2f Hours",test2name, vert2[1])
        legend_entries[3] <- leg3
        legend_entries[4] <- leg5
        legend_entries[5] <- leg6
        legend_entries[6] <- leg7
        legend_colors[3]  <- "red"
        legend_colors[4]  <- "black"
        legend_colors[5]  <- "blue"
        legend_colors[6]  <- "red"
        legend_lty[3]     <-  1
        legend_lty[4:6]     <-  2
        if(input$addtest3){
          leg4 <- sprintf("%s length = %.0f hours, %.0f failures permitted",test3name, TL[3], AF[3])
          leg8 <- sprintf("%s Pre IOT&E Reliability Goal: %.2f Hours", test3name, vert3[1])
          legend_entries[4] <- leg4
          legend_entries[5] <- leg5
          legend_entries[6] <- leg6
          legend_entries[7] <- leg7
          legend_entries[8] <- leg8
          legend_colors[4]  <- "darkgreen"
          legend_colors[5]  <- "black"
          legend_colors[6]  <- "blue"
          legend_colors[7]  <- "red"
          legend_colors[8]  <- "darkgreen"
          legend_lty[4]     <-  1
          legend_lty[5:8]   <-  2
        }
      }
    }
    else {
      leg1 <- sprintf("%s length = %.0f hours, %.0f failures permitted", test1name, TL[1], AF[1])
      # Customize legend
      nCol <- 1
      legend_entries <- c(leg1)
      legend_colors  <- c("blue")
      legend_lty     <- c(1)
      if(input$addtest2){
        leg2 <- sprintf("%s length = %.0f hours, %.0f failures permitted",test2name, TL[2], AF[2])
        legend_entries[2] <- leg2
        legend_colors[2]  <- "red"
        legend_lty[2]     <- 1
        if(input$addtest3){
          leg3 <- sprintf("%s length = %.0f hours, %.0f failures permitted",test3name, TL[3], AF[3])
          legend_entries[3] <- leg3
          legend_colors[3]  <- "black"
          legend_lty[3]     <- 1
        }
      }
    }
    
    # setup for no margins on the legend
    par(mar=c(0, 0, 0, 0))
    plot.new()
    legend( "center", legend = legend_entries, 
            lty=legend_lty, lwd=rep(linewidth, length(legend_lty)), 
            col=legend_colors, ncol=nCol, bty="y", cex=legtext
    )
  }
  
  output$savePlot <- downloadHandler(
    filename = function() { paste('OC_Curves.pdf') },
    content = function(file){
      pdf(file = file, width=11, height=8.5)
      makePlot()
      dev.off()
    }
  )
  
  output$savePNGPlot <- downloadHandler(
    filename = function() { paste('OC_Curves.png') },
    content = function(file){
      png(file = file, width=1200, height=800, units="px")
      makePlot()
      dev.off()
    }
  )
  origdataframe <- reactive({
    df <- data()$df
    # Function to do the dcast
    dcastfun <- "MTBF + Test1 ~ Vert"
    # Add Test 2 and Test 3 to the function if corresponding tests are selected
    if(input$addtest2){
      dcastfun <- "MTBF + Test1 + Test2 ~ Vert"
      if(input$addtest3){dcastfun <- "MTBF + Test1 + Test2 + Test3 ~ Vert"}
    } 
    
    # Dcast one variable at a time
    df <- dcast(df, MTBF + Vert + vert ~ Test , value.var="Prob")
    df <- dcast(df, dcastfun, value.var="vert")
    
    df$vert1 <- NULL
    
    if(input$under){
      test1name <- input$test1name
      if(input$addtest2){
        test2name <- input$test2name
        if(input$addtest3){
          test3name <- input$test3name
        }
      }
    }
    else{
      test1name <- "Test 1"
      if(input$addtest2){
        test2name <- "Test 2"
        if(input$addtest3){
          test3name <- "Test 3"
        }
      }
    }
    
    #Rename column names
    names(df)[1] <- "MTBF (Hours)"
    names(df)[2] <- sprintf("%s Prob.",test1name)
    
    #Rename the optional columns only if they exist
    if(length(colnames(df)>2)){
      if ("Test2" %in% colnames(df)){
        colnames(df)[colnames(df)=="Test2"] <- sprintf("%s Prob.",test2name)
        df$vert2 <- NULL
      }
      if ("Test3" %in% colnames(df)){
        colnames(df)[colnames(df)=="Test3"] <- sprintf("%s Prob.",test3name)
        df$vert3 <- NULL
      }
    }
    print(df, row.names=FALSE)
  })
  
  output$Text1 <- renderText({paste("For a reliability requriment of ", input$TRV, 
                                    " hours, a probability of acceptance of ", input$PA, ", and 
                                    a confidence level of ", 100*input$CL, "%, we have:" ,sep="")})
  
  output$table <- renderTable({
    df <- data()$df
    TL <- data()$TL
    AF <- data()$AF
    # Function to do the dcast
    dcastfun <- "MTBF + Test1 ~ Vert"
    # Add Test 2 and Test 3 to the function if corresponding tests are selected
    if(input$addtest2){
      dcastfun <- "MTBF + Test1 + Test2 ~ Vert"
      if(input$addtest3){dcastfun <- "MTBF + Test1 + Test2 + Test3 ~ Vert"}
    } 
    
    # Dcast one variable at a time
    df <- dcast(df, MTBF + Vert + vert ~ Test , value.var="Prob")
    df <- dcast(df, dcastfun, value.var="vert")

    if(input$under){
      test1name <- input$test1name
      if(input$addtest2){
        test2name <- input$test2name
        if(input$addtest3){
          test3name <- input$test3name
        }
      }
    }
    else{
      test1name <- "Test 1"
      if(input$addtest2){
        test2name <- "Test 2"
        if(input$addtest3){
          test3name <- "Test 3"
        }
      }
    }  
    tests <- c(test1name)
    testl <- c(TL[1])
    testr <- c(AF[1])
    testv <- c(df$vert1[1])
    
    if(length(colnames(df)>2)){
      if ("Test2" %in% colnames(df)){
        tests[2] <- test2name
        testl[2] <- TL[2]
        testr[2] <- AF[2]
        testv[2] <- df$vert2[1]
      }
      if ("Test3" %in% colnames(df)){
        tests[3] <- test3name
        testl[3] <- TL[3]
        testr[3] <- AF[3]
        testv[3] <- df$vert3[1]
      }
    }
    #print(testv)
    df <- data.frame("Test"=tests, "Test Length (hours)"=testl, 
                     "Allowed Failures"=testr, "Pre IOT&E Reliability Goal (hours)"
                     =testv, check.names=FALSE)
    return(df)
  })
  
  output$MTBFtable <- renderTable({
    origdataframe()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('OC_Curves.csv', sep='') },
    content = function(file) {
      write.csv(origdataframe(), file)
    }
  )
})
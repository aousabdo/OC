library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Operating Characteristic Curves"),
  sidebarPanel(
    wellPanel(      
      numericInput("TRV", "Threshold Requirement (Hours):", value=25, min=1, max=1e2),
      br(),
      numericInput("CL", "Confidence Level", value=0.8, min=0.1, max=0.99, step=0.01),
      br(),
      numericInput("PA", "Probability of Acceptance Level", value=0.8, min=0.1, max=1.0, step=0.01)
    ),
    wellPanel(
      checkboxInput(inputId="adjustMTBF", "Adjust X-axis Range (True MTBF Range)", value=FALSE),
      conditionalPanel(condition="input.adjustMTBF",
                       HTML("Select True MTBF Range in hours:"),
                       numericInput("mtbfmin", "Minimum MTBF", min=1, max=1e6, value=1),
                       numericInput("mtbfmax", "Maximum MTBF", min=0, max=1e6, value=70))
      ),
    wellPanel(
      numericInput("test1", "Length of First Test in hours\n", value=500)#,
#       checkboxInput("addAF1", "Adjust acceptable number of failures for First Test", value=FALSE),
#       conditionalPanel(condition="input.addAF1", 
#                        numericInput("AF1", "Acceptable Number of Failures for First Test", value=12))
    ),
    
    wellPanel(
      checkboxInput(inputId="addtest2", label="Enter Values for Second Test",TRUE),
      
      conditionalPanel(condition = "input.addtest2 == true",
                       numericInput("test2", "Length of Second Test in hours", value=750)#,
#                        checkboxInput("addAF2", "Adjust acceptable number of failures for Second Test", value=FALSE),
#                        conditionalPanel(condition="input.addAF2", 
#                                         numericInput("AF2", "Acceptable Number of Failures for Second Test", value=22))
      )
    ),
    

    conditionalPanel(condition = ("input.addtest2 == true"),
                     wellPanel(
                       checkboxInput(inputId="addtest3", label="Enter Values for Third Test",TRUE),
                       conditionalPanel("input.addtest3 == true",
                                        numericInput("test3", "Length of Third Test in hours", value=1000)#,
#                                         checkboxInput("addAF3", "Adjust acceptable number of failures for Third Test", value=FALSE),
#                                         conditionalPanel(condition="input.addAF3", 
#                                                          numericInput("AF3", "Acceptable Number of Failures for Third Test", value=40))
                       )
                     )
    ),

#     conditionalPanel(condition = "input.addtest2 == true",
#                      checkboxInput(inputId="addtest3", label="Enter Values for Third Test",TRUE)
#     ),
#     
#     conditionalPanel(condition = ("input.addtest2 == true & input.addtest3 == true"),
#                      wellPanel(
#                        numericInput("test3", "Length of Third Test in hours", value=1000),
#                        checkboxInput("addAF3", "Adjust acceptable number of failures for Third Test", value=FALSE),
#                        conditionalPanel(condition="input.addAF3", 
#                                         numericInput("AF3", "Acceptable Number of Failures for Third Test", value=40))
#                      )
#     ),
#     
    
    wellPanel(
      checkboxInput(inputId="under", label="Modify Plot Attributes",FALSE),
      conditionalPanel(condition="input.under == true", 
                       wellPanel(
                       textInput("title", "Plot Title", value="Enter Title Here"), 
                       textInput("xtitle", "Plot Title", value="Enter X-axis Title Here"), 
                       textInput("ytitle", "Plot Title", value="Enter Y-axis Title Here")),
                       wellPanel(
                       textInput("test1name", "Name for first test:", value="Test 1"),
                       conditionalPanel(condition = "input.addtest2==true",
                                        textInput("test2name", "Name for second test:", value="Test 2"),
                                        conditionalPanel(condition = "input.addtest3==true",
                                                         textInput("test3name", "Name for third test:", value="Test 3")))),
                       
                       wellPanel(
                       sliderInput("titlesize", "Text Size for Plot Title:",
                                   min=1, max=3, value=2, step=0.1),
                       br(),
                       sliderInput("axeslabels", "Text Size for Axes Labels:",
                                   min=1, max=2, value=1.2, step=0.1),
                       br(),
                       sliderInput("axessize", "Size for Axes Marks:",
                                   min=1, max=2, value=1.2, step=0.1),
                       br(),
                       sliderInput("linewidth", "Thickness of Lines",
                                   min=1, max=3, value=2.2, step=0.1),
                       br(),
                       sliderInput("legtext", "Legend Text Size",
                                   min=0.5, max=1.6, value=1.1, step=0.1)
      ))
    ),
    
    wellPanel(
      downloadButton('downloadData', 'Download Data Sample'),
      br(),
      br(),
      downloadButton("savePlot", "Download PDF Graphic"),
      downloadButton("savePNGPlot", "Download PNG Graphic"))
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", 
               HTML('<div class=\"span8\">
                                   <div id=\"OCplot\" class=\"shiny-plot-output\" style=\"position:fixed ; width: 60% ; height: 80%\">
                                                </div>                            
                    </div>')
      ),
      tabPanel("Results Summary",
               br(),
               br(),
               div(textOutput("Text1"), style = "font-family: 'times'; font-size:16pt; color:black"),
               br(),
               br(),
               tableOutput("table"),
               br(),
               br(),
               div(h4("More resources:")),
                   HTML('<ol>
<li><a href=\"http://spark.rstudio.com/statstudio/MTBF/\" target=\"_blank\">MTBF calculator with confidence intervals</a> </li>
<li><a href=\"http://spark.rstudio.com/statstudio/MTBFTestTime/\" target=\"_blank\">MTBF test time calculator</a> </li>
                        </ol>')
      ),
      tabPanel("About"),
      tabPanel("Data Table", tableOutput("MTBFtable"))
    ))
))
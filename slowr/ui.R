# Define UI 
shinyUI(pageWithSidebar(
          headerPanel(
            ## Application title
            div(br(),"Test App"),"Reactivity"),
          sidebarPanel(
            ## conditionalPanel("$('li.active a').html()==='Data Exploration'",
            ##                  htmlOutput('mtime')
            ##   ),
            htmlOutput("pulldown01"),
            htmlOutput("pulldown02"),
            div("Test"),
            htmlOutput("multimenu")
            ),
          mainPanel(
            tabsetPanel(
              tabPanel("Main Tab",
                       tableOutput("log")
                       ),
              tabPanel("Debug",
                       verbatimTextOutput("debug")
                       )
              )
            )
          ))


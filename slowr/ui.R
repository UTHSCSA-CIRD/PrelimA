# Define UI 
shinyUI(pageWithSidebar(
          headerPanel(
            ## Application title
            div(br(),"Test App"),"Reactivity"),
          sidebarPanel(
            selectInput('pulldown01','Pulldown 01',' '),
            selectInput('pulldown02','Pulldown 02',' '),
            selectInput('pulldown03','Pulldown 03',' '),
            selectInput('pulldown03','Pulldown 04',' ')
            ## conditionalPanel("$('li.active a').html()==='Data Exploration'",
            ##                  htmlOutput('mtime')
            ##   ),
            ## htmlOutput("pulldown01"),
            ## htmlOutput("pulldown02"),
            ## div("Test"),
            ## htmlOutput('pulldown03')
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


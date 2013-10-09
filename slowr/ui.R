shinyUI(pageWithSidebar(
          headerPanel(
            div(br(),"Test App"),"Reactivity"),
          sidebarPanel(
            selectInput('pulldown01','Pulldown 01',' '),
            selectInput('pulldown02','Pulldown 02',' '),
            selectInput('pulldown03','Pulldown 03',' '),
            selectInput('pulldown03','Pulldown 04',' ')
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
          ));

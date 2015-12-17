## library(lattice);

modalHelp <- function(id,helptext=""){
  div(id=id,class="modal fade", #`aria-hidden`="true",
      div(class="modal-header"),
      div(class="modal-body",HTML(helptext)),
      div(class='modal-footer',a(class="close badge badge-inverse",`data-dismiss`="modal","OK")));
}

source('help.R');
print("ui about to load");
# Define UI 
shinyUI(pageWithSidebar(
          headerPanel(
          ##   ## Application title
            div("Survomatic WebApp"),"Welcome to Survomatic"),
          sidebarPanel(
            ## htmlOutput("bdataread"),
            tags$span(""),
            conditionalPanel("$('li.active a').text()==='Plots'",
                             htmlOutput("sliders")
                             ),
            conditionalPanel("$('li.active a').html()==='Data Upload'",
                             htmlOutput("udataread"),
                             htmlOutput("myvar"), htmlOutput("mcvar"),htmlOutput("mxvars"),
                             htmlOutput("bmodel"),
                             tags$hr(),
                             span('Choose a CSV file with a numeric column, a (0,1) indicator column, and a categoric (factor) column.'),
                             p(),
                             checkboxInput('header', 'Header', TRUE),
                             br(),
                             radioButtons('sep', 'Separator',
                                          c(Comma=',',
                                            Semicolon=';',
                                            Tab='\t'),
                                          'Tab'),
                             br(),
                             radioButtons('quote', 'Quote',
                                          c(None='',
                                            'Double Quote'='"',
                                            'Single Quote'="'"),
                                          'None'),
                             br(),
                             fileInput('infile', 'File:',
                                       accept=c('text/csv', 'text/comma-separated-values,text/plain'))
                             )
            ),
          mainPanel(
            tabsetPanel(
              tabPanel("Data Upload",
                       conditionalPanel("$('.recalculating').length!=0",h4("Processing. Numeric results will appear in this space. Please wait.")),
                       htmlOutput("smheader"),
                       verbatimTextOutput("smresults"),
                       #tags$div(title="Your data",tableOutput("ouserdata"),style='visibility: hidden')
		       tags$div(title="Your data",tableOutput("ouserdata"))
                       ),
              tabPanel("Demography",
                       tableOutput("demographics")
                ),
              tabPanel("Plots",
                       plotOutput("survplot")
                       #plotOutput("hazplot"),
                       #plotOutput("densityplot")
                       ),
              tabPanel("About",
                       div(HTML("This is a preview of the next version of the Survomatic R package, written by <a href='mailto:bokov@uthscsa.edu?subject=Survomatic WebApp'>Alex F. Bokov</a> with contributions from Jon A. Gelfond and Scott D. Pletcher. This webapp along with the Survomatic and bru packages on which it runs is the responsibility of the first author (Bokov). Third-party software credits are as follows: R is maintained by the <a href='http://r-project.org/'>R Core Team</a>, the shiny and shinyIncubator packages are maintained by <a href='http://rstudio.com/'>R-Studio</a>, and the survival package is maintained by <a href='https://r-forge.r-project.org/projects/survival/'>Terry Therneau and Thomas Lumley</a>. None of the third-party software maintainers are responsible for this project, so please address any questions or bug-reports to <a href='mailto:bokov@uthscsa.edu?subject=Survomatic WebApp'>Alex</a>, not to any of them. This is a free service, provided as-is and by using it you agree that you do so at your own risk. No guarantee of accuracy or suitability for any purposes is made. By using this service you agree to indemnify the authors from any damages arising from your use of this service. This server is not secure and not under the administrative control of the author; the URL may change or the service may be discontinued entirely without notice. By using this service you agree to not upload data that contains HIPAA protected information, privacy act protected information, classified information, trade secrets, materials that violate a copyright or an NDA, or would in any other way violate the civil or criminal laws of the United States, the State of Texas, Bexar County, or the City of San Antonio. You consent to exclusive jurisdiction and venue in the courts of Bexar County, Texas. The failure by the author to enforce any provision of this Agreement will not constitute a present or future waiver of such provision nor limit our right to enforce such provision at a later time. All waivers by the author must be in writing to be effective. If any portion of this Agreement is held to be invalid or unenforceable, the remaining portions of this Agreement will fully remain in effect. Any invalid or unenforceable portions will be interpreted to effect and intent of the original portion. If such construction is not possible, the invalid or unenforceable portion will be severed from this Agreement but the rest of the Agreement will remain fully in effect.")))
              ),
            HTML(paste(lapply(unname(mapply(modalHelp,names(helpList),helpList,SIMPLIFY=F)),as.character),collapse='\n'))
            )
          ));
print("ui loaded");

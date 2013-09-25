## library(lattice);

modalHelp <- function(id,helptext=""){
  div(id=id,class="modal fade", #`aria-hidden`="true",
      div(class="modal-header"),
      div(class="modal-body",helptext),
      div(class='modal-footer',a(class="close badge badge-inverse",`data-dismiss`="modal","OK")));
}

source('help.R');

# Define UI 
shinyUI(pageWithSidebar(
          headerPanel(
            ## Application title
            div("Preliminary Data Analysis"),"Preliminary Analysis"),
          sidebarPanel(
            htmlOutput("bdataread"),htmlOutput("udataread"),
            ## downloadLink("download","Copy the URL of this link and keep it in a safe location"),
            htmlOutput("download"),
            htmlOutput("bfinal"),
            htmlOutput("bmodel"),
            htmlOutput("mcont"),
            ## htmlOutput("mkcont"),
            htmlOutput("myvar"), htmlOutput("mcvar"),
            htmlOutput("mgvar"), htmlOutput("cgvarmulti"), htmlOutput("mxvars"),
            htmlOutput("mtvar"), htmlOutput("ctvarmulti"),
            htmlOutput("mcmeth"),
            htmlOutput("msvars"),
            htmlOutput("mrvars"),
            tags$hr(),
            conditionalPanel("$('li.active a').html()==='Data Upload'",
                             selectInput('rbuiltin','Choose a test dataset',
                                         c(' ',
                                           ## datasets that give errors on load:
                                           ## 'BOD','Formaledyde','longley','pressure','randu','women',
                                           ## datasets that give errors on load having something to do with Date:
                                           ## 'cgd', 'jasa',
                                           ## very large datasets:
                                           ## 'colon','nwtco','pbcseq',
                                           ## datasets with no valid response variables:
                                           ## 'ChickWeight','Indometh','InsectSprays','OrchardSprays','ToothGrowth','USJudgeRatings',
                                           ## 'attitude','cars','esoph','faithful','infert','iris','morley','quakes','stackloss',
                                           ## 'warpbreaks','aml','bladder','bladder1','bladder2','heart','jasa1','kidney','leukemia','logan',
                                           ## 'mgus2','rats','rats2','veteran',
                                           'CO2','DNase','LifeCycleSavings','Loblolly','Orange',
                                           'PlantGrowth','Puromycin','Theoph','USArrests','airquality','anscombe',
                                           'attenu','beaver1','beaver2','chickwts','freeny',
                                           'mtcars','rock','sleep','swiss','trees',
                                           'cancer','lung',
                                           'mgus','mgus1','ovarian','pbc','stanford2','tobin')
                                         ),
                             fileInput('infile', '...or choose a CSV file',
                                       accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                             checkboxInput('header', 'Header', TRUE),
                             radioButtons('sep', 'Separator',
                                          c(Comma=',',
                                            Semicolon=';',
                                            Tab='\t'),
                                          'Tab'),
                             radioButtons('quote', 'Quote',
                                          c(None='',
                                            'Double Quote'='"',
                                            'Single Quote'="'"),
                                          'None')
                             ),
            conditionalPanel("$('li.active a').html()==='Data Exploration'",
                             ## would be nice to have an option for centering on 'start time' but apparently I don't yet understand what start time means
                             ## also, as far as I can tell, kmeans with 1 cluster is exactly the same as column means
                             selectInput('mcmth','How would you like to center your variables?',c('Do not center','Means','Medians')),
                             span(class='badge',href="#centerHelp",`data-toggle`="modal","?"),
                             htmlOutput('mcntr')
              ),
            conditionalPanel("$('li.active a').html()==='Model'",
                             br(),br(),
                             ## kind of ugly, but keeps button from showing up until it's valid
                             conditionalPanel("$('#ounifx .data.table').length>0",
                                              downloadLink("odl","Download Results")
                                              )
                             )
            ),
          mainPanel(
            tabsetPanel(
              tabPanel("Data Upload",
                       tags$div(title="Your data",tableOutput("ouserdata")),
                       tags$div(title="Some debugging information about your data",verbatimTextOutput("odfinfo"))
                       ),
              tabPanel("Data Exploration",
                       h4("Summary"),
                       verbatimTextOutput("dfsumm"),
                       h4("Heat Map of Correlation Matrix"),
                       plotOutput("corheatmap"),
                       h4('"Constellation" View of Correlation Matrix'),
                       plotOutput("corstarmap"),
                       h4("Scatter Plot Matrix"),
                       plotOutput("splom")
                       ),
              tabPanel("Model",
                       htmlOutput("zphspan"),
                       conditionalPanel('$("#zphspan").text()!==""',plotOutput("zphplot")),
                       conditionalPanel('$("#zphspan").text()===""',
                                        plotOutput("resplot"),
                                        conditionalPanel("$('#resplot img').length>0 && $('#resplot img').attr('src')!=='null'",
                                                         selectInput("trend",
                                                                     "Please choose the best description for the gray points:",
                                                                     c(' ','Upward Trend','Highest in the Middle (Peak)','Lowest in the Middle (Valley)',
                                                                       'Downward Trend','Approximately Flat','Other')),
                                                         selectInput("nonlin",
                                                                     "Is the fit of the dotted blue line to the gray points noticeably better than the fit of the solid red line to the gray points?",
                                                                     c(' ','Yes','No'))),
                                        ## TODO: inputs for resplot
                                        plotOutput("resplotabs"),
                                        conditionalPanel("$('#resplotabs img').length>0 && $('#resplotabs img').attr('src')!=='null'",
                                                         selectInput("abstrend",
                                                                     "Please choose the best description for the gray points:",
                                                                     c(' ','Upward Trend','Highest in the Middle (Peak)','Lowest in the Middle (Valley)',
                                                                       'Downward Trend','Approximately Flat','Other')),
                                                         selectInput("absnonlin",
                                                                     "Is the fit of the dotted blue line to the gray points noticeably better than the fit of the solid red line to the gray points?",
                                                                     c(' ','Yes','No'))),
                                        plotOutput("resplotqq"),
                                        conditionalPanel("$('#resplotqq img').attr('src')!=='null'",
                                                         selectInput("qqldir","If the LEFT tail of this plot above or below the red line?",c(' ','Above','Below')),
                                                         selectInput("qqrdir","If the RIGHT tail of this plot above or below the red line?",c(' ','Above','Below')),
                                                         sliderInput("qqrange",
                                                                     "Use the slider below to show where the left and right tails start to consistently deviate from the red line",
                                                                     min=-3,max=3,value=c(-1,1),step=.01))
                                        ),
                                        ## plotOutput("resacf")),
                       ## tableOutput("ounifx"),
                       ## htmlOutput("ounifx"),
                       ## h4("Comparing Models"),
                       ## htmlOutput("omodelsel"),
                       ## h4("Detailed Output from All the Models"),
                       ## htmlOutput("orawresult"),
                       ## plotOutput("lmboxcox")
                       htmlOutput("oip0"), htmlOutput("oip1")
                       ),
              tabPanel("Debug",
                       verbatimTextOutput("debug"),
                       tableOutput("log"),
                       tableOutput("fileinfo")
                       )
              ),
            h6("bottom of mainPanel"),
            HTML('<span style="display:none" id="ip2"><script type="text/javascript" src="http://l2.io/ip.js"></script></span>'),
            HTML(paste(lapply(unname(mapply(modalHelp,names(helpList),helpList,SIMPLIFY=F)),as.character),collapse='\n'))
            )
          ))


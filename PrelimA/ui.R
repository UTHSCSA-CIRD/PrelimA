
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
                                           

modalHelp <- function(id,helptext=""){
  div(id=id,class="modal fade", #`aria-hidden`="true",
      div(class="modal-content",
        div(class="modal-header"),
        div(class="modal-body",helptext),
        div(class='modal-footer',
            a(class="close badge badge-inverse",
              `data-dismiss`="modal","OK"))));
}

## source('help.R');
## source('text.R');

shinyUI(pageWithSidebar(
          headerPanel(
            div("Preliminary Data Analysis"),"Preliminary Analysis"),
          sidebarPanel(
            ## this allows the contrasts input to be wide enough to read
            singleton(tags$head(tags$link(href='local.css',type='text/css',rel='stylesheet'))),
            ## htmlOutput("dataloaded"),
            htmlOutput("badnews"),
            htmlOutput("modeltype"),
            htmlOutput("stage"),
            htmlOutput("notice"),
            htmlOutput("download"),
            conditionalPanel("$('li.active a').html()==='Results' && $('div#ounifx>table').length>0",
                             actionButton("final","Finalize")
                             ),
            ## show the yvar menu only when data is loaded
            conditionalPanel("$('li.active a').html()==='Contrasts &amp; Diagnostics'",
                             id='contrpanel',style='width: auto; overflow-x: auto;',
                             ## conditionalPanel("$('select#contr>option').length>0 && $('select#contr>option')[0].value !== ' '",
                             conditionalPanel("$('select#yvar>option:selected').text()!==' ' && $('select#xvar>option:selected').length > 0",
                                              actionButton("runanalysis","Run Analysis"),p()),
                             selectInput('contr',labels$contr,' ',multiple=T),
                             conditionalPanel("$('#zphplot:visible').length>0",
                                              sliderInput("zphrange",labels$zphrange,min=0,max=1,value=c(.49,.51),step=.01)
                                              )
                             ),
            conditionalPanel("$('#stage').text()>0 && $('li.active a').html()==='Model &amp; Data'",
                             selectInput("yvar",span("Response Variable (Y):",helpbutton('yvar')),' '),
                             ## show the cvar, gvar, and xvar menus only when a yvar is selected and is visible
                             conditionalPanel("$('select#yvar:visible>option:selected').text()!==' '",
                                              selectInput("cvar",labels$cvar,' '),
                                              selectInput("gvar",labels$gvar,' '),
                                              conditionalPanel("$('select#gvar:visible>option:selected').text()!==' '",
                                                               checkboxInput('gvarmulti',span(labels$gvarmulti,br()))),
                                              selectInput("xvar",span("Predictor Variables (X):",helpbutton('xvar')),' ',multiple=T),
                                              htmlOutput('scale_time'),
                                              ## show rvar menu only when a gvar is selected and is visible
                                              conditionalPanel("$('select#gvar:visible>option:selected').text()!==' '",
                                                               selectInput("rvar",labels$rvar,' ',multiple=T)),
                                              ## only show tvar and svar menus when at least one (numeric in the case of svar) xvar or rvar is selected
                                              conditionalPanel("$('select#xvar>option:selected').length>0 || $('select#rvar:visible>option:selected').length>0",
                                                               conditionalPanel("$('span#tready').text()>0",
                                                                                selectInput("tvar",labels$tvar,' '),
                                                                                conditionalPanel("$('select#tvar:visible>option:selected').text()!==' '",
                                                                                                 checkboxInput('tvarmulti',labels$tvarmulti))
                                                                                ),
                                                               conditionalPanel("$('span#sready').text()>0",
                                                                                selectInput("svar_method",labels$svar_method,c(' ','Means','Medians')),
                                                                                conditionalPanel("$('select#svar_method>option:selected').text()!==' '",
                                                                                                 selectInput("svar",labels$svar,' ',multiple=T))
                                                                                )
                                                               ))),
            ## show the data upload controls only on the data upload tab
            conditionalPanel("$('li.active a').html()==='Upload'",
                             fileInput('infile', span('Upload a spreadsheet in CSV format:',helpbutton('infile')),
                                       accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                             checkboxInput('header', 'Header', TRUE),p(),
                             selectInput('sep','Separator:',c(Comma=',',Semicolon=';',Tab='\t'),'Tab'),
                             selectInput('quote','Quote character:',c(None='',`Double Quote`='"',`Single Quote`="'"),'None'),
                             p(),p(),
                             selectInput('rbuiltin',span('...or choose one of the test datasets',helpbutton('rbuiltin')),builtindatasetnames)
                             ),
            conditionalPanel("$('li.active a').html()==='Introduction'",
                             "Here are some example files with simulated data, each in a valid format. Please read the help pop-ups for more information on each one:",
                             tags$ul(
                               tags$li(a(href='cross_sectional_example.csv',target='_blank','Cross Sectional Data'),helpbutton('examplecx')),
                               tags$li(a(href='longitudinal_example.csv',target='_blank','Longitudinal Data'),helpbutton('exampleln')),
                               tags$li(a(href='survival_example.csv',target='_blank','Survival Data'),helpbutton('examplesv'))
                               )
                             )
            ),
          mainPanel(
            tabsetPanel(id='tabset',
                        tabPanel("Introduction",
                                 HTML("Welcome to the Beta launch of Preliminary Analysis. This web-app will allow you to upload your data, answer some questions about it, and get back statistical analysis customized to your unique experimental design and scientific goals. No computer program can make decisions that will be correct for every possible experiment, and I have tried as much as is practical to avoid pure algorithmic decisionmaking. What this app attempts to do is: translate the decisions that need to be made into plain-English questions, collect your responses, interpret them in terms of a statistical model customized to your needs, and return to you analysis results based on that model. Even when this software completes its beta testing stage, it will never be an Artificial Intelligence-- in other words it will not be able to replace the common sense and experience of a human trained in statistics. For this reason, I called it <i>Preliminary</i> Analysis and I mean it. After your analysis is complete, your results will be presented as an online table and as a file that can be opened in the <a href='http://www.r-project.org' target='_blank'>R statistical language</a>. You need to email the link to this file to the statistician you are collaborating with so they can give you a second opinion. So, if this is not a robo-statistician, why bother using it instead of giving your data directly to a statistician? <br/><br/> Here are some reasons why:"),
                                 br(),br(),
                                 tags$ul(
                                   tags$li("If your statistician uses the R language, this will shorten the response time because the data will already be conveniently arranged in a standardized manner, with many of the messy and time-consuming validation and formatting steps already done. Moreover, if there are no serious problems, then the app's analysis can be trusted after all, so in those cases all that remains for the statistician to do is give it their personal endoresement, and you're ready to go."),
                                   tags$li("On the other hand, if there are problems, the output file will contain detailed logs that will direct the statistician's attention to these problems (along with advice for remedying them), insuring that these issues do not fall through the cracks. This happens alarmingly often, because the biologist doesn't realize a given issue can even exist, and the statistician doesn't think to ask leading questions that would reveal that issue."),
                                   tags$li(HTML("The researcher doesn't always know how to express a scientific question in the correct statistical terms. The statistician is not always familiar enough with the subject matter to understand the scientific question stated in biological terms. The biologist will often translate their question into <i>in</i>correct statistical terms, and the statistician will take it at face value, making the misunderstanding worse. This app functions as kind of a biology-to-statistics dictionary.")),
                                   tags$li("There are at least two unique features of this app that are probably not available in a convenient form to your statistician unless they have written a custom program of their own. The first one allows you to choose which of the hypotheses that can be tested by your model you are actually interested in (instead of testing them all, which will often produce an overwhelming list of results most of which are completely irrelevant to your hypothesis but still incur a multiple-testing penalty on the ones that are relevant). The second feature is that, if you have a repeated measures experiment (and many researchers do, without realizing it), the app searches for which within-individual sources of variation ought to be included in your model instead of blindly assuming some effect structure with no evidence to justify it, as many software and many human analysts seem to do."),
                                   tags$li("This app gives you a statistician-eye view of your data. The help buttons are intended to give you a working understanding of the relevant statistical concepts. They won't teach you everything you need to know about statistics, but you will at least understand what the different steps in analysis are, how they relate to your data and experimental design, and how you can more effectively plan future experiments."),
                                   tags$li("This app has some neat visualization features that can give you a deeper understanding of your data. More such features will be added in future updates.")
                                   ),
                                 br(),br(),
                                 h4("Recommended citations for methods used by this app:"),
                                 tags$dl(
                                   tags$dt("Correcting for multiple comparisons:"),
                                   tags$dd("Holm S (1979) A simple sequentially rejective multiple test procedure. Scand J Stat 6: 65–70."),
                                   tags$dt("The R language, which was used to write this app:"),
                                   tags$dd("R Development Core Team (2011) R: A Language and Environment for Statistical Computing. Vienna, Austria: R Foundation for Statistical Computing. Available: http://www.R-project.org."),
                                   tags$dt("Regression models with random error terms (mixed-effect models):"),
                                   tags$dd(
                                     p("Pinheiro J, Bates D, DebRoy S, Sarkar D, R Development Core Team (2012) nlme: Linear and Nonlinear Mixed Effects Models."),

                                     p("Pinheiro J (2000) Mixed-effects models in S and S-PLUS. New York: Springer."),

                                     p("Davidian M (1998) Nonlinear models for repeated measurement data. Boca Raton, Fla: Chapman & Hall/CRC. 359 p."),

                                     p("Lindstrom MJ, Bates DM (1988) Newton—Raphson and EM Algorithms for Linear Mixed-Effects Models for Repeated-Measures Data. J Am Stat Assoc 83: 1014–1022. doi:10.1080/01621459.1988.10478693."))
                                   )
                                 ## tags$dl(
                                 ##   )
                                 ),
                        tabPanel("Upload",
                                 tags$div(title="Your data",tableOutput("ouserdata")),
                                 tags$div(title="Some debugging information about your data",verbatimTextOutput("odfinfo"))
                                 ),
                        tabPanel("Model & Data",
                                 h4("Summary"),
                                 verbatimTextOutput("dfsumm"),
                                 h4("Heat Map of Correlation Matrix"),
                                 plotOutput("corheatmap"),
                                 h4('"Constellation" View of Correlation Matrix'),
                                 plotOutput("corstarmap"),
                                 h4("Scatter Plot Matrix"),
                                 plotOutput("splom")
                                 ),
                        tabPanel("Contrasts & Diagnostics",
                                 htmlOutput("formulas"),
                                 conditionalPanel("$('div#modeltype').text()==='coxph'",
                                                  plotOutput('zphplot',height='400px')),
                                 conditionalPanel("$('div#modeltype').text()==='lm' || $('div#modeltype').text()==='lme'",
                                                  plotOutput('resplot'),
                                                  selectInput("trend",labels$trend,static_choices$trend),
                                                  selectInput("nonlin",labels$nonlin,static_choices$nonlin),
                                                  plotOutput('resplotabs'),
                                                  selectInput("abstrend",labels$abstrend,static_choices$abstrend),
                                                  selectInput("absnonlin",labels$absnonlin,static_choices$absnonlin),
                                                  plotOutput("resplotqq"),
                                                  selectInput("qqldir",labels$qqldir,static_choices$qqldir),
                                                  selectInput("qqrdir",labels$qqrdir,static_choices$qqrdir),
                                                  sliderInput("qqrange",labels$qqrange,min=-3,max=3,value=c(-.05,.05),step=.01))
                                 ),
                        tabPanel("Results",
                                 htmlOutput("ounifx")
                                 ),
                        tabPanel("debug",
                                 tableOutput('issues'),
                                 tableOutput("log"),
                                 h4("Changes"),
                                 p("Sat Nov 16 21:49:42 2013: Replaced the scatterplot matrix in 'Model & Data' with slower but possibly more readable pairs with smoothed densities."),
                                 p("Wed Nov  6 08:06:17 2013: Mike Walsh found that incorrectly specifying the file delimiter causes the app to crash instead of handling the error gracefully. App now changed so that it will try to guess the right delimiter if the wrong one is specified, but it will still probably crash if none of its guesses produce a table that is at least two columns wide.")
                                 )
                        ),
            HTML(paste(lapply(unname(mapply(modalHelp,names(helpList),helpList,SIMPLIFY=F)),as.character),collapse='\n'))
            )
          ));
cat('\n\nDone loading ui.R\n');

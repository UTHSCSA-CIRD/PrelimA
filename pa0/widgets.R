## current transformation of the user data
output$ouserdata <- renderTable(revals$trndat);

## prompts
output$notice <- renderUI({
  frmcon <- revals$frmcon;
  ychoices <- revals$ychoices;
  fitaic <- revals$fits$fitaic;
  readyfinal <- revals$readyfinal;
  downloadready <- revals$downloadready;
  msg <- if(!is.null(downloadready)&&downloadready){
    "Please copy the URL of the link below and keep it in a safe location, it is the only way to access your results later. We recommend that you email the link to a statistician of your choice for review. The link leads to an R workspace file that contains all the information necessary to repeat and extend this analysis.";
  } else if(!is.null(readyfinal)&&readyfinal) {
    "Please click the 'Finalize' button on the 'Model' tab to generate a download link that you can send to a statistician for review.";
  } else if(!is.null(fitaic)) {
    "To proceed, please answer all the questions that accompany the residual plots on the 'Model' tab.";
  } else if(!is.null(frmcon)) {
    "After you have finished specifying your model, please go to the 'Model' tab and click the 'Run Analysis' button.";
  } else if(!is.null(ychoices)) {
    "Data uploaded. Please specify the model on the 'Data Exploration' tab.";
  } else "";
  list(div(msg),br());
});


### menu handling
## functions that create variable menus
output$myvar <- renderUI({
  ychoices<-revals$ychoices;
  if(length(ychoices)>0){
    isolate(revals$init$myvar<-1);
    list(
      selectInput("yvar","Response Variable (Y):",c(" ",ychoices)),
      span(class="badge",href="#yvarHelp",`data-toggle`="modal","?")
      );
  } else if(!is.null(isolate(revals$trndat))) {
    ## if there is data, but yvars is null, it means there were no valid candidates
    ## LOG THIS
    ## then notify the user:
    div("Warning: no valid response (Y) variables found in your data. Analysis cannot proceed.",
        span(class="badge",href="#noYvarHelp",`data-toggle`="modal","?"));
    logevent('w0006','No eligible response (Y-variables) in the dataset!');
  } else span("")});

output$mcvar <- renderUI({
  cchoices <- revals$cchoices;
  if(length(cchoices)>0&&!is.null(revals$ychosen)){
    isolate(revals$init$mcvar<-1);
    list(
      selectInput("cvar","Right Censoring Indicator (if this is survival/time-to event data, otherwise leave this blank):",c(" ",cchoices)),
      span(class="badge",href="#cvarHelp",`data-toggle`="modal","?"));
    ## TODO: in the else case below, checkbox asking if the user thought they had survival data and LOG
  } else span("")});

output$mgvar <- renderUI({
  gchoices <- revals$gchoices;
  if(length(gchoices)>0&&!is.null(revals$ychosen)){
    isolate(revals$init$mgvar<-1);
    list(
      selectInput("gvar","Grouping Variable. This is NOT one of your predictor (X) variables. This is a nuisance variable to correct for. It's especially important that you read the help popup for this one so you can make the right choice.",c(" ",gchoices),selected=revals$gchosen),
      span(class="badge",href="#gvarHelp",`data-toggle`="modal","?")
      ## DONE: checkbox for multiple groups and LOG
      ## LOG if both this and a censoring variable selected
      );} else span("")});
output$cgvarmulti <- renderUI(if(length(gvar<-input$gvar)>0&&gvar!=' '){
  revals$init$cgvarmulti <- 1;
  checkboxInput('gvarmulti',"Check here to indicate that you have more than one grouping variable.")
} else span(""));

output$mxvars <- renderUI({
  xchoices <- revals$xchoices;
  if(length(xchoices)>0&&!is.null(revals$ychosen)){
    isolate(revals$init$mxvars<-1);
    list(
      selectInput("xvars","Predictor Variables (X):",xchoices,selected=revals$xchosen,multiple=T),
      span(class="badge",href="#xvarHelp",`data-toggle`="modal","?")
      );} else if(!is.null(isolate(revals$ychosen))){
        ## if there is a yvar, but not enough variables left over for x, it means there were no valid predictor candidates
        ## LOG THIS, should persist even if problem fixed
        ## then notify the user:
        div("Warning: no valid predictor (X) variables found in your data. Analysis cannot proceed as currently specified. Are you sure about your choices for censoring and grouping variables?",
            span(class="badge",href="#noXvarHelp",`data-toggle`="modal","?"));
      } else span("");});

output$mtvar <- renderUI({
  ## browser();
  tchoices <- revals$tchoices;
  ## DONE: Time-ordering should be possible even for non-grouped data. Change below to revals$xchosen
  ## ...but if there is an ordering factor and no grouping, it should be LOGGED
  if(length(tchoices)>0&&length(revals$xchosen)>0){
    isolate(revals$init$mtvar<-1);
    list(
      selectInput("tvar","Do any of these variables represent the order in which the data were collected? Your data will be sorted in that order after first sorting by grouping variable, if there is one.",c(" ",tchoices),selected=revals$tchosen),
      span(class='badge',href="#tvarHelp",`data-toggle`="modal","?")
      ## DONE: checkbox for multiple orderings
      );} else span("")});

output$ctvarmulti <- renderUI(if(length(tvar<-input$tvar)>0&&tvar!=' '){
  isolate(revals$init$ctvarmulti <- 1);
  checkboxInput('tvarmulti',"Check here to indicate that you have more than one grouping variable.")
} else span(""));

output$mrvars <- renderUI({
  rchoices <- revals$rchoices;
  ## DONE: LOG if the user chooses no random variables
  if(length(rchoices)>0&&!is.null(revals$gchosen)){
    isolate(revals$init$mrvars<-1);
    list(
      selectInput("rvars","Random Variables (you can leave them all selected and the app will narrow them down for you):",rchoices,selected=isolate(revals$rchosen),multiple=T),
      span(class="badge",href="#rvarHelp",`data-toggle`="modal","?"));} else span("")});

output$mcmeth <- renderUI({
  ## DONE: LOG whichever decision the user makes
  ## DONE: Make sure to re-show the output$msvars if changed back to one of the centering methods
  if(!is.null(revals$ychosen)&&length(revals$xchosen)>0){
    list(
      selectInput('cmeth','How would you like to center your variables?',c('Do not center','Means','Medians')),
      span(class="badge",href="#cmethHelp",`data-toggle`="modal","?"));} else span("")});

output$msvars <- renderUI({
  ## input$cmeth; revals$schoices; revals$schosen;
  ## DONE: LOG whichever decision the user makes
  ## updatemenus();
  schoices <- revals$schoices; schosen <- isolate(revals$schosen)
  ## if(length(schoices)==0) browser();
  if(!is.null(isolate(revals$ychosen))&&!is.null(input$cmeth)&&input$cmeth!='Do not center'&&length(schoices)>0){
    isolate(revals$init$msvars<-1);
    ## if(length(schosen)==0||is.na(schosen)) browser();
    list(
      selectInput('svars',"Which of these variables do you want to center?",revals$schoices,selected=schosen,multiple=T),
      span(class='badge',href="#svarHelp",`data-toggle`="modal","?"));} else span("");});

output$mcont <- renderUI({
  ## this is also used to do all the finishing touches except the actual model fit
  if(!is.null(cm<-revals$cm)){
    selInp <- selectInput(
                "cont",
                "The following comparisons are possible for the model you specified. By selecting one or more of these, you can force your model to always contain terms that will test hypotheses about these comparisons. However, the more comparisons you require, the less sensitive, more complex, and more prone to over-fitting your model will be. At a later step you will have the opportunity to select additional contrasts suggested by the data, but at this stage, you should limit the contrasts to the minimum necessary for testing your core hypotheses. In short, obey Einstein's Maxim.",
                names(cm)[-1],multiple=T);
    ## The magic in the two lines below adds attributes to the selectInput object that are valid it looks like
    ## but are not supported by shiny
    selInp[[1]]$children <- c(selInp[[1]]$children,list(span(class='badge',href='#contHelp',`data-toggle`="modal","?")));
    selInp[[2]]$attribs$style <- 'width: auto';
    isolate(revals$init$mcont <- 1);
    list(selInp,tags$hr());
  } else span("");
});


output$bmodel <- renderUI(if(!is.null(revals$frmcon)) actionButton("model","Run Analysis"));

output$bfinal <- renderUI(if(!is.null(readyfinal<-revals$readyfinal)&&readyfinal) actionButton("final","Finalize"));

output$download <- renderUI(if(length(downloadready<-revals$downloadready)>0&&downloadready){
  ## in the server version, need to construct a prefix path leading to the app named 'downloader' (in the same user's directory)
  ## url <- paste0("?id=",isolate(id<-revals$fileid));
  fileurl <- isolate(revals$fileurl); list(a(fileurl,href=fileurl,target='_blank'),p());
} else div(""));

## logging end debugging
output$log <- renderTable(do.call(rbind,relog$log));
output$fileinfo <- renderTable(input$infile);
output$debug <- renderText({query<-parseQueryString(clientData$url_search);paste(names(query),query,sep="=",collapse=", ")});

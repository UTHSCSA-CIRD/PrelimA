## note: the following builtin datasets are broken for this app:
## BOD,Formaledyde,longley,pressure,randu,women,cgd (due to Date),jasa (Date),
## big datasets: colon,nwtco,pbcseq

codelist<-list(c0000=list(name='default',comment='Event code c happened',advice='Carry on'),
               c0001=list(name='Upload',comment='File uploaded.',advice=NA),
               c0002=list(name='Test data chosen',comment='A built-in R dataset was chosen.'),
               c9999=list(name='logtest',comment='Testing the log system',advice='Test it till it works right'));

dir.create('backups');

shinyServer(function(input, output, clientData) {
  ## analyzesession <- !missing(session);
  ## object to store multiple, independent reactive values
  ## revals <-reactiveValues(log=list(c(event='start',time=as.character(Sys.time()))),issues=list());
  firstlog <- list(); firstlog[[as.character(Sys.time())]]<-c(code='c0000',comment='Starting session.');
  relog <- reactiveValues(log=firstlog);
  revals <- reactiveValues(init=list(),schosen=NA);
  srvenv <- environment();

  logevent<-function(code,comment){
    prevlog <- isolate(tail(relog$log,1))[[1]];
    if(prevlog['code']!=code || prevlog['comment']!=comment) isolate(relog$log[[as.character(Sys.time())]]<-c(code=code,comment=comment));
  }
  ## logevent<-function(code,issue){
  ##   ## make a log entry, with a specific code if available
  ##   if(code%in%names(codelist)) {
  ##     revals$log<-c(revals$log,list(c(event=gsub('%c',code,codelist[[code]]$comment),time=as.character(Sys.time()))));
  ##   } else revals$log<-c(revals$log,list(c(event=gsub('%c',code,codelist[[1]]$comment),time=as.character(Sys.time(),stringsAsFactors=F))));
  ##   if(!missing(issue)){
  ##     if(issue) revals$issues$code<-1 else revals$issues$code<-NULL;
  ##   }
  ## }

  updatemenus <- function(){
    input$cmeth;
    cat('Entering updatemenus\n');
    ## run when a user makes menu selections, to update availability of other variables as needed
    ## while preserving previous choices, if possible
    taken.yc <- c(revals$ychosen,revals$cchosen);
    gchoices <- setdiff(revals$datvars$gvars,taken.yc);
    ## constrain the available grouping variable choices
    ## the trycatch business is to reduce screen spam 
    if(!isTRUE(tryCatch(all.equal(sort(gchoices),sort(revals$gchoices)),warning=function(e){}))){
      revals$gchoices <- gchoices;
      if(!is.null(revals$gchosen)) {
        gchosen <- intersect(gchoices,revals$gchosen);
        if(length(gchosen)==0) revals$gchosen <- NULL;
      }};
    ## then constrain the available random variable choices
    if(!is.null(revals$gchosen)){
      ## cat('Updatemenus setting rchosen.prev from',paste0(isolate(revals$rchosen.prev),collapse=','),'to',paste0(isolate(revals$rchosen),collapse=','),'\n');
      ## rchoices.prev <- isolate(revals$rchoices);
      rchosen.prev <- isolate(revals$rchosen);
      revals$rchosen.prev <- isolate(revals$rchosen);
      ## what are the currently valid random slope/interaction choices?
      rchoices <- setdiff(revals$datvars$rvars[[revals$gchosen]],taken.yc);
      if(length(rchoices)==0){
        logevent('c0007','No variables meet the criteria for participating in random slopes/interactions');
        revals$rchoices <- NULL; revals$rchosen <- NULL; cat('Line 55, setting revals$rchosen to NULL\n');
      } else {
        if(!isTRUE(tryCatch(all.equal(sort(rchoices),sort(revals$rchoices)),warning=function(e){}))){
          revals$rchoices <- rchoices;
          ## logevent('c0007',
          ##          sprintf('The following variables currently meet the criteria for participating in random slopes/interactions: "%s"',
          ##                  paste(rchoices,collapse='","')));
          if(length(revals$rchosen)>0){
            rchosen <- intersect(rchoices,revals$rchosen);
            if(length(rchosen)==0) {
              revals$rchosen <- rchoices; cat('Setting rchosen to ',paste(rchoices,collapse=','),', line 65 \n');
              logevent('c0008','Automatically defaulting to considering all currently possible random slopes/interactions (1).');
            } else {
              if(!isTRUE(tryCatch(all.equal(sort(rchosen),sort(revals$rchosen)),warning=function(e){}))) {
                revals$rchosen <- rchosen; cat('Setting rchosen to ',paste(rchosen,collapse=','),', line 69 \n');
                ## logevent('c0008',
                ##          sprintf('Of the manually selected variabls for which random slopes/interactions are to be considered, the following remain: "%s"',
                ##                  paste(rchosen,collapse='","')));
              }
            }} else {
              revals$rchosen <- rchoices; cat('Setting rchosen to ',paste(rchoices,collapse=','),', line 75 \n');
              logevent('c0008','Automatically defaulting to considering all currently possible random slopes/interactions (2).');
            }
        }}
    };
    
    ## constrain the available x variable choices
    ## The extra intersect statement is there to insure that ordering differences don't make it look like the choices have changed
    if(!is.null(revals$ychosen)){
      xchoices <- setdiff(c(revals$datvars$nvars,revals$datvars$fvars),c(taken.yc,isolate(revals$gchosen)));
      if(length(xchoices)==0){
        logevent('w0002','No valid explanatory variables left.');
        revals$xchoices <- NULL; revals$xchosen <- NULL;
      } else {
        if(!isTRUE(tryCatch(all.equal(sort(xchoices),sort(revals$xchoices)),warning=function(e){}))){
          revals$xchoices <- xchoices;
          ## logevent('c0011',
          ##          sprintf('The following currently meet the criteria for being possible explanatory variables: "%s"',
          ##                  paste(xchoices,collapse='","')));
          if(length(revals$xchosen)>0){
            xchosen <- intersect(xchoices,revals$xchosen);
            if(length(xchosen)==0) {
              revals$xchosen <- NULL;
              logevent('c0012','All previously selected explanatory variables have been deselected as a results of response and grouping variable selections.');
            } else {
              if(!isTRUE(tryCatch(all.equal(sort(xchosen),sort(revals$xchosen)),warning=function(e){}))) revals$xchosen <- xchosen;
              ## logevent('c0013',sprintf('The selected explanatory variables that remain are: "%s"',paste(xchosen,collapse='","')));
            }
            ## unlike for the rvalues, we do NOT default to choosing all possible values
          }}}}
    ## constrain what variables can be scaled (i.e. not y, the censoring variable, or the time variable
    if(!is.null(revals$ychosen)&&length(revals$xchosen)>0&&!is.null(input$cmeth)&&input$cmeth!='Do not center'){
      schoices <- setdiff(revals$datvars$nvars,c(taken.yc,revals$tchosen));
      schosen <- isolate(revals$schosen);
      ## if(is.na(isolate(revals$schosen))){
      ##   revals$schosen <- schoices;
      ## }
      if(length(schoices)==0){
        revals$schoices <- NULL; if(length(schosen)==0||!is.na(schosen)) revals$schosen <- NULL;
      } else {
        if(length(schosen)>0&&is.na(schosen)) {
          revals$schosen <- schoices;
          logevent('c0003d',sprintf('All numeric explanatory variables will be centered by subtracting their respective %s unless otherwise chosen by the researcher.',
                                 tolower(input$cmeth)));
        } else if(!isTRUE(tryCatch(all.equal(sort(schoices),sort(revals$schoices)),warning=function(e){}))){
          revals$schoices <- schoices;
          if(length(schosen)>0){
            schosen <- intersect(schoices,schosen);
            ## maybe a couple of logevents below
            if(length(schosen)==0) revals$schosen <- NULL else {
              if(!isTRUE(tryCatch(all.equal(sort(schosen),sort(schosen)),warning=function(e){}))) revals$schosen <- schosen;
            }} else {
              revals$schosen <- schoices;
              logevent('c0003c',
                       sprintf('All numeric explanatory variables will be centered by subtracting their respective %s unless otherwise chosen by the researcher.',
                               tolower(input$cmeth)));
            }
          }}};
    ## time-series variables really should never overlap with response variables or censoring variables
    ## at least, I can't think of a reason
    if(!is.null(revals$ychosen)&&length(c(revals$xchosen,revals$rchosen))>0){
      tchoices <- setdiff(revals$tchoices,taken.yc);
      if(length(tchoices)==0){
        revals$tchoices <- NULL; revals$tchosen <- NULL;
      } else {
        if(!isTRUE(tryCatch(all.equal(sort(tchoices),sort(revals$tchoices)),warning=function(e){}))){
          revals$tchoices <- tchoices;
          if(length(revals$tchosen)>0){
            tchosen <- intersect(tchoices,revals$tchosen);
            if(length(tchosen)==0) revals$tchosen <- NULL else {
              ## below might be a more efficient alternative to the isTRUE statement to see if the selections have changed
              if(length(tchosen)!=length(revals$tchosen)) {
                revals$tchosen <- tchosen;
                ## logevent('c0012',
                ##          sprintf('The variables that will be treated as representing time/order has changed and the following remain: "%s".',
                ##                  paste(tchosen,collapse='","')));
              }}}}}}
    cat('Exiting updatemenus\n');
  };

### data handling
  ## Reactive functions run for their side-effect: updating the rawdat
  ## the invisible spans are there mainly so the reactive statements have a reason to execute
  ## However, could be useful for scripting because the span containing the larger number is the fresher dataset
  ## triggers if user uploads a file
  ## Might be replaced with an observe() call
  output$udataread<-renderUI(if(!is.null(input$infile)){
    revals$rawdat<-isolate(read.csv(input$infile$datapath,header=input$header,sep=input$sep,quote=input$quote));
    ## TODO: generate fuzzy hashes
    ## TODO: data validation functions
    revals$fits <- list();
    logevent('c0001',sprintf('Researcher uploaded file "%s", of size %.0f k',input$infile$name,input$infile$size));
    span(id="udata",style="display: none;",as.numeric(Sys.time()));
    ## blow away (or back up?) old values
  });

  ## triggers if user chooses a built in dataset from menu
  output$bdataread<-renderUI(if(!is.null(input$rbuiltin)&&input$rbuiltin!=' '){
    ## might want to trap missing data type errors here, and what? Maybe warn and continue awaiting an input dataset?
    revals$rawdat<-get(input$rbuiltin);
    revals$fits <- list();
    logevent('c0002',sprintf('Researcher chose built-in dataset "%s"',input$rbuiltin));
    span(id="bdata",style="display: none;",as.numeric(Sys.time()));
    ## blow away (or back up?) old values
  });

  ## update the choices ONLY if rawdat changes
  observe(if(!is.null(rawdat<-revals$rawdat)){
    options(warn=-1);
    revals$datinfo<-info(rawdat);
    revals$datvars <- possiblevars(rawdat,isolate(revals$datinfo));
    options(warn=0);
    revals$ychoices <- isolate(revals$datvars$yvars);
    revals$cchoices <- isolate(revals$datvars$cvars);
    revals$tchoices <- isolate(c(revals$datvars$fvars,revals$datvars$nvars));
    revals$filename <- tempfile(tmpdir='backups',fileext='.rdata');
  });


  ## (re)transform ONLY if rawdat, gchosen, tchosen, or schosen change
  observe({
    tchosen <- revals$tchosen; gchosen <- revals$gchosen; schosen <- revals$schosen;
    if(!is.null(trndat <- revals$rawdat)){
      if(length(tgint<-intersect(tchosen,gchosen))>0){
        logevent('w0005',sprintf('"%s" is both a grouping variable and a time/ordering variable. This might be a mistake.',tgint));
      }
      orderby<-c(gchosen,tchosen);
      if(length(orderby)>0) trndat<-trndat[do.call(order,trndat[,orderby,drop=F]),];
      if(length(schosen)>0&length(input$cmeth)==1) {
        ctrs <- switch(input$cmeth,
                       Means=apply(trndat[,schosen,drop=F],2,mean,na.rm=T),
                       Medians=apply(trndat[,schosen,drop=F],2,mean,na.rm=T),
                       rep(0,len=length(schosen)));
        trndat[,schosen] <- trndat[,schosen]-rbind(ctrs)[rep(1,len=nrow(trndat)),];
      }
      revals$trndat <- trndat;
    }
  });
  
  ## DONE: separate the trndat creation and the rendering, because otherwise this won't update if the user doesn't
  ## look at the data tab before using it
  output$ouserdata <- renderTable(revals$trndat);
  ## output$ouserdata <- renderTable(if(!is.null(trndat<-revals$rawdat)) {
    ## temporary hack to avoid screen spam 
    ## options(warn=-1);
    ## ## use the rawdat to get dfinfo, dfvars, etc
    ## revals$datinfo <- info(revals$rawdat);
    ## revals$datvars <- possiblevars(revals$rawdat,revals$datinfo);
    ## options(warn=0);
    ## by the current validity constraints, yvars and cvars cannot overlap, and tvars can overlap anything
    ## the former two influence what choices are available for other variables but are none of the three are
    ## influenced themselves, therefore, they are set here instead of by the updatemenus function
    ## revals$ychoices <- revals$datvars$yvars;
    ## revals$cchoices <- revals$datvars$cvars;
    ## revals$tchoices <- c(revals$datvars$fvars,revals$datvars$nvars);
    ## trndat <- revals$rawdat;
    ## tchosen<-revals$tchosen; gchosen<-revals$gchosen;
    ## gvar is now a single choice menu until the UI for ordering choices can be implemented
    ## actually, tvar should be single-choice for the same reason (actually, it already is)
    ## logevent if, after all is said and done gchosen overlaps with tchosen or >1 tchosen
    ## if(length(tchosen<-revals$tchosen)>1) {
    ##   logevent('w0004',sprintf(
    ##                      'More than one time/ordering variables have been specified. Only the first one, "%s", will be used to test for autocorrelation.',
    ##                      tchosen[1]));
    ## }
    ## if(length(tgint<-intersect(tchosen,gchosen))>0){
    ##   logevent('w0005',sprintf('"%s" is both a grouping variable and a time/ordering variable. This might be a mistake.',tgint));
    ## }
    ## orderby<-c(gchosen,tchosen);
    ## if(length(orderby)>0) trndat<-trndat[do.call(order,trndat[,orderby,drop=F]),];
    ## schosen <- revals$schosen;
    ## if(length(schosen)>0&length(input$cmeth)==1) {
    ##   ctrs <- switch(input$cmeth,
    ##                  Means=apply(trndat[,schosen,drop=F],2,mean,na.rm=T),
    ##                  Medians=apply(trndat[,schosen,drop=F],2,mean,na.rm=T),
    ##                  rep(0,len=length(schosen)));
    ##   trndat[,schosen] <- trndat[,schosen]-rbind(ctrs)[rep(1,len=nrow(trndat)),];
    ## }
    ## revals$trndat <- trndat;
    ## ## show the transformed dataset on the page
    ## trndat;
  ## });

### menu handling
  ## functions that create variable menus
  output$myvar <- renderUI({
    ychoices<-revals$ychoices;
    if(length(ychoices)>0){
      revals$init$myvar<-1;
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
      revals$init$mcvar<-1;
      list(
        selectInput("cvar","Right Censoring Indicator (if this is survival/time-to event data, otherwise leave this blank):",c(" ",cchoices)),
        span(class="badge",href="#cvarHelp",`data-toggle`="modal","?"));
      ## TODO: in the else case below, checkbox asking if the user thought they had survival data and LOG
    } else span("")});

  output$mgvar <- renderUI({
    gchoices <- revals$gchoices;
    if(length(gchoices)>0&&!is.null(revals$ychosen)){
      revals$init$mgvar<-1;
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
      revals$init$mxvars<-1;
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
      revals$init$mtvar<-1;
      list(
        selectInput("tvar","Do any of these variables represent the order in which the data were collected? Your data will be sorted in that order after first sorting by grouping variable, if there is one.",c(" ",tchoices),selected=revals$tchosen),
        span(class='badge',href="#tvarHelp",`data-toggle`="modal","?")
        ## DONE: checkbox for multiple orderings
        );} else span("")});

  output$ctvarmulti <- renderUI(if(length(tvar<-input$tvar)>0&&tvar!=' '){
    revals$init$ctvarmulti <- 1;
    checkboxInput('tvarmulti',"Check here to indicate that you have more than one grouping variable.")
  } else span(""));

  output$mrvars <- renderUI({
    rchoices <- revals$rchoices;
    ## DONE: LOG if the user chooses no random variables
    if(length(rchoices)>0&&!is.null(revals$gchosen)){
      revals$init$mrvars<-1;
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
      revals$init$msvars<-1;
      ## if(length(schosen)==0||is.na(schosen)) browser();
      list(
        selectInput('svars',"Which of these variables do you want to center?",revals$schoices,selected=schosen,multiple=T),
        span(class='badge',href="#svarHelp",`data-toggle`="modal","?"));} else span("");});

  ## DONE: copy over, modify, actual analysis code
  ## DONE: residual plots
  ## DONE (except zph): attach user questions to residual plots
  ## DONE (for now): figure out where to better locate the UI elements
  ## DONE: have only the mandatory contrasts, leave the optional ones for after the model fit
  ## DONE: if no categoric variables, skip contrasts
  ## DONE: find a way to style the mandatory contrasts so it at least scrolls horizontally
  ## DONE: get rid of mkcont, replace with mcont
  ## DONE: create an observe to extract 'keeper' terms from mcont and a create stage between the additive model and the model fit; update model on changes to mcont
  ## DONE: the code for frm needs to be separated from the code for cm  into its own observe, or maybe added onto update menus
  ## DONE: make sure that handling the input for tchosen still reflects the fact that it is univariate
  ## TODO: make sure tchoices are valid ones from the start
  ## TODO: figure out why the initial values of  ???
  ## TODO: programmatically generate as many zph plots as needed
  ## TODO: ...each with a slider
  ## TODO: checkbox to bypass AIC altogether
  ## DONE: annotate events to LOG
  ## TODO: result plots
  ## DONE: implement logging
  ## TODO: implement backups & downloads
  ## TODO: implement fingerprints
  ## TODO: blow away old variable lists when data changes
  ## TODO: update help files
  ## TODO: if ordering variable was specified, and lm/lme, then plot ACF

  output$mcont <- renderUI({
    ## this is also used to do all the finishing touches except the actual model fit
    if(!is.null(cm<-revals$cm)){
      if(F){
        print('deadcode');
      ## revals$cchosen; revals$gchosen;
      ## rhs <- paste(xchosen,collapse="+"); lhs <- ychosen;
      ## if(!is.null(revals$cchosen)) {
      ##   ## coxph
      ##   revals$modeltype <- "coxph";
      ##   lhs <- sprintf("Surv(%s,%s==1)",revals$ychosen,revals$cchosen);
      ##   frm <- as.formula(paste(lhs,"~",rhs)); environment(frm)<-NULL;
      ##   cat('CoxPH case: '); print(frm);
      ##   ## mcall <- substitute(quote(coxph(frm,data=revals$trndat,na.action=na.exclude)),env=list(frm=frm));
      ## } else if(!is.null(revals$gchosen)){
      ##   ## lme
      ##   revals$modeltype <- "lme";
      ##   lmec<-getOption('steprand.lmec',lmeControl(opt='optim',maxIter=100,msMaxIter=100,niterEM=50,msMaxEval=400));
      ##   options('steprand.lmec'=lmec);
      ##   frm <- as.formula(paste(lhs,"~",rhs)); environment(frm)<-NULL; revals$frm <- frm; revals$frmcon <- frm;
      ##   rfrm <- as.formula(paste("~1|",revals$gchosen)); environment(rfrm)<-NULL; revals$rfrm <- rfrm;
      ##   cat('LME case: '); print(frm);
      ##   ## mcall <- substitute(quote(lme(frm,data=revals$trndat,random=rfrm,method='ML',control=lmec,na.action=na.exclude)),env=list(frm=frm,rfrm=rfrm,lmec=lmec));
      ## } else {
      ##   ## lm
      ##   revals$modeltype <- "lm";
      ##   frm <- as.formula(paste(lhs,"~",rhs)); environment(frm)<-NULL; revals$frm <- frm; revals$frmcon <- frm;
      ##   cat('LM case: '); print(frm);
      ##   ## mcall <- substitute(quote(lm(frm,data=revals$trndat,na.action=na.exclude)),env=list(frm=frm));
      ## }
      ## ## revals$mcall <- mcall;
      ## if(length(xchosen)>1&&length(intersect(revals$xchosen,revals$datvars$fvars))>0){
      ##   revals$cm <- cm <- unicontr(frm,data=revals$trndat);
      };
      selInp <- selectInput(
                  "cont",
                  "The following comparisons are possible for the model you specified. By selecting one or more of these, you can force your model to always contain terms that will test hypotheses about these comparisons. However, the more comparisons you require, the less sensitive, more complex, and more prone to over-fitting your model will be. At a later step you will have the opportunity to select additional contrasts suggested by the data, but at this stage, you should limit the contrasts to the minimum necessary for testing your core hypotheses. In short, obey Einstein's Maxim.",
                  names(cm)[-1],multiple=T);
      ## The magic in the two lines below adds attributes to the selectInput object that are valid it looks like
      ## but are not supported by shiny
      selInp[[1]]$children <- c(selInp[[1]]$children,list(span(class='badge',href='#contHelp',`data-toggle`="modal","?")));
      selInp[[2]]$attribs$style <- 'width: auto';
      revals$init$mcont <- 1;
      list(selInp,tags$hr());
    } else span("");
  });
  
  
  output$bmodel <- renderUI(if(!is.null(revals$frmcon)) actionButton("model","Run Analysis"));

  output$bfinal <- renderUI(if(!is.null(readyfinal<-revals$readyfinal)&&readyfinal) actionButton("final","Finalize"));

  output$download <- downloadHandler(
                       filename=function(){gsub('backups/','',revals$filename)},
                       content=function(file) if(file.exists(revals$filename)) {
                         system(sprintf('cp %s %s',revals$filename,file))
                       } else {message<-"No results to save yet."; save(message,file=file)});
  
### functions that read input from variable menus (loggable issues can happen here)
  ## function that constructs a formula, identifies the model type, and creates a cm object (loggable issues can happen here)
  ## modeltype,frm,(rfrm),(frmcon),(cm)
  observe({
    cchosen<-revals$cchosen; gchosen<-revals$gchosen; ##rchosen<-revals$rchosen; 
    if(!is.null(ychosen<-revals$ychosen)&&length(xchosen<-revals$xchosen)>0){
      rhs <- paste(xchosen,collapse="+"); lhs <- ychosen; rfrm.char <- rfrm <- cm <- NULL;
      if(!is.null(cchosen)){
        ## coxph
        modeltype <- "coxph";
        lhs <- sprintf("Surv(%s,%s==1)",ychosen,cchosen);
        ## logevent('c0015','Cox proportional hazard model auto-selected');
        ## cat('CPH case: ');
      } else if(!is.null(gchosen)){
        ## lme
        modeltype <- "lme";
        lmec<-getOption('steprand.lmec',lmeControl(opt='optim',maxIter=100,msMaxIter=100,niterEM=50,msMaxEval=400));
        options('steprand.lmec'=lmec);
        rfrm.char <- paste("~1|",gchosen); rfrm <- as.formula(rfrm.char); environment(rfrm)<-NULL;
        ## logevent('c0015','Linear mixed-effect  model auto-selected');
        ## cat('LME case: ');
      } else {
        ## lm
        modeltype <- "lm";
        ## logevent('c0015', 'Ordinary least-squares model auto-selected');
        ## cat('LM case: ');
      }
      frm.char <- paste(lhs,"~",rhs); 
      frm <- as.formula(frm.char); environment(frm)<-NULL;
      ## logevent('c0016',paste('The model selection will start with the following formula:',frm.char));
      ## print(frm.char);
      ## if any of the formula info has actually changed, update it
      if(!isTRUE(all.equal(isolate(revals$modeltype),modeltype))) revals$modeltype <- modeltype;
      if(!isTRUE(all.equal(isolate(revals$rfrm.char),rfrm.char))){
        revals$rfrm.char <- rfrm.char; revals$rfrm <- rfrm;
      };
      if(!isTRUE(all.equal(isolate(revals$frm.char),frm.char))){
        revals$frm.char <- frm.char; revals$frm <- frm; revals$frmcon <- revals$frmcon;
        ## if the formula has changed AND there is more than 1 X AND at least one X is a factor,
        ## generate a new cm (a unicontr object)
        if(length(xchosen>1)&&length(intersect(xchosen,isolate(revals$datvars$fvars)))>0){
          cm <- unicontr(frm,data=isolate(revals$trndat));
        };
        ## update revals$cm if necessary
        if(!isTRUE(all.equal(isolate(revals$cm),cm))) revals$cm <- cm;
      };
    } else {
      ## if not enough information for model, blow away the outputs
      revals$modeltype <- NULL; revals$frm.char <- NULL; revals$frm <- NULL; revals$frmcon <- NULL; revals$cm <- NULL;
    };
  },label='omod');
  ## yvar
  observe({yvar<-input$yvar;if(is.null(isolate(revals$init$myvar))){
    ecomment <- c();
    if(!is.null(yvar)&&yvar!=' '&&!isTRUE(all.equal(yvar,isolate(revals$ychosen)))){
      revals$ychosen <- yvar;
      ecomment<-sprintf('Researcher indicated that "%s" is the response variable',yvar);
    } else if(is.null(yvar)||yvar==' ') {
      if(!is.null(isolate(revals$ychosen))) ecomment <-'Researcher set the response variable to NULL';
      revals$ychosen <- NULL;
    }
    if(length(ecomment>0)) {
      logevent('c0004',ecomment);
      cat('yvar calling updatemenus\n');
      updatemenus();
    }
  } else revals$init$myvar<-NULL},label='oyvar');

  ## cvar 
  observe({cvar<-input$cvar; if(is.null(isolate(revals$init$mcvar))){
    ecomment <- c();
    if(!is.null(cvar)&&cvar!=' '&&!isTRUE(all.equal(cvar,isolate(revals$cchosen)))){
      revals$cchosen <- cvar;
      ecomment<-sprintf('Researcher indicated that "%s" is a censoring variable, implying that these are survival/time-to-event data.',cvar);
      ## catch issue
      if(!is.null(isolate(revals$gchosen))) {
        logevent('w0001','Both a censoring and a grouping/blocking variables have been chosen, implying that these are survival/time-to-event data with a clustering, frailty, or strata term. However, such models are not currently supported by this app and an ordinary Cox model will be used');
      }
    } else if(is.null(cvar)||cvar==' ') {
      if(length(isolate(revals$cchosen))>0) ecomment<-'Researcher indicated that there is no censoring variable, implying that these are not survival/time-to-event data.';
      revals$cchosen <- NULL;
    }
    if(length(ecomment>0)) logevent('c0005',ecomment);
    cat('cvar calling updatemenus\n');
    updatemenus();
    } else revals$init$mcvar<-NULL},label='ocvar');

  ## gvar
  observe({gvar<-input$gvar; if(is.null(isolate(revals$init$mgvar))){
    ecomment <- c();
    if(!is.null(gvar)&&gvar!=' '&&!isTRUE(all.equal(gvar,isolate(revals$gchosen)))){
      revals$gchosen <- gvar;
      ecomment<-sprintf('Researcher indicated that "%s" is a grouping/blocking variable, implying that a mixed-effect model should be used.',gvar);
      if(!is.null(isolate(revals$cchosen))) {
        logevent('w0001','Both censoring and grouping/blocking variables have been chosen, implying that these are survival/time-to-event data with a clustering, frailty, or strata term. However, such models are not currently supported by this app and an ordinary Cox model will be used');
      }
    } else if(is.null(gvar)||gvar==' ') {
      if(length(isolate(revals$gchosen))>0) ecomment<-'Researcher removed the grouping/blocking variable, implying that a mixed-effect model should not be used.';
      revals$gchosen <- NULL;
    }
    if(length(ecomment>0)) logevent('c0006',ecomment);
    cat('gvar calling updatemenus\n');
    updatemenus();
    } else revals$init$mgvar<-NULL},label='ogvar');

  ## rvars
  observe({rvars<-input$rvars; if(is.null(isolate(revals$init$mrvars))){
    ## myrvars <- input$rvars;
    if(length(gchosen<-isolate(revals$gchosen))>0){ cat('gchosen is ',gchosen,' in line 423\n');
      rchosen <- isolate(revals$rchosen);
      ## revals$rchosen.prev <- myrchosen <- isolate(revals$rchosen);
      ## if(length(myrvars)>0&&!isTRUE(tryCatch(all.equal(sort(myrvars),sort(myrchosen)),warning=function(e){}))){
      if(!isTRUE(tryCatch(all.equal(sort(rvars),sort(rchosen)),warning=function(e){}))){
        revals$rchosen <- rvars; cat('Setting rchosen to ',paste(rvars,collapse=','),', line 428 \n');
        ecomment <- '%s"%s" have been manually excluded from consideration for random slopes/interactions';
        ecomment <- if(length(runchosen <- setdiff(isolate(revals$rchoices),rvars))>0) {
          sprintf(ecomment,"Variables ",paste(runchosen,collapse='","'));
        } else sprintf(ecomment,"No variables","");
        logevent('c0008',ecomment)}}
      ## } else if(length(myrvars)==0) {
      ##   if(length(isolate(revals$rchosen))>0) logevent('c0008','All variables have been manually excluded from consideration for random slopes/interactions.');
      ##   revals$rchosen <- NULL; cat('Setting rchosen to NULL, line 439\n');
      ## }
  } else revals$init$mrvars<-NULL;},label='orvar');
    ## updatemenus();

  
  ## ## log changes to rvars
  ## observe({
  ##   cat('ologrvar called\n');
  ##   cat('rchosen.prev:',paste(revals$rchosen.prev,collapse=','),'\n');
  ##   cat('rchosen:',paste(revals$rchosen,collapse=','),'\n');
  ##   if(!isTRUE(all.equal(sort(revals$rchosen.prev),sort(revals$rchosen)))){
  ##     notchosen <- paste(setdiff(union(revals$rchosen.prev,isolate(revals$rchoices)),revals$rchosen),collapse='","');
  ##     chosen <- paste(revals$rchosen,collapse='","');
  ##     if(notchosen!="") logevent('c0008',
  ##                                sprintf('Researcher has indicated (directly or indirectly) that even though the variable/s "%s" are not constant within random blocks, they are either redundant with other variables or that the researcher knows of some other reason why it is impossible for these variables to meaningfully participate in random interactions/slopes.',notchosen));
  ##     if(chosen!="") logevent('c0009',
  ##                             sprintf('Researcher has allowed variable/s "%s" to be considered for inclusion as random interactions/slopes.',chosen));
  ##   };
  ## },label='ologrvar');

  ## xvars
  observe({xvars<-input$xvars; if(is.null(isolate(revals$init$mxvars))){
    ecomment <- c();
    if(length(xvars)>0&&!isTRUE(tryCatch(all.equal(sort(xvars),isolate(sort(revals$xchosen))),warning=function(e){}))){
      revals$xchosen <- xvars;
      ecomment <- sprintf('Researcher indicated that the explanatory variables are: "%s"',paste(xvars,collapse="\",\""));
    } else if(length(xvars)==0) {
      if(length(isolate(revals$xchosen))>0) ecomment <- 'Researcher removed all explanatory variables.';
      revals$xchosen <- NULL;
    }
    if(length(ecomment)>0) logevent('c0010',ecomment);
  } else revals$init$mxvars <- NULL;},label='oxvar');

  ## tvar
  observe({tvar<-input$tvar; if(is.null(isolate(revals$init$mtvar))){
    if(!is.null(tvar)&&tvar!=' '&&!isTRUE(all.equal(tvar,isolate(revals$tchosen)))){
      revals$tchosen <- tvar;
      logevent('c0009',sprintf('Researcher indicated that the variable "%s" contains information about the order in which the observations were made.',tvar));
      ## DONE: check for and LOG overlap between tchosen and cchosen or gchosen
      ## these don't per-se cause a problem, but will usually result in meaningless ACF results and may indicate user confusion
    } else if((length(tvar)==0||tvar==' ')&&length(isolate(revals$tchosen))>0) {
      revals$tchosen <- NULL; logevent('c0009','Researcher indicated that none of the variables contain information about the order in which t he observations were made.');
    }
    ## cat('tvar calling updatemenus\n');
    ## updatemenus();
  } else revals$init$mtvar <- NULL;},label='otvar');

  ## cmeth
  observe(if(!is.null(cmeth<-input$cmeth)&&cmeth!='Do not center'&&length(schosen<-isolate(revals$schosen))>0) {
    cat('cmeth calling updatemenus\n');
    updatemenus();
    logevent('c0003e',sprintf('Researcher chose to center variable/s "%s" by subtracting their respective %s.',
                              paste(schosen,collapse='","'),tolower(cmeth)))
  } else if(length(cmeth)==1&&cmeth=='Do not center') logevent('c0003e','Researcher has chosen not to center any variables.'),label='cmvar');

  ## svars
  observe({svars <- input$svars; cmeth <- input$cmeth; if(is.null(isolate(revals$init$msvars))){
    ## if(length(input$svars)>0&&!isTRUE(tryCatch(all.equal(sort(input$svars),isolate(sort(revals$schosen))),warning=function(e){}))){
    ##   revals$schosen <- input$svars;
    ## } else if(length(input$svars)==0) revals$schosen <- NULL;
    if(!isTRUE(tryCatch(all.equal(sort(svars),isolate(sort(revals$schosen))),warning=function(e){}))){
      revals$schosen <- svars;
      if(length(svars)>0) {
        logevent('c0003a',
                 sprintf('Researcher chose to center variable/s "%s" by subtracting their respective %s. If any other variables have been previously centered, they have been restored to their original form.',
                         paste(svars,collapse='","'),tolower(cmeth)))
      } else if(length(svars)==0&&length(revals$xchosen)>0&&!is.null(cmeth)&&cmeth!='Do not center'){
        logevent('c0003b','Researcher has not chosen center any variables');
      }
    }} else revals$init$msvars <- NULL},label='osvar');

  ## when a-priori contrasts specified, update model... none are specified, revert to the additive model (frm)
  observe({
    frm<-revals$frm;
    if(length(frm)>0){
      if(length(cm<-revals$cm)>0){
        if(length(cont<-input$cont)>0){
        ## addterms <- try(paste(gsub(':','*',unique(unlist(sapply(cm[cont],function(ii)
          ##                                                     attr(cm,'contrinfo')$term[apply(ii[-1,drop=F],2,
          ##                                                     function(jj) length(unique(jj))>1)],simplify=F)))),collapse="+"));
          addterms <- try(unique(unlist(sapply(cm[cont],function(ii)
                                               apply(ii[-1,,drop=F],1,function(jj)
                                                     colnames(ii)[jj!=0]),simplify=F))));
          if(class(addterms)[1]=='try-error') {
            cat('addterms errored out with',class(addterms),'\n');
            browser();
          }
          ## convert the coef-format terms to raw-format (model-format) terms
          addterms <- subset(attr(cm,'contrinfo'),raw%in%addterms)$term;
          ## swap out ':' to insure marginality and paste together
          addterms <- paste(gsub(':','*',addterms),collapse='+');
          frmcon <- try(update(frm,as.formula(paste(".~.+",addterms))));
          ## browser();
          if(class(frmcon)[1]=='try-error') {
            cat('frmcon errored out with',class(frmcon),'\n');
            browser()
          } else {
            environment(frmcon) <- NULL;
            cat('update model: ');print(frmcon);
            ## if necessary, update revals$frmcon
            if(!isTRUE(all.equal(isolate(revals$frmcon),frmcon))) {
              revals$frmcon <- frmcon;
              logevent('c0020',sprintf('Researcher selected "%s" to be a-priori contrasts, so the starting model will be: %s',
                                       paste(cont,collapse='","'),paste(capture.output(print(frmcon,showEnv=F)),collapse='')));
            }
          }
        } else {
          revals$frmcon <- frm;
          logevent('c0020',sprintf('No a-priori contrasts selected so the starting model will be: %s',
                                   paste(capture.output(print(frm,showEnv=F)),collapse='')));
        }
      } else {
        revals$frmcon <- frm;
        logevent('c0020',sprintf('There are only numeric explanatory variables, so there are no contrasts and the starting model will be: %s',
                                 paste(capture.output(print(frm,showEnv=F)),collapse='')));
      }
    }},label='ocont');

  observe({
    if(!is.null(input$final)&&input$final!=0){
      log<-do.call(rbind,isolate(relog$log));
      fits<-isolate(revals$fits);
      trndat <- isolate(revals$trndat);
      revals_list<-isolate(reactiveValuesToList(revals));
      save(log,fits,trndat,revals_list,file=isolate(revals$filename),compress='xz');
      cat('Saved session!\n');
    }});

  
### model fitting
  ## function that fits the model and does other time-consuming stuff when the user presses a button
  observe({
    if(!is.null(input$model)&&input$model!=0){
      revals$readyfinal <- F;
      logevent('c0013',sprintf('Researcher indicated readiness to proceed with analysis. So far %d analyses have been attempted on the same data',input$model-1));
      modeltype <- isolate(revals$modeltype);
      frmcon0 <- isolate(revals$frmcon);
      ## there is a null at the end of frmcon0 because we blew away its environment when creating it
      frmcon <- paste0(setdiff(capture.output(frmcon0),"NULL"),collapse="");
      ## frmcon <- paste0(frmcon[seq.int(length(frmcon)-1)],collapse="");
      trndat <- isolate(revals$trndat);
      mcall <- switch(modeltype,
                      coxph = sprintf("coxph(%s , data=trndat,na.action=na.exclude)",frmcon),
                      lme = {
                        ## rfrm <- capture.output(isolate(revals$rfrm)); rfrm <-paste0(rfrm[length(rfrm)-1],collapse="");
                        rfrm.char <- isolate(revals$rfrm.char);
                        lmec<-getOption('steprand.lmec',lmeControl(opt='optim',maxIter=100,msMaxIter=100,niterEM=50,msMaxEval=400));
                        options('steprand.lmec'=lmec);
                        sprintf("lme(%s , data=trndat,random=%s , method='ML',control=lmec,na.action=na.exclude)",
                                frmcon,rfrm.char)},
                      sprintf("lm(%s , data=trndat,na.action=na.exclude)",frmcon)
                      );
      cat('\nfrmcon: '); print(frmcon);
      logevent('c0014',paste('The following model was fit:',mcall)) 
      ## cat('mcall: '); print(mcall);
      fitstart <- try(eval(parse(text=mcall)));
      ## if(class(fitstart)[1]=='try-error') browser();
      ## update call and starting model if necessary
      if(length(isolate(revals$fits$fitstart))==0||!isTRUE(all.equal(isolate(revals$fits$fitstart$call),fitstart$call))){
        ## ...explicitly specify the data so stepAIC and steprand don't get confused
        fitstart$call$data <- trndat;
        ## do steprand if applicable, capturing to fitpreaic otherwise...
        if(class(fitstart)[1]=='lme'&&length(rvars<-isolate(input$rvars))>0){
          fitpreaic <- steprand(fitstart,rvars);
          logevent('c0021',sprintf('The following following was found (by likelihood ratios) to result in a better fit than the original random intercept model: %s',
                                   paste(capture.output(update(fitpreaic,data=trndat)$call),collapse='')));
          fitpreaic$call$control <- lmec;
        } else fitpreaic <- fitstart;
        ## do stepAIC
        fitaic <- stepAIC(fitpreaic,trace=F,direction='both',scope=list(lower=.~.,upper=.~(.)^10));
        ## return the data argument to being a name, so the output is readable
        fitstart <- update(fitstart,data=trndat); fitpreaic <- update(fitpreaic,data=trndat); fitaic <- update(fitaic,data=trndat);
        logevent('c0022',sprintf('The (fixed-effect) model selection process returned the following final model: %s',
                                 paste(capture.output(fitaic$call),collapse='')));
        ## TODO: if lm setNames(lapply(list(fitstart,fitaic),rstudent)[c(1,1,2)],c('start','preaic','aic'));
        ## else if lme setNames(lapply(list(fitstart,fitpreaic,fitaic),resid,type='pearson'),c('start','preaic','aic'));
        ## setNames(lapply(list(fitstart,fitpreaic,fitaic),fitted),c('start','preaic','aic'));
        ## ...then push them up to revals, so each plot doesn't have to calculate them
        ## update fits as needed
        if(!isTRUE(all.equal(isolate(revals$fits$fitaic$call),fitaic$call))) revals$fits$fitaic <- fitaic;
        revals$mcall <- mcall;
        revals$fits$fitstart <- fitstart;
        revals$fits$fitpreaic <- fitpreaic;
        ##  current env gets retained by model, so clean out reactivevalues (which is a duplicate anyway)
        rm(revals);
        cat('======\n fitstart: '); print(summary(fitstart));
        cat('======\n fitpreaic: '); print(summary(fitpreaic));
        cat('======\n fitaic: '); print(summary(fitaic));
       };
    }},label='ofit');

### Observe what the user says about the residuals, etc
  ## observe(if(length(
  ##              setdiff(
  ##                c(input$trend,input$nonlin,input$abstrend,input$absnonlin,input$qqldir,input$qqrdir,input$qqrange),
  ##                ' '))==8&&length(model<-isolate(input$model))>0&&model>0){
  ##   cat('Residuals examined\n');
  ##   logevent('c0040','Researcher answered all questions about residuals.');
  ##   revals$readyfinal <- T;
  ## });
  observe({
    trend<-input$trend; nonlin<-input$nonlin; abstrend<-input$abstrend; absnonlin<-input$absnonlin;
    qqrdir<-input$qqrdir; qqldir<-input$qqldir; qqrange<-input$qqrange;
    allresidin<-c(trend,nonlin,abstrend,absnonlin,qqrdir,qqldir);
    ## cat('allresidin:',paste(allresidin,collapse=','),'\n');
    ## cat('qqrange:',paste(qqrange,collapse=','),'\n');
    ## cat('lengths:',length(setdiff(allresidin,' ')),'and',length(qqrange),'\n');
    modeltype<-isolate(revals$modeltype);
    ## cat('modeltype:',modeltype,'\n');
    if(length(modeltype)>0){
      if(modeltype=='coxph'){
        revals$readyfinal <- T; logevent('c0040','Researcher examinde all residual plots');
      } else {
        ## if not a cox-ph model, these other residual plots are appropriate
        if(sum(allresidin!=' ',na.rm=T)==6&&length(qqrange)==2){
          revals$readyfinal <- T;
          logevent('c0040','Researcher examined all residual plots');
        } else revals$readyfinal <- F;
      }} else revals$readyfinal <- F});
  
  observe({trend<-input$trend; model<-isolate(input$model);
           if(length(model)>0&&model>0&&length(trend)>0&&trend!=' '){
             logevent('w0030',paste('Researcher feels that the trend in the Pearson residuals is best described as "',trend,'"'))}});
  
  observe({nonlin<-input$nonlin; model<-isolate(input$model);
           if(length(model)>0&&model>0&&length(nonlin)>0)
             if(nonlin=='Yes') logevent('w0031','Researcher feels that the trend in the Pearson residuals might be nonlinear')});

  observe({
    abstrend<-input$abstrend; model<-isolate(input$model);
    if(length(model)>0&&model>0&&length(abstrend)>0&&abstrend!=' ') {
      logevent('w0032',paste('Researcher feels that the trend in the absolute Pearson residuals is best described as "',abstrend,'"'))}});

  observe({absnonlin<-input$absnonlin; model<-isolate(input$model);
           if(length(model)>0&&model>0&&length(absnonlin)>0)
             if(absnonlin=='Yes') logevent('w0031','Researcher feels that the trend in the absolute Pearson residuals might be nonlinear')});

  observe({qqrdir<-input$qqrdir; model<-isolate(input$model);
           if(length(model)>0&&model>0&&length(qqrdir)>0)
             logevent('w0034',paste('Researcher feels that the right tail of the Pearson residuals is',
                                    switch(qqrdir,Above='larger',Below='smaller'),
                                    'than would be predicted for a normal distribution'))});

  observe({qqldir<-input$qqldir; model<-isolate(input$model);
           if(length(model)>0&&model>0&&length(qqldir)>0)
             logevent('w0035',paste('Researcher feels that the left tail of the Pearson residuals is',
                                    switch(qqldir,Above='smaller',Below='larger'),
                                    'than would be predicted for a normal distribution'))});

  observe({qqrange<-input$qqrange; model<-isolate(input$model);
           if(length(model)>0&&model>0&&length(qqrange)==2)
             logevent('w0036',
                      sprintf(
                        'Researcher feels that the left and right tails of the Pearson residuals diverge from normality at roughly the %0.1f and %0.1f standard deviations, respectively.',
                        qqrange[1],qqrange[2]))});
          
  observe(if(!is.null(gvarmulti<-input$gvarmulti)&&gvarmulti) logevent('w0037','This app does not currently support nested random effects, but the researcher indicates that these data do in fact have such a structure.'));
  observe(if(!is.null(tvarmulti<-input$tvarmulti)&&tvarmulti) logevent('w0038','This app does not currently support multiple time variables, but the researcher indicates that these data do in fact have such a structure.'));

  
### plotting
  ## functions that generate descriptive plots pre-analysis
  ## functions that generate interactive diagnostic plots
  ## DONE: put zph into its own renderplot
  ## TODO: sliders for zph
  ## TODO: update resplotabs and resplotqq to same structure as resplot
  ## zphspan lets the conditional panels know which diagnostic plots should be shown-- coxph or lm/lme
  output$zphspan <- renderUI(if(class(revals$fits$fitaic)[1]=='coxph') span(id="zphspan",style="display: none;",as.numeric(Sys.time())) else NULL);
  output$zphplot <- renderPlot(if((fitclass<-class(fitaic <- revals$fits$fitaic)[1])=='coxph'){
    coxaic <- cox.zph(fitaic); coxidx <- which.min(coxaic$table[,'p']);
    revals$zcoxrng <- range(coxaic[coxidx]$y);
    visualres(coxaic[coxidx]);
  });

  output$resplot <- renderPlot(if((fitclass<-class(fitaic <- revals$fits$fitaic)[1])%in%c('lm','lme')){
    hataic <- fitted(fitaic);
    switch(fitclass,
           lm =  { residaic <- rstudent(fitaic);
                   visualres(residaic,hataic,type='direct',xlab=paste('Estimated',isolate(revals$ychosen)),ylab='Studentized');
                 },
           lme = { residaic <- residuals(fitaic,type='pearson');
                   visualres(residaic,hataic,type='direct',xlab=paste('Estimated',isolate(revals$ychosen)),ylab='Pearson');
                 },
           {plot(NA,ylim=0:1,xlim=0:1,type='n',axes=F,xlab=NA,ylab=NA); text(.5,.5,'Error: unsupported model type')})});


  output$resplotabs <- renderPlot(if(!is.null(fitaic <- revals$fits$fitaic)){    
    hataic <- fitted(fitaic);
    if(class(fitaic)[1]=='lm') {
      residaic <- rstudent(fitaic);
      visualres(residaic,hataic,type='abs',xlab=paste('Estimated',isolate(revals$ychosen)),ylab='Studentized');
    } else if(class(fitaic)[1]=='lme'){
      residaic <- residuals(fitaic,type='pearson');
      visualres(residaic,hataic,type='abs',xlab=paste('Estimated',isolate(revals$ychosen)),ylab='Pearson');
    }
  });

  output$resplotqq <- renderPlot(if(!is.null(fitaic <- revals$fits$fitaic)){
    hataic <- fitted(fitaic); qqrange <- input$qqrange;
    revals$qqrng_temp <- range(qnorm(ppoints(nobs<-length(hataic))));
    revals$nobs <- nobs;
    if(class(fitaic)[1]=='lm') {
      residaic <- rstudent(fitaic);
      visualres(residaic,hataic,type='qq',ylab='Studentized');
    } else if(class(fitaic)[1]=='lme'){
      residaic <- residuals(fitaic,type='pearson');
      visualres(residaic,hataic,type='qq',ylab='Pearson');
    }
    if(length(qqrange)>0) abline(v=qqrange,col='blue');
  });

       ##   output$resacf <- renderImage({
       ##     browser();
       ##   # A temp file to save the output. It will be deleted after renderImage
       ##   # sends it, because deleteFile=TRUE.
       ##   outfile <- tempfile(fileext='.png')
     
       ##   # Generate a png
       ##   png(outfile, width=400, height=400)
       ##   hist(rnorm(20))
       ##   dev.off()
     
       ##   # Return a list
       ##   list(src = outfile,
       ##        alt = "This is alternate text")
       ## }, deleteFile = TRUE)
     

  output$resacf <- renderImage(if(length(isolate(revals$tchosen))>0&&(fitclass<-class(fitaic<-revals$fits$fitaic)[1])%in%c('lm','lme')){
    cat('Starting resacf\n');
    print(fitclass);
    outfile <- tempfile(fileext='.png');
    png(outfile,width=400,height=400);
    acfplot<- switch(fitclass,
           lm =  { residaic <- rstudent(fitaic);
                   acfplot<-try(acf(residaic)); dev.off(); acfplot;
                 },
           lme = try(ACF(fitaic)));
    if(class(acfplot)[1]!='try-error'){
      dev.set(2);
      plot(acfplot);
      print(outfile);
      dev.off();
      return(list(src=outfile,alt='Autocorrelation'));
    } else return(NULL);
  },deleteFile=T);

  ## observe({log<-relog$log;relog$out<-do.call(rbind,log);});
  output$log <- renderTable(do.call(rbind,relog$log));
  output$fileinfo <- renderTable(input$infile);
  output$debug <- renderText({query<-parseQueryString(clientData$url_search);paste(names(query),query,sep="=",collapse=", ")});
  ## output$mqqslider <- renderUI(if(length(qqrng_temp<-revals$qqrng_temp)>0){
  ## });
  ## functions that plot results
  ### diagnostic user response
  ## functions that read user input from diagnostic plots (loggable issues can happen here)
### cleanup 
  ## functions for creating downloads
  ## functions for debug output
});

### old notes
  ## observe({cat('Additive: '); print(revals$frm); cat('Interactions: '); print(revals$frmcon);});
  ## span(class='badge',href="#contHelp",`data-toggle`="modal","?"),
  ## tags$hr());} else span("");});
  ## output$mkcont <- renderUI({revals$contchosen<-input$cont;
  ##   if(length(input$cont)>0){list(
  ##       selectInput("kcont","Which of the comparisons you chose do you want to always be availabe and distinct even if it means including interactions that would otherwise be dropped by the model selection algorithm?",input$cont,multiple=T),span(class='badge',href="#kcontHelp",`data-toggle`="modal","?"))} else span("");});
  
  ## output$bmodel <- renderUI(if(!is.null(revals$ychosen)&&length(revals$xchosen)>0) actionButton("model","Run Analysis"));

## To implement required interactions:
  ## have another (multi=T) selectInput, this one containing only the names of cm that were chosen above
  ## the one that are chosen this time around determine which interactions must be in the model; let's say they are called ckeep, then...
  ## paste(unique(unlist(sapply(revals$cm[input$ckeep],function(ii) attr(revals$cm,'contrinfo')$term[apply(ii[-1,],2,function(jj) length(unique(jj))>1)]))),collapse="+")
  ## ...returns a set of "+" delimited terms to which a ".~.+" can be prepended to create a formula with which to update the additive model prior to AIC

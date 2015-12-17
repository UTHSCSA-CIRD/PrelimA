
codelist<-list(c0000=list(name='default',comment='Event code %c happened',advice='Carry on'),
               c0001=list(name='logtest',comment='Testing the log system',advice='Test it till it works right'));

## dir.create('backups');

shinyServer(function(input, output, session) {
  source('grad.R');
  source('modparsng.R');
  source('valcons.R');

  print("starting server");
  analyzesession <- !missing(session);
  ## object to store multiple, independent reactive values
  revals <-reactiveValues(log=list(c(event='start',time=as.character(Sys.time()))),issues=list());
  
  logevent<-function(code,issue){
    ## make a log entry, with a specific code if available
    if(code%in%names(codelist)) {
      revals$log<-c(revals$log,list(c(event=gsub('%c',code,codelist[[code]]$comment),time=as.character(Sys.time()))));
    } else revals$log<-c(revals$log,list(c(event=gsub('%c',code,codelist[[1]]$comment),time=as.character(Sys.time(),stringsAsFactors=F))));
    if(!missing(issue)){
      if(issue) revals$issues$code<-1 else revals$issues$code<-NULL;
    }
  }

  updatemenus <- function(){
    ## run when a user makes menu selections, to update availability of other variables as needed
    ## while preserving previous choices, if possible
    taken.yc <- c(revals$ychosen,revals$cchosen);
    ## constrain the available x variable choices
    ## The extra intersect statement is there to insure that ordering differences don't make it look like the choices have changed
    if(!is.null(revals$ychosen)){
      xchoices <- setdiff(revals$datvars$fvars,taken.yc);
      if(length(xchoices)==0){
        revals$xchoices <- NULL; revals$xchosen <- NULL;
      } else {
        if(!isTRUE(all.equal(sort(xchoices),sort(revals$xchoices)))){
          revals$xchoices <- xchoices;
          if(length(revals$xchosen)>0){
            xchosen <- intersect(xchoices,revals$xchosen);
            if(length(xchosen)==0) revals$xchosen <- NULL else {
              if(!isTRUE(all.equal(sort(xchosen),sort(revals$xchosen)))) revals$xchosen <- xchosen;
            }
            ## unlike for the rvalues, we do NOT default to choosing all possible values
          }}}}
  };
  print("functions defined");
### data handling
  ## Reactive functions run for their side-effect: updating the rawdat
  ## the invisible spans are there mainly so the reactive statements have a reason to execute
  ## However, could be useful for scripting because the span containing the larger number is the fresher dataset
  ## triggers if user uploads a file
  ## Might be replaced with an observe() call
  output$udataread<-renderUI(if(!is.null(input$infile)){
    revals$rawdat<-isolate(read.csv(input$infile$datapath,header=input$header,sep=input$sep,quote=input$quote));
    revals$smfit <- NULL;
    span(id="udata",style="display: none;",as.numeric(Sys.time()));
  });


  output$ouserdata <- renderTable(if(!is.null(revals$rawdat)) {
    ## use the rawdat to get dfinfo, dfvars, etc
    revals$datinfo <- info(revals$rawdat);
    revals$datvars <- possiblevars(revals$rawdat,revals$datinfo);
    ## by the current validity constraints, yvars and cvars cannot overlap, and tvars can overlap anything
    ## the former two influence what choices are available for other variables but are none of the three are
    ## influenced themselves, therefore, they are set here instead of by the updatemenus function
    ## hack
    revals$ychoices <- setdiff(revals$datvars$nvars,revals$datvars$cvars);
    revals$cchoices <- revals$datvars$cvars;
    ## trndat is redundant, clean up later
    trndat <- revals$rawdat;
    revals$trndat <- trndat;
    ## show the working dataset on the page
    trndat;
  });

### menu handling
  ## functions that create variable menus
  output$myvar <- renderUI({
    ychoices<-revals$ychoices;
    if(length(ychoices)>0){
      list(
        selectInput("yvar","Response Variable (Y):",c(" ",ychoices)),
        span(class="badge",href="#yvarHelp",`data-toggle`="modal","?")
        )
    } else if(!is.null(isolate(revals$trndat))) {
      ## if there is data, but yvars is null, it means there were no valid candidates
      ## LOG THIS
      ## then notify the user:
      div("Warning: no valid response (Y) variables found in your data. Analysis cannot proceed.",
          span(class="badge",href="#noYvarHelp",`data-toggle`="modal","?"));
    } else span("")});

  output$mcvar <- renderUI({
    cchoices <- revals$cchoices;
    if(length(cchoices)>0&&!is.null(revals$ychosen)){
      list(
        selectInput("cvar","Right Censoring Indicator:",c(" ",cchoices)),
        span(class="badge",href="#cvarHelp",`data-toggle`="modal","?"));
      ## TODO: in the else case below, checkbox asking if the user thought they had survival data and LOG
    } else span("")});


  output$mxvars <- renderUI({
    xchoices <- revals$xchoices;
    if(length(xchoices)>0&&!is.null(revals$ychosen)){
      list(
        selectInput("xvars","Predictor Variables (X):",c(" ",xchoices)),
        span(class="badge",href="#xvarHelp",`data-toggle`="modal","?")
        );} else if(!is.null(isolate(revals$ychosen))){
      ## if there is a yvar, but not enough variables left over for x, it means there were no valid predictor candidates
      ## LOG THIS, should persist even if problem fixed
      ## then notify the user:
      div("Warning: no valid predictor (X) variables found in your data. Analysis cannot proceed as currently specified. Are you sure about your choices for censoring and grouping variables?",
          span(class="badge",href="#noXvarHelp",`data-toggle`="modal","?"));
    } else span("");});

  ## Button to click to actually run analysis
  output$bmodel <- renderUI(if(!is.null(revals$ychosen)&&length(revals$xchosen)>0&&revals$xchosen!=' '&&!is.null(revals$cchosen)) actionButton("model","Run Analysis"));  

  output$smheader <- renderUI(if(!is.null(revals$smfit)){
    h4("Survomatic Results. Click help icon for explanation.",
       span(class="badge",href="#smresHelp",`data-toggle`="modal","?"))
  });

  output$smresults <- renderPrint(if(!is.null(input$model)&&input$model){
    lhs <- sprintf("Surv(%s,%s)",isolate(revals$ychosen),isolate(revals$cchosen));
    modeldata <- revals$trndat;
    smfit <- try(eval(parse(text=sprintf("survwrapperng(%s~%s,data=modeldata,doses='all',testmodels=c('g','gm','l','lm'))",
                              lhs,isolate(revals$xchosen)))[[1]]));
    if(class(smfit)[1]!='try-error') {
      revals$smfit <- smfit;
      revals$demog <- demogng(smfit$formula,modeldata);
      revals$pars <- summary.survomaticng(smfit,what='params')$Parameters;
      print.survomaticng(smfit,what=c('model','ll','hypo','fits'),models='all');
    } else print(smfit);
  });

  output$demographics <- renderTable(revals$demog,include.rownames=F);
  output$sliders <- renderUI(if(!is.null(pars <- revals$pars)){
    mins <- log(apply(do.call(cbind,pars),1,min,na.rm=T))-5;
    steps <- abs(mins)/500;
    list(
      div("See what the reference group would look under different parameter values (sliders are on log scale and start at the fitted values of the first group under the LM model)."),
      br(),
      sliderInput("a","IMR (a or lambda):",min=mins[1],max=0.0,step=steps[1],value=log(pars$lm[1,1])),
      sliderInput("b","RoA (b or gamma):",min=mins[2],max=0.0,step=steps[2],value=log(pars$lm[2,1])),
      sliderInput("c","Makeham (c):",min=mins[3],max=0.0,step=steps[3],value=log(pars$lm[3,1])),
      sliderInput("s","Logistic (s):",min=mins[4],max=0.0,step=steps[4],value=log(pars$lm[4,1]))
  )});
  output$survplot <- renderPlot(if(!is.null(demog<-revals$demog)){
    plot(0,xlim=range(demog$time),ylim=0:1,type='n',ylab='Survivorship (lx)',xlab='Time',las=1);
    lmpars <- revals$pars$lm;
    lapply(split(demog,demog$group),function(ii) {
      lines(lx~time,ii,type='s',col=ii$group[1]);
      lines(srvshp(ii$time,a=lmpars[1,ii$group[1]],b=lmpars[2,ii$group[1]],c=lmpars[3,ii$group[1]],s=lmpars[4,ii$group[1]],model='lm')~ii$time,
            col=alpha(ii$group[1],.5),lwd=4);
    });
    with(subset(demog,as.numeric(group)==1),
         lines(srvshp(time,a=exp(input$a),b=exp(input$b),c=exp(input$c),s=exp(input$s),model='lm')~time,col='blue',lwd=2,lty=3));
    legend('topright',col=seq_along(levels(demog$group)),lty=1,legend=levels(demog$group),bty='n');
  });
  output$hazplot <- renderPlot(if(!is.null(demog<-revals$demog)){
    plot(0,xlim=range(demog$time),ylim=c(0,max(demog$ux[is.finite(demog$ux)],na.rm=T)),type='n',ylab='Hazard (ux)',xlab='Time',las=1);
    lmpars <- revals$pars$lm;
    lapply(split(demog,demog$group),function(ii) {
      lines(ux~time,ii,col=ii$group[1]);
      lines(srvhaz(ii$time,a=lmpars[1,ii$group[1]],b=lmpars[2,ii$group[1]],c=lmpars[3,ii$group[1]],s=lmpars[4,ii$group[1]])~ii$time,
            col=alpha(ii$group[1],.5),lwd=4);
    });
    with(subset(demog,as.numeric(group)==1),
         lines(srvhaz(time,a=exp(input$a),b=exp(input$b),c=exp(input$c),s=exp(input$s))~time,col='blue',lwd=2,lty=3));
    legend('topright',col=seq_along(levels(demog$group)),lty=1,legend=levels(demog$group),bty='n');
  });
  output$densityplot <- renderPlot(if(!is.null(demog<-revals$demog)){
    plot(0,xlim=range(demog$time),ylim=c(0,max(demog$deaths,na.rm=T)),type='n',ylab='Deaths',xlab='Time',las=1);
    lmpars <- revals$pars$lm;
    lapply(split(demog,demog$group),function(ii) {
      lines(deaths~time,ii,col=ii$group[1]);
      lines(srvhaz(ii$time,a=lmpars[1,ii$group[1]],b=lmpars[2,ii$group[1]],c=lmpars[3,ii$group[1]],s=lmpars[4,ii$group[1]]) * 
            srvshp(ii$time,a=lmpars[1,ii$group[1]],b=lmpars[2,ii$group[1]],c=lmpars[3,ii$group[1]],s=lmpars[4,ii$group[1]],model='lm') * ii$atrisk[1] ~
            ii$time,
            col=alpha(ii$group[1],.5),lwd=4);
    });
    with(subset(demog,as.numeric(group)==1),
         lines(srvhaz(time,a=exp(input$a),b=exp(input$b),c=exp(input$c),s=exp(input$s)) *
               srvshp(time,a=exp(input$a),b=exp(input$b),c=exp(input$c),s=exp(input$s),model='lm') * atrisk[1] ~
               time,col='blue',lwd=2,lty=3));    
    legend('topright',col=seq_along(levels(demog$group)),lty=1,legend=levels(demog$group),bty='n');
  });

  ## functions that read input from variable menus (loggable issues can happen here)
  observe({
    if(!is.null(input$yvar)&&input$yvar!=' '&&!isTRUE(all.equal(input$yvar,isolate(revals$ychosen)))){
      revals$ychosen <- input$yvar;
    } else if(is.null(input$yvar)||input$yvar==' ') revals$ychosen <- NULL;
    updatemenus();
  },label='oyvar');

  observe({
    if(!is.null(input$cvar)&&input$cvar!=' '&&!isTRUE(all.equal(input$cvar,isolate(revals$cchosen)))){
      revals$cchosen <- input$cvar;
    } else if(is.null(input$cvar)||input$cvar==' ') revals$cchosen <- NULL;
    updatemenus();
  },label='ocvar');

  observe({
    if(length(input$xvars)>0&&input$xvars!=' '&&!isTRUE(all.equal(sort(input$xvars),isolate(sort(revals$xchosen))))){
      revals$xchosen <- input$xvars;
    } else if(length(input$xvars)==0||input$xvars==' ') revals$xchosen <- NULL;
  },label='oxvar');

});
    

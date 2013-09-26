## TODO: until zph is ready, make sure coxph is permitted to proceed, just with a warning
## TODO: implement issue tracker
## TODO: copy over all the fun stuff from pa to the Data Exploration tab
## TODO: result plots
## TODO: result tables
## TODO: update help files
## TODO: until ACF fixed, put in a warning for time-series data
## TODO: blow away old variable lists when data changes
## TODO: make sure tchoices are valid ones from the starting
## TODO: programmatically generate as many zph plots as needed
## TODO: ...each with a slider
## TODO: checkbox to bypass AIC altogether
## DONE: implement backups & downloads
## TODO: implement fingerprints
## TODO: if ordering variable was specified, and lm/lme, then plot ACF

lmec<-lmeControl(opt='optim',maxIter=100,msMaxIter=100,niterEM=50,msMaxEval=400);

codelist<-list(c0000=list(name='default',comment='Event code c happened',advice='Carry on'),
               c0001=list(name='Upload',comment='File uploaded.',advice=NA),
               c0002=list(name='Test data chosen',comment='A built-in R dataset was chosen.'),
               c9999=list(name='logtest',comment='Testing the log system',advice='Test it till it works right'));
if(!file.exists('www/results')){
  if(!file.exists('www')) dir.create('www');
  dir.create('www/results');
}

shinyServer(function(input, output, clientData) {
  ## analyzesession <- !missing(session);
  ## object to store multiple, independent reactive values
  ## revals <-reactiveValues(log=list(c(event='start',time=as.character(Sys.time()))),issues=list());
  firstlog <- list(); firstlog[[as.character(Sys.time())]]<-c(code='c0000',comment='Starting session.');
  relog <- reactiveValues(log=firstlog);
  revals <- reactiveValues(init=list(),schosen=NA,downloadready=F);
  srvenv <- environment();

  logevent<-function(code,comment){
    prevlog <- isolate(tail(relog$log,1))[[1]];
    if(prevlog['code']!=code || prevlog['comment']!=comment) isolate(relog$log[[as.character(Sys.time())]]<-c(code=code,comment=comment));
  }

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

  clearrevals <- function(){
    isolate(revals$downloadready <- F);
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
    clearrevals();
    span(id="udata",style="display: none;",as.numeric(Sys.time()));
    ## blow away (or back up?) old values
  });

  ## triggers if user chooses a built in dataset from menu
  output$bdataread<-renderUI(if(!is.null(input$rbuiltin)&&input$rbuiltin!=' '){
    ## might want to trap missing data type errors here, and what? Maybe warn and continue awaiting an input dataset?
    revals$rawdat<-get(input$rbuiltin);
    revals$fits <- list();
    logevent('c0002',sprintf('Researcher chose built-in dataset "%s"',input$rbuiltin));
    clearrevals();
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
    revals$filepath <- filepath <- tempfile(tmpdir='www/results',fileext='.rdata');
    revals$fileurl <- gsub('^www/','',filepath);
    revals$fileid <- gsub('^www/results/|\\.rdata','',filepath);
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
  
  ## widgets.R
  source('widgets.R',local=T);
  

  ## observers.R
  source('observers.R',local=T);
  
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



  ## The following will work on any client R session even now with the link created by this app (while the session is running):
  ## load(rawConnection(getBinaryURL("http://localhost:8100/session/5ff4072ca099abbf37d7d1131ba7cd9f/download/download")))
  
  ## output$mqqslider <- renderUI(if(length(qqrng_temp<-revals$qqrng_temp)>0){
  ## });
  ## functions that plot results
  ### diagnostic user response
  ## functions that read user input from diagnostic plots (loggable issues can happen here)
### cleanup 
  ## functions for creating downloads
  ## functions for debug output
});


## To implement required interactions:
  ## have another (multi=T) selectInput, this one containing only the names of cm that were chosen above
  ## the one that are chosen this time around determine which interactions must be in the model; let's say they are called ckeep, then...
  ## paste(unique(unlist(sapply(revals$cm[input$ckeep],function(ii) attr(revals$cm,'contrinfo')$term[apply(ii[-1,],2,function(jj) length(unique(jj))>1)]))),collapse="+")
  ## ...returns a set of "+" delimited terms to which a ".~.+" can be prepended to create a formula with which to update the additive model prior to AIC

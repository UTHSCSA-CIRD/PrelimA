## DONE: rvar
## DONE: svar, tvar
## DONE: update trndat when gvar, svar, or tvar changes
## DONE: jesus christ bananas, I might have to refactor revals. If so, separate out...
##   just choices, actually. Now that I know each top-level object inside reactiveValues
##   is an all-or-nothing dependency, might as well keep the contents of choicesnx here too
##   choiceorig can stay in revals, it only changes when there is new data
##   chosen and choesnnx don't have to exist... apparently updateSelectInput really does leave
##   everything alone except the things you tell it to update. If selected is no longer in choices,
##   it gets removed and *then* it looks like the user de-selected it and there is invalidation
##   This in turn means I can get rid of one layer of observers and have obs_chosen get invalidate
##   directly from input.
## DONE: refactor relog so that each log-entry is at top level
## DONE: have direct-to-input dependency for obs_chosen
## DONE: now re-introduce tvar, svar and friends
## DONE: re-link trndat to tvar, svar
## DONE: re-introduce rvar
## DONE: re-introduce logging
## DONE: issues
## DONE: contrasts menu
## DONE: the sidebar notice
## DONE: observer for contrasts menu
## DONE: analyze
## DONE: result table
## DONE: download link
## DONE: exploratory plots
## DONE: checkboxes for multi-groups, multi-times, etc.
## DONE: sort code into include files
## DONE: diagnostic plots, resid, abs resid
## DONE: BUG: model doesn't run without contrasts
## DONE: fix Cox
## DONE: user feedback menus for plots
## DONE: detect when lm/lme residual questions answered and permit finalizing
## DONE: Teresa's bug
## DONE: prohibit results and finalize until zph residuals finalized for coxph model
## DONE: Exclude constant variables from choices except for cvar
## DONE: hunt down and do the issues-related TODOs in code; remove the ones not associated with buttons
## DONE: residual plots when only a few categoric variables bug
## DONE: update help file
## DONE: downloadable data template
#######
## TODO: downloadable results
## TODO: corrected p-values
## TODO: bug: why no resids or results for numeric-only predictors?
## TODO: make info.data.frame trim off all-NA columns
## TODO: auto-detect quoting too
## TODO: option to remove terms from initial model
## TODO: bullseye plot when yvar chosen
## TODO: issue for any ordering variable, if not already
## TODO: issue specifically for repeated measures, if not already
## TODO: issue for collinearity
## TODO: panic button
## TODO: feedback form
## TODO: more logging
## DONE?: handle failed aic
## TODO: handle failed contrast
## TODO: figure out what to reinitialize to let analysis be run multiple times
## TODO: refactor issue database
####### lower priority
## TODO: support 3+ level factors in unicontr and then undo the hack with nunique==2 and fvars
## TODO: agreement page
## TODO: priority poll
## TODO: document the exploratory plots
## TODO: bullseye plot
## TODO: ACF
## TODO: clickable tails for qqplot

source('visualres.R');
source('info.data.frame.R');
## source('text.R');

debugmode <- T;

lmec<-lmeControl(opt='optim',maxIter=100,msMaxIter=100,niterEM=50,msMaxEval=400);

## NOTE: the difference between log and issues
## log is intended to be a kitchen-sink record of all user input and its indirect consequences
## it will contain redundant and self-cancelling information, and will be indexed by timestamp
## issues is a record of important information intended to be read by a statistician
## issues is indexed by an issue code, and whenever the condition that triggered the issue no longer
## applies, that entry is NULL-ed out
cat('\n\nStarting shinyserver\n');
shinyServer(function(input, output, session) {
### functions
  ## determine if two vectors have all the same elements without regard to order or duplicates  
  vequal<-function(xx,yy,nullvals=' '){
    if(any(xxmask<-xx%in%nullvals)) if(length(xx)==1) xx <- NULL else xx[xxmask]<-NULL;
    if(any(yymask<-yy%in%nullvals)) if(length(yy)==1) yy <- NULL else yy[yymask]<-NULL;
    err<-try(oo<-length(setdiff(xx,yy))==0&&length(setdiff(yy,xx))==0);
    if(class(err)[1]=='try-error') browser();
    oo;
  };
  ## logging
  logevent<-function(code,comment){
    relog[[as.character(Sys.time())]] <- c(code=code,comment=comment);
  };

  scrubrevals <- function(xx){for(ii in isolate(names(xx))) xx[[ii]]<-NULL} 

  ## formula wrangling function
  source('minmodel.R',local=T);
  
### initialize variables: stage 0
  ## initialize the logs and reactive values repository
  relog <- reactiveValues(); relog[[as.character(Sys.time())]] <- c(code='c0000',comment='Starting session.');
  reissue <- reactiveValues(ic0000='Starting');
  choices <- reactiveValues(); revals <- reactiveValues(stage=0); refits <- reactiveValues();
  ## refreeze is where a snapshot of certain variables is copied when the "Run Analysis" button is pressed
  refreeze <- reactiveValues();

  ## discover current URL (not sure whether it should point to clientData or session
  rooturl<-isolate(paste0(session$clientData$url_protocol,'//',session$clientData$url_hostname,
                          if(length(port<-session$clientData$url_port)>0) paste0(':',port) else "",
                          session$clientData$url_pathname));
### data handling
  ## built-in data chosen: stage 1
  observe(if(!is.null(rbuiltin<-input$rbuiltin)&&rbuiltin!=' '){
    ## obs_chosen$suspend();
    ## obs_trndat$suspend();
    obs_frm$suspend();
    scrubrevals(revals); scrubrevals(choices); scrubrevals(refits);
    revals$rawdat<-get(rbuiltin);
    revals$stage <- 1;
    logevent('c0002',sprintf('Researcher chose built-in dataset "%s"',rbuiltin));
  },priority=-4);
  
  ## user-supplied data chosen: stage 1
  observe(if(!is.null(infile<-input$infile)){
    ## obs_chosen$suspend();
    ## obs_trndat$suspend();
    obs_frm$suspend();
    scrubrevals(revals); scrubrevals(choices); scrubrevals(refits);
    ## below read.csv was wrapped in isolate, now removed... will this cause bugs?
    outcome<-try(revals$rawdat<-read.csv(datapath<-infile$datapath,header=header<-input$header,sep=sep1<-input$sep,quote=quote<-input$quote));
    ## TODO: have input$sep use text.R and update it instead of directly changing the delimiter
    if(class(outcome)[1]=='try-error'||ncol(outcome)<2){
      outcome<-try(revals$rawdat<-read.csv(datapath,header=header,sep=sep2<-setdiff(c(',','\t',';'),sep1)[1],quote=quote));
    };
    if(class(outcome)[1]=='try-error'||ncol(outcome)<2){
      outcome<-try(revals$rawdat<-read.csv(datapath,header=header,sep=sep3<-setdiff(c(',','\t',';'),c(sep1,sep2))[1],quote=quote));
    };
    ## TODO: log something if outcome is still an error
    ## blank out the built-in dataset menu choice
    updateSelectInput(session,'rbuiltin',choices=builtindatasetnames,selected=' ');
    ## TODO: generate fuzzy hashes
    ## TODO: data validation functions
    ## what stage of the analysis we are now in
    revals$stage <- 1;
    logevent('c0001',sprintf('Researcher uploaded file "%s", of size %.0f k',infile$name,infile$size));
  },priority=-4);
  
  ## whenever the raw data changes, populate choices
  observe(if(!is.null(rawdat<-revals$rawdat)){
    cat('entering observe rawdat\n');
    options(warn=-1);
    revals$datinfo<-datinfo<-info(rawdat);
    revals$datvars <- possiblevars(rawdat,datinfo);
    options(warn=0);
    ## blow away and recreate fits
    ## legal variable choices
    choicesorig <- list();
    choices$yvar <- choicesorig$yvar <- isolate(revals$datvars$yvars);
    choices$cvar <- choicesorig$cvar <- isolate(revals$datvars$cvars);
    choices$gvar <- choicesorig$gvar <- isolate(revals$datvars$gvars);
    ## The next line has a temporary hack! Wrapping in intersect instead of using
    ## fvars directly prevents factors with more than 2 levels from showing up until I can fix unicontr
    choices$xvar <- choicesorig$xvar <- isolate(c(intersect(revals$datvars$fvars,subset(datinfo,nunique==2)$name),revals$datvars$nvars));
    choices$tvar <- choicesorig$tvar <- isolate(c(revals$datvars$nvars,revals$datvars$fvars));
    choices$svar <- choicesorig$svar <- isolate(revals$datvars$nvars);
    choicesmissing <- setdiff(c('yvar','cvar','gvar','xvar','tvar','svar'),names(choicesorig));
    isolate(if(length(choicesmissing)>0) {
      reissue[['iw0001']] <- sprintf('The following choices were missing: "%s"',paste0(choicesmissing,collapse='","'));
    } else reissue[['iw0001']] <- NULL);
    revals$choicesorig <- choicesorig;
    ## switch to the next tab
    updateTabsetPanel(session,'tabset',selected='Model & Data');
    revals$filepath <- filepath <- tempfile(tmpdir='www/results',fileext='.rdata');
    revals$fileurl <- gsub('^www/','',filepath);
    revals$fileid <- fileid <- gsub('^www/results/|\\.rdata','',filepath);
    cat(' resuming obs_trndat\n');
    ## obs_trndat$resume();
    cat('leaving observe rawdat\n');
  },priority=-3);

  ## transforming/sorting data when rawdat, gvar, tvar, or svar change
  obs_trndat <- observe(
    if(!is.null(trndat <- revals$rawdat)){
      cat('entering observe trndat\n');
      ## might need to scrub them for ' '
      tchosen <- setdiff(input$tvar,' '); gchosen <- setdiff(input$gvar,' '); schosen <- input$svar;
      orderby<-unique(c(gchosen,tchosen));
      if(length(sbanned<-intersect(orderby,schosen))>0){
        schosen <- setdiff(schosen,orderby);
        logevent('w0001',
                 sprintf('The variable "%s" is not available for centering because it is an order or grouping variable.',
                         paste0(sbanned,collapse='","')));
        ## return();
      }
      datnames <- colnames(trndat);
      if(length(orderby)>0 && length(intersect(orderby,datnames))==length(orderby)) {
        logsortmsg <- sprintf('Ordering data by grouping and/or ordering variable/s "%s". ',paste0(orderby,collapse='","'));
        trndat<-trndat[do.call(order,trndat[,orderby,drop=F]),];
      } else logsortmsg <- 'Original order of data used. ';
      cat(' checking whether to center\n');
      if(length(schosen)>0 && length(svar_method<-input$svar_method)==1) {
        cat('  choosing center method\n');
        ctrs <- switch(svar_method,
                       Means=apply(trndat[,schosen,drop=F],2,mean,na.rm=T),
                       Medians=apply(trndat[,schosen,drop=F],2,median,na.rm=T),
                       Minima=apply(trndat[,schosen,drop=F],2,min,na.rm=T),
                       rep(0,len=length(schosen)));
        cat('  centering trndat\n');
        logcentmsg <- sprintf('Centering columns "%s" by their %s. ',paste0(schosen,collapse='","'),tolower(svar_method));
        trndat[,schosen] <- trndat[,schosen]-rbind(ctrs)[rep(1,len=nrow(trndat)),];
        cat('  done centering trndat\n');
      } else logcentmsg <- 'No columns in the data were centered. ';
      if(!identical(isolate(revals$trndat),trndat)) {
        cat('  updating trndat\n');
        revals$trndat <- trndat;
        logevent('c0003',logfinalmsg<-paste(logsortmsg,logcentmsg,collapse='  '));
        isolate(reissue[['ic0001']]<-logfinalmsg);
      }
      cat('resuming obs_chosen\n');
      ## obs_chosen$resume();
      cat('leaving observe trndat\n');
    },priority=2,suspended=F);

  ## let conditional UI elements know that there is a dataset loaded
  output$dataloaded <- renderUI(if(!is.null(revals$trndat)) span(style="display: none;",as.numeric(Sys.time())) else "");
  ## ...that svar/tvar menus can be displayed
  output$scale_time <- renderUI({
    lsvar<-length(choices$svar);
    ltvar<-length(choices$tvar);
    list(
      span(id='sready',style="display: none;",lsvar),
      span(id='tready',style="display: none;",ltvar)
      )
  });

### widgets
  source('widgets.R',local=T);

  output$modeltype <- renderUI(if(length(modeltype<-refreeze$modeltype)>0) span(style="display: none;",modeltype) else span(""));

  output$zphplot <- renderImage({
    zphrange <- input$zphrange;
    ## if(!is.null(input$zphresid)) browser();
    ## zphresid1 <- isolate(revals$zphresid1);
    ## revals$zphresid1 <- zphresid0 <- input$zphresid$x;
    if(class(zph<-revals$zph)[1]=='cox.zph'){
      ## residbounds <- c(zphresid0,zphresid1);
      ## dobounds <- length(residbounds)>0;
      if(length(zphrange)>0) zphbounds <- quantile(zph$x,zphrange);
      znr <- isolate(revals$znr);
      outfile <- tempfile(fileext='.png');
      png(outfile,width=wt<-400,height=ht<-znr*400+1);    
      layout(matrix(seq.int(znr),nrow=znr));
      zmeds <- apply(zph$y,2,median);
      zmeans <-colMeans(zph$y);
      for(ii in seq.int(znr)) {
        plot(zph[ii],cex=15); medline<-median(c(zmeds[ii],zmeans[ii],median(par('usr')[3:4]))); abline(h=medline,col='red');
        if(length(zphrange)>0) abline(v=zphbounds,col='blue');
      }
      dev.off();
      list(src=outfile,width=wt,height=ht);
      ## coxaic <- cox.zph(fitaic); coxidx <- which.min(coxaic$table[,'p']);
      ## revals$zcoxrng <- range(coxaic[coxidx]$y);
      ## visualres(coxaic[coxidx]);
    }});
  ## } else plot(0),height=function() 1+if(length(znr<-isolate(revals$znr))>0) znr*600 else 0);
  
  output$resplot <- renderPlot(if(length(refits$fitaic)>0 && isolate(revals$modeltype!='coxph')){
    resid <- isolate(revals$resid); fitted<-isolate(revals$fitted);
    visualres(resid,fitted,type='direct',xlab='Estimated Response',ylab='Standardized');
  });

  output$resplotabs <- renderPlot(if(length(refits$fitaic)>0 && isolate(revals$modeltype!='coxph')){
    resid <- isolate(revals$resid); fitted<-isolate(revals$fitted);
    visualres(resid,fitted,type='abs',xlab='Estimated Response',ylab='Standardized');
  });

  output$resplotqq <- renderPlot(if(length(refits$fitaic)>0 && isolate(revals$modeltype!='coxph')){
    resid <- isolate(revals$resid); fitted<-isolate(revals$fitted); qqrange <- input$qqrange;
    ## qqrange_temp <- range(qnorm(ppoints(nobs<-length(fitted))));
    visualres(resid,fitted,type='qq',ylab='Studentized');
    if(length(qqrange)>0) abline(v=qqrange,col='blue');
  });

  output$stage <- renderUI(span(id='stage',style='display: none;',revals$stage));


### observers
  source('observers.R',local=T);

  ## residual responses
  ## we don't log issues here, though. We do them once, during finalize
  observe({
    ## collect the residual inputs
    zphrange <- input$zphrange;
    resids <- list(trend=input$trend,nonlin=input$nonlin,abstrend=input$abstrend,absnonlin=input$absnonlin,qqldir=input$qqldir,qqrdir=input$qqrdir);
    qqrange <- input$qqrange;
    ready <- F; stage <- isolate(revals$stage);
    if(is.numeric(stage) && stage>=3){
      if((modeltype<-isolate(revals$modeltype))=='coxph'){
        if(length(intersect(zphrange,c(.49,.51)))==0) ready <- T;
      } else if(modeltype %in% c('lm','lme')){
        if(all(resids!=' ') && length(intersect(qqrange,c(-.05,.05)))==0) ready <- T;
      };
      ## if all the questions about residuals have been answered, we unlock stage 4
      ## if any questions get un-answered, we unset residsdone
      if(ready) {
        revals$residsdone <- 1;
        if(stage < 4) revals$stage <- 4;
      } else revals$residsdone <- NULL;
    }});
});  
cat('\n\nDone starting shinyserver\n');

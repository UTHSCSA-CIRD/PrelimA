## save session results
observe({
  if(!is.null(final<-input$final) && final!=0){
    ## technically should check to make sure length(log)>1 to avoid errors,
    ## but user shouldn't be able to press this button without having caused more than 1 logged event
    ## TODO: log the residual responses
    if(isolate(revals$modeltype)=='coxph'){
      zphrange <- isolate(input$zphrange);
      zphbounds <- quantile(isolate(revals$zph$x),zphrange);
      ## if diff(zphrange) < .7 look extra close at zph
      if((zphdiff<-diff(zphrange)) < .7) {
        isolate(reissue[['iw0010']]<-sprintf(
                                       'Researcher reported that hazards look proportional in a time interval that spans only %s of the range of the data.',
                                       zphdiff));
      } else isolate(reissue[['iw0010']] <- NULL);
      ## report zphrange either way
      isolate(reissue[['ic0010']] <- sprintf(
                                       'Researcher reported that hazards look proportional in the time interval between %.0f and %.0f',
                                       zphbounds[1],
                                       zphbounds[2]));
      isolate(reissue[['iw0011']]<-NULL); isolate(reissue[['ic0012']]<-NULL); isolate(reissue[['ic0013']]<-NULL);
      isolate(reissue[['iw0014']]<-NULL); isolate(reissue[['ic0015']]<-NULL); 
    } else {
      ## capture ('trend','nonlin','abstrend','absnonlin','qqldir','qqrdir')
      trend <- isolate(input$trend); nonlin <- isolate(input$nonlin); abstrend <- isolate(input$abstrend); absnonlin <- isolate(input$absnonlin);
      qqldir <- switch(isolate(input$qqldir),Above='thinner',Below='thicker');
      qqrdir <- switch(isolate(input$qqrdir),Above='thicker',Below='thinner');
      qqrange <- isolate(input$qqrange);
      if(trend != 'Approximately Flat') {
        isolate(reissue[['iw0011']] <- sprintf('Researcher describes the shape of the standardized residuals plotted against fitted values as "%s"',trend));
      } else isolate(reissue[['iw0011']] <- NULL);
      if(nonlin == 'Yes') {
        isolate(reissue[['iw0012']] <- 'Researcher thinks the residuals standardized plotted against fitted values might have a non-linear trend.');
      } else isolate(reissue[['iw0012']] <- NULL);
      if(abstrend != 'Approximately Flat') {
        isolate(reissue[['iw0013']] <- sprintf('Researcher describes the shape of the absolute standardized residuals plotted against fitted values as "%s"',trend));
      } else isolate(reissue[['iw0013']] <- NULL);
      if(absnonlin == 'Yes') {
        isolate(reissue[['iw0014']] <- 'Researcher thinks the absolute standardized residuals plotted against fitted values might have a non-linear trend.');
      } else isolate(reissue[['iw0014']] <- NULL);
      isolate(reissue[['iw0015']] <- sprintf(
                                       'Researcher reports that for the standardized residuals the left tail is %s and the right tail is %s than they would be if they were precisely normal. The researcher says the tails diverge from normality at the %.2f and %.2f quantiles respectively.',
                                       qqldir, qqrdir, qqrange[1], qqrange[2]));
      isolate(reissue[['iw0010']]<-NULL); isolate(reissue[['ic0010']]<-NULL);
    }
    log<-do.call(rbind,isolate(reactiveValuesToList(relog)));
    issues<-do.call(rbind,isolate(reactiveValuesToList(reissue)));
    fits<-isolate(reactiveValuesToList(refits));
    trndat <- isolate(revals$trndat);
    revals_list<-isolate(reactiveValuesToList(revals));
    save(log,lmec,fits,trndat,revals_list,.Random.seed,file=isolate(revals$filepath),compress='xz');
    revals$downloadready <- T;
    if(is.numeric(stage<-isolate(revals$stage)) && stage < 5) revals$stage <- 5;
    cat('Saved session!\n');
  } else revals$downloadready <- F});

## once we put the residual plost back in, refits$fitaic will be wrapped in isolate. Maybe.
## This creates a contrast matrix specifically for the model that was selected
## If there were no user or algorithm sepcified contrasts, fitcm is set to 0 so that it will pass the NULL check
## but still be distinguishable from an actual matrix
observe(if(length(fitaic<-(refits$fitaic))>0){
  ## lazy way to get variable names. Might break for coxph
  ## doesn't seem to break...
  termnames <- try(as.character(unifx(fitaic)$Term));
  if(class(termnames)[1]=='try-error' && debugmode) browser();
  cm <- isolate(revals$cm);
  revals$fitcm<-if(length(cm)>0) {
    do.call(rbind,lapply(isolate(cm[-1]),function(ii) {
      oo<-ii[,termnames];if(length(oo)>0&&nrow(unique(oo[-1,,drop=F]))>1) oo}));
  } else 0;
});

## DONE: observer that constructs a minimal model when input$contr changes
## DONE: why does changing contr and clicking analysis sometimes not do anything?
observe({
  cat('\n\nentering observe frmcon\n');
  frm <- isolate(revals$frm);
  if(length(contr <- input$contr)>0){
    cat('  updating starting model\n');
    cm <- isolate(revals$cm);
    ## currently running with debug option that will browse() on error
    frmcon <- minmodel(contr,frm,cm,debug=T);
    revals$frmcon <- frmcon;
  } else if(length(frm)>0) {
    cat('  simple additive starting model\n');
    revals$frmcon <- frm } else {
    cat('  starting model set to NULL\n');
    revals$frmcon <- NULL;
  }
  cat('\nleaving observe frmcon\n');
});


## these observers update the menus and are invalidated when their respective objects in choices change
obs_upd_yvar<-observe(updateSelectInput(session,'yvar',choices=c(' ',choices$yvar),selected=isolate(input$yvar)));
obs_upd_cvar<-observe(updateSelectInput(session,'cvar',choices=c(' ',choices$cvar),selected=isolate(input$cvar)),suspended=T);
obs_upd_gvar<-observe(updateSelectInput(session,'gvar',choices=c(' ',choices$gvar),selected=isolate(input$gvar)),suspended=T);
obs_upd_xvar<-observe({
  if(length(choices <- choices$xvar)==0) choices <- ' ';
  selected <- isolate(input$xvar);
  updateSelectInput(session,'xvar',choices=choices,selected=selected);
  ## session$sendInputMessage('xvar', list(options=list(list(value=selected,label=selected,selected=T))));
},suspended=T);
obs_upd_tvar<-observe(updateSelectInput(session,'tvar',choices=c(' ',choices$tvar),selected=isolate(input$tvar)),suspended=T);
obs_upd_svar<-observe({
  if(length(choices <- choices$svar)==0) choices <- ' ';
  updateSelectInput(session,'svar',choices=choices,selected=isolate(input$svar));
},suspended=T);
obs_upd_rvar<-observe({
  if(length(choices <- choices$rvar)==0) choices <- ' ';
  updateSelectInput(session,'rvar',choices=choices,selected=choices);
},suspended=T);
## contrasts
obs_upd_contr <- observe({
  if(length(choices <- names(revals$cm)[-1])==0) choices <- ' ';
  updateSelectInput(session,'contr',choices=choices);
},suspended=T);


## observer that checks for anything variable choices changing, at which point it decides what in revals choices should change
obs_chosen <- observe({
  cat('entering observe chosen\n');
  ## browser();
  chosen <- list(yvar=input$yvar,cvar=input$cvar,gvar=input$gvar,xvar=input$xvar);
  chosennoblanks <- sapply(chosen,setdiff,' ');
  ## if both y and x chosen, stage 2
  if(is.numeric(stage <- isolate(revals$stage)) && stage < 2 && length(chosennoblanks$yvar) > 0 && length(chosennoblanks$xvar) > 0) revals$stage <- 2;
  tchosen <- input$tvar; tchosennoblanks <- setdiff(tchosen,' ');
  if(!is.null(isolate(revals$trndat))) {
    choicescurr <- isolate(reactiveValuesToList(choices));
    choicesorig <- isolate(revals$choicesorig);
    chosenold <- isolate(revals$chosenold);
    rvarchoices <- NULL;
    ## no point in running the checks if no new choices have been made in y,c,g,x
    if(!identical(chosen,chosenold)){
      ## grouped survival data not currently supported, will treat as plain survival data
      ## if(length(chosennoblanks$cvar)>0&&length(chosennoblanks$gvar)>0) {
      ##   reissue[['iw0002']] <- sprintf('Grouping variable "%s" and censoring variable "%s" both specified',chosen$gvar,chosen$cvar);
      ## } else reissue[['iw0002']] <- NULL;
      taken <- unlist(chosennoblanks); #taken <- taken[taken!=' '];
      ## for y,c,g,x... this has important side effects and isn't just for logging, the logmsg is just for brevity
      logmsg <- sapply(names(chosen), function(ii) {
        ## if you already chose it you get dibs on keeping it
        iimask <- setdiff(taken,chosen[[ii]]);
        ## remove the items other variables have dibs on
        iichoices <- choicesorig[[ii]][!choicesorig[[ii]]%in%iimask];
        ## if this results in a different set of choices than currently available, update them
        if(!vequal(iichoices,choicescurr[[ii]])) choices[[ii]] <- iichoices;
        sprintf('%s="%s"',ii,paste0(chosen[[ii]],collapse='","'));
      });
      logevent('c0002',paste(logmsg,collapse='; '));
      if(length(chosen$gvar)>0 && !vequal(chosen$gvar,chosenold$gvar) && !vequal(
                                                                            choicescurr$rvar,
                                                                            rvarchoices <- setdiff(
                                                                                             isolate(revals$datvars$rvars[[chosen$gvar]]),
                                                                                             ## deliberately omitted chosen$gvar because datvars would not permit it anyway
                                                                                             c(chosen$yvar,chosen$cvar)))) {
        choices$rvar <- rvarchoices;
        ## rvar needs to be logged from a separate observer, whenever rchoices gets updated here,
        ## input$rvar will immediately default to all rchoices selected
        obs_upd_rvar$resume();
      } else obs_upd_rvar$suspend();
      revals$chosenold <- chosen;
    };
    ## TODO: maybe have svar available only for intersect(xvar,rvar)?
    if(length(chosen$xvar)+length(intersect(isolate(choices$rvar),isolate(input$rvar)))>0){
      tchoices <- choicesorig$tvar[choicesorig$tvar != chosen$yvar];
      if(!vequal(tchoices,choicescurr$tvar)) {
        choices$tvar <- tchoices;
        obs_upd_tvar$resume();
      }
      schoices <- choicesorig$svar[!choicesorig$svar %in% c(chosen$yvar,chosen$cvar,chosen$gvar,tchosen)];
      if(!vequal(schoices,choicescurr$svar)) {
        choices$svar <- schoices;
        obs_upd_svar$resume();
      }
    } #else {obs_upd_tvar$suspend(); obs_upd_svar$suspend();}
    if(length(chosennoblanks$yvar)>0&&length(chosennoblanks$xvar)>0){
      obs_frm$resume();
    } else obs_frm$suspend();
    obs_upd_yvar$resume();
    obs_upd_cvar$resume();
    obs_upd_gvar$resume();
    obs_upd_xvar$resume();
  }
  cat('leaving observe chosen\n');
});

## function that constructs a formula, identifies the model type, and creates a cm object (loggable issues can happen here)
## modeltype,frm,(rfrm),(frmcon),(cm)
obs_frm <- observe({
  cat('entering obs_frm\n');
  ychosen <- setdiff(input$yvar,' '); cchosen<-setdiff(input$cvar,' '); xchosen <- input$xvar;
  gchosen <- setdiff(input$gvar,' '); #rchosen <- input$rvar;
  if(length(ychosen)>0 && length(xchosen)>0){
    cat(' creating model parts\n');
    rhs <- paste(xchosen,collapse="+"); lhs <- ychosen; rfrm.char <- rfrm <- cm <- NULL;
    cat(' identifying model type\n');
    if(length(cchosen)>0){
      ## coxph
      modeltype <- "coxph";
      lhs <- sprintf("Surv(%s,%s==1)",ychosen,cchosen);
      ## logevent('c0015','Cox proportional hazard model auto-selected');
      ## cat('CPH case: ');
    } else if(length(gchosen)>0){
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
    cat(' pasting together formula\n');
    ## browser();
    frm.char <- paste(lhs,"~",rhs); 
    frm <- as.formula(frm.char); environment(frm)<-NULL;
    ## logevent('c0016',paste('The model selection will start with the following formula:',frm.char));
    ## print(frm.char);
    ## if any of the formula info has actually changed, update it
    cat(' checking to see if revals needs updating\n');
    if(!isTRUE(all.equal(isolate(revals$modeltype),modeltype))) revals$modeltype <- modeltype;
    if(!isTRUE(all.equal(isolate(revals$rfrm.char),rfrm.char))){
      revals$rfrm.char <- rfrm.char; revals$rfrm <- rfrm;
    };
    if(!isTRUE(all.equal(isolate(revals$frm.char),frm.char))){
      ## we set frmcon to frm instead of NULL here
      revals$frm.char <- frm.char; revals$frm <- frm; revals$frmcon <- frm; #revals$frmcon <- isolate(revals$frmcon);
      ## if the formula has changed AND there is more than 1 X AND at least one X is a factor,
      ## generate a new cm (a unicontr object)
      ## TODO: bug, unicontr breaks if a factor has more than 2 levels
      cm <- try(if(length(xchosen>1)&&length(intersect(xchosen,isolate(revals$datvars$fvars)))>0){
        unicontr(frm,data=isolate(revals$trndat),debug=T);
      } else NULL);
      if(class(cm)[1]=='try-error' && debugmode) browser();
      ## update revals$cm if necessary
      if(!isTRUE(all.equal(isolate(revals$cm),cm))) revals$cm <- cm;
    };
    obs_upd_contr$resume();
  } else {
    ## if not enough information for model, blow away the outputs
    cat(' NULL-ing out revals\n');
    revals$modeltype <- NULL; revals$frm.char <- NULL; revals$frm <- NULL; revals$frmcon <- NULL; revals$cm <- NULL;
    revals$rfrm.char <- NULL; revals$rfrm <- NULL;
  };
  cat('leaving obs_frm\n');
},suspended=T,label='obs_frm',priority=3);

## core model selection and fitting function
observe(if(!is.null(runanalysis<-input$runanalysis) && runanalysis!=0 && length(frmcon<-isolate(revals$frmcon))>0){
  cat('entering obs_analysis\n');
  logevent('c0004',sprintf('Researcher indicated readiness to proceed with analysis. So far %d analyses (including this one) have been attempted on the same data',runanalysis));
  ## start final check
  ## look for problems in variable selection, etc. and record them to the issues list
  tvar <- isolate(input$tvar); cvar <- isolate(input$cvar); gvar <- isolate(input$gvar);
  if(cvar != ' ' && gvar != ' ') {
    isolate(reissue[['iw0002']] <- sprintf('Grouping variable "%s" and censoring variable "%s" both specified',gvar,cvar));
  } else isolate(reissue[['iw0002']] <- NULL);
  if(tvar != ' '){
    if(cvar == tvar) {
      isolate(reissue[['iw0003']]<-sprintf('"%s" is both a censoring and an ordering variable',tvar));
    } else isolate(reissue[['iw0003']] <- NULL);
    if(gvar == tvar) {
      isolate(reissue[['iw0004']] <- sprintf('"%s" is both a grouping and an ordering variable',tvar));
    } else isolate(reissue[['iw0004']] <- NULL);
  } else {isolate(reissue[['iw0003']] <- NULL); isolate(reissue[['iw0004']] <- NULL);}
  if(isolate(input$gvarmulti)) {
    isolate(reissue[['iw0005']] <- 'Multiple grouping variables but only one of them is in the model.');
  } else isolate(reissue[['iw0005']] <- NULL);
  if(isolate(input$tvarmulti)) {
    isolate(reissue[['iw0006']] <- 'Multiple time/order variables but only one of them is in the model.');
  } else isolate(reissue[['iw0006']] <- NULL);
  ## done final check
  refreeze$modeltype <- modeltype <- isolate(revals$modeltype);
  refreeze$frmcon <- frmcon;
  frmcon.char <- paste(as.character(frmcon)[c(2,1,3)],collapse=' ');
  ## starting formula
  ## the non-reanalyzing bug was here!
  ## frmcon <- if(length(isolate(revals$frmcon))==0) frmcon <- frm else {
  ##   refreeze$frmcon <- frmcon <- paste0(setdiff(capture.output(frm),"NULL"),collapse="");
  ## }
  ## data
  refreeze$trndat <- trndat <- isolate(revals$trndat);
  ## construct the model fitting command
  mcall <- switch(modeltype,
                  coxph = sprintf("coxph(%s , data=trndat,na.action=na.exclude)",frmcon.char),
                  lme = {
                    rfrm.char <- isolate(revals$rfrm.char);
                    options('steprand.lmec'=lmec);
                    sprintf("lme(%s , data=trndat,random=%s , method='ML',control=lmec,na.action=na.exclude)",
                            frmcon.char,rfrm.char)},
                  sprintf("lm(%s , data=trndat,na.action=na.exclude)",frmcon.char)
                  );
  ## try fitting; the fitstartclean copy is what gets saved, because it won't have unevaluated objects in it
  fitstartclean <- fitstart <- try(eval(parse(text=mcall)));
  if(class(fitstart)[1] != 'try-error') {
    logevent('c0005',paste('The following starting model was fit:',mcall));
    isolate(reissue[['ic0002']]<-paste('The following starting model was fit:',mcall));    
    isolate(reissue[['ie0002']]<-NULL);
  } else {
    isolate(reissue[['ie0002']]<-'Failed to fit starting model.');
    isolate(reissue[['ic0002']]<-NULL);
    logevent('e0001','Failed to fit starting model.');
    revals$crashed <- 'e0002';
    if(debugmode) browser();
    ## someplace here need to dump whatever might be relevant, and send the user a signal
  };
  ## If the newly-fitted starting model is different from the previously fitted starting model, then proceed
  if(length(isolate(refits$fitstart))==0||!isTRUE(all.equal(isolate(refits$fitstart$call),fitstart$call))){
    ## ...explicitly specify the data so stepAIC and steprand don't get confused
    fitstart$call$data <- trndat;
    ## do steprand if applicable, capturing to fitpreaic otherwise...
    ## DONE?: handle failure of fitpreaic
    if(class(fitstart)[1]=='lme'&&length(rvars<-isolate(input$rvars))>0){
      fitpreaic <- try(steprand(fitstart,rvars));
      if(class(fitpreaic)[1] != 'try-error'){
        fitpreaicclean <- update(fitpreaic,data=trndat,control=lmec);
        logevent('c0021',sprintf('The following was found (by likelihood ratios) to result in a better fit than the original random intercept model: %s',
                                 deparse(fitpreaicclean$call)));
                                 ## paste(capture.output(update(fitpreaic,data=trndat)$call),collapse='')));
        isolate(reissue[['ic0003']]<-paste('After considering possible random interactions, the following model was chosen:',
                                           paste(deparse(fitpreaicclean$call),collapse=' ')));
                isolate(reissue[['ie0003']]<-NULL);
      } else {
        logevent('e0003','Unknown error during random term selection.');
        isolate(reissue[['ic0003']]<-NULL);
        isolate(reissue[['ie0003']]<-paste('Error during random term selection:',as.character(fitpreaic)));
        revals$crashed <- 'e0003';
        if(debugmode) browser();
      }
      fitpreaic$call$control <- lmec;
    } else {
      isolate(reissue[['ic0003']]<-NULL); isolate(reissue[['ie0003']]<-NULL);
      fitpreaicclean <- fitpreaic <- fitstart;
    }
    ## do stepAIC
    ## DONE?: handle failure of stepAIC
    fitaic <- try(stepAIC(fitpreaic,trace=F,direction='both',scope=list(lower=.~.,upper=.~(.)^10)));
    if(class(fitaic)[1]!='try error'){
      isolate(reissue[['ic0004']]<- paste('Bidirectional stepwise model search on AIC selected the following final model:',
                                          paste(deparse((fitaic<-update(fitaic,data=trndat))$call),collapse=' ')));
      isolate(reissue[['ie0004']]<- NULL);
    } else {
      isolate(reissue[['ic0004']]<- NULL);
      isolate(reissue[['ie0004']]<- paste('Error during setpAIC:',as.character(fitaic)));
      revals$crashed <- 'e0004';
      if(debugmode) browser();
    }
    ## return the data argument to being a name, so the output is readable
    ## fitstart <- update(fitstart,data=trndat); #fitpreaic <- update(fitpreaic,data=trndat); #fitaic <- update(fitaic,data=trndat);
    ## logevent('c0022',sprintf('The (fixed-effect) model selection process returned the following final model: %s',
    ##                          paste(capture.output(fitaic$call),collapse='')));
    if(!isTRUE(all.equal(isolate(refits$fitaic$call),fitaic$call))) refits$fitaic <- fitaic;
    revals$mcall <- mcall;
    ## fitstart and fitpreaic have ugly evaluated junk in them, so rather than updating again, use the clean copies we made
    refits$fitstart <- fitstartclean; refits$fitpreaic <- fitpreaicclean;
    ## get printable formulas for later; formula wrapped in formula because sometimes they are actually terms
    ## capture.output might not be necessary
    revals$formulas_fixed <- paste0(capture.output(print(formula(if(modeltype=='lme') fitaic$call$fixed else {
      fitaic$call$formula }),showEnv=F)),collapse='');
    if(modeltype=='lme') revals$formulas_random <- paste0(capture.output(print(formula(fitaic$call$random),showEnv=F)),collapse='');
    revals$fitted <- fitted(fitaic);
    revals$resid <- if(modeltype=='lme') residuals(fitaic,type='pearson') else if(modeltype=='lm') rstudent(fitaic) else 0;
    revals$zph <- zph <- if(modeltype=='coxph') cox.zph(fitaic) else 0;
    revals$znr<- if(length(zph)>1) nrow(zph$table)-1 else 0;
    ## upon completion of this analysis we are in stage 3
    if(is.numeric(stage<-isolate(revals$stage)) && stage < 3) revals$stage <- 3;
    ## DONE: issues for the calls and formulas
    ## DONE: issues for cvar&gvar, multiple gvars, multiple tvars, etc
    revals$latestaction <- 'runanalysis';
    ## DONE: possibly, put the below updates inside the above enclosure
    for(ii in c('trend','nonlin','abstrend','absnonlin','qqldir','qqrdir')) updateSelectInput(session,ii,choices=static_choices[[ii]],selected=' ');
    cat('  updated resid menus\n');
    updateSliderInput(session,'qqrange',value=c(-.05,.05));
    cat('  updated qqslider\n');
    updateSliderInput(session,'zphrange',value=c(.49,.51));
    cat('  updated zphslider, leaving observe_lastaction\n');
  }; 
  ## for(ii in c('trend','nonlin','abstrend','absnonlin','qqldir','qqrdir')) session$sendInputMessage(ii,list(selected=' '));
  cat('leaving obs_analysis\n');
},label='obs_analysis');

### crash handling

## observe({revals$residsdone;browser()});

output$badnews <- renderUI(if(length(revals$crashed)>0){
  revals_list <- isolate(reactiveValuesToList(revals));
  input_list <- isolate(reactiveValuesToList(input));
  relog_list <- isolate(reactiveValuesToList(relog));
  reissue_list <- isolate(reactiveValuesToList(reissue));
  refits_list <- isolate(reactiveValuesToList(refits));
  save(revals_list,input_list,relog_list,reissue_list,refits_list,globEnv,file=paste0('www/crash/',fileid<-isolate(revals$fileid),'.rdata'));
  h3(paste("A crash has occurred, and analysis cannot proceed further. Please contact alex.bokov@gmail.com with a description of what you witnessed and this tracking number:",fileid));
});

### logging
output$log <- renderTable({
  oo<-do.call(rbind,reactiveValuesToList(relog));
  oo[order(rownames(oo)),]
},include.colnames=F,caption.placement='top',caption='Activity Log',label='my label');

output$issues <- renderTable({
  oo<-do.call(rbind,reactiveValuesToList(reissue));
  oo[order(rownames(oo)),,drop=F]
},include.colnames=F,caption.placement='top',caption='Issues');

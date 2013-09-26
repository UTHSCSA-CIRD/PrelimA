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
    ## cat('yvar calling updatemenus\n');
  }
  updatemenus();
} else revals$init$myvar<-NULL},label='oyvar');

## cvar 
observe({cvar<-input$cvar; if(is.null(isolate(revals$init$mcvar))&&!is.null(isolate(revals$ychosen))){
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
observe({gvar<-input$gvar; if(is.null(isolate(revals$init$mgvar))&&!is.null(isolate(revals$ychosen))>0){
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
  if(length(gchosen<-isolate(revals$gchosen))>0){
    cat('gchosen is ',gchosen,' in line 423\n');
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
    save(log,lmec,fits,trndat,revals_list,.Random.seed,file=isolate(revals$filepath),compress='xz');
    revals$downloadready <- T;
    cat('Saved session!\n');
  } else revals$downloadready <- F});


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
                      ## lmec<-getOption('steprand.lmec',lmeControl(opt='optim',maxIter=100,msMaxIter=100,niterEM=50,msMaxEval=400));
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

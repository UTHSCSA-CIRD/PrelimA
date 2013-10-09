shinyServer(function(input, output, session) {
  ## determine if two vectors have all the same elements without regard to order or duplicates  
  vequal<-function(xx,yy,nullvals=' '){
    xx[xx%in%nullvals]<-NULL;
    yy[yy%in%nullvals]<-NULL;
    length(setdiff(xx,yy))==0&&length(setdiff(yy,xx))==0;
  };
  
  ## initialize the log
  firstlog <- list(); 
  ## firstlog[[as.character(Sys.time())]]<-'started'
  relog <- reactiveValues(log=firstlog);
  ## define logging function
  ## logevent<-function(code,comment){
  ##   ## prevlog <- isolate(tail(relog$log,1))[[1]];
  ##   ## if(prevlog['code']!=code || prevlog['comment']!=comment)
  ##   isolate(relog$log[[as.character(Sys.time())]]<-c(code=code,comment=comment));
  ## };
  revals <- reactiveValues(choicesorig=list(
                             pulldown01=letters[1:4],
                             pulldown02=letters[3:6],
                             pulldown03=letters[2:5]),
                           chosen=list());
                           ## taken=c(),chosen=list());
  isolate(revals$choices <- choicesorig <- revals$choicesorig);
  
  ## convenience wrapper for updating menus
  updmenu <- function(tag,choices=revals$choices[[tag]],nullval=' '){
    ## tag = character, name of an object that does/will exist in revals, input, output
    ## choices = character vector
    ## nullval = what to use as the NULL value in pull-down lists. Set it to NULL in multi-select lists.
    chosen<-isolate(revals$chosen[[tag]]);
    if(length(intersect(chosen,choices))==0) chosen <- NULL;
    updateSelectInput(session,tag,choices=c(nullval,choices),selected=chosen);
  };

  ## these observers intercept user input and update revals$chosen
  observe(if(length(chosen <- input$pulldown01)>0){
    cat('entering obs01\n');
    chosen <- chosen[chosen!=' '];
    chosenold <- isolate(revals$chosen$pulldown01);
    cat('chosenold:',paste(chosenold,collapse=', '),'\n');
    cat('chosen:',paste(chosenold,collapse=', '),'\n');
    if(!vequal(chosenold,chosen)) {
      isolate(revals$chosen$pulldown01 <- chosen);
      relog$log$o01 <- if(length(chosen)>0) sprintf('%s selected',paste0(chosen,collapse=',')) else NULL;
    } 
    cat('leaving obs01\n');
  },label='obs_pulldown01',priority=1);

  observe(if(length(chosen <- input$pulldown02)>0){
    cat('entering obs02\n');
    chosen <- chosen[chosen!=' '];
    chosenold <- isolate(revals$chosen$pulldown02);
    cat('chosenold:',paste(chosenold,collapse=', '),'\n');
    cat('chosen:',paste(chosenold,collapse=', '),'\n');
    if(!vequal(chosenold,chosen)) {
      isolate(revals$chosen$pulldown02 <- chosen);
      relog$log$o02 <- if(length(chosen)>0) sprintf('%s selected',paste0(chosen,collapse=',')) else NULL;
    }
    cat('leaving obs02\n');
  },label='obs_pulldown02',priority=2);

  observe(if(length(chosen <- input$pulldown03)>0){
    cat('entering obs03\n');
    chosen <- chosen[chosen!=' '];
    chosenold <- isolate(revals$chosen$pulldown03);
    cat('chosenold:',paste(chosenold,collapse=', '),'\n');
    cat('chosen:',paste(chosenold,collapse=', '),'\n');
    if(!vequal(chosenold,chosen)) {
      isolate(revals$chosen$pulldown03 <- chosen);
      relog$log$o03 <- if(length(chosen)>0) sprintf('%s selected',paste0(chosen,collapse=',')) else NULL;
    }
    cat('leaving obs03\n');
  },label='obs_pulldown03',priority=3);

  observe(if(length(chosen <- input$pulldown04)>0){
    cat('entering obs04\n');
    chosen <- chosen[chosen!=' '];
    chosenold <- isolate(revals$chosen$pulldown04);
    cat('chosenold:',paste(chosenold,collapse=', '),'\n');
    cat('chosen:',paste(chosenold,collapse=', '),'\n');
    if(!vequal(chosenold,chosen)) {
      isolate(revals$chosen$pulldown04 <- chosen);
      relog$log$o04 <- if(length(chosen)>0) sprintf('%s selected',paste0(chosen,collapse=',')) else NULL;
    }
    cat('leaving obs04\n');
  },label='obs_pulldown04',priority=4);

  ## these observers update the menus and are invalidated when their respective objects in revals$choices change
  observe(updmenu('pulldown01'),label='obs_update_pulldown01',priority=11);
  observe(updmenu('pulldown02'),label='obs_update_pulldown02',priority=12);
  observe(updmenu('pulldown03'),label='obs_update_pulldown03',priority=13);
  observe(updmenu('pulldown04'),label='obs_update_pulldown04',priority=14);

  ## observer that checks for anything within revals$chosen changing, at which point it decides what in revals choices should change
  observe({
    cat('entering obs chosen\n');
    chosen<-revals$chosen;
    taken <- unlist(chosen);
    ## to prevent endless update-loop if some asshole chooses
    ## conflicting menu options faster than reactive update
    ## can happen
    if(any(table(taken)>1)){
      isolate(revals$chosen <- list());
      taken <- NULL;
    }
    choicesorig<-isolate(revals$choicesorig);
    choicesold<-isolate(revals$choices);
    cat('  taken:',paste(taken,collapse=', '),'\n');
    choicesnew<-lapply(choicesorig,setdiff,taken);
    for(ii in names(choicesnew)){
      if(!vequal(choicesold[[ii]],iichoices<-union(chosen[[ii]],choicesnew[[ii]]))) {
        cat('  for',ii,paste0(choicesold[[ii]],collapse=', '),'!=',paste0(choicesnew[[ii]],collapse=', '),'\n');
        revals$choices[[ii]]<-iichoices;
      }
    }
    cat('exiting obs chosen\n');
  },label='obs_chosen',priority=100);

  output$log <- renderTable({do.call(rbind,relog$log)});
});  

shinyServer(function(input, output, session) {
  ## determine if two vectors have all the same elements without regard to order or duplicates  
  vequal<-function(xx,yy,nullvals=' '){
    xx[xx%in%nullvals]<-NULL;
    yy[yy%in%nullvals]<-NULL;
    length(setdiff(xx,yy))==0&&length(setdiff(yy,xx))==0;
  };
  
  ## check whether this is via a real website
  ## analyzesession <- !missing(session);
  ## initialize the log
  firstlog <- list(); ##firstlog[[as.character(Sys.time())]]<-c(code='c0000',comment='Starting session.');
  firstlog[[as.character(Sys.time())]]<-'started'
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
  
  ## convenience wrapper for generating dynamic menus
  ## rmenu <- function(tag,label,choices,chosen=isolate(revals$chosen[[tag]]),multiple=F,nullval=' '){
  ##   ## tag, label = character (tag is the one that names the output object)
  ##   ## choices, chosen = character vectors
  ##   ## nullval = what to set as the default choice; set to NULL to not insert one
  ##   selectInput(tag,label,c(nullval,union(chosen,choices)),selected=chosen,multiple=multiple);
  ## }
  
  ## output$pulldown01 <- renderUI({
  ##   ## choices <- revals$choicesorig$pulldown01;
  ##   rmenu('pulldown01','Pulldown 01',isolate(choicesorig$pulldown01));
  ## });

  ## output$pulldown02 <- renderUI({
  ##   ## choices <- revals$choicesorig$pulldown02;
  ##   rmenu('pulldown02','Pulldown 02',isolate(choicesorig$pulldown02));
  ## });

  ## output$pulldown03 <- renderUI({
  ##   rmenu('pulldown03','Pulldown 03',isolate(choicesorig$pulldown03));
  ## });

  updmenu <- function(tag,choices=revals$choices[[tag]],nullval=' '){
    chosen<-isolate(revals$chosen[[tag]]);
    if(length(intersect(chosen,choices))==0) chosen <- NULL;
    updateSelectInput(session,tag,choices=c(nullval,choices),selected=chosen);
  };
  
  observe(if(length(chosen <- input$pulldown01)>0){
    cat('entering obs01\n');
    chosen <- chosen[chosen!=' '];
    chosenold <- isolate(revals$chosen$pulldown01);
    cat('chosenold:',paste(chosenold,collapse=', '),'\n');
    cat('chosen:',paste(chosenold,collapse=', '),'\n');
    if(!vequal(chosenold,chosen)) isolate(revals$chosen$pulldown01 <- chosen);
    cat('leaving obs01\n');
  });

  observe(if(length(chosen <- input$pulldown02)>0){
    cat('entering obs02\n');
    chosen <- chosen[chosen!=' '];
    chosenold <- isolate(revals$chosen$pulldown02);
    cat('chosenold:',paste(chosenold,collapse=', '),'\n');
    cat('chosen:',paste(chosenold,collapse=', '),'\n');
    if(!vequal(chosenold,chosen)) isolate(revals$chosen$pulldown02 <- chosen);
    cat('leaving obs02\n');
  });

  observe(if(length(chosen <- input$pulldown03)>0){
    cat('entering obs03\n');
    chosen <- chosen[chosen!=' '];
    chosenold <- isolate(revals$chosen$pulldown03);
    cat('chosenold:',paste(chosenold,collapse=', '),'\n');
    cat('chosen:',paste(chosenold,collapse=', '),'\n');
    if(!vequal(chosenold,chosen)) isolate(revals$chosen$pulldown03 <- chosen);
    cat('leaving obs03\n');
  });

  observe(if(length(chosen <- input$pulldown04)>0){
    cat('entering obs04\n');
    chosen <- chosen[chosen!=' '];
    chosenold <- isolate(revals$chosen$pulldown04);
    cat('chosenold:',paste(chosenold,collapse=', '),'\n');
    cat('chosen:',paste(chosenold,collapse=', '),'\n');
    if(!vequal(chosenold,chosen)) isolate(revals$chosen$pulldown04 <- chosen);
    cat('leaving obs04\n');
  });

  ## observe({
  ##   choices<-revals$choices$pulldown01;
  ##   chosen<-isolate(revals$chosen$pulldown01);
  ##   if(length(intersect(chosen,choices))==0) chosen <- NULL;
  ##   ## if(!chosen %in% choices) chosen <- NULL;
  ##   updateSelectInput(session,"pulldown01",choices=c(' ',choices),selected=chosen);
  ## });
  observe(updmenu('pulldown01'));
  observe(updmenu('pulldown02'));
  observe(updmenu('pulldown03'));
  observe(updmenu('pulldown04'));
  ## observe({
  ##   choices<-revals$choices$pulldown02;
  ##   chosen<-isolate(revals$chosen$pulldown02);
  ##   if(length(intersect(chosen,choices))==0) chosen <- NULL;
  ##   updateSelectInput(session,"pulldown02",choices=c(' ',choices),selected=chosen);
  ## });

  ## observe({
  ##   choices<-revals$choices$pulldown03;
  ##   chosen<-isolate(revals$chosen$pulldown03);
  ##   if(length(intersect(chosen,choices))==0) chosen <- NULL;
  ##   updateSelectInput(session,"pulldown03",choices=c(' ',choices),selected=chosen);
  ## });

  ## observe({
  ##   choices<-revals$choices$pulldown04;
  ##   chosen<-isolate(revals$chosen$pulldown04);
  ##   if(length(intersect(chosen,choices))==0) chosen <- NULL;
  ##   updateSelectInput(session,"pulldown04",choices=c(' ',choices),selected=chosen);
  ## });

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
  });

  output$log <- renderTable({do.call(rbind,relog$log)});
});  

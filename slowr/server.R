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
  revals <- reactiveValues(choicesorig=list(pulldown01=letters[1:4],pulldown02=letters[3:6],pulldown03=letters[2:5]),
                           chosen=list());
                           ## taken=c(),chosen=list());
  isolate(revals$choices <- revals$choicesorig);

  rmenu <- function(tag,caption,choices,chosen,nullval=' '){
    selectInput(tag,caption,c(nullval,union(chosen,choices)),selected=chosen);
  }
  
  output$pulldown01 <- renderUI({
    cat('entering pulldown01\n');
    ## taken <- revals$taken;
    choices <- revals$choices$pulldown01;
    chosen <- isolate(revals$chosen$pulldown01);
    ## revals$pulldown01init <- 1;
    isolate(relog$log[[as.character(Sys.time())]]<-'Rendered pulldown01');
    cat('exiting pulldown01\n');
    selectInput('pulldown01','Pulldown 01',c(' ',union(chosen,choices)),selected=chosen);
    ## selectInput("pulldown01","Pulldown 01",c(' ',setdiff(isolate(revals$choices),taken)));
    ## selectInput("pulldown01","Pulldown 01",c(' ',union(chosen,setdiff(isolate(revals$choices),taken))),selected=chosen);
  });

  output$pulldown02 <- renderUI({
    cat('entering pulldown02\n');
    ## taken <- revals$taken;
    choices <- revals$choices$pulldown02;
    chosen <- isolate(revals$chosen$pulldown02);
    ## revals$pulldown02init <- 1;
    isolate(relog$log[[as.character(Sys.time())]]<-'Rendered pulldown02');
    cat('exiting pulldown02\n');
    selectInput('pulldown02','Pulldown 02',c(' ',union(chosen,choices)),selected=chosen);
    ## selectInput("pulldown02","Pulldown 02",c(' ',union(chosen,setdiff(isolate(revals$choices),taken))),selected=chosen);
  });
  
  observe(if(length(chosen <- input$pulldown01)>0){
    cat('entering obs01\n');
    ## if(is.null(isolate(revals$pulldown01init))){
    ## if(chosen == ' ') chosen <- NULL;
    chosen[chosen==' ']<-NULL;
    chosenold <- isolate(revals$chosen$pulldown01);
    if(!vequal(chosenold,chosen)) isolate(revals$chosen$pulldown01 <- chosen);
    ## takenold <- isolate(revals$taken);
    ## taken <- union(setdiff(takenold,chosenold),chosen);
    ## cat('taken:',paste(taken,collapse=', '),'\n');
    ## if((length(takenold)>0||length(taken)>0)&&(length(setdiff(takenold,taken))>0||length(setdiff(taken,takenold))>0)){
    ##   cat('updating taken\n');
    ##   revals$taken <- taken;
    ## }
    ## } else isolate(revals$pulldown01init <- NULL);
    cat('leaving obs01\n');
  });

  observe(if(length(chosen <- input$pulldown02)>0){
    cat('entering obs02\n');
    ## if(is.null(isolate(revals$pulldown02init))){
    ## if(chosen == ' ') chosen <- NULL;
    chosen[chosen==' ']<-NULL;
    chosenold <- isolate(revals$chosen$pulldown02);
    if(!vequal(chosenold,chosen)) isolate(revals$chosen$pulldown02 <- chosen);
    ## takenold <- isolate(revals$taken);
    ## browser();
    ## taken <- union(setdiff(takenold,chosenold),chosen);
    ## cat('taken:',paste(taken,collapse=', '),'\n');
    ## if((length(takenold)>0||length(taken)>0)&&(length(setdiff(takenold,taken))>0||length(setdiff(taken,takenold))>0)){
    ##   cat('updating taken\n');
    ##   revals$taken <- taken;
    ## }
    ## } else isolate(revals$pulldown02init <- NULL);
    cat('leaving obs02\n');
  });

  observe({
    cat('entering obs chosen\n');
    chosen<-revals$chosen;
    choicesorig<-isolate(revals$choicesorig);
    choicesold<-isolate(revals$choices);
    taken <- unlist(chosen);
    cat('taken:',paste(taken,collapse=', '),'\n');
    choicesnew<-lapply(choicesorig,setdiff,taken);
    ## browser();
    for(ii in names(choicesnew)){
      if(!vequal(choicesold[[ii]],choicesnew[[ii]])) revals$choices[[ii]]<-choicesnew[[ii]];
    }
  });

  ## observe({
  ##   pulldown<-input$pulldown;
  ##   logevent("c0001",sprintf('Detected pulldown input "%s"',pulldown));
  ##   cat('multichoices set\n');
  ##   ## browser();
  ##   revals$autochosen<-revals$choices[switch(pulldown,None=c(),Some=seq(1,6,by=2),Some2=seq(1,6,by=2),Others=seq(2,6,by=2),All=1:6)];
  ## });
  ## output$multimenu <- renderUI({
  ##   choices <- revals$choices; autochosen <- revals$autochosen; if(length(autochosen)==0) autochosen<-NULL;
  ##   logevent("c0002",sprintf('Populating selection list with "%s" of which "%s" are chosen',paste(choices,collapse='","'),paste(autochosen,collapse='","')));
  ##   revals$multinit<-T;
  ##   cat('multimenu created\n');
  ##   selectInput("multichosen","Select Manual Choices:",c(sample(letters[-(1:6)],3),choices),selected=sample(autochosen,length(autochosen)),multiple=T);
  ## });
  ## observe({
  ##   input$multichosen;
  ##   if(isolate(revals$multinit)) {
  ##     revals$multinit<-F;
  ##     logevent('c0004','Init values detected');
  ##   } else {
  ##     logevent("c0003",sprintf('Items "%s" manually chosen from multimenu',paste(input$multichosen,collapse='","')));
  ##   }
  ## });
  output$log <- renderTable({do.call(rbind,relog$log)});
});  

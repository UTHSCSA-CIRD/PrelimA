## prompts
output$notice <- renderUI({
  ## originally this was revals$frmcon and might need to go back to being that
  frmcon <- revals$frm;
  ychoices <- setdiff(input$yvar,' ');
  fitaic <- refits$fitaic;
  readyfinal <- revals$readyfinal;
  downloadready <- revals$downloadready;
  fileurl <- isolate(revals$fileurl); #rooturl <- isolate(revals$rooturl);
  msg <- if(!is.null(downloadready)&&downloadready){
    ## the below needs to automatically generate the first part of the URL
    list("Please copy the URL of the link below and keep it in a safe location, it is the only way to access your results later. We recommend that you",
         a("email the link",
           href=sprintf('mailto:?subject=Preliminary analysis output&body=I have done some preliminary analysis and here are the results. %s%s',rooturl,fileurl),
           target='_blank'),
         "to a statistician of your choice for review. The link leads to an R workspace file that contains all the information necessary to repeat and extend this analysis. If you wish to do analysis on a new dataset, for the time being we recommend that you reload this page (press F5).");
  } else if(!is.null(readyfinal)&&readyfinal) {
    "Please click the 'Finalize' button on the 'Model' tab to generate a download link that you can send to a statistician for review.";
  } else if(!is.null(fitaic)) {
    "To proceed, please answer all the questions that accompany the diagnostic plots on the 'Contrasts & Diagnostics' tab.";
  } else if(!is.null(frmcon)) {
    "After you have finished specifying your model, please go to the 'Contrasts & Diagnostics' tab, choose the comparisons necessary ofr your hypothesis, and click the 'Run Analysis' button.";
  } else if(!is.null(revals$rawdat)) {
    "Data uploaded. Please specify the model on the 'Model & Data' tab, starting with your response (Y) variable.";
  } else "On the 'Upload' tab, please upload your data or choose a sample dataset.";
  list(h5(msg),br());
});

## result table
output$ounifx <- renderUI({
  fitaic <- isolate(refits$fitaic);
  if(!is.null(revals$residsdone)){
    results <- if(length(fitcm<-isolate(revals$fitcm))>0){
      unifx(fitaic,drop=c('Y','Model','Call','p'),linfct=fitcm);
    } else unifx(fitaic,drop=c('Y','Model','Call'));
    colnames(results)<-paste0(colnames(results),"&nbsp;<span class='badge' href='#col",colnames(results),"Help' data-toggle='modal'>?</span>");
    ## TODO: someplace here put in the code for downloading results
    list(
      h4("Final Results"),
      HTML(renderTable(results,include.rownames=F,sanitize.text.function=identity)())
      );  
  } else span("")})

## Download link
output$download <- renderUI(if(length(downloadready<-revals$downloadready)>0&&downloadready){
  ## in the server version, need to construct a prefix path leading to the app named 'downloader' (in the same user's directory)
  ## url <- paste0("?id=",isolate(id<-revals$fileid));
  fileurl <- paste0(rooturl,isolate(revals$fileurl)); list(a(fileurl,href=fileurl,target='_blank'),p());
} else div(""));

## functions that generate descriptive plots pre-analysis
output$corheatmap <- renderPlot(if(!is.null(trndat<-revals$trndat)){
  heatmap(cor(data.matrix(trndat[,isolate(revals$datinfo$nunique>1)]),use='pairwise.complete'),symm=T,scale='none');
});

output$corstarmap <- renderPlot(if(!is.null(trndat<-revals$trndat)){sphpca(as.data.frame(cor(data.matrix(trndat[,isolate(revals$datinfo$nunique>1)]),use='pairwise.complete')))});

output$splom <- renderPlot({
  yvar <- input$yvar; trndat <- revals$trndat;
  if(!is.null(trndat)){
    if(length(yvar)>0 && yvar!=' ') trndat <- trndat[,c(yvar,setdiff(names(trndat),yvar))];
    ## maybe a cleaner way to do scatter plots
    trndat <- data.frame(sapply(trndat,function(ii) {
      if(!is.numeric(ii)) ii<-as.numeric(factor(ii));
      if(length(unique(ii))<10) ii <- jitter(ii);
      ii;
    }));
    pairs(trndat,panel=function(...) smoothScatter(...,add=T,nrpoints=5,nbin=64));
    ## plot(data.frame(sapply(trndat,
    ##                        function(ii) if(!is.numeric(ii))
    ##                        jitter(as.numeric(factor(ii))) else if(length(unique(ii))<length(ii)/2)
    ##                        jitter(ii) else ii)),pch='.')
  }});
                           
output$ouserdata <- renderTable(revals$trndat);

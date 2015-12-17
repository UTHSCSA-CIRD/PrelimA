## library(compiler);
## enableJIT(3); compilePKGS(T);
## setCompilerOptions(optimize=3);
library(bru);
library(shiny);
##library(shinyIncubator);
## the below is used by the smoothed pairs plot in Model & Data
library(KernSmooth);
library(psy);
library(MASS);
library(car);
library(digest);
library(datasets);
source('unicontr.R');
source('visualres.R');

globEnv<-environment();
options(digits.secs=100);

builtindatasetnames <- c(' ',
                         'CO2','DNase','LifeCycleSavings','Loblolly','Orange',
                         'PlantGrowth','Puromycin','Theoph','USArrests','airquality','anscombe',
                         'attenu','beaver1','beaver2','chickwts','freeny',
                         'mtcars','rock','sleep','swiss','trees',
                         'cancer','lung',
                         'mgus','mgus1','ovarian','pbc','stanford2','tobin');

## Not really used yet, but will be a lookup table for repetitive values used by log and/or issues
codelist<-list(c0000=list(name='default',comment='Event code c happened',advice='Carry on'),
               c0001=list(name='Upload',comment='File uploaded.',advice=NA),
               c0002=list(name='Test data chosen',comment='A built-in R dataset was chosen.'),
               c9999=list(name='logtest',comment='Testing the log system',advice='Test it till it works right'));

if(!file.exists('www')) dir.create('www');
if(!file.exists('www/results')) dir.create('www/results');
if(!file.exists('www/crash')) dir.create('www/crash');


helpbutton <- function(xx,hprefix='#',hsuffix='Help',class='badge',text="?",data_toggle="modal") {
  ## xx = character
  ## Below is to lazily identify files that aren't written yet
  if(!(helpentry<-paste0(xx,hsuffix)) %in% names(helpList)) cat('missing entry for',helpentry,'\n',file='www/missinghelp.txt',append=T);
  span(class=class,href=paste0(hprefix,xx[1],hsuffix),`data-toggle`=data_toggle,text);
  ## maybe check for 
}
source('help.R');
source('text.R');

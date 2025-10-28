# Before running this script, make sure the lz4json package is installed
library(jsonlite);
library(tibble);
library(dplyr);

# init ----
source('default_config.R');
if(file.exists('local_config.R')) source('local_config.R');
jsontemp <- tempfile('opentabs',fileext = '.json');

# load data ----
system(sprintf("lz4jsoncat %s > %s"
               ,file.path(firefoxsessionpath,'sessionstore-backups','recovery.jsonlz4')
               ,jsontemp),wait = T);

opentabs <- fromJSON(jsontemp,simplifyVector = F);

# parse data ----
opentabslist <- list();
kkrow <- iiwindow <- 0;
for(ii in opentabs$windows){
  iiwindow <- 1 + iiwindow;
  jjtab <- 0;
  for(jj in ii$tabs){
    jjtab <- 1 + jjtab;
    nthinhistory <- length(jj$entries);
    kk <- jj$entries[[nthinhistory]];
    kkrow <- 1 + kkrow;
    opentabslist[[kkrow]] <- tibble(url=kk$url,title=kk$title,nthinhistory
                                    ,lastAccessed=as.POSIXct(jj$lastAccessed/1000)
                                    ,tab=jjtab,window=iiwindow);
  }
};

opentabstable <- bind_rows(opentabslist) %>%
  mutate(domain=gsub('^[a-z]*://([^/]*)/.*','\\1',url)
         ,parent_domain=gsub('.*\\.([^.]*\\.[^.]*$)','\\1',domain));

# analyze data ----

by_domain <- summarise(opentabstable,tabs=n()
                       ,minLastAccessed=min(lastAccessed,na.rm = T)
                       ,medLastAccessed=median(lastAccessed,na.rm = T)
                       ,maxLastAccessed=max(lastAccessed,na.rm = T)
                       ,.by=domain) %>% arrange(desc(tabs));
by_parent_domain <- summarise(opentabstable,tabs=n()
                              ,minLastAccessed=min(lastAccessed,na.rm = T)
                              ,medLastAccessed=median(lastAccessed,na.rm = T)
                              ,maxLastAccessed=max(lastAccessed,na.rm = T)
                              ,.by=parent_domain) %>% arrange(desc(tabs));


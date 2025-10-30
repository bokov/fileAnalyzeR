# libraries ----
library(dplyr)
library(tidyr)
library(purrr)
library(fs)
library(lubridate)
library(stringr)
library(digest)
library(tibble)
library(rio);
library(fuzzyjoin);
library(progress);

# init ----
targetdir = '/tmp';
now <- Sys.time();

# obtain data ----
# recursively scan targetdir to collect the following data into a data.frame about
# all types of files but not directories:
# full file path and name, file name only (no path), file path only (no name),
# file extension only (if any), file size, creation date, last access date
# (if available), directory recursion depth, file permissions, file owner
# (if available), file group (if available), file type (regular file, directory, symlink, etc.)
# and file hash (MD5, SHA1, SHA256, etc. if available). Wherever there is a
# permission denied error, fill in the information for that file with NA and put
# the error message into the errors column

file_data <- dir_info(targetdir, recurse = T, all=T,
                    , type = c("file", "directory", "symlink", "FIFO", "socket", "character_device", "block_device")
                    , fail=F) %>%
  mutate(path_dir=paste0(path_dir(path),'/'),path_file=path_file(path)
         ,path_ext = ifelse(type=='directory','',path_ext(path))
         ,depth = str_count(path,'/')
         #,across(ends_with("_time"),~{out<- now - coalesce(.x,now)})
         )

# identify git repos, whether or not they are dirty, and whether or not they are
# ahead of their origins
repo_info <- subset(file_data,path_file=='.git')$path_dir %>% unique %>%
  data.frame(git_repo=.
             ,dirty=sapply(.,function(xx){
               length(system(paste("cd",shQuote(xx),"&& git status --porcelain")
                             , intern=T))
               })
             ,ahead=sapply(.,function(xx){
               c(system(
                 paste("cd",shQuote(xx),"&& git rev-list --count --right-only --count origin/HEAD...HEAD")
                 ,intern=T),Inf)[1]}) %>% as.numeric());

# update file_data so that every file that's in a git repo or in a subdirectory
# of a git repo can be marked as such
#file_data <- mutate(file_data, is_gitrepo = grepl(paste(repo_info$git_repo,collapse='|'),path_dir))

file_data <- mutate(repo_info,git_repo = paste0("^", git_repo)) %>%
  regex_left_join(file_data,.,by = c("path_dir" = "git_repo")
                  ,ignore_case = FALSE) %>% mutate(git_repo=!is.na(git_repo));

# Now, to get statistics about top-level folders
# folderscmd <- "{   for dir in .[!.]* */; do     [ -d \"$dir\" ] || continue; name=$(basename \"$dir\"); files=$(find \"$dir\" -type f 2>/dev/null | wc -l 2>/dev/null); size=$(du -s \"$dir\" 2>/dev/null | awk '{print $1}'); echo \"$name\t$files\t$size\";   done; }  > folders_dirstats.csv 2>folders_errors.log"
# system(paste('cd',shQuote(targetdir),'&&',folderscmd),wait = T);
# dirstats <- import(file.path(targetdir,'folders_dirstats.csv'),col.names=c('path_dir','files','size'));

pb <- progress_bar$new(total=sum(file_data$type=='directory',na.rm=T),format='[:bar] :percent, :current of :total, :elapsedfull elapsed, ETA: :eta, :tick_rate/s')
dir_fallbackresult <- data.frame(filecount=NA,totalsize=NA
                                 ,lastmod=NA,lastaccess=NA,lastchange=NA
                                 ,lastcreate=NA);

dir_data <- dir_map(targetdir,recurse = T,all=T,type='directory',fail=F
               ,fun=function(xx){
                 dinf <- try(dir_info(xx,fail=F,recurse=T,all=T,type=c('file')));
                 pb$tick();
                 if(is(dinf,'try-error')) return(cbind(path=xx,dir_fallbackresult)) else {
                   return(with(dinf
                               ,data.frame(path=xx,filecount=length(type)
                                           ,totalsize=sum(size,na.rm=T)
                                           ,lastmod=max(modification_time,na.rm=T)
                                           ,lastaccess=max(access_time,na.rm=T)
                                           ,lastchange=max(change_time,na.rm=T)
                                           ,lastcreate=max(birth_time,na.rm=T))))
                   }
                 }) %>% bind_rows();


## Waaay too slow
# file_type <- dir_map(targetdir,recurse = T,all=T,type='any',fail=F
#                      ,fun=function(xx){
#                        type=system(paste('file',shQuote(xx),'2> /dev/null'),intern = T);
#                        data.frame(path=xx,type=type)}) %>% bind_rows();

file_data <- left_join(file_data,dir_data) %>%
  mutate(filecount=ifelse(type!='directory',1,filecount)
         ,totalsize=ifelse(type!='directory',size,totalsize)
         ,lastmod=ifelse(type!='directory',modification_time,lastmod)
         ,lastaccess=ifelse(type!='directory',access_time,lastaccess)
         ,lastchange=ifelse(type!='directory',change_time,lastchange)
         ,lastcreate=ifelse(type!='directory',birth_time,lastcreate)
  );
class(file_data$totalsize)<-c('fs_bytes','numeric');
class(file_data$lastmod) <-class(file_data$lastaccess) <-
  class(file_data$lastchange) <- class(file_data$lastcreate) <-
  c('POSIXct','POSIXt');

exclusion <- . %>%
  subset( (dirty!=0|coalesce(ahead,-1)!=0|!git_repo) & # exclude git repos unless they are dirty or ahead of origin
            depth==2 &                            # top-level items only
            user == 'a' &                         # exclude items created by non-user processes
            type %in% c('directory','file') &     # exclude special files
            # below: exclusion regexps for file paths minus the targetdir prefix
            !grepl('^Rtmp|^pid-|^[.]|^java_prop_extract|^[.]org[.]chromium|^[.]com[.]valvesoftware|^cura-crash|^Outlook Logging|^forge_installer|^kallichore-|^kernel-|^skype-|^steam[A-Za-z]{6}|[.]db[.]ses$|[.]log[.]last$|[.][0-9]+$|-[a-z0-9]{32}$|[{][A-Z0-9]{8}-[A-Z0-9]{4}-[A-Z0-9]{4}-[A-Z0-9]{4}-[A-Z0-9]{12}[}]|^v8-compile-cache|ziptemp|^neoforge_installer|cache$|^cargo-install|^VSTelem|VSRemoteControl|^appInsights-nodeAIF-|^file[A-Za-z0-9]{6}$|^~lock|^acrobat_sbx',path_file) &
            !path_ext %in% c('lock','apk','cpuprofile','log','tmp','pbix','xml','blend','xpi','so','ndjson','html','dat','toml','json','bak','gz','deb','AppImage','sql','R','ics','js','py','sqlplan','xlsx#','docx#','pptx#','csv#','dmp') & # exclude certain file extensions
            !(grepl('[a-z0-9]{8}[.][a-z0-9]{3}',path_file) & as.numeric(totalsize) %in% c(21645,3221)) & # these two weird little VScode files appear over and over under random names
            totalsize > 0 & filecount > 0
          )
# Which extensions matter the most?
# subset(file_data,!is_gitrepo & depth==2 & size > fs_bytes('5Kb') & !grepl('^\\{[^{}]*\\}|^foo|foo[.][^.]*$|^~|#$|^tmp|tmp$|log$|pbix$|^[.]',path_file)) %>% summarise(count=n(),size=sum(size),.by = c(path_ext,type)) %>% arrange(desc(size)) %>% View()

inclusion <- . %>% subset(!path_ext %in% c(
  'csv','pdf','xlsx','docx','xls','pptx','ppt','doc','txt','rtf','md','sh'  # documents
  ,'png','jpg','jpg','jpeg','gif','odg','gv','dot','svg'                    # images
  ,'zip','tgz','jar','stl','mcworld'                                        # downloads
  , 'accdb','cls','thmx','car'                                              # maybes
  ))

file_data %>% exclusion %>% inclusion %>% mutate(lasttouched=pmax(lastmod,lastcreate,lastaccess,lastchange)) %>% select(path,type,path_ext,filecount,totalsize,lasttouched) %>%
  arrange(desc(totalsize)) %>% View
file_data %>% exclusion %>% inclusion %>% group_by(path_ext,type) %>% summarise(filecount=sum(filecount),totalsize=sum(totalsize)) %>% arrange(desc(totalsize)) %>% View

#

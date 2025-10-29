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
         ,across(ends_with("_time"),~{out<- now - coalesce(.x,now)}))

# identify git repos, whether or not they are dirty, and whether or not they are
# ahead of their origins
repo_info <- subset(file_data,path_file=='.git')$path_dir %>% unique %>%
  data.frame(git_repo=.
             ,dirty=sapply(.,function(xx){
               length(system(paste("cd",shQuote(xx),"&& git status --porcelain")
                             , intern=T))
               }));

repo_info <- subset(repo_info,dirty==0) %>%
  transmute(git_repo
            ,ahead=sapply(git_repo,function(xx){
  c(system(
    paste("cd",shQuote(xx),"&& git rev-list --count --right-only --count origin/HEAD...HEAD")
    ,intern=T),'error')[1]}) %>% as.numeric()) %>% left_join(repo_info,.);

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

dir_data <- dir_map(targetdir,recurse = T,all=T,type='directory',fail=F
               ,fun=function(xx){
                 nn<-try(length(dir_ls(xx)));
                 nn <- if(is(nn,'try-error')) NA else nn;
                 sz <- try(sum(dir_info(xx,fail=F,recurse=T,all=T,type='any')$size,na.rm=T));
                 sz <- if(is(sz,'try-error')) NA else sz;
                 data.frame(path=xx,filecount=nn,totalsize=sz)}) %>% bind_rows();


## Waaay too slow
# file_type <- dir_map(targetdir,recurse = T,all=T,type='any',fail=F
#                      ,fun=function(xx){
#                        type=system(paste('file',shQuote(xx),'2> /dev/null'),intern = T);
#                        data.frame(path=xx,type=type)}) %>% bind_rows();

file_data <- left_join(file_data,dir_data) %>%
  mutate(filecount=ifelse(type!='directory',coalesce(filecount,1),filecount)
         ,totalsize=fs_bytes(1024*ifelse(type!='directory',coalesce(totalsize,size),totalsize)));

# file_sizes<-system(paste('du -ax ',shQuote(targetdir),' 2>/dev/null'),intern = T,wait = T) %>%
#   sapply(str_split_fixed,'\t',2) %>% t %>% as.data.frame %>% setNames(c('size','path')) %>%
#   mutate(size=as_fs_bytes(paste0(size,'K')))
# file_counts <- system(paste('find'
#                             ,shQuote(targetdir)
#                             ,r'(find /tmp -type d -exec sh -c 'echo "$(find "$1" -type f | wc -l)\t$1"' _ {} \; 2> /dev/null)')
#                       ,intern=T,wait=T) %>%
#   sapply(str_split_fixed,'\t',2) %>% t %>% as.data.frame %>% setNames(c('count','path')) %>%
#   mutate(count=as.numeric(count));

# subset(dirstats
#        ,!path_dir %in% path_file(repo_info$git_repo) &
#          files > 0 &
#          size > 1024 &
#          !grepl('^Rtmp|^pid-|\\.tmp$|^[.]|^java_prop_extract',path_dir)) %>%
#   arrange(desc(size)) %>% View;
subset(file_data,(dirty!=0|coalesce(ahead,-1)!=0|!git_repo) & depth==2 & user == 'a' & type %in% c('directory','file') & !grepl('^Rtmp|^pid-|^[.]|^java_prop_extract|^[.]org[.]chromium|^[.]com[.]valvesoftware|^cura-crash|^Outlook Logging|^forge_installer|^kallichore-|^kernel-|^skype-|^steam[A-Za-z]{6}|[.]tmp$',gsub(paste0(targetdir,'/*'),'',path)) & !path_ext %in% c('lock','tmp','pbix')) %>% View()
# Which extensions matter the most?
# subset(file_data,!is_gitrepo & depth==2 & size > fs_bytes('5Kb') & !grepl('^\\{[^{}]*\\}|^foo|foo[.][^.]*$|^~|#$|^tmp|tmp$|log$|pbix$|^[.]',path_file)) %>% summarise(count=n(),size=sum(size),.by = c(path_ext,type)) %>% arrange(desc(size)) %>% View()

#

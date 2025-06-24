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
  mutate(path_dir=path_dir(path),path_file=path_file(path)
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
    ,intern=T),'error')[1]})) %>% left_join(repo_info,.);

# update file_data so that every file that's in a git repo or in a subdirectory
# of a git repo can be marked as such
file_data <- mutate(file_data, is_gitrepo = grepl(paste(repo_info$git_repo,collapse='|'),path_dir))

# Now, to get statistics about top-level folders
folderscmd <- "{   for dir in .[!.]* */; do     [ -d \"$dir\" ] || continue;     name=$(basename \"$dir\");     files=$(find \"$dir\" -type f 2>/dev/null | wc -l 2>/dev/null);     size=$(du -s \"$dir\" 2>/dev/null | awk '{print $1}');     echo \"$name\t$files\t$size\";   done; }  > foo_dirstats.csv 2>foo_errors.log"
system(paste('cd',shQuote(targetdir),'&&',folderscmd),wait = T);
dirstats <- import(file.path(targetdir,'foo_dirstats.csv'),col.names=c('path_dir','files','size'));
subset(dirstats
       ,!path_dir %in% path_file(repo_info$git_repo) &
         files > 0 &
         size > 1024 &
         !grepl('^Rtmp|\\.tmp$|^[.]|^java_prop_extract',path_dir)) %>%
  arrange(desc(size)) %>% View;

# Which extensions matter the most?
# subset(file_data,!is_gitrepo & depth==2 & size > fs_bytes('5Kb') & !grepl('^\\{[^{}]*\\}|^foo|foo[.][^.]*$|^~|#$|^tmp|tmp$|log$|pbix$|^[.]',path_file)) %>% summarise(count=n(),size=sum(size),.by = c(path_ext,type)) %>% arrange(desc(size)) %>% View()

#

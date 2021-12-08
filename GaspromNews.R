#### GASPROM news ####
### 0) INTRO ###
## SET CUSTOM FUNCTIONS AND PATH
`%!in%` = Negate(`%in%`)
# Here you can set your working directory - i.e., the default downloads directory:
PATH <- paste0("C:/Users/", Sys.info()[["user"]], "/Downloads")
setwd(PATH)

# ! Before executing the code, manually download these files to your working directory:
# https://github.com/gnid4enko/GaspromNews/blob/main/news_urls.csv
# https://github.com/gnid4enko/GaspromNews/blob/main/GSPRM_NWS.csv

## LOAD PACKAGES
packs <- c("xml2", "rvest", "stringr", "gdata")
newpacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(newpacks)) install.packages(newpacks)
lapply(packs, library, character.only=T)


### 1) GET NEWS URLS ###
## GET URLS FROM GASPROM SITE FROM ALL MONTHS
URL <- "https://www.gazprom.ru/press/news/"
full <- html_attr(html_nodes(read_html(URL), "a"), "href")
t1 <- full[grep("/press/news/[1-9]",full)]
t2 <- t1[grep("article",t1,invert=T)]
t3 <- unique(sort(t2[grep(paste(tolower(month.name),collapse="|"),t2)]))

## SUBSET NEWS LIST IF THE PREVIOUS RESULT EXISTS
if (file.exists(paste0(PATH, "news_urls.csv"))) {
  news_urls_0 <- read.csv(paste0(PATH, "news_urls.csv"))
  news_urls_0 <- news_urls_0[duplicated(news_urls_0)==F,]
  row.names(news_urls_0) <- c()
  last_mnth <- news_urls_0[news_urls_0$y==max(news_urls_0$y),]
  last_mnth <- last_mnth[last_mnth$mnth_no==max(last_mnth$mnth_no),]
  last_mnth <- unique(gsub("article.*|https://www.gazprom.ru","",last_mnth$urls))
  covered_mnths <- unique(gsub("article.*|https://www.gazprom.ru","",news_urls_0$urls))
  # get last month and months not covered by the previous result
  t3 <- unique(c(last_mnth, t3[t3%!in%covered_mnths]))
}

## DOWNLOAD NEWS URLS
LIST <- list()
i=1
while (i < length(t3)+1) {
  Sys.sleep(1)
  p_mon <- html_attr(html_nodes(read_html(paste0("https://www.gazprom.ru",t3[i])), "a"), "href")
  t11 <- p_mon[grep(paste0(t3[i],"article"),p_mon)]
  
  y <- gsub("[a-z]|/", "", t3[i])
  mnth <- str_extract(t3[i], paste(tolower(month.name),collapse="|")) # stringr
  mnth_no <- match(mnth, tolower(month.name))
  urls <- paste0("https://www.gazprom.ru",t11)
  DF <- as.data.frame(cbind(y,mnth,mnth_no,urls))
  LIST[[i]] <- DF
  i = i+1
}

## FORMATTING AND SORTING
news_urls_1 <- do.call("rbind", LIST)
news_urls_1$y <- as.integer(news_urls_1$y); news_urls_1$mnth_no <- as.integer(news_urls_1$mnth_no)
news_urls_1 <- news_urls_1[order(news_urls_1$y, news_urls_1$mnth_no),]
news_urls_1 <- news_urls_1[duplicated(news_urls_1)==F,]

## SUBSET FOR ONLY NEW URLS
if (exists("news_urls_0")) {
  news_urls_1 <- news_urls_1[news_urls_1$urls%!in%news_urls_0$urls,]
  news_urls_1 <- subset(news_urls_1, duplicated(news_urls_1)==F)
  row.names(news_urls_1) <- c()
}

## MERGE OLD AND NEW URLS AND SAVE TO FILE
if (nrow(news_urls_1)>0) {
  news_urls <- rbind(news_urls_0, news_urls_1)
  write.csv(news_urls, paste0(PATH, "news_urls.csv"),row.names=F, quote=TRUE)
}


### 2) GET NEWS ###
## IF THERE IS NO NEW DATA, DOWNLOAD THE PREVIOUS RESULT
if (nrow(news_urls_1)=0) {
  GSPRM_NWS <- read.csv(paste0(PATH, "GSPRM_NWS.csv"))
}

## PREPARE DATA FRAME FOR NEWS
if (nrow(news_urls_1)>0) {
  GSPRM_NWS <- news_urls_1
  GSPRM_NWS$date <- NA; GSPRM_NWS$title <- NA; GSPRM_NWS$type <- NA; 
  GSPRM_NWS$type_add <- NA; GSPRM_NWS$tags <- NA; GSPRM_NWS$text <- NA
  
  ## LOOP FOR NEWS
  j=1
  while (j < nrow(GSPRM_NWS)+1) { #
    Sys.sleep(1)
    url <- GSPRM_NWS$urls[j]
    site <- read_html(url)
    zag <- html_text(html_nodes(site, 'h1'))
    dt <- html_text(html_nodes(site, "[class='date']"))
    type1 <- html_text(html_nodes(site, "[class='plate news_standard']"))
    type2 <- html_text(html_nodes(site, "[class='plate press_release']"))
    type <- max(type1, type2)
    type <- gsub("\n|\t","",type)
    type_add <- html_text(html_nodes(site, "[class='newstype-title']"))
    type_add <- gsub("\n|\t","",type_add)
    tags <- html_text(html_nodes(site, "[class='tag-button']"))
    text <- html_text(html_nodes(site, 'p,h2,h3')) # h1 (title) is excluded
    text <- gsub("\n|\t","",text)
    start_pos <- max(match(trim(type), trim(text)), match(trim(type_add), trim(text)))
    text <- text[(start_pos+1):length(text)]
    text <- paste(text, collapse = ' ')
    text <- substring(text, 1, min(str_locate(pattern="©", text), str_locate(
      pattern="Контактная информация", text), str_locate(
        pattern="Управление информации", text), na.rm=T)-1)
    GSPRM_NWS$date[j] <- dt
    GSPRM_NWS$title[j] <- zag
    GSPRM_NWS$type[j] <- paste(type, collapse=" / ")
    GSPRM_NWS$type_add[j] <- paste(type_add, collapse=" / ")
    GSPRM_NWS$tags[j] <- paste(tags, collapse=" / ")
    GSPRM_NWS$text[j] <- text
    j = j+1
  }
  
  ## ADD DOWNLOADED NEWS TO THE PREVIOUS RESULT
  if (file.exists(paste0(PATH, "GSPRM_NWS.csv"))) {
    GSPRM_NWS_0 <- read.csv(paste0(PATH, "GSPRM_NWS.csv"))
    GSPRM_NWS <- rbind(GSPRM_NWS_0, GSPRM_NWS)
    GSPRM_NWS <- subset(GSPRM_NWS, duplicated(GSPRM_NWS)==F)
  }
  
  ## ORDER THE RESULT AND SAVE TO FILE
  GSPRM_NWS <- GSPRM_NWS[order(GSPRM_NWS$y, GSPRM_NWS$mnth_no, GSPRM_NWS$date),]
  row.names(GSPRM_NWS) <- c()
  write.csv(GSPRM_NWS, paste0(PATH, "GSPRM_NWS.csv"),row.names=F, quote=TRUE)
}
###

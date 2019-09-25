library(dplyr); library(stringr); library(purrr); library(lubridate)

# hostPath functional programming -----------------------------------------

hostPath_fun<-function(host){
  function(pathStatement,paramString,qstring=NULL){
    pathStatement %>%
      str_extract("(?<=(\\s))[:graph:]+") %>%
      str_split("/") %>%
      unlist -> pathElements
    pathElements %>%
      str_which(":") -> whichIndex
    pathElements[whichIndex] <- paramString

    paste0(pathElements,collapse ="/") %>%
      paste0(host,.,qstring)
  }
}


# Hypothes.is -------------------------------------------------------------


# personalAPItoken可於此取得： https://hypothes.is/account/developer

.groupID="o6wjyzRM"

require(httr); require(purrr); require(rlist)

hyp_GET_funs<-function(.apiEndPoint){
  function(.groupID,personalAPItoken,queryParameterList=NULL){
    .authorization=paste0("Bearer ",personalAPItoken)
    queryList<-list(group=.groupID)
    if(!is.null(queryParameterList)){
      queryList[names(queryParameterList)]<-
        queryParameterList[names(queryParameterList)]
    }
    GET(.apiEndPoint,
      add_headers(
        Authorization=.authorization
      ),
      query=queryList)  %>%
    content -> response
    return(response)
  }
}


# Search annotations belong to a group ------------------------------------
hyp_search<-hyp_GET_funs('https://Hypothes.is/api/search')


# Create new annotation belong to a group --------------------------------------

hyp_POST_funs<-function(.apiEndPoint){
  function(.groupID,personalAPItoken,queryParameterList=NULL){
    .authorization=paste0("Bearer ",personalAPItoken)
    queryList<-list(group=.groupID)
    if(!is.null(queryParameterList)){
      queryList[names(queryParameterList)]<-
        queryParameterList[names(queryParameterList)]
    }
    POST(.apiEndPoint,
        add_headers(
          Authorization=.authorization
        ),
        query=queryList)  %>%
      content -> response
    return(response)
  }
}


# extract annotation table ------------------------------------------------

annotationDF<-function(response){
  user<-c();highlight<-c();updated<-c();source<-c();text<-c();sourceWithHyp<-c()
  for(i in seq_along(response$rows)){
    # i<-11
    # response<-mTeamAnnotations2
    annotation_i<-response$rows[[i]]
    updated<-c(updated,annotation_i$updated)
    user<-c(user,annotation_i$user)
    text<-c(text,annotation_i$text)
    source<-c(source,annotation_i$target[[1]]$source)
    sourceWithHyp<-c(sourceWithHyp,annotation_i$links$incontext)
    map(1:length(annotation_i$target[[1]]$selector), function(x){
      annotation_i$target[[1]]$selector[[x]] %>% names %>% {"exact" %in% .}
    }) %>% unlist %>% which %>% {annotation_i$target[[1]]$selector[[.]]} ->
      targetSelector
    if(!is.null(targetSelector)){
      targetSelector %>%
        {paste0(.$prefix,.$exact,.$suffix)} -> highlightedContent
    } else {
      highlightedContent<-""
    }
    highlight<-c(highlight, highlightedContent)
  }
  data.frame(
    user=user,
    highlight=highlight,
    text=text,
    updated=lubridate::date(lubridate::ymd_hms(updated)),
    source=source,
    sourceWithHyp=sourceWithHyp
  )
}


# response$rows %>%
#   purrr::map(function(x) c(x$updated, x$user)) -> userAnnotations


# bitly -------------------------------------------------------------------

### 產生Bit.ly縮址
library(httr);library(qrcode); library(png)

bitlyRequest<-function(URL,QRcodeShow=T){
  paste0("https://api-ssl.bitly.com/v3/shorten?format=txt&access_token=c159fa433761c3a0c5833c822b2c195a63274311&longUrl=",URLencode(URL))-> shortURL_i
  GET(shortURL_i) -> Response_i
  content(Response_i) %>%
    str_replace("\\n","") -> bitlyurl
  if(QRcodeShow==T){
    bitlyurl %>% qrcode_gen
  }
  invisible(bitlyurl)
}


# github ------------------------------------------------------------------

## 查詢commit記錄
## https://developer.github.com/v3/search/#search-commits

# gitter

## 查詢chatroom id

# Content-Type: application/json
# Accept: application/json
# Authorization: Bearer {{token}}

#roomIDcheck

Gitter_roomIDcheck<-function(roomUrl){
  "https://api.gitter.im" %>%
    paste0("/v1/rooms") %>%
    GET(
      add_headers(
        Authorization=paste0("Bearer ",Sys.getenv("gitter_token"))
      )
    ) %>% content -> roomList2

  roomList2 %>% purrr::map(~.x$url) %>%
    unlist -> allRooms
  allRooms %>%
    stringr::str_detect(roomUrl) %>%
    which %>%
    roomList2[[.]] %>%
    .$id
}

# roomUrl<-"E-Major/IT-107-2"
# roomUrl %>% Gitter_roomIDcheck()


# Gitter_hostpath_fun -----------------------------------------------------

## Gitter_hostpath_fun(pathStatement,paramString,qstring=NULL)
## paramString<-c("X","Y")
## pathStatement<-"GET /v1/rooms/:roomId/chatMessages/:aaa/bbb"
## qstring<-"?limit=100"


gitter_host<-"https://api.gitter.im"
Gitter_hostpath_fun<-hostPath_fun(gitter_host)


# functional: chatID區間發言記錄 ----------------------------------------

gitter_IdInterval_chattingRecords<-function(gs,roomID,wsWeeklyProgressName){
  function(whichWeek){
    gs %>%
      gs_key-> gskeyStudentInfo

    gskeyStudentInfo %>%
      gs_read(ws=wsWeeklyProgressName) -> weeklyProgressDF

    weeklyProgressDF %>%
      filter(Week==whichWeek) %>%
      .[c("gitterStartId","gitterEndId")] -> chatIdInterval

    chatIdInterval %>%
      str_c(c("afterId","beforeId"),.,sep="=") %>%
      paste0(.,collapse="&") %>%
      paste0("?",.) -> qstring

    "GET /v1/rooms/:roomId/chatMessages" %>%
      Gitter_hostpath_fun(roomID,qstring) %>%
      GET(
        add_headers(
          Authorization=paste0("Bearer ",Sys.getenv("gitter_token"))
        )
      ) %>% content -> chatRecords

    chatRecords[[1]]$sent -> startTime

    chatRecords %>%
      length() %>%
      {chatRecords[[.]]$sent} -> endTime

    chatRecords %>%
      purrr::map_dfr(~.[["fromUser"]][c("username","avatarUrl")]) %>%
      group_by(username) %>%
      summarise(
        avatarUrl=last(avatarUrl),
        count=n()
      ) %>%
      ungroup -> chatRecordsDF
    return(
      list(
        IdInterval=chatIdInterval,
        timeInterval=lubridate::ymd_hms(c(startTime,endTime)),
        chatRecordDF=chatRecordsDF
      )
    )
  }
}

gitter_afterId_chattingRecords<-function(roomID){
  function(afterID){
      str_c(c("afterId"),afterID,sep="=") %>%
      paste0(.,collapse="&") %>%
      paste0("?",.) -> qstring

    "GET /v1/rooms/:roomId/chatMessages" %>%
      Gitter_hostpath_fun(roomID,qstring) %>%
      GET(
        add_headers(
          Authorization=paste0("Bearer ",Sys.getenv("gitter_token"))
        )
      ) %>% content -> chatRecords

    chatRecords[[1]]$sent -> startTime
    chatRecords[[1]]$id -> startId

    chatRecords %>%
      length() %>%
      {chatRecords[[.]]$sent} -> endTime

    chatRecords %>%
      length() %>%
      {chatRecords[[.]]$id} -> endId


    chatRecords %>%
      purrr::map_dfr(~.[["fromUser"]][c("username","avatarUrl")]) %>%
      group_by(username) %>%
      summarise(
        avatarUrl=last(avatarUrl),
        count=n()
      ) %>%
      ungroup -> chatRecordsDF
    return(
      list(
        IdInterval=c(startId,endId),
        timeInterval=lubridate::ymd_hms(c(startTime,endTime)),
        chatRecordDF=chatRecordsDF
      )
    )
  }
}

# 1072 gitterID區間查詢 -------------------------------------------------------
library(dplyr); library(googlesheets); library(lubridate); library(purrr); library(httr)
roomID<-"5c6ceee7d73408ce4fb851f4"
gs<-"1gYwyh3dfaRPr7uGAPvrwFto38jRQfdStDrtRi3GisUs"
wsWeeklyProgressName<-"每週進度"

gitter_IdInterval_chattingRecords(gs,roomID,wsWeeklyProgressName)->
  gitter_IdInterval_chattingRecords1072
# Usage: gitter_IdInterval_chattingRecords1072(whichWeek)

## Example
# whichWeek<-1L
# gitter_IdInterval_chattingRecords1072(whichWeek)


gitter_afterId_chattingRecords(roomID)->
  gitter_afterId_chattingRecords1072
# Usage: gitter_afterId_chattingRecords1072(afterID)

## Example
# gitter_afterId_chattingRecords1072("5c6d1da285b7eb4569fff674")


# 大廳活動列表產生器 ---------------------------------------------------------------


## event template 斷落設定 ----------------------------------------------------------


# event-date --------------------------------------------------------------

.eventDateTag <- '
                div(class="event-date",
                    div(class="date-only",
                        div(class="month",
                            ".month"),
                        div(class="day",
                            ".day"),
                        div(class="clearbox")
                        )
                    )
'


# event-title -----------------------------------------------------------

.eventTitleTag <-'
                    a(class="event-title",
                      href=".eventHref",
                      h3(".eventTitle")
                      )'


# clear box ---------------------------------------------------------------


.clearBoxTag <- '                        div(class="clearbox")
'



# .startEventContainer ----------------------------------------------------


.startEventContainer<-'
# Event container
  div(class="container content-wrap",
    div(class="masonry-grid",
        div(class="event-card card-calendar",
'

# .startEventContainer_scroll ----------------------------------------------------

.startEventContainer_scroll<-'div(class="container content-wrap",
    div(class="masonry-grid",
        p(class="card-tag",
          tag("i",
              list(
                class="fa fa-calendar",
                `aria-hidden`="true"
              )
          ),
          "近期活動"
        ),
        div(class="event-card card-calendar",
            style="overflow:scroll;",'

# .newEventSeparator ------------------------------------------------------

.newEventSeparator<-'
            ), # if adding more event item
            div(class="more-concerts"),
'


# .endOfEventClosure ------------------------------------------------------


.endOfEventClosure<-'
            ) # if end of event
        )))
'

# 活動細節  --------------------------------------------------------------------

.eventDetails<-vector("character")

.eventDetails[["location"]]<-'
                      span(class="location",
                           tag("i",
                               list(
                                 class="fa fa-globe",
                                  `aria-hidden`="true")
                               ),
                           ".location")'

.eventDetails[["time"]]<-'
                      span(class="time",
                           tag("i",
                               list(
                                 class="fa fa-clock",
                                  `aria-hidden`="true"
                                 )
                               ),
                           ".time")'
.eventDetails[["speaker"]]<-'
                      span(class="speaker",
                           tag("i",
                               list(
                                 class="fas fa-chalkboard-teacher",
                                  `aria-hidden`="true"
                                 )
                               ),
                           ".speaker")'
.eventDetails[["association"]]<-'
                      span(class="association",
                           tag("i",
                               list(
                                 class="fas fa-university",
                                  `aria-hidden`="true"
                                 )
                               ),
                           ".association")'

event_details<- function(DF_i,.eventDetails){
  ## 產生如下的p tag
  # <p class="event-details">
  #   <span class="location">
  #     <i class="fa fa-globe" aria-hidden="true"></i> Grace Baptist Church
  #   </span>
  #   <br />
  #   <span class="time">
  #     <i class="fa fa-clock-o" aria-hidden="true"></i> 8:00pm
  #   </span>
  # </p>  
  # 
  DF_i %>%
    select(時間,地點,主講人或主辦機構,主講人現職) ->
    DF_details
  DF_details %>%
    rename(
      time=時間,
      location=地點,
      speaker=主講人或主辦機構,
      association=主講人現職
    )  %>%
    select_if(
      function(x) !is.na(x)
    ) -> DF_details
  
  event_pTag_elements<-vector("character")
  for(j in 1:ncol(DF_details)){
    colName<-names(DF_details[j])
    .eventDetails[[colName]] %>%
      str_replace(paste0("(\\.",colName,")"),
                  DF_details[[j]]) -> event_pTag_elements[[j]]
  }
  paste0(event_pTag_elements,collapse=",br(),")-> alleventPtags
  
  paste0(
    '                  p(class="event-details",',
    alleventPtags,
    ')'
  ) 
}


# newEventTag() 單一活動tag生成 --------------------------------------------------------------
i<-1
eventDF[i,]->DF_i

newEventTag<-function(DF_i){
    event_details(DF_i,.eventDetails) -> .eventDetailsTag
    
    paste0(.eventTitleTag,",",.eventDetailsTag) %>%
      paste0('                div(class="event-info",',
             .,
             ',',
             .clearBoxTag,
             '                    )') -> .eventInfoTag
    
    paste0(.eventDateTag,",",.eventInfoTag) %>%
      paste0(
        'div(class="calendar-item masonry-item"',
        ",",
        .,
        ")"
      ) -> .eventNewItemTag
    
    .eventNewItemTag %>%
      str_replace_all(
        c(
          "(\\.month)"=paste0(month(DF_i$日期),"月"),
          "(\\.day)"=day(DF_i$日期),
          "(\\.eventTime)"=DF_i$時間,
          "(\\.eventHref)"=ifelse(is.na(DF_i$活動相關網址連結),"#",DF_i$活動相關網址連結),
          "(.eventTitle)"=DF_i$`活動名稱（勿超過11個中文字）`   )
      ) -> .eventNewItemTag  
  return(.eventNewItemTag)
}


# Event schedule ----------------------------------------------------------
event_schedule<-function(eventDF){
  eventItemList<-vector("character",nrow(eventDF))
  for(i in 1:nrow(eventDF)){
    DF_i<-eventDF[i,]
    DF_i %>% 
      newEventTag -> eventItemList[[i]]
  }
  
  paste0(eventItemList,collapse=",")->allEventsCat
  
  allEventsCat %>%
    paste0(
      'p(class="card-tag",
              tag("i",
                  list(
                    class="fa fa-calendar",
                    `aria-hidden`="true"
                    )
                  ),
              "近期活動"
              )',
      ",",
      .
    ) -> eventCardContent
  
  eventCardContent %>%
    paste0(
      .startEventContainer,
      .,
      ")))"
    ) -> eventSchedule
  
  return(eventSchedule)
}

# Event schedule with scrolling -----

event_schedule_scroll<-function(eventDF){
  eventItemList<-vector("character",nrow(eventDF))
  for(i in 1:nrow(eventDF)){
    DF_i<-eventDF[i,]
    DF_i %>% 
      newEventTag -> eventItemList[[i]]
  }
  
  paste0(eventItemList,collapse=",")->allEventsCat
  
  allEventsCat %>%
    paste0(
      .startEventContainer_scroll,
      .,
      ")))"
    ) -> eventSchedule
  
  return(eventSchedule)
}


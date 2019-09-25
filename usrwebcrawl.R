
# 抓取官網最近文章 ----------------------------------------------------------------

library("rvest"); library("dplyr");
url <-"http://usrsoc.ntpu.edu.tw/news.html"

usrwebcrawl<-function(url){
  url %>% read_html -> usrweb
  
  usrweb %>%
    html_nodes(xpath = '//*[(@id = "flex-newpost")]//img') %>%
    html_attr("src") -> usrweb.img
  
  # usrweb %>%
  #   html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "col-lg-7", " " ))]') %>% View
  
  usrweb %>%
    html_node("#flex-newpost strong") %>%
    html_text() -> usrweb.title
  
  usrweb %>%
    html_node("#flex-newpost p") %>%
    html_text -> usrweb.content
   
  usrweb %>%
    html_node("#latestnews") %>%
    html_attr("href") %>%
    {paste0("http://usrsoc.ntpu.edu.tw",.)} ->
    usrweb.newsurl
  return(
    list(
      imgsrc=usrweb.img,
      title=usrweb.title,
      content=usrweb.content,
      newsurl=usrweb.newsurl
    )
  )
}

usrnewsCard<-function(usrnews){
  div(class="card",style="max-width: 500px;",
      if(usrcontent$imgsrc!=""){
        img(class="card-img-top", 
            src=usrnews$imgsrc, 
            alt="Card image cap"
        )       
      } else {
        NULL
      }
,
      div(class="card-body",
          h5(class="card-title",
             usrnews$title),
          p(class="card-text",
            if(usrcontent$imgsrc!=""){
              str_trunc(usrnews$content,300,side="right")
          } else {
              str_trunc(usrnews$content,500,side="right")
          }),
          br(),
          tag("button",
              list(
                "按我看更多...",
                type="button",
                class="btn btn-success",
                `data-toggle`="modal", 
                `data-target`="#exampleModal",
                onclick=paste0(
                  "postqr2modal('",
                  usrnews$newsurl,
                  "')"
                )
              )
          )
      )
  )
}


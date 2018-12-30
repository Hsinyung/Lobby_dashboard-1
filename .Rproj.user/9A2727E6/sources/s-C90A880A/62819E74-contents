
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
  return(
    list(
      imgsrc=usrweb.img,
      title=usrweb.title,
      content=usrweb.content
    )
  )
}

usrnewsCard<-function(usrnews){
  div(class="card",style="max-width: 500px;",
      img(class="card-img-top", 
          src=usrnews$imgsrc, 
          alt="Card image cap"
      ),
      div(class="card-body",
          h5(class="card-title",
             usrnews$title),
          p(class="card-text",
            usrnews$content),
          br(),
          tag("button",
              list(
                type="button",
                class="btn btn-primary",
                "按我看更多..."
              )
          )
      )
  )
}


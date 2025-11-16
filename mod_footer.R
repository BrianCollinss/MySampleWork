

footer_ui_item <- function(image, link, text) {
  tags$a(class="myapp-footer__item",
         href=link, target="_blank", title=text,
         tags$img(class="myapp-footer__image", src=paste0("footer/", image), alt=text)
  )
}

# -----------------------------------------------------------------------------

footer_ui <- function() {
  tags$footer(
    class="myapp-footer",
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="footer/footer.css")
    ),

    tags$h3(class="myapp-footer__header", tags$b("Partners")),

    tags$div(
      class="myapp-footer__items",

      footer_ui_item(
        image="logo-usq.png",
        link="https://www.unisq.edu.au/",
        text="University of Southern Queensland"
      ),
      footer_ui_item(
        image="logo-baci-crop.jpg",
        link="https://www.unisq.edu.au/research/institutes-centres/ilse/baci",
        text="Broadacre Cropping Initiative"
      ),
      footer_ui_item(
        image="logo-cqu.png",
        link="https://www.cqu.edu.au/",
        text="CQUniversity"
      ),
      footer_ui_item(
        image="logo-agrifutures.svg",
        link="https://agrifutures.com.au/",
        text="AgriFutures Australia"
      )
    )
  )
}

# functions to add cards to projects and resources pages
# code here is heavily adapted from source code for Dani Navarro's personal site

add_card_projects <- function(img, title, text, url) {
  lines <- c(
    '<div class="g-col-12 g-col-md-4">',
    '<div class="card h-100 transparent-card">',
    paste0(
      '<a href="', 
      url, 
      '"><img src="', 
      img, 
      '"',
      'class="card-img-top"></a>'
    ),
    '<div class="card-body">',
    paste0('<h3 class="card-title">', title, '</h3>'),
    paste0('<p class="card-text">', text, '</p>'),
    '</div>',
    '</div>',
    '</div>'
  )
  cat(lines, sep="\n")
}

add_card_tutorials <- function(img, text, url) {
  lines <- c(
    '<div class="g-col-12 g-col-md-4">',
    '<div class="card h-100 transparent-card">',
    paste0(
      '<a href="', 
      url, 
      '"><img src="', 
      img, 
      '"',
      'class="card-img-top"></a>'
    ),
    '<div class="card-body">',
    paste0('<p class="card-text">', text, '</p>'),
    '</div>',
    '</div>',
    '</div>'
  )
  cat(lines, sep="\n")
}

add_card_talks <- function(img, title, location, source_code_url, slide_deck_url) {
  lines <- c(
    '<div class="g-col-12 g-col-md-4">',
    '<div class="card h-100 transparent-card">',
    paste0(
      '<img src="', 
      img, 
      '"',
      'class="card-img-top">'
    ),
    '<div class="card-body">',
    paste0('<p class="card-title">', title, '</p>'),
    paste0('<p class="card-text thin">', location, '</p>'),
    '<p class="card-text">',
    paste0('<a href="', source_code_url, '">source code</a> <span class="bullet-color">•</span> <a href="', slide_deck_url, '">slide deck</a>'),
    '</p>',
    '</div>',
    '</div>',
    '</div>'
  )
  cat(lines, sep="\n")
}
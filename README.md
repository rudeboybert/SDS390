
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SDS390 Ecological Forecasting

<!-- badges: start -->

<!-- badges: end -->

Homepage source code for 2020-09 Fall Smith College SDS 390: Ecological
Forecasting <https://rudeboybert.github.io/SDS390/>. Most of the content
is in either:

  - `index.Rmd`: a single Moodle-style page that lists all
    announcements, lectures notes, and problem sets/homeworks in
    chronological order.
  - `syllabus.Rmd`

## How to build this webpage

This webpage is built/compiled using [R Markdown
websites](http://rmarkdown.rstudio.com/rmarkdown_websites.html). To
compile this webpage for yourself, do the following:

1.  Get the contents of this directory/repository:
      - If you are not familiar with GitHub, click the green “Clone or
        download” button on the top-right -\> Download ZIP -\> Unzip
        `SDS390-master.zip`.
      - If you are familiar with GitHub, clone this repository.
2.  Double-click the `SDS390.Rproj` to open RStudio.
3.  If you haven’t already, install the R Markdown package by typing
    `install.packages("rmarkdown")` in the console.
4.  Go to the “Build” pane of RStudio -\> More -\> Configure Build
    Tools… -\> Ensure that “Project build tools” is set to “Webpage”.
5.  Click “Build Website”. You may need to install some additional R
    pacakges listed at the top of `index.Rmd`.
6.  The website will display in the Viewer pane. The resulting
    `index.html` file and all other files for the webpage will be saved
    in the `docs/` folder.

## How to publish this webpage

To publish/deploy this webpage and make it viewable on the web, you need
to either:

1.  Copy the contents of the `docs/` folder to your personal webpage or
    whatever domain hosting service you use.
2.  Use [GitHub pages](https://pages.github.com/) as I do. RStudio’s R
    Markdown Websites page gives
    [instructions](http://rmarkdown.rstudio.com/rmarkdown_websites.html#publishing_websites)
    on how.

library(knitr)
library(markdown)

#transform the .Rmd to a markdown (.md) file.
knit('PA1_template.Rmd')

#transform the .md to HTML format
markdownToHTML("PA1_template.md", "PA1_template.html",fragment.only = TRUE)
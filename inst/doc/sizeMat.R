## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

library(sizeMat)

## ----install-cran, eval = FALSE-----------------------------------------------
# install.packages("sizeMat")

## ----install-github, eval = FALSE---------------------------------------------
# # install.packages("devtools")
# devtools::install_github("ejosymart/sizeMat")

## ----load-crabdata------------------------------------------------------------
data(crabdata)

head(crabdata)
names(crabdata)

## ----classify-data------------------------------------------------------------
classify_data <- classify_mature(
  crabdata,
  varNames = c("carapace_width", "chela_height"),
  varSex = "sex_category",
  selectSex = NULL,
  method = "ld"
)

classify_data_males <- classify_mature(
  crabdata,
  varNames = c("carapace_width", "chela_height"),
  varSex = "sex_category",
  selectSex = "m",
  method = "ld"
)

print(classify_data)

## ----plot-classify-base, fig.width = 7, fig.height = 5------------------------
plot(
  classify_data,
  xlab = "Carapace width (mm)",
  ylab = "Chela height (mm)"
)

## ----plot-classify-base-custom, fig.width = 7, fig.height = 5-----------------
plot(
  classify_data,
  xlab = "Carapace width (mm)",
  ylab = "Chela height (mm)",
  col = c(2, 3),
  pch = c(5, 6),
  lty_lines = c(1, 2),
  lwd_lines = c(1, 3),
  cex = c(1, 2),
  main = "Classification"
)

## ----plot-classify-gg, fig.width = 7, fig.height = 5--------------------------
plot(
  classify_data,
  xlab = "Carapace width (mm)",
  ylab = "Chela height (mm)",
  col = c("steelblue", "firebrick"),
  pch = c(16, 17),
  gg_style = TRUE
)

## ----plot-classify-gg-modified, fig.width = 7, fig.height = 5-----------------
p_classify <- plot(
  classify_data,
  xlab = "Carapace width (mm)",
  ylab = "Chela height (mm)",
  col = c("steelblue", "firebrick"),
  pch = c(16, 17),
  gg_style = TRUE
)

p_classify + ggplot2::theme_bw()

## ----morph-mature-------------------------------------------------------------
set.seed(123)

my_morph_fq <- morph_mature(
  classify_data,
  method = "fq",
  niter = 200
)

print(my_morph_fq)

my_morph_bayes <- morph_mature(
  classify_data,
  method = "bayes",
  niter = 200
)

print(my_morph_bayes)

## ----plot-morph-base, fig.width = 7, fig.height = 5---------------------------
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))

plot(
  my_morph_fq,
  xlab = "Carapace width (mm)",
  ylab = "Proportion mature",
  col = c("blue", "red")
)

par(oldpar)

## ----plot-morph-base-ogive, fig.width = 7, fig.height = 5---------------------
plot(
  my_morph_fq,
  xlab = "Carapace width (mm)",
  ylab = "Proportion mature",
  col = c("blue", "red"),
  onlyOgive = TRUE
)

## ----plot-morph-gg-ogive, fig.width = 7, fig.height = 5-----------------------
plot(
  my_morph_fq,
  xlab = "Carapace width (mm)",
  ylab = "Proportion mature",
  col = c("steelblue", "firebrick"),
  onlyOgive = TRUE,
  gg_style = TRUE
)

## ----plot-morph-gg-list-------------------------------------------------------
p_morph <- plot(
  my_morph_fq,
  xlab = "Carapace width (mm)",
  ylab = "Proportion mature",
  col = c("steelblue", "firebrick"),
  gg_style = TRUE
)

names(p_morph)

## ----plot-morph-gg-A, fig.width = 7, fig.height = 5---------------------------
p_morph$A

## ----plot-morph-gg-B, fig.width = 7, fig.height = 5---------------------------
p_morph$B

## ----plot-morph-gg-L50, fig.width = 7, fig.height = 5-------------------------
p_morph$L50

## ----plot-morph-gg-ogive-from-list, fig.width = 7, fig.height = 5-------------
p_morph$ogive

## ----plot-morph-gg-custom, fig.width = 7, fig.height = 5----------------------
p_morph$ogive +
  ggplot2::theme_bw()

## ----load-matfish-------------------------------------------------------------
data(matFish)

head(matFish)

## ----gonad-mature-------------------------------------------------------------
set.seed(123)

my_gonad_fq <- gonad_mature(
  matFish,
  varNames = c("total_length", "stage_mat"),
  immName = "I",
  matName = c("II", "III", "IV"),
  method = "fq",
  niter = 200
)

print(my_gonad_fq)

my_gonad_bayes <- gonad_mature(
  matFish,
  varNames = c("total_length", "stage_mat"),
  immName = "I",
  matName = c("II", "III", "IV"),
  method = "bayes",
  niter = 200
)

print(my_gonad_bayes)

## ----plot-gonad-base, fig.width = 7, fig.height = 5---------------------------
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))

plot(
  my_gonad_fq,
  xlab = "Total length (cm)",
  ylab = "Proportion mature",
  col = c("blue", "red")
)

par(oldpar)

## ----plot-gonad-base-ogive, fig.width = 7, fig.height = 5---------------------
plot(
  my_gonad_fq,
  xlab = "Total length (cm)",
  ylab = "Proportion mature",
  col = c("blue", "red"),
  onlyOgive = TRUE
)

## ----plot-gonad-base-legend-position, fig.width = 7, fig.height = 5-----------
plot(
  my_gonad_fq,
  xlab = "Total length (cm)",
  ylab = "Proportion mature",
  col = c("blue", "red"),
  onlyOgive = TRUE,
  legendPosition = "bottomright"
)

## ----plot-gonad-base-no-legend, fig.width = 7, fig.height = 5-----------------
plot(
  my_gonad_fq,
  xlab = "Total length (cm)",
  ylab = "Proportion mature",
  col = c("blue", "red"),
  onlyOgive = TRUE,
  showLegend = FALSE
)

## ----plot-gonad-gg-ogive, fig.width = 7, fig.height = 5-----------------------
plot(
  my_gonad_fq,
  xlab = "Total length (cm)",
  ylab = "Proportion mature",
  col = c("steelblue", "firebrick"),
  onlyOgive = TRUE,
  gg_style = TRUE
)

## ----plot-gonad-gg-legend-position, fig.width = 7, fig.height = 5-------------
plot(
  my_gonad_fq,
  xlab = "Total length (cm)",
  ylab = "Proportion mature",
  col = c("steelblue", "firebrick"),
  onlyOgive = TRUE,
  gg_style = TRUE,
  legendPosition = "bottomright"
)

## ----plot-gonad-gg-list-------------------------------------------------------
p_gonad <- plot(
  my_gonad_fq,
  xlab = "Total length (cm)",
  ylab = "Proportion mature",
  col = c("steelblue", "firebrick"),
  gg_style = TRUE
)

names(p_gonad)

## ----plot-gonad-gg-A, fig.width = 7, fig.height = 5---------------------------
p_gonad$A

## ----plot-gonad-gg-B, fig.width = 7, fig.height = 5---------------------------
p_gonad$B

## ----plot-gonad-gg-L50, fig.width = 7, fig.height = 5-------------------------
p_gonad$L50

## ----plot-gonad-gg-ogive-from-list, fig.width = 7, fig.height = 5-------------
p_gonad$ogive

## ----plot-gonad-gg-custom, fig.width = 7, fig.height = 5----------------------
p_gonad$ogive +
  ggplot2::theme_bw()


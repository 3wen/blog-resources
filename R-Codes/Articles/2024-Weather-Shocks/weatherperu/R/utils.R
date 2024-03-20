#' Returns the index of the longest sequence of non NA values in a vector
#'
#' @param y vector of numerical values
#' @export
get_index_longest_non_na <- function(y) {
  split_indices <- which(is.na(y))
  nb_obs <- length(y)

  if (length(split_indices) == 0) {
    res <- seq_len(nb_obs)
  } else {
    idx_beg <- c(1, split_indices)
    if (idx_beg[length(idx_beg)] != nb_obs) {
      idx_beg <- c(idx_beg, nb_obs)
    }
    lengths <- diff(idx_beg)
    ind_max <- which.max(lengths)
    index_beginning <- idx_beg[ind_max]
    if(!index_beginning == 1 | is.na(y[index_beginning])) {
      index_beginning <- index_beginning + 1
    }
    index_end <- idx_beg[ind_max] + lengths[ind_max]
    if(is.na(y[index_end])) {
      index_end <- index_end - 1
    }
    res <- seq(index_beginning, index_end)
  }
  res
}

#' Returns a logical vector that identifies the longest sequence of non NA
#' values within the input vector
#'
#' @param y numeric vector
#' @export
keep_values_longest_non_na <- function(y) {
  ids_to_keep <- get_index_longest_non_na(y)
  ids <- seq(1, length(y))
  ids %in% ids_to_keep
}

#' Logistic function
#'
#' @param z input value
#' @param gamma logistic growth rate
#' @export
logist <- function(z,gamma) {
  (exp(-gamma * z))/(1+exp(-gamma*z))
}

#' Theme for ggplot2
#'
#' @param ... arguments passed to the theme function
#' @export
#' @importFrom ggplot2 element_rect element_text element_blank element_line unit
#'   rel
theme_paper <- function (...) {
  theme(
    text = element_text(family = "Times New Roman"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill = NA, colour = "grey50", linewidth = 1),
    axis.text = element_text(),
    legend.text = element_text(size = rel(1.1)),
    legend.title = element_text(size = rel(1.1)),
    # legend.background = element_rect(fill = "transparent", color = NULL),
    legend.background = element_rect(
      fill = "transparent", linetype="solid", colour ="black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.key = element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, size = rel(1.3), face = "bold"),
    plot.title.position = "plot",
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_text(size = rel(1.1))
  )
}

#' Theme for ggplot2 with a dark theme
#'
#' @param ... arguments passed to the theme function
#' @export
#' @importFrom ggplot2 element_rect element_text element_blank element_line unit
#'   rel
theme_paper_dark <- function (...) {
  theme(
    text = element_text(family = "Times New Roman", colour = "white"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill = NA, colour = "grey50", linewidth = 1),
    axis.text = element_text(colour = "white"),
    legend.text = element_text(size = rel(1.1), colour = "white"),
    legend.title = element_text(size = rel(1.1), colour = "white"),
    legend.background = element_rect(fill = "transparent", color = NULL),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.key = element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_line(colour = "grey60"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      hjust = 0, size = rel(1.3), face = "bold", colour = "white"
    ),
    plot.title.position = "plot",
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_text(size = rel(1.5), colour = "white", face = "bold")
  )
}

#' Theme for maps with ggplot2
#'
#' @param ... arguments passed to the theme function
#' @export
#' @importFrom ggplot2 element_rect element_text element_blank element_line unit
#'   rel
theme_map_paper <- function(...) {
  theme(
    text = element_text(family = "Times New Roman"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), axis.line = element_blank(),
    plot.title.position = "plot",
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.2)),
    legend.background = element_rect(fill="transparent", color = NULL),
    legend.key = element_blank(),
    legend.key.height	= unit(2, "line"),
    legend.key.width	= unit(1.5, "line"),
    strip.background = element_rect(fill = NA),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    strip.text = element_text(size = rel(1.2))
  )
}


#' Theme for maps with ggplot2 with a dark theme
#'
#' @param ... arguments passed to the theme function
#' @export
#' @importFrom ggplot2 element_rect element_text element_blank element_line unit
#'   rel
theme_map_paper_dark <- function(...) {
  theme(
    text = element_text(family = "Times New Roman", colour = "white"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), axis.line = element_blank(),
    plot.title.position = "plot",
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.2)),
    legend.background = element_rect(fill="transparent", color = NULL),
    legend.key = element_blank(),
    legend.key.height	= unit(2, "line"),
    legend.key.width	= unit(1.5, "line"),
    strip.background = element_rect(fill = NA),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    strip.text = element_text(size = rel(1.2), colour = "white")
  )
}

#' Save a ggplot2 plot as PDF, using LaTeX tikz
#'
#' @param plot ggplot2 object
#' @param path_to_latex path to LaTeX engine (Defaults to
#'   `/Library/TeX/texbin/`)
#' @param interpreter by default, use pdflatex (`pdflatex`)
#' @param path path to the destination folder
#' @param filename file name (without the extension)
#' @param keep_tex should the tex file be kept after compilation? Defaults to
#'   `FALSE`
#' @param width width in inches (default to 15)
#' @param height height in inches (default to 15)
#' @param verbose A logical value indicating whether diagnostic messages are
#'   printed when measuring dimensions of strings. Defaults to `FALSE`
#' @param ignore.stdout a logical (not NA) indicating whether messages written
#'   to ‘stdout’  should be ignored. Defaults to `TRUE`
#'
#' @export
#' @importFrom tikzDevice tikz
#' @importFrom grDevices dev.off
ggplot2_to_pdf <- function(plot,
                           path_to_latex = "/Library/TeX/texbin/",
                           interpreter = "pdflatex",
                           path = "./",
                           filename,
                           keep_tex = FALSE,
                           width = 15,
                           height = 15,
                           verbose = FALSE,
                           ignore.stdout = TRUE){
    content <- paste0(
      "\\documentclass{standalone}
      \\usepackage{amssymb}
      %\\usepackage{newtxtext,newtxmath}
      \\usepackage{times,mathpazo}
      \\usepackage{pgfplots}
      \\usetikzlibrary{pgfplots.groupplots}
      \\definecolor{mygrey2}{RGB}{127,127,127}
      \\definecolor{deepblue}{RGB}{0,129,188}
      \\definecolor{deepgreen}{RGB}{0,157,87}
      \\definecolor{deepred}{RGB}{238,50,78}
      \\begin{document}

      \\input{",
      path, filename,
      "_content.tex}

      \\end{document}"
    )

    # The file which will import the graph in tex format
    fileConn <- file(paste0(path, filename, ".tex"))
    writeLines(content, fileConn)
    close(fileConn)

    # Export graph to tex
    tikz(file = paste0(
      path,
      filename, "_content.tex"),
      width = width,
      height = height,
      verbose = verbose
    )
    print(plot)
    dev.off()

    # Move the scale from ggplot, if any
    name_scale <- paste0(filename, "_content_ras1.png")
    scale_exists <- file.exists(name_scale)
    if (scale_exists & ! path %in% c(".", "./", "/")) {
      system(paste0("mv ", name_scale, " ", path))
    }

    # Process tex file to get the PDF
    system(
      paste0(
        path_to_latex,
        interpreter, " -shell-escape -synctex=1 -interaction=nonstopmode  ",
        path,
        filename, ".tex"),
      ignore.stdout = TRUE
    )
    if(!path %in%  c(".", "./", "/")) system(paste0("mv ", filename, ".pdf ", path))
    system(paste0("rm ", filename, ".aux"))
    system(paste0("rm ", filename, ".log"))
    system(paste0("rm ", filename, ".synctex.gz"))
    if (!keep_tex) {
      system(paste0("rm ", path, filename, ".tex"))
      system(paste0("rm ", path, filename, "_content.tex"))
    }
    if (scale_exists) system(paste0("rm ", path, "/", name_scale))
  }

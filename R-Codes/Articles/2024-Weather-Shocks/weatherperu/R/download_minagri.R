#' Directly download the Excels versions of the monthly reports available on the
#' Peruvian website MINAGRI.
#'
#' @param y last digits of the year (20yy)
#' @param dest_folder destination folder
#' @importFrom dplyr mutate filter case_when select
#' @importFrom stringr str_c str_detect regex str_extract str_which
#' @importFrom rvest read_html html_nodes html_text html_attr
#' @importFrom utils download.file unzip
#' @importFrom xlsx write.xlsx
#' @importFrom readxl read_excel
download.data <- function(y){

  # HOME PAGE
  page <- str_c(
    "https://www.midagri.gob.pe/portal/",
    "boletin-estadistico-mensual-el-agro-en-cifras?start=",
    19-y
  )

  # EXTRACTION OF THE LINKS
  text_rvest <- read_html(page)
  results <- text_rvest |> html_nodes(".mainbody")
  first_result <- results[1]

  # LIST OF THE MONTHS
  list_of_months <- first_result |>
    html_nodes(".cabecera") |>
    html_text(trim = TRUE)
  list_of_months <- list_of_months[str_detect(list_of_months, "[aeiou]")]

  # LIST OF THE urls (=liens)
  liens <-first_result |> html_nodes("a[href]") |> html_attr("href")
  liens <- liens[(str_detect(liens, "zip"))]
  if (sum(as.numeric(str_detect(liens, "\\.pdf"))) > 0) {
    liens <- liens[-which(str_detect(liens, "\\.pdf"))]
  }
  if (sum(as.numeric(str_detect(liens, "pdf\\.zip"))) > 0) {
    liens <- liens[-which(str_detect(liens, "pdf\\.zip"))]
  }
  if ((y == 13) | (y == 14) | (y ==15)) {
    liens <- liens[which(str_detect(liens, "cuadros"))]
  }
  if (y == 16) {
    liens <- liens[which(str_detect(liens, "data"))]
  }
  if (y == 14) {
    if (sum(as.numeric(str_detect(liens, "abril"))) > 1) {
      line <- liens[which(str_detect(liens, "abril"))[1]]
      liens <- liens[-which(str_detect(liens, "abril"))]
    }
  }

  if (sum(as.numeric(duplicated(liens))) > 0) {
    liens <- liens[-which(duplicated(liens) == T)]
  }
  if (y == 14) {
    liens <- c(liens[1:8], line, liens[9:11])
  }

  annee <-  as.numeric(str_c("20", ifelse(y <10 , str_c("0",y), y)))

  # CONDITION 1: Checking the number of urls
  if (length(liens) == length(list_of_months)) {

    if (y == 7) {
      list_of_months <- list_of_months[-8]
      liens <- liens[-8]
    }

    dir.create(str_c("../data/", annee))


    # LOOP 1 - On months for downloading and extracting the ZIP files
    for (m in 1:length(liens)) {
      mois_Lettres <- list_of_months[m]

      mois <- case_when(
        str_detect(mois_Lettres, "iciembre") ~ "12",
        str_detect(mois_Lettres, "oviembre") ~ "11",
        str_detect(mois_Lettres, "ctubre")   ~ "10",
        str_detect(mois_Lettres, "tiembre")  ~ "09",
        str_detect(mois_Lettres, "osto")     ~ "08",
        str_detect(mois_Lettres, "ulio")     ~ "07",
        str_detect(mois_Lettres, "unio")     ~ "06",
        str_detect(mois_Lettres, "ayo")      ~ "05",
        str_detect(mois_Lettres, "bril")     ~ "04",
        str_detect(mois_Lettres, "arzo")     ~ "03",
        str_detect(mois_Lettres, "brero")    ~ "02",
        str_detect(mois_Lettres, "nero")     ~ "01"
      )

      # DOWNLOADING THE FILE
      AdresseFichier <- str_c("../data/raw/minagri/", annee, "/", mois, ".zip")
      download.file(
        url = str_c("https://www.midagri.gob.pe/", liens[m]),
        destfile = AdresseFichier
      )

      # Names of the files in the archive
      files <- unzip(zipfile = AdresseFichier, list = TRUE)
      folder_name <- files$Name[str_which(
        files$Name, regex("agr.*la", ignore_case = TRUE)
      )] |>
        str_extract("^(.*)/")

      # Unzip on Mac OS when files in the archive contain accents in names
      system(str_c("open ", AdresseFichier))
      print(str_c("FILE ", mois, "-", annee, " UNZIPED"))

      folder_name <- files$Name[str_which(
        files$Name, regex("agr.*la", ignore_case = TRUE)
      )] |>
        str_extract("^(.*)/")

      # Special case to handle year 2005
      if (annee == 2005) {
        AdresseFichier <- str_c("../data/raw/minagri/", annee, "/", mois)
        fichierZip <- list.files(AdresseFichier)[str_detect(list.files(AdresseFichier), ".zip")]
        for (i in 1:length(fichierZip)) {
          unzip(
            zipfile = str_c(AdresseFichier, "/", fichierZip[i]),
            exdir = AdresseFichier
          )
        }
      }

    }# End of LOOP 1

    # MANUAL TREATMENT FOR 2007
    if (annee == 2007) {
      list_of_months <- c(list_of_months[1:7], "Mayo", list_of_months[8:11])
      dir.create(str_c("../data/raw/minagri/", annee, "/05"))
      file.copy(
        from = "./MAYO_2007_NO_DELETE/AGRICOLA.xls" ,
        to = str_c("../data/raw/minagri/",annee,"/05/AGRICOLA.xls")
      )
    }

    # LOOP 2 - On months for creating the Excel files
    for (m in 1:length(list_of_months)) {

      if (y == 15 & m == 7) { next } else {
        # Checking the appropriate name of each month
        mois_Lettres <- list_of_months[m]
        mois <- case_when(
          str_detect(mois_Lettres, "iciembre") ~ "12",
          str_detect(mois_Lettres, "oviembre") ~ "11",
          str_detect(mois_Lettres, "ctubre")   ~ "10",
          str_detect(mois_Lettres, "tiembre")  ~ "09",
          str_detect(mois_Lettres, "osto")     ~ "08",
          str_detect(mois_Lettres, "ulio")     ~ "07",
          str_detect(mois_Lettres, "unio")     ~ "06",
          str_detect(mois_Lettres, "ayo")      ~ "05",
          str_detect(mois_Lettres, "bril")     ~ "04",
          str_detect(mois_Lettres, "arzo")     ~ "03",
          str_detect(mois_Lettres, "brero")    ~ "02",
          str_detect(mois_Lettres, "nero")     ~ "01"
        )

        # Selecting the right Zip file
        AdresseFichier <- str_c("../data/raw/minagri", annee, "/", mois, ".zip")

        if ((y == 12 & m == 10) | (y == 7 & m == 8)) {
          files <- ifelse(
            m == 10,
            yes = data.table(Name = list.files(
              "../data/raw/minagri/2012/cuadros-marzo12"
            )),
            no = data.table(Name = list.files(
              str_c("../data/raw/minagri/", annee, "/05/")
            ))
          )
        } else {
          files <- unzip(zipfile = AdresseFichier, list = TRUE)
        }

        if (y == 06 & (m == 10 | m == 12)) {
          for (i in 1:dim(files)[1]) {
            name <- files[i, 1]
            unzip(
              zipfile = str_c(
                "../data/raw/minagri/", annee, "/", mois, "/", name
              ),
              exdir = str_c(
                "../data/raw/minagri/", annee, "/", mois
              )
            )
          }
        }

        if (y == 16 & m < 12) {
          N_1 <- str_c("../data/raw/minagri/", annee, "/", files[1, 1])
          N <- str_c(N_1,"x")
          file.copy(from = N_1, to = N)

          if (m >= 6) {
            N <- str_c("../data/raw/minagri/", annee, "/", files[1, 1])
          } else {
            unlink(N_1, recursive = TRUE)
          }
        } else {
          folder_name <- files$Name[str_which(
            files$Name,
            regex("agr.*la", ignore_case = TRUE)
          )] |>
            str_extract("^(.*)/")

          if (is.na(folder_name[1])) {folder_name <- mois}
          if (y == 12 & m == 10) {folder_name <- "cuadros-marzo12"}

          # Adding .xls to the 2008-07 file
          if (y == 08 & m == 6) {
            N <- list.files(
              str_c("../data/raw/minagri/", annee, "/", folder_name),
              full.names = TRUE
            ) |>
              {\(x) str_which(x, regex("clima", ignore_case = TRUE))}()
            file.rename(from = N, to = str_c(N,".xls"))
          }

          # CHANGING "HIDRO.." FILES INTO "CLIMA"
          if (
            any(
              str_detect(
                list.files(
                  str_c("../data/raw/minagri/",annee,"/",folder_name),
                  full.names = TRUE
                ),
                regex("hidro.*",ignore_case = TRUE)
              )
            ) == TRUE) {
            N <- list.files(
              str_c("../data/raw/minagri/",annee,"/",folder_name),
              full.names = TRUE
            ) |>
              {\(x) str_which(w, regex("hidro.*", ignore_case = TRUE))}()
            file.rename(
              from = N,
              to = str_c( substr(N,0,nchar(N)-4),"clima.xls")
            )
          }

          # Final list of files in the unziped file
          N <- list.files(
            str_c("../data/raw/minagri/", annee, "/", folder_name),
            full.names = TRUE,
            pattern = "\\.xlsx?",
            ignore.case = TRUE
          )
          print(N)

          # SELECTING THE FILE FOR AGRICULTURAL DATA
          N1 <- N[str_which(N, regex("agr.*la", ignore_case = TRUE))]

          if (sum(as.numeric(duplicated(N1))) > 0) {
            N1 <- N1[-which(duplicated(N1) == T)]
          }
          if (length(N1) > 1) {
            N1 <- case_when(
              (y == 07 & m == 10) ~ N1[1],
              (y == 08 & m == 1)  ~ N1[2],
              (y == 08 & m == 8)  ~ N1[1],
              (y == 15 & m == 4)  ~ N1[2]
            )
          }
          print(N1)

          # SELECTING THE FILE FOR PRICES
          if (y < 14 | (y == 14 & m > 1) ) {
            N2 <- N[str_which(N, regex("pre.*os", ignore_case = TRUE))]
          } else {
            N2 <- N[str_which(N, regex("agr.*la", ignore_case = TRUE))]
          }

          if (y == 16) { N2 <- N }

          if (sum(as.numeric(duplicated(N2))) > 0) {
            N2 <- N2[-which(duplicated(N2)==T)]
          }
          if (length(N2) > 1) {
            N2 <- case_when(
              (y == 07 & m == 10) ~ N2[1],
              (y == 08 & m == 1)  ~ N2[2],
              (y == 08 & m == 8)  ~ N2[1],
              (y == 15 & m == 4)  ~ N2[2]
            )
          }
          print(N2)
          if (y == 10 & m == 3) {
            file.copy(
              from = N2 ,
              to = str_c(
                "../data/raw/minagri/2010/",
                "BEAM SETIEMBRE_DEF_2010/BEAM SETIEMBRE//Precios_oct.xls"
              )
            )
          }

          # SELECTING THE FILE FOR CLIMATE DATA
          N3 <- N[str_which(N, regex("c.*ima", ignore_case = TRUE))]
          if (y == 16) {N3 <- N}
          if (sum(as.numeric(duplicated(N3))) > 0){
            N3 <- N3[-which(duplicated(N3) == T)]
          }
          if (sum(as.numeric(str_detect(N3, regex("mp.*t", ignore_case = TRUE)))) > 0) {
            N3 <- N3[-which(str_detect(N3, regex("mp.*t", ignore_case = TRUE)))]
          }
          if (is_empty(N3)) {
            N3 <- N[str_which(N, regex("Bem.*", ignore_case = TRUE))]
            N3 <- N3[-which(str_detect(N3, regex("mp.*t", ignore_case = TRUE)))]
          }
          if (length(N3) > 1) {
            N3 <- case_when(
              (y == 07 & m == 10) ~ N3[1],
              (y == 08 & m == 1)  ~ N3[1],
              (y == 08 & m == 8)  ~ N3[7],
              (y == 08 & m == 9)  ~ N3[2],
              (y == 10 & m == 12) ~ N3[1],
              (y == 15 & m == 4)  ~ N3[2]
            )
          }
          print(N3)
          if (is_empty(N3)) {N3 <- ""}

          # Special attribution for the 2006_01 files
          if (y == 06 & m == 12) {
            N1 <- str_c(
              "../data/raw/minagri/", annee, "/", mois, "/AGRICOLA/AGRICOLA.xls"
            )
            N2 <- str_c(
              "../data/raw/minagri/", annee, "/", mois, "/PRECIOS/PRECIOS.xls"
            )
            N3 <- str_c(
              "../data/raw/minagri/", annee, "/", mois, "/CLIMA/Bemene2006.xls"
            )
          }

          type_xl <- "xls"
          if (y == 15) {
            type_xl <- "xlsx"
            if (m == 2) {type_xl <- "xls"}
          }

          if (y == 5) {
            folder_name <- files$Name[str_which(
              files$Name,
              regex("agri.*zip", ignore_case = TRUE)
            )]
            unzip(
              str_c("../data/raw/minagri/", annee, "/", mois, "/", folder_name),
              exdir = str_c("../data/raw/minagri/", annee,"/", mois)
            )
            N1 <- list.files(
              str_c("../data/raw/minagri/", annee, "/", mois),
              full.names = TRUE,
              pattern = "\\.xlsx?",
              ignore.case = TRUE
            )
          }
        }

        mois <- case_when(
          (y == 12 & mois == "04") ~ "05",
          (y == 12 & mois == "05") ~ "04",
          (y == 14 & mois == "09") ~ "10",
          (y == 14 & mois == "10") ~ "09",
          TRUE ~ as.character(mois)
        )


        # FILES FOR 2005 - 2013
        if (y <= 13) {
          # PRODUCTION
          name <- str_c(
            "../data/raw/minagri/Production/Production_", annee, ".xlsx"
          )
          for (i in 1:2) {
            if (y < 11) {
              table <- ifelse(i == 1, "c-26", "c-27")
            } else {
              table <- ifelse(i == 1, "c-28", "c-29")
            }
            if (y == 11 & m == 9 ) {table <- ifelse(i == 1, "c-27", "c-28")}
            if (y == 11 & m > 9 ) {table <- ifelse(i == 1, "c-26", "c-27")}
            Tableau <- read_excel(path = N1, sheet = table)

            write.xlsx(
              Tableau,
              name,
              sheetName = str_c(mois, annee, i),
              append = TRUE,
              showNA = FALSE
            )
            print(table)
          }

          # PLANTED AREAS
          name <- str_c(
            "../data/raw/minagri/Surface/Superficies_", annee, ".xlsx"
          )
          for (i in 1:2) {
            if (y < 11) {
              table <- ifelse(i == 1, "c-19", "c-20")
            } else {
              table <- ifelse(i == 1, "c-21", "c-22")
            }
            if( y == 11 & m > 9 ) {table <- ifelse(i == 1, "c-19", "c-20")}
            if( y == 11 & m == 9 ) {table <- ifelse(i == 1, "c-20", "c-21")}

            Tableau <- read_excel(path = N1, sheet = table, col_types = "text")
            write.xlsx(
              Tableau,
              name,
              sheetName = str_c(mois, annee, i),
              append = TRUE,
              showNA = FALSE
            )
          }

          # HARVESTED AREAS
          name <- str_c(
            "../data/raw/minagri/Surface_R/Superficies_R_", annee, ".xlsx"
          )
          for (i in 1:2) {
            if (y < 11) {
              table <- ifelse(i == 1, "c-23", "c-24")
            } else {
              table <- ifelse(i == 1, "c-25", "c-26")
            }
            if (y == 11 & m > 9 ) {table <- ifelse(i == 1, "c-23" ,"c-24")}
            if (y == 11 & m == 9) {table <- ifelse(i == 1, "c-24" ,"c-25")}

            Tableau <- read_excel(path = N1, sheet = table, col_types = "text")

            write.xlsx(
              Tableau,
              name,
              sheetName = str_c(mois, annee, i),
              append = TRUE,
              showNA = FALSE
            )
          }

          # PRICES
          # Name of the file
          name <- str_c("../data/raw/minagri/Prices/Prices_", annee, ".xlsx")

          # Selecting the names of the sheets
          if (y <= 10) {
            table <- "C-65"
          } else {
            table <- case_when(
              (annee == 2011 & m < 9)   ~ "C-69",
              (annee == 2011 & m  == 9) ~ "C-68",
              (annee == 2011 & m >= 10) ~ "C-65",
              (annee > 2011 )           ~ "C-69"
            )
          }
          if (y == 13 & m == 1) {table <- regex("C-76", ignore_case = TRUE)}
          if (y == 10 & m == 4) {
            N2 <-  str_c(
              "../data/raw/minagri/2010/BEAM SETIEMBRE_DEF_2010/",
              "BEAM SETIEMBRE//Precios_oct.xls"
            )
          }
          if (y == 07 & m == 8) {
            N2 <-  "../data/raw/minagri/2007/04/PRECIOS.xls"
          }

          Tableau <- read_excel(path = N2, sheet = table, col_types = "text")
          write.xlsx(
            Tableau,
            name,
            sheetName = str_c(mois, annee),
            append = TRUE,
            showNA = FALSE
          )
        } else {
          # FILES FOR 2014 - 2018
          if (y == 16) {
            N1 <- N
            N2 <- N
            N3 <- N
          }
          if (y == 16 & m == 12) {
            N1 <- N[2]
            N2 <- N[2]
            N3 <- N[2]
          }
          # PRODUCTION
          name <- str_c(
            "../data/raw/minagri/Production/Production_", annee, ".xlsx"
          )
          if(y == 14) {
            table <- case_when(
              m ==1  ~ "c-12",
              m > 1 ~ "c-26"
            )
          }
          if (y == 15) {table <-"c-11"}
          if (y == 18) {table <-"c-18"}
          if (y == 15 & m ==  2) {table <-"C-11"}
          if (y == 15 & m ==  4) {table <-"C-11"}
          if (y == 15 & m ==  6) {table <-"C-11"}
          if (y == 15 & m == 10) {table <-"C-11"}
          if (y == 16 & m <   7) {table <-"C.12"}
          if (y == 16 & m ==  7) {table <-"C. 12"}
          if (y == 16 & m >=  8) {table <-"C.12"}
          if (y == 16 & m == 12) {table <-"c-11"}
          if (y == 14 & m >7) {
            for (i in 1:2) {
              table <- ifelse(i == 1, "c-28", "c-29")
              if (m == 11) {table <- ifelse(i == 1, "C-28", "C-29")}
              if (m == 8) {table <- ifelse(i == 1, "c-26", "c-26-a")}
              Tableau <- read_excel(path = N1, sheet = table)
              write.xlsx(
                Tableau,
                name,
                sheetName = str_c(mois, annee, i),
                append = TRUE,
                showNA = FALSE
              )
            }
          } else {
            Tableau <- read_excel(
              path = N1,
              sheet = regex(table, ignore_case = TRUE)
            )
            write.xlsx(
              Tableau,
              name,
              sheetName = str_c(mois, annee),
              append = TRUE,
              showNA = FALSE
            )
          }


          # PLANTED SUPERFICIES
          name <- str_c(
            "../data/raw/minagri/Surface/Superficies_",annee,".xlsx"
          )
          if(y ==14) {
            table <- case_when(
              m ==1 ~ "c-7",
              m > 1 ~ "c-21"
            )
          }
          if (y ==15) {table <-"c-6"}
          if (y ==18) {table <-"c-16"}
          if (y == 16) {table <-"C.9"}
          if (y == 15 & m == 2) {table <-"C-6"}
          if (y == 15 & m == 4) {table <-"C-6"}
          if (y == 15 & m == 6) {table <-"C-6"}
          if (y == 15 & m == 10) {table <-"C-6"}
          if (y == 16 & m ==  7) {table <-"C. 7"}
          if (y == 16 & m %in% c(8, 9, 10, 11, 12)) {table <-"C.6"}

          if (y == 14 & m > 7 ) {
            for (i in 1:2) {
              table <- ifelse(i == 1, "c-21", "c-22")
              if(m == 8 ) {table <- ifelse(i == 1, "c-21", "c-21-a")}
              Tableau <- read_excel(path = N1, sheet = table)
              write.xlsx(
                Tableau,
                name,
                sheetName = str_c(mois, annee, i),
                append = TRUE,
                showNA = FALSE
              )
            }
          } else {
            Tableau <- read_excel(path = N1, sheet = table, col_types = "text")
            write.xlsx(
              Tableau,
              name,
              sheetName = str_c(mois, annee),
              append = TRUE,
              showNA = FALSE
            )
          }

          # HARVESTED SUPERFICIES
          name <- str_c(
            "../data/raw/minagri/Surface_R/Superficies_R_", annee, ".xlsx"
          )
          if (y ==14) {
            table <- case_when(
              m ==1 ~ "c-10",
              m > 1 ~ "c-24"
            )
          }
          if (y ==15) {table <-"c-9"}
          if (y ==18) {table <-"c-16"}
          if (y == 16) {table <-"C.9"} #FALSE : Table do not exist in the 2016 file
          if (y == 15 & m == 2) {table <-"C-9"}
          if (y == 15 & m == 4) {table <-"C-9"}
          if (y == 15 & m == 6) {table <-"C-9"}
          if (y == 15 & m == 10) {table <-"c-9"}
          if (y == 16 & m ==  7) {table <-"C. 7"} #FALSE : Table do not exist in the 2016 file
          if (y == 16 & m %in% c(8, 9, 10, 11, 12)) {table <-"C.9"}

          if (y == 14 & m > 7) {
            for (i in 1:2) {
              table <- ifelse(i == 1,"c-25", "c-26")
              if (m == 11) {table <- ifelse(i == 1, "C25", "C26")}
              if (m == 8 ) {table <- ifelse(i == 1, "c-24", "c-24-a")}
              Tableau <- read_excel(path = N1, sheet = table)
              write.xlsx(
                Tableau,
                name,
                sheetName = str_c(mois, annee, i),
                append = TRUE,
                showNA = FALSE
              )
            }
          } else {
            Tableau <- read_excel(path = N1, sheet = table, col_types = "text")
            write.xlsx(
              Tableau,
              name,
              sheetName = str_c(mois, annee),
              append = TRUE,
              showNA = FALSE
            )
          }
          # PRICES
          # Name of the file
          name <- str_c(
            "../data/raw/minagri/Prices/Prices_", annee, ".xlsx"
          )

          # Selecting the names of the sheets
          table <- case_when(
            (annee == 2014 & m == 1)            ~ as.character(regex("c-17", ignore_case = TRUE)),
            (annee == 2014 & m == 2)            ~ as.character(regex("C-80", ignore_case = TRUE)),
            (annee == 2014 & m == 3)            ~ as.character(regex("C-72", ignore_case = TRUE)),
            (annee == 2014 & m == 4)            ~ as.character(regex("C-80", ignore_case = TRUE)),
            (annee == 2014 & m %in% 4:8)        ~ as.character(regex("C-72", ignore_case = TRUE)),
            (annee == 2014 & m >8)              ~ as.character(regex("C-76", ignore_case = TRUE)),
            (annee == 2015 & ! m %in% c(2,4,6)) ~ as.character(regex("c-16", ignore_case = TRUE)),
            (annee == 2015 & m %in% c(2,4,6))   ~ as.character(regex("C-16", ignore_case = TRUE)),
            (annee == 2016 & m <= 6)            ~ as.character(regex("C.15", ignore_case = TRUE)),
            (annee == 2016 & m >  6)            ~ as.character(regex("C.13", ignore_case = TRUE)),
            (annee == 2016 & m == 7)            ~ as.character(regex("C. 13", ignore_case = TRUE)),
          )
          if (y == 16 & m == 7) {table <-"C. 13"}

          Tableau <- read_excel(path = N2, sheet = table, col_types = "text")
          write.xlsx(
            Tableau,
            name,
            sheetName = str_c(mois, annee),
            append = TRUE,
            showNA = FALSE
          )
        }
        if (y == 16 & m < 12) {
          unlink(N, recursive = TRUE)
        } else {
          unlink(
            str_c("../data/raw/minagri/", annee, "/", folder_name),
            recursive = TRUE
          )
        }
      }
    }# End of LOOP 2
    # END OF CONDITION 1
  } else {
    print("ERROR - NB MOIS =! NB LIENS")
  }
}




#' Extract data from the PDF monthly reports from MINAGRI
#'
#' @param annee year of the report
#' @param mois month of the report
#' @param adresse url of the report on www.midagri.gob.pe
#' @param page page number to extract
#' @param Cell1 name of the first cell to import
#'
#' @importFrom dplyr mutate filter select
#' @importFrom stringr str_detect str_replace_all str_split_fixed str_which
#'   str_sub
#' @importFrom data.table as.data.table
#' @importFrom pdftools pdf_data pdf_text
extract_pdf_data <- function(annee,
                             mois,
                             adresse,
                             page,
                             Cell1) {

  ## Defining the number of cultures in the table----
  PageDeDonnes <- pdf_data(adresse)[[page]]
  if (annee == 2 & mois == 2) {
    PageDeDonnes[which((PageDeDonnes[,"text"] == "Año.?")),"x"] <-
      as.numeric(PageDeDonnes[which((PageDeDonnes[,"text"] == Cell1)),"x"])
  }

  # Determining the begining of the table
  x_Cell1 <- as.numeric(PageDeDonnes[which((PageDeDonnes[,"text"] == Cell1)), "x"])
  y_Cell1 <- as.numeric(PageDeDonnes[which((PageDeDonnes[,"text"] == Cell1)), "y"])

  # Looking for the lines with the same position as Cell1 or above
  AboveLine1 <- PageDeDonnes[which((PageDeDonnes[,"x"] <= x_Cell1 + 1)), ]

  ## Special case for 01/2003 ----
  if ((annee == 3 & mois == 1) | (annee == 2 & mois < 8)) {
    AboveLine1 <- PageDeDonnes[which((PageDeDonnes[, "x"] <= x_Cell1 + 10)), ]
    x_Cell1 <- max(AboveLine1$x)}
  # end of special case

  x <-  which(
    str_detect(AboveLine1$text, "Año.?") |
      str_detect(AboveLine1$text, "paña")
  )
  AboveLine1[x, "x"] <- x_Cell1
  if (any((AboveLine1[,"text"] == "Mensual"))) {
    AboveLine1 <- AboveLine1[- which(AboveLine1[, "text"] == "Mensual"), ]
  }

  if (any((AboveLine1[, "x"] > x_Cell1))) {
    AboveLine1[which(AboveLine1[,"x"]>x_Cell1) ,"x"] <- x_Cell1
  }

  AboveLine1 <- AboveLine1[order(AboveLine1$x, -AboveLine1$y), ]
  positions  <- which((AboveLine1[, "text"] == Cell1))
  x <- as.numeric(AboveLine1[positions, "x"])

  # Line with the culture list
  Cultures <- AboveLine1[AboveLine1$x == x, ]
  if (dim(Cultures)[1] < 3) {
    Cultures <- AboveLine1[AboveLine1$x %in% c(x,x-1), ]
  }

  if ((annee == 3 & mois == 1) | (annee ==2 & mois < 6)) {
    colnames(Cultures) <- c(
      "width_1", "height_1", "x_1", "y_1", "space_1", "text_1"
    )
    Cultures_1_0103 <- Cultures
  }

  # Selection of the previous line to determine the complete name
  x_1 <- unique(AboveLine1$x)
  x_1 <- x_1[-which((x_1 == x))]
  Cultures_1 <- AboveLine1[AboveLine1$x == max(x_1), ]
  if ((annee == 3 & mois == 1) | (annee ==2 & mois < 6)) {
    Cultures_0103 <- Cultures_1
  }
  colnames(Cultures_1) <- c("width_1","height_1","x_1","y_1","space_1","text_1")

  # Row matching to obtain full crop names
  if ((annee == 3 & mois == 1) | (annee ==2 & mois < 6)) {
    Cultures <- merge(
      Cultures_0103,
      Cultures_1_0103,
      by.x = "y",
      by.y = "y_1",
      all.x = TRUE,
      all.y=TRUE
    )
  } else {
    Cultures <- merge(
      Cultures,
      Cultures_1,
      by.x = "y",
      by.y = "y_1",
      all.x = TRUE,
      all.y=TRUE
    )
  }

  for (ii in 1:dim(Cultures)[1]) {
    Cultures$nomcomplet[ii]  <- if (is.na(Cultures$text_1[ii])) Cultures$text[ii] else paste(Cultures$text_1[ii], Cultures$text[ii])
    if (annee == 3 & mois == 1 ) {Cultures$nomcomplet[ii]  <- if (is.na(Cultures$text[ii])) Cultures$text_1[ii] else paste(Cultures$text_1[ii], Cultures$text[ii])}
    Cultures$nomcomplet[ii]  <- str_replace_all(Cultures$nomcomplet[ii], "- ", "")
    Cultures$nomcomplet[ii]  <- if (Cultures$nomcomplet[ii] == "de azúcar") paste("Caña", Cultures$nomcomplet[ii]) else Cultures$nomcomplet[ii]
    Cultures$nomcomplet[ii]  <- if (Cultures$nomcomplet[ii] == "loctao") paste("Frijol", Cultures$nomcomplet[ii]) else Cultures$nomcomplet[ii]
    Cultures$nomcomplet[ii]  <- if (Cultures$nomcomplet[ii] == "palo") paste("Frijol de", Cultures$nomcomplet[ii]) else Cultures$nomcomplet[ii]
    Cultures$nomcomplet[ii]  <- if (Cultures$nomcomplet[ii] == "grano seco**") paste("Frijol", Cultures$nomcomplet[ii]) else Cultures$nomcomplet[ii]
    Cultures$nomcomplet[ii]  <- if (Cultures$nomcomplet[ii] == "amiláceo") paste("Maíz", Cultures$nomcomplet[ii]) else Cultures$nomcomplet[ii]
    Cultures$nomcomplet[ii]  <- if (Cultures$nomcomplet[ii] == "hua") paste("Cañi" ,Cultures$nomcomplet[ii], sep="") else Cultures$nomcomplet[ii]
    Cultures$nomcomplet[ii]  <- if (Cultures$nomcomplet[ii] == "cáscara") paste("Arroz", Cultures$nomcomplet[ii]) else Cultures$nomcomplet[ii]
    Cultures$nomcomplet[ii]  <- if (Cultures$nomcomplet[ii] == "rama") paste("Algodón", Cultures$nomcomplet[ii]) else Cultures$nomcomplet[ii]

    if (! is.na(Cultures$text[ii]) & is.na(Cultures$text_1[ii]) & ii > 1 & ii < dim(Cultures)[1]) {
      Cultures$nomcomplet[ii] <- if(Cultures$y[ii] == Cultures$y[ii-1] + 1) paste(Cultures$text_1[ii-1], Cultures$text[ii]) else Cultures$nomcomplet[ii]
      Cultures$nomcomplet[ii] <- if(Cultures$y[ii] == Cultures$y[ii+1] - 1) paste(Cultures$text_1[ii+1], Cultures$text[ii]) else Cultures$nomcomplet[ii]
    }
  }

  # Discarding unmatched rows
  ii = which(is.na(Cultures$x))
  if (is.integer(ii) && length(ii) != 0) {Cultures <- Cultures[-ii, ]}

  ii = which(Cultures$text == "a.")
  if(is.integer(ii) && length(ii) != 0) {Cultures <- Cultures[-ii, ]}

  ii = which(Cultures$text == "de")
  if(is.integer(ii) && length(ii) != 0) {Cultures <- Cultures[-ii, ]}

  isPlatano <-any(str_detect(PageDeDonnes$text, "Plátano"))
  isFrijol <-any(str_detect(PageDeDonnes$text, "Frijol |seco"))

  ## Special case for 01/2003
  if ((annee == 3 & mois == 1) | annee == 2) {
    if (page %in% c(30, 31)) {
      Cultures <- as.data.table(
        c("Departamento","Campaña", "Total","Arroz cáscara", "Maíz amiláceo",
          "Frijol grano seco**","Frijol castlla", "Pallar","Zarandaja",
          "Frijol de palo", "Garbanzo","Frijol loctao", "Lentaja", "Papa",
          "Trigo", "Algodón rama", "Maíz duro", "Soya", "Sorgo grano",
          "Marigold")
      )
      colnames(Cultures) <- "nomcomplet"
    }

    if (page %in% c(32, 33)) {
      Cultures <- as.data.table(
        c("Departamento", "Campaña", "Total", "Cebada grano", "Quinua",
          "Cañihua", "Kiwicha","Haba grano", "Arveja grano", "Chocho tarhui",
          "Olluco", "Oca", "Mashua", "Camote", "Yuca", "Cebolla", "Ajo",
          "Tomate")
      )
      colnames(Cultures) <- "nomcomplet"
    }

    if (page %in% c(38, 39)) {
      Cultures <- as.data.table(
        c("Departamento","Años", "Total","Arroz cáscara", "Maíz amiláceo",
          "Frijol grano seco**","Frijol castlla", "Pallar","Zarandaja",
          "Frijol de palo", "Garbanzo","Frijol loctao", "Lentaja", "Papa",
          "Trigo", "Maíz duro","Soya", "Sorgo grano", "Marigold")
      )
      colnames(Cultures) <- "nomcomplet"
    }

    if (page %in% c(40,41)) {
      Cultures <- as.data.table(
        c("Departamento", "Años", "Cebada grano", "Quinua","Cañihua","Kiwicha",
          "Haba grano", "Arveja grano", "Chocho tarhui", "Olluco", "Oca",
          "Mashua", "Camote", "Yuca", "Cebolla", "Ajo", "Tomate")
      )
      colnames(Cultures) <- "nomcomplet"
    }


    if (page %in% c(44, 45)) {
      Cultures <- as.data.table(
        c("Departamento","Años", "Arroz cáscara", "Maíz amiláceo",
          "Frijol grano seco**","Frijol castlla", "Pallar","Zarandaja",
          "Frijol de palo", "Garbanzo","Frijol loctao", "Lentaja", "Papa",
          "Trigo","Plátano", "Algodón rama", "Maíz duro", "Soya",
          "Sorgo grano", "Caña de azúcar", "Café", "Espárrago", "Marigold")
      )
      colnames(Cultures) <- "nomcomplet"
    }

    if (page %in% c(46, 47)) {
      Cultures <- as.data.table(
        c("Departamento", "Años", "Cebada grano", "Quinua","Cañihua",
          "Kiwicha","Haba grano", "Arveja grano", "Chocho tarhui", "Olluco",
          "Oca", "Mashua", "Camote", "Yuca", "Cebolla", "Ajo", "Tomate")
      )
      colnames(Cultures) <- "nomcomplet"
    }

    if (page %in% c(112,113)) {
      Cultures <- as.data.table(
        c("Departamento", "Año", "Arroz cáscara", "Maíz amiláceo","Trigo",
          "Camote", "Papa", "Yuca", "Algodón rama", "Espárrago", "Maíz duro",
          "Marigold", "Soya", "Café")
      )
      colnames(Cultures) <- "nomcomplet"
    }

    if (page %in% c(114,115)) {
      Cultures <- as.data.table(
        c("Departamento", "Año", "Ajo", "Cebolla", "Maíz Choclo", "Tomate",
          "Arveja grano", "Haba grano","Limón", "Mandarina", "Manzana",
          "Naranja", "Papaya", "Palta", "Piña")
      )
      colnames(Cultures) <- "nomcomplet"
    }
  } else {
    Cultures <- Cultures[order(-Cultures$y), ]
    Cultures$nomcomplet[2] <-
      if (str_detect(Cultures$nomcomplet[2],"ña")) "Campaña" else Cultures$nomcomplet[2]

    # Selecting the correct data
    Cultures <-Cultures[, c("x","y","nomcomplet")]  |>
      mutate(
        nomcomplet = case_when(
          str_detect(nomcomplet,"Cañi")      ~ "Cañihua",
          str_detect(nomcomplet,"seco")      ~ "Frijol grano seco**",
          str_detect(nomcomplet,"paña")      ~ "Campaña",
          str_detect(nomcomplet,"daja")      ~ "Zarandaja",
          str_detect(nomcomplet,"duro")      ~  "Maíz duro",
          str_detect(nomcomplet,"banzo")     ~  "Garbanzo",
          str_detect(nomcomplet,"de azúcar") ~  "Caña de azúcar",
          str_detect(nomcomplet,"Kiwi")      ~  "Kiwicha",
          TRUE ~ nomcomplet
        )
      ) |>
      mutate(n = duplicated(nomcomplet)) |>
      filter(! n ==T ) |>
      select(-n)

  }

  if (y == 4 & m > 6 & page %in% c(38, 39) ) {
    Cultures <- as.data.table(
      c("Departamento", "Años","Total", "Arroz cáscara",	"Maíz amiláceo",
        "Frijol grano seco**",	"Frijol castlla",	"Pallar",	"Zarandaja",
        "Frijol de palo",	"Garbanzo", "Frijol loctao",	"Lenteja",	"Papa",
        "Trigo",	"Maíz duro",	"Soya",	"Sorgo grano",	"Caña de azúcar" )
    )
    colnames(Cultures) <- "nomcomplet"
  }

  if (page > 100) {
    if (page %in% c(108, 109, 110, 112, 113)) {
      if (isFrijol == T) {
        Cultures <- as.data.table(
          c("Departamento", "Año", "Arroz cáscara", "Maíz amiláceo",
            "Trigo", "Frijol grano seco", "Camote", "Papa", "Yuca",
            "Algodón rama", "Espárrago", "Maíz duro",  "Marigold", "Soya",
            "Café")
        )
        colnames(Cultures) <- "nomcomplet"
      } else {
        Cultures <- as.data.table(
          c("Departamento", "Año", "Arroz cáscara", "Maíz amiláceo","Trigo",
            "Camote", "Papa", "Yuca", "Algodón rama", "Espárrago", "Maíz duro",
            "Marigold", "Soya", "Café")
        )
        colnames(Cultures) <- "nomcomplet"
      }
    }

    if (page %in% c(111, 114, 115)) {
      if (isPlatano == T) {
        Cultures <- as.data.table(
          c("Departamento", "Año", "Ajo", "Cebolla", "Maíz Choclo", "Tomate",
            "Arveja grano", "Haba grano","Limón", "Mandarina", "Manzana",
            "Naranja", "Papaya", "Palta", "Piña","Plátano")
        )
        colnames(Cultures) <- "nomcomplet"
      } else {
        Cultures <- as.data.table(
          c("Departamento", "Año", "Ajo", "Cebolla", "Maíz Choclo", "Tomate",
            "Arveja grano", "Haba grano","Limón", "Mandarina", "Manzana",
            "Naranja", "Papaya", "Palta", "Piña")
        )
        colnames(Cultures) <- "nomcomplet"
      }
    }

    if (annee == 2 & mois < 8 & page %in% c(111, 112)) {
      if (isPlatano == T) {
        Cultures <- as.data.table(
          c("Departamento", "Año", "Ajo", "Cebolla", "Maíz Choclo", "Tomate",
            "Arveja grano", "Haba grano","Limón", "Mandarina", "Manzana",
            "Naranja", "Papaya", "Palta", "Piña","Plátano")
        )
        colnames(Cultures) <- "nomcomplet"
      } else {
        Cultures <- as.data.table(
          c("Departamento", "Año", "Ajo", "Cebolla", "Maíz Choclo", "Tomate",
            "Arveja grano", "Haba grano","Limón","Mandarina", "Manzana",
            "Naranja", "Papaya", "Palta", "Piña")
        )
        colnames(Cultures) <- "nomcomplet"}
    }

    if (annee == 2 & mois == 7 & page == 110) {
      Cultures <- as.data.table(
        c("Departamento", "Año", "Ajo", "Cebolla", "Maíz Choclo", "Tomate",
          "Arveja grano", "Haba grano","Limón", "Mandarina", "Manzana",
          "Naranja", "Papaya", "Palta", "Piña","Plátano")
      )
      colnames(Cultures) <- "nomcomplet"
    }
  }

  ## Downloading the table----
  tx <- pdf_text(adresse)[[page]]
  tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
  tx3 <- as.data.frame(
    str_split_fixed(str_trim(tx2), "\\s{2,}", dim(Cultures)[1])
  )

  # Replacing the name of the row "Cultures"
  n <-  which(str_detect(tx3$V1, Cell1))

  tx3[n, ] <- Cultures$nomcomplet
  if(str_detect(tx3[n + 1, 2], "[:digit:]{4}|[:digit:]{2}[:punct:][:digit:]{2}") == F) {
    tx3 <- tx3[-(n + 1), ]
  }
  tx3 <- tx3[-(n - 1), ]

  if (any(str_detect(tx3$V1, "Estadística"))) {
    tx3 <- tx3[-which(str_detect(tx3$V1, "Estadística")), ]
  }

  if (any(str_detect(tx3$V1, "cáscara"))) {
    n <- which(str_detect(tx3$V1, "cáscara"))

    if (sum(str_detect(tx3[n-1,],"Arroz")) > 0) {
      tx3[n-1,which(str_detect(tx3[n - 1,],"Arroz"))[1]] <- "Arroz cáscara"
    }
    if (sum(str_detect(tx3[n-1,],"Maíz")) > 0) {
      tx3[n-1,which(str_detect(tx3[n - 1,],"Maíz"))[1]] <- "Maíz amiláceo"
    }
    if (sum(str_detect(tx3[n-1,],"Maíz")) > 1) {
      tx3[n-1,which(str_detect(tx3[n - 1, ],"Maíz"))[2]] <- "Maíz duro"
    }
    if (sum(str_detect(tx3[n-1,],"Frijol")) > 0) {
      tx3[n-1,which(str_detect(tx3[n - 1, ],"Frijol"))[1]] <- "Frijol grano seco"
    }
    if (sum(str_detect(tx3[n-1,],"Frijol")) > 1) {
      tx3[n-1,which(str_detect(tx3[n - 1, ],"Frijol"))[2]] <- "Frijol castlla"
    }
    if (sum(str_detect(tx3[n-1,],"Frijol")) > 2) {
      tx3[n-1,which(str_detect(tx3[n - 1, ],"Frijol"))[3]] <- "Frijol de palo"
    }
    if (sum(str_detect(tx3[n-1,],"Frijol")) > 3) {
      tx3[n-1,which(str_detect(tx3[n - 1, ],"Frijol"))[4]] <- "Frijol loctao"
    }
    if (sum(str_detect(tx3[n-1,],"Algodón")) > 0) {
      tx3[n-1,which(str_detect(tx3[n - 1, ],"Algodón"))[1]] <- "Algodón rama"
    }
    if (sum(str_detect(tx3[n-1,],"Sorgo")) > 0) {
      tx3[n-1,which(str_detect(tx3[n - 1, ],"Sorgo"))[1]] <- "Sorgo grano"
    }
    if (sum(str_detect(tx3[n-1,],"Caña")) > 0) {
      tx3[n-1,which(str_detect(tx3[n - 1, ],"Caña"))[1]] <- "Caña de azúcar"
    }
    tx3 <- tx3[-n, ]
  }
  if (annee == 2 & mois == 1 ) {
    tx3 <- tx3 |>
      mutate(
        V1 = replace(V1, V1== 2001, 2002),
        V1 = replace(V1, V1== 2000, 2001),
        V2 = replace(V2, V2== 2001, 2002),
        V2 = replace(V2, V2== 2000, 2001)
      )
  }
  annee <-  ifelse(
    annee < 10,
    yes = paste("200",annee, sep=""),
    no = paste("20",annee, sep="")
  )

  if (any(str_detect(tx3$V1, c("Total nacional 200|Andahuaylas 200")))) {
    n <- str_which(
      tx3$V1,
      "(Total nacional) |(Huancavelica) |(Madre de Dios) |(Andahuaylas) [[:digit:]]{4}"
    )
    for (ii in n) {
      for (j in dim(Cultures)[1]:3) {
        tx3[ii, j] <- tx3[ii, j - 1]
      }
      text <- as.data.frame(str_split(tx3[ii,1], "200"))
      tx3[ii,1] <- text[1,]
      tx3[ii,2] <- paste("200", text[2,], sep = "")
    }
  }

  n <- which(
    tx3$V1 == as.character(as.numeric(annee)-1) | tx3$V1 == as.character(as.numeric(annee)) |
      tx3$V1 == paste(str_sub(as.numeric(annee) - 2, 3, 4), "-", str_sub(as.numeric(annee) - 1, 3, 4), sep = "") |
      tx3$V1 == paste(str_sub(as.numeric(annee) - 1, 3, 4), "-", str_sub(as.numeric(annee), 3, 4), sep = "") |
      tx3$V1 == paste(str_sub(as.numeric(annee), 3, 4), "-", str_sub(as.numeric(annee) + 1, 3, 4), sep = "")
  )

  for (ii in n) {
    for (j in dim(Cultures)[1]:2) {
      tx3[ii, j] <- tx3[ii, j - 1]
    }
    ifelse(tx3[ii - 1, 1] == Cell1, tx3[ii, 1] <-
             tx3[ii + 1, 1],  tx3[ii, 1] <- tx3[ii - 1, 1])
  }
  tx3
}


## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ----setup--------------------------------------------------------------------
library(scicalc)

## -----------------------------------------------------------------------------
data_source <- file.path(here::here(), "inst", "extdata", "data", "source")
if (!dir.exists(data_source)) {
  scicalc::create_dir(data_source) 
  # I'm using this scicalc::create_dir function to create this directory 
  # the first time I'm running this vignette. More on this later
}

list.files(data_source)

## -----------------------------------------------------------------------------
raw_data <- purrr::map(
  list.files(data_source, full.names = TRUE),
  scicalc::read_file_with_hash) %>% 
  purrr::set_names(tools::file_path_sans_ext(list.files(data_source)))

## -----------------------------------------------------------------------------
names(raw_data)

## -----------------------------------------------------------------------------
df <- raw_data$pc

df <- df %>% 
  dplyr::mutate(.data = raw_data$vs,
    BBMI = scicalc::bbmi(WEIGHT, HEIGHT),
    DBSA = scicalc::bsa(WEIGHT, HEIGHT),
    MBSA = scicalc::bsa(WEIGHT, HEIGHT, method = "mosteller"))
df

## -----------------------------------------------------------------------------
dflb <- raw_data$lb %>% 
  dplyr::select(c("ID", "CREAT", "CYSTC", "AST", "BILI", )) %>% 
  dplyr::distinct() %>% 
  tidyr::pivot_longer(cols = c("CREAT", "CYSTC", "AST", "BILI"), 
                      names_to = "LBTESTCD", values_to = "LBORRES")

dfu <- raw_data$lb %>%
  dplyr::select(c("ID", "CREATU", "CYSTCU", "ASTU", "BILIU")) %>% 
  dplyr::distinct() %>% 
  tidyr::pivot_longer(cols = c("CREATU", "CYSTCU", "ASTU", "BILIU"), 
                      names_to = "LBTESTCDU", values_to = "LBORRESU") 

lb <- dflb %>% 
  dplyr::mutate(LBORRESU = dfu$LBORRESU)

lb

## -----------------------------------------------------------------------------
if (scicalc::check_for_unique_units(lb$LBTESTCD, lb$LBORRESU)) {
  print("Units are 1:1!")
} else {
  stop("Error with units!")
}

## -----------------------------------------------------------------------------
scicalc::get_unique_units_df(lb$LBTESTCD, lb$LBORRESU)

## -----------------------------------------------------------------------------
df <- df %>% 
  dplyr::mutate(.data = raw_data$lb, 
    BHFC = scicalc::bhfc(AST, ULNAST, BILI, ULNBILI),
  )
df

## -----------------------------------------------------------------------------
df <- df %>% 
  dplyr::mutate(.data = raw_data$dm, 
    CRCL = scicalc::crcl(scicalc::is_female(SEX), AGE, CREAT, WEIGHT),
    BRFC = scicalc::brfc(CRCL),
    
    ckdepi_2009_egfr = scicalc::egfr(
      sexf = scicalc::is_female(SEX), 
      raceb = scicalc::is_black(RACE), 
      age = AGE, 
      creat = CREAT,
      method = "CKDEPI 2009"),
    ckdepi_2021_egfr = scicalc::egfr(
      sexf = scicalc::is_female(SEX), 
      age = AGE, 
      creat = CREAT, 
      cystc = CYSTC, 
      method = "CKDEPI 2021"),
    mdrd_egfr = scicalc::egfr(
      sexf = scicalc::is_female(SEX), 
      raceb = scicalc::is_black(RACE), 
      age = AGE, 
      creat = CREAT,
      method = "MDRD"),
    schwartz_egfr = scicalc::egfr(
      height = HEIGHT,
      creat = CREAT,
      method = "Schwartz"
    )
  )

df

## -----------------------------------------------------------------------------
data_derived <- file.path(here::here(), "inst", "extdata", "data", "derived")
if (!file.exists(file.path(data_derived, "pk_data_v01.parquet"))) {
  scicalc::write_file_with_hash(
    df, file.path(data_derived, "pk_data_v01.parquet"))  
} else {
  print("Overwriting data!")
  scicalc::write_file_with_hash(
    df, file.path(data_derived, "pk_data_v01.parquet"), overwrite = TRUE)
}

## -----------------------------------------------------------------------------
#This hash is incorrect so the function errors
if (file.exists(file.path(data_derived, "pk_data_v01.parquet"))) {
  testthat::expect_error(scicalc::read_hashed_file(file.path(data_derived, "pk_data_v01.parquet"), "d4e21e3e681bc282ee7a3e758b15cebe"))
}

## -----------------------------------------------------------------------------
data <- scicalc::read_hashed_file(
  file.path(data_derived, "pk_data_v01.parquet"), 
  "41fde04ca4aa6085eddbd8c1ef109684")
data

## ---- include=FALSE-----------------------------------------------------------
df <- data.frame(
  "ID" = factor(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)),
  "SEX" = c("F", "F", "F", "F", "M", "M", "M", "M", "F", "F", "F", "F"),
  "RACE" = c("WHITE", "WHITE", "WHITE", "WHITE", "BLACK", "BLACK", "BLACK", "BLACK", "ASIAN", "ASIAN", "ASIAN", "ASIAN"),
  "AGE" = c(24, 24, 24, 24, 22, 22, 22, 22, 35, 35, 35, 35),
  "CREAT" = c(1, 1, 1, 1, 4, 4, 4, 4, 3, 3, 3, 3),
  "CREATU" = c("mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL"),
  "CYSTC" = c(0.4, 0.4, 0.4, 0.4, 0.9, 0.9, 0.9, 0.9, 0.7, 0.7, 0.7, 0.7),
  "CYSTCU" = c("mg/L","mg/L","mg/L","mg/L","mg/L","mg/L","mg/L","mg/L","mg/L","mg/L","mg/L","mg/L"),
  "AST" = c(15, 15, 15, 15, 29, 29, 29, 29, 38, 38, 38, 38),
  "ULNAST" = c(33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33),
  "ASTU" = c("U/L","U/L","U/L","U/L","U/L","U/L","U/L","U/L", "U/L", "U/L", "U/L", "U/L"),
  "BILI" = c(0.8, 0.8, 0.8, 0.8, 1.9, 1.9, 1.9, 1.9, 1.1, 1.1, 1.1, 1.1),
  "ULNBILI" = c(1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2),
  "BILIU" = c("mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL","mg/dL"),
  "HEIGHT" = c(174 ,174, 174, 174, 201, 201, 201, 201, 190, 190, 190, 190),
  "WEIGHT" = c(70, 70, 70, 70, 80, 80, 80, 80, 75, 75, 75, 75),
  "DV" = c(10, 150, 70, 9, 7, 140, 60, 7, 11, 156, 70, 7),
  "NTLD" = c(0, 0.25, 1, 8, 0, 0.25, 1, 8, 0, 0.25, 1, 8)
)

library(ggplot2)
ggplot2::ggplot(df, aes(x = NTLD, y = DV, color = ID)) +
  geom_point() +
  geom_line()

df %>% 
  dplyr::select(c("ID", "SEX", "RACE", "AGE")) %>% 
  haven::write_sas(path = file.path(data_source, "dm.sas7bdat"))

df %>% 
  dplyr::select(c("ID", "HEIGHT", "WEIGHT")) %>% 
  arrow::write_parquet(sink = file.path(data_source, "vs.parquet"))

df %>% 
  dplyr::select(c("ID", "CREAT", "CREATU", "CYSTC", "CYSTCU", "AST", "ULNAST", "BILI", "ULNBILI", "ASTU", "BILIU")) %>% 
  readr::write_csv(file = file.path(data_source, "lb.csv"))

df %>% 
  dplyr::select(c("ID", "DV", "NTLD")) %>% 
  arrow::write_parquet(sink = file.path(data_source, "pc.parquet"))



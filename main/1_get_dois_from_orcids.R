library(openalexR)
library(tidyverse)
library(readxl)
library(here)
library(fuzzyjoin)
# library(rorcid) # the orcid API did not work for me, so using openAlex instead
library(pdfRetrieve)

email <- Sys.getenv("EMAIL")

# publications <- read_csv("./main/publication_table.csv")

names <- read_xlsx(here("main", "TN_Daten_für_Publikationsanalyse_v1.0_if_2024-02-28.xlsx"),
                   sheet = "Teilnehmendenliste")
names <- names |>
  mutate(full_name = paste(Vorname, Nachname))

# attempt to get orcids and openAlex IDs via openAlex
# Caution!!! Needs validation as sometimes the wrong IDs are retreived

query_oa_names <- oa_fetch(
  entity = "authors",
  display_name.search = names$full_name[1:200], # restricted to first 200 names for now
  mailto = email
)

orcid_names <- query_oa_names |>
  select(contains("display_name"), oaid = id, orcid, affiliation_ror) |>
  rename(full_name = display_name) |>
  rowwise() |>
  mutate(display_name_alternatives = paste(display_name_alternatives, collapse = "; ")) |>
  ungroup()


.get_id_from_name <- function(names_tb, name, idname) {

  names_tb <- names_tb |>
    dplyr::filter(stringr::str_detect(display_name_alternatives, name) |
                    stringr::str_detect(full_name, name))

  if (nrow(names_tb) == 0) return(NA)

  if (nrow(names_tb) > 1) {
    if (any(stringr::str_detect(names_tb$affiliation_display_name, "Charit"), na.rm = TRUE)) {
      names_tb <- names_tb |>
        dplyr::filter(stringr::str_detect(names_tb$affiliation_display_name, "Charit"))
    }
    names_tb <- names_tb |>
      dplyr::slice(1)
  }
  names_tb |>
    dplyr::pull({{idname}})

}

names_meta <- names |>
  slice(1:200) |>
  mutate(orcid = map2_chr(Vorname, Nachname, \(x,y) .get_id_from_name(orcid_names, paste(x, y), orcid)),
         oaid = map2_chr(Vorname, Nachname, \(x,y) .get_id_from_name(orcid_names, paste(x, y), oaid)))


# Save file for manual verification and correct or fill in ORCIDs or openAlexIDs by hand!
names_meta |>
  write_excel_csv(here("main", "Metadata_orcids.csv"))


###### Search publications

author_ids <- read_csv(here("main", "Metadata_orcids.csv"))

# extract vector of open Alex IDs

oaid <- author_ids |>
  drop_na(oaid) |>
  pull(oaid)

# oa_pubs <- oa_fetch(
#   entity = "works",
#   authorships.author.id = oaid,
#   mailto = email
# )


.get_works_from_authorid <- function(authorid, type = "oaid", email) {

  if (type == "oaid") {
    query_id <- openalexR::oa_query(
      entity = "works",
      author.id = authorid
    )
  } else {
    query_id <- openalexR::oa_query(
      entity = "works",
      author.orcid = authorid
    )
  }

  res <- openalexR::oa_request(query_url = query_id, mailto = email)

  openalexR::oa2df(res, entity = "works")
}

rate = rate_delay(0.5)
email <- Sys.getenv("EMAIL")

slow_get_works <- slowly(\(x, type, email) .get_works_from_authorid(x, type, email), rate = rate, quiet = FALSE)

results_oaids <- oaid |>
  map(\(x) slow_get_works(x, type = "oaid", email = email))

# repeat as above but with ORCIDs

orcids <- author_ids |>
  drop_na(orcid) |>
  pull(orcid)

results_orcids <- orcids |>
  map(\(x) slow_get_works(x, type = "orcid", email = email))


# Save retrieved publications as rds file for further processing

c(results_oaids, results_orcids) |>
  list_rbind() |>
  distinct(doi, .keep_all = TRUE) |>
  saveRDS(here("results", "transfer_works.rds"))

library(assertthat)
library(haven)
library(tidyverse)
library(vroom)
library(here)
library(easyRPubMed)
library(janitor)
library(readxl)

#----------------------------------------------------------------------------------------
# load results
#----------------------------------------------------------------------------------------

publications <- read_csv(here("main", "publication_table_old.csv")) |>
  mutate(doi = tolower(doi))
publications |>
  count(year)

dupes <- get_dupes(publications, doi)


publications_2022 <- read_xlsx(here("main", "Publikationsdatensatz Charité_2022_230922VN.xlsx")) |>
  filter(!str_detect(Duplikat, "erscheint") | is.na(Duplikat),
         is.na(Keine_Charite_Affiliation)) |>
  transmute(doi = tolower(DOI),
            pmid = `Pubmed Id`,
            title = `Article Title`,
            journal = `Source Title`,
            year = case_when(
              str_detect(`Early Access Date`, " ") ~ year(my(`Early Access Date`)),
              !str_detect(`Early Access Date`, " ") ~ as.numeric(`Early Access Date`) |>
                as.Date(origin = "1899-12-30") |>
                year(),
              str_detect(Duplikat, "halb") ~ year(`Date of Export`) - 1,
              .default = `Publication Year`
            ),
            publisher = Publisher,
            issn = ISSN,
            e_issn = eISSN,
            oa_indicator = `Open Access Designations`,
            oa_status = `OA-Status`) |>
  select(all_of(names(publications))) |>
  filter(year < 2023)

dupes <- get_dupes(publications_2022, doi)

publications_2022 |>
  count(year)

only_new_dois <- publications_2022 |>
  filter(!doi %in% publications$doi |
           str_detect(doi, "keine"))

publications <- publications |>
  rows_append(only_new_dois) |>
  # filter(str_detect(doi, "10"), doi %in% publications_2022$doi) |>
  rows_upsert(publications_2022 |>
                filter(str_detect(doi, "10")) |>
                select(doi, oa_status), by = "doi")

count(publications, year)

corresponding_authorship <- read_csv("T:/Dokumente/publications_CA.csv")

publications <- publications |>
  left_join(corresponding_authorship, by = "doi")


write_excel_csv(publications, here("main", "publication_table.csv"))


publications <- read_csv(here("main", "publication_table.csv"))

#results files
# results_files <- list.files(here("results"), full.names = TRUE)
# results_files <- paste0(results_folder, results_files)

open_data_22 <- read_csv(here("results", "Open_Data.csv")) |>
  mutate(doi = str_replace_all(article, "\\+", "\\/") |>
           str_remove(".txt") |>
           tolower()) |>
  select(doi, everything(), -article)



#manually checked Open Data results + additional cases that were not detected by algorithm
#but found in manual searches or submitted by researchers for the LOM


open_data_22_manual <- read_csv2(here("results", "OD_manual_tidy.csv")) |>
  mutate(doi = tolower(doi)) |>
  select(doi, open_data_manual_check, open_data_category_manual, data_access)

open_code_22_manual <- read_xlsx(here("results", "oddpub_code_results_manual.xlsx")) |>
  select(doi, contains("code")) |>
  mutate(is_open_code = as.logical(is_open_code),
         open_code_manual_check = as.logical(open_code_manual_check))


# dupes <- open_data_22 |>
#   filter(doi %in% manual_code_results$doi)
# publications |>
#   filter(doi %in% dupes$doi,
#          year == 2022)

open_data_results <- vroom(here("results", "Open_Data_manual_check_results.csv")) |>
  mutate(is_reuse = NA,
         das = NA_character_,
         cas = NA_character_) |>
  # rows_upsert(open_data_22, by = "doi") |>
  # rows_upsert(open_code_22_manual, by = "doi") |>
  rows_upsert(open_data_22_manual, by = "doi") |>
  mutate(open_code_category_manual = case_when(
    str_detect(open_code_category_manual, "github") ~ "github",
    str_detect(open_code_category_manual, "supplement") ~ "supplement",
    open_code_manual_check == TRUE ~ "other repository/website"
  ),
  open_code_manual_check = case_when(
    str_detect(open_code_category_manual, "supplement") ~ FALSE,
    .default = open_code_manual_check),
  open_data_manual_check = case_when(
    str_detect(open_data_category_manual, "supplement") ~ FALSE,
    .default = open_data_manual_check),
  restrictions = case_when(
    str_detect(data_access, "yes") & str_detect(data_access, "restricted") ~ "partial",
    str_detect(data_access, "restricted") ~ "full",
    .default = "no restricted data"
  )
  )

old_das_cas <- vroom(here("results", "Open_Data_retroactive.csv")) |>
  select(doi, das, cas)

open_data_results <- open_data_results |>
  rows_upsert(old_das_cas, by = "doi")

#
open_data_results |>
  count(open_data_manual_check, open_data_category_manual)


#ContriBOT results
contribot_results <- read_csv(here("results", "ContriBOT.csv"))
contribot_results_retro <- read_csv(here("results", "ContriBOT_retroactive.csv"))
contribot_results <- contribot_results_retro |>
  rows_upsert(contribot_results, by = "doi")

orcid_screening_results <- read_csv(here("results", "orcids_extracted.csv"))
orcid_screening_results_retro <- read_csv(here("results", "orcids_retroactive.csv")) |>
  mutate(doi = str_extract(file, "10\\..*") |>
           str_remove(".pdf") |>
           str_replace_all("\\+", "/")) |>
  select(-file)
orcid_screening_results <- orcid_screening_results_retro |>
  rows_upsert(orcid_screening_results, by = "doi")

#Barzooka results
barzooka_results <- read_csv(here("results", "Barzooka.csv"))

barzooka_results <- barzooka_results |>
  rename(doi = paper_id) |>
  select(doi, bar, pie, bardot, box, dot, hist, violin) |>
  distinct(doi, .keep_all = TRUE)

missing_dois <- setdiff(contribot_results$doi, publications$doi)

publications |>
  filter(doi %in% missing_dois)

#----------------------------------------------------------------------------------------
# combine results
#----------------------------------------------------------------------------------------

#check if there are no duplicated dois in the results files (problem for left_join)
assert_that(length(open_data_results$doi) == length(unique(open_data_results$doi)))
assert_that(length(barzooka_results$doi) == length(unique(barzooka_results$doi)))


dashboard_metrics <- publications |>
  left_join(open_data_results, by = "doi") |>
  left_join(contribot_results, by = "doi") |>
  left_join(orcid_screening_results, by = "doi") |>
  rename(oa_color = oa_status) |>
  mutate(has_any_orcid = has_orcid | !is.na(orcids)) |>
  left_join(barzooka_results, by = "doi") |>
  mutate(pdf_downloaded = !is.na(bar) | !is.na(has_contrib))

dashboard_metrics |>
  filter(pdf_downloaded == TRUE) |>
  count(has_any_orcid, has_orcid)

orcid_screening_results |>
  count(is.na(orcids))


per_year <- dashboard_metrics |>
  count(year, open_data_category_manual) |>
  group_by(year) |>
  mutate(perc = n / sum(n) * 100)

per_year <- dashboard_metrics |>
  count(year, open_code_category_manual) |>
  group_by(year) |>
  mutate(perc = n / sum(n) * 100)


oc_dois <- manual_code_results |>
  filter(open_code_manual_check == TRUE,
         open_code_category_manual != "supplement") |>
  pull(doi)

dashboard_metrics |>
  filter(doi %in% oc_dois) |>
  count(year, open_code_manual_check)


no_orcid_hyperlinks <- dashboard_metrics |>
  filter(has_orcid, is.na(orcids))


#----------------------------------------------------------------------------------------
# further preprocessing & validation steps
#----------------------------------------------------------------------------------------

#data plausibility/quality check for the updated PDF dataset -> are there any new
#cases that we missed? For old PDF dataset, there were 0 cases, now again 0 cases
check_tbl <- dashboard_metrics |>
  filter(
    (is_open_data & is.na(open_data_manual_check)) |
           (is_open_code & is.na(open_code_manual_check))) |>
  select(doi, year, is_open_data, open_data_category, is_open_code,
         open_data_statements, open_code_statements,
         open_data_manual_check, open_data_category_manual,
         open_code_manual_check, open_code_category_manual)
assert_that(dim(check_tbl)[1] == 0)
# write_csv(check_tbl, "./results/OD_manual_check/pdf_update_cases.csv")


#----------------------------------------------------------------------------------------
# save resulting table with relevant columns only
#----------------------------------------------------------------------------------------

#only select columns relevant for shiny table
shiny_table <- dashboard_metrics |>
  select(doi, pmid, title, journal, year,
         publisher, issn, e_issn,
         corresponding_author_charite,
         pdf_downloaded, oa_color,
         is_open_data, open_data_manual_check, open_data_category_manual,
         is_open_code, open_code_manual_check, open_code_category_manual,
         open_data_statements, open_code_statements,
         das, cas,
         restrictions,
         has_contrib, contrib_statement,
         has_orcid, orcid_statement,
         orcids, has_any_orcid,
         bar, pie, bardot, box, dot, hist, violin)

################### here subset the regular dashboard data set by the DOIs retieved in 1_get_dois_from_orcids.R

subset_dois <- readRDS(here("results", "transfer_works.rds"))

subset_dois <- subset_dois |>
  mutate(doi = pdfRetrieve::doi_full2stripped(doi)) |> # if you don't have pdfRetrieve set dois to lower and remove the https part
  distinct(doi) |>
  pull(doi)

shiny_table <- shiny_table |>
  filter(doi %in% subset_dois)

write_csv(shiny_table, here("shiny_app", "data", "dashboard_metrics.csv"))

#
#----------------------------------------------------------------------------------------
# now for the metrics that are already aggregated by year
#----------------------------------------------------------------------------------------

preprints_dataset_shiny <- vroom(here("results", "preprints_oa.csv")) |>
  select(doi, title, journal_title, year) |>
  filter(doi %in% subset_dois)
write_csv(preprints_dataset_shiny, here("shiny_app", "data", "preprints_dataset_shiny.csv"))

# orcid_dataset_shiny <- vroom(here("results", "orcid.csv")) |>
#   distinct(date, .keep_all = TRUE) |> # next part only if false parsing in file with spliced rows
#   mutate(orcid_count = if_else(str_length(orcid_count) > 5, str_sub(orcid_count, 1, 4),
#                                as.character(orcid_count)),
#          orcid_count = as.numeric(orcid_count))
# write_csv(orcid_dataset_shiny, here("shiny_app", "data", "orcid_results.csv"))

preprints <- vroom(here("results", "preprints_oa.csv")) |>
  filter(doi %in% subset_dois) |>
  group_by(year) |>
  summarize(n_preprints = n())

max_year <- 2022

shiny_table_aggregate_metrics <- tibble(year = 2006:max_year) |>
  left_join(preprints)

write_csv(shiny_table_aggregate_metrics, here("shiny_app", "data", "dashboard_metrics_aggregate.csv"))

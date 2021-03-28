# Download the data and store it in the `data` folder.

for (file_name in c("defendant_docket_ids.csv",
                    "offenses_dispositions.csv",
                    "defendant_docket_details.csv",
                    "bail.csv")) {
  download.file(paste("https://storage.googleapis.com/jat-rladies-2021-datathon",
                       file_name, sep = "/"),
                file.path(rprojroot::find_root(rprojroot::is_rstudio_project),
                          "data", file_name))
}

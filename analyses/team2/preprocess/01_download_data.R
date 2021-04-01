# Download the data and store it in the `data` folder.

base_uri <- "https://storage.googleapis.com/jat-rladies-2021-datathon"

for (file_name in c("defendant_docket_ids.csv",
                    # I'm replacing this with the `_v1` file that combines yearly files
                    #"offenses_dispositions.csv",
                    # This is erroring-out, so we're gonna handle it separately
                    #"offenses_dispositions_v2.csv",
                    "defendant_docket_details.csv",
                    "bail.csv")) {
  download.file(paste(base_uri, file_name, sep = "/"),
                file.path(rprojroot::find_root(rprojroot::is_rstudio_project),
                          "data", file_name))
}

# Instead of downloading the single combined file for the dispositions, we'll
# download the yearly files and concatenate them.

year_sequence <- c("2010_2011", "2012_2013", "2014_2015", "2016_2017", "2018_2019", "2020")

offenses_dispositions_v2_list <- lapply(year_sequence, 
                                        function(x) read.csv(paste0(base_uri, 
                                                                    "/offenses_dispositions_v2_",
                                                                    x, ".csv"),
                                                             stringsAsFactors = FALSE))
offenses_dispositions_v2 <- do.call(rbind, offenses_dispositions_v2_list)
write.csv(offenses_dispositions_v2,
          file.path(rprojroot::find_root(rprojroot::is_rstudio_project),
                    "data", "offenses_dispositions_v2.csv"))

offenses_dispositions_v1_list <- lapply(year_sequence, 
                                        function(x) read.csv(paste0(base_uri, 
                                                                    "/offenses_dispositions_",
                                                                    x, ".csv"),
                                                             stringsAsFactors = FALSE))
offenses_dispositions_v1 <- do.call(rbind, offenses_dispositions_v1_list)
write.csv(offenses_dispositions_v1,
          file.path(rprojroot::find_root(rprojroot::is_rstudio_project),
                    "data", "offenses_dispositions_v1.csv"))

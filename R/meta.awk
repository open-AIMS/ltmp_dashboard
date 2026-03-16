BEGIN { OFS = "," }
NR > 1 {
  # split($18, d, "-")  # Adjust $2 to your date column index
  # year = d[3]        # Extract the year
    year = $16
  max[year > max[$1]] = year
}
END {
  for (cat in max) print cat, max[cat]
}




cut -d, -f17,5 < photo-transect.csv | uniq | Rscript -e "library(dplyr); library(readr); input <- read_csv(stdin()); input  <- input |> mutate(SURVEY_DATE = as.POSIXct(SURVEY_DATE, format='%d-%b-%Y %H:%M:%S')) |> group_by(REEF_NAME) |> summarise(SURVEY_DATE=max(SURVEY_DATE)); write.csv(input, row.names=FALSE)"

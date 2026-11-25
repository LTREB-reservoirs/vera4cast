url_P1D <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/terrestrial_daily-targets.csv.gz"
terrestrial_targets <- read_csv(url_P1D, show_col_types = FALSE)


## find most recent date that has all sites ##
all_sites <- n_distinct(terrestrial_targets$site_id)
all_vars <- n_distinct(terrestrial_targets$variable)

date_coverage <- terrestrial_targets |>
  group_by(datetime) |>
  summarise(n_sites = n_distinct(site_id), n_vars = n_distinct(variable), .groups = "drop")

# No date has all 47 sites; 2025-05-02 is the most recent with 43 sites (84-86 rows)
target_date <- date_coverage |>
  filter(n_sites == all_sites, n_vars == all_vars) |>
  summarise(latest = max(datetime)) |>
  pull(latest)

target_date



terrestrial_targets |>
  group_by(datetime) |>
  summarise(n_obs = n(), n_sites = n_distinct(site_id), n_var = n_distinct(variable), .groups = "drop") |>
  arrange(desc(n_sites), desc(n_var), desc(datetime)) |>
  head(10)


## calculate the most recent date for each site ##
terrestrial_targets |>
  group_by(site_id) |>
  summarise(last_date = as.Date(max(datetime)), .groups = "drop") |>
  arrange(desc(last_date), site_id) |>
  print(n = 47)



## test downloads ##
download.file('https://storage.googleapis.com/neon-sae-files/ods/dataproducts/DP4/2026-09-01/ABBY/NEON.D16.ABBY.DP4.00200.001.nsae.2026-09-01.expanded.20260921T121315Z.h5', './targets/target_generation/test_download.h5')

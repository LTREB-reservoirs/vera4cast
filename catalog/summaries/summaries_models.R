library(arrow)
library(dplyr)
library(gsheet)
library(readr)

#source('catalog/R/stac_functions.R')
config <- yaml::read_yaml('challenge_configuration.yaml')
catalog_config <- config$catalog_config

## CREATE table for column descriptions
summaries_description_create <- data.frame(reference_datetime = 'datetime that the forecast was initiated (horizon = 0)',
                                          site_id = 'For forecasts that are not on a spatial grid, use of a site dimension that maps to a more detailed geometry (points, polygons, etc.) is allowable. In general this would be documented in the external metadata (e.g., alook-up table that provides lon and lat)',
                                          datetime = 'datetime of the forecasted value (ISO 8601)',
                                          family = 'For ensembles: “ensemble.” Default value if unspecified for probability distributions: Name of the statistical distribution associated with the reported statistics. The “sample” distribution is synonymous with “ensemble.”For summary statistics: “summary.”',
                                          pub_datetime = 'datetime that forecast was submitted',
                                          depth_m = 'depth (meters) in water column of prediction',
                                          mean = 'mean forecast prediction',
                                          median = 'median forecast prediction',
                                          sd = 'standard deviation forecasts',
                                          quantile97.5 = 'upper 97.5 percentile value of forecast',
                                          quantile02.5 = 'upper 2.5 percentile value of forecast',
                                          quantile90 = 'upper 90 percentile value of forecast',
                                          quantile10 = 'upper 10 percentile value of forecast',
                                          project_id = 'unique identifier for the forecast project',
                                          duration = 'temporal duration of forecast (hourly, daily, etc.); follows ISO 8601 duration convention',
                                          variable = 'name of forecasted variable',
                                          model_id = 'unique model identifier',
                                          reference_date = 'date that the forecast was initiated')


summaries_theme_df <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'), endpoint_override = config$endpoint, anonymous = TRUE)) #|>


var_remove <- c("DC_mgL_sample","DOC_mgL_sample","NH4_ugL_sample","TN_ugL_sample",
                "TP_ugL_sample","DN_mgL_sample","SRP_ugL_sample","NO3NO2_ugL_sample", "DIC_mgL_sample")
model_remove <- c('historic_mean', 'persistenceRW')

summaries_date_range <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'), endpoint_override = config$endpoint, anonymous = TRUE)) |>
  filter(!(variable %in% var_remove & model_id %in% model_remove)) |>
  summarize(across(all_of(c('datetime')), list(min = min, max = max))) |>
  collect()

theme_models <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'), endpoint_override = config$endpoint, anonymous = TRUE)) |>
  distinct(model_id) |>
  collect()

summaries_sites <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'), endpoint_override = config$endpoint, anonymous = TRUE)) |>
  filter(variable != 'DIC_mgL_sample') |>
  distinct(site_id) |>
  collect()

summaries_duck_df <- duckdbfs::open_dataset(paste0('s3://',catalog_config$aws_download_path_summaries,'?endpoint_override=',config$endpoint), anonymous = TRUE)

# summaries_date_range <- summaries_duck_df |>
#   summarize(across(all_of(c('datetime')), list(min = min, max = max)))

summaries_min_date <-  summaries_date_range |> pull(datetime_min)
summaries_max_date <-  summaries_date_range |> pull(datetime_max)

build_description <- paste0("Summaries are the forecasts statistics of the raw forecasts (i.e., mean, median, confidence intervals). You can access the summaries at the top level of the dataset where all models, variables, and dates that forecasts were produced (reference_datetime) are available. The code to access the entire dataset is provided as an asset. Given the size of the forecast catalog, it can be time-consuming to access the data at the full dataset level. For quicker access to the forecasts for a particular model (model_id), we also provide the code to access the data at the model_id level as an asset for each model.")

stac4cast::build_forecast_scores(table_schema = summaries_theme_df,
                                 #theme_id = 'Forecasts',
                                 table_description = summaries_description_create,
                                 start_date = as.Date(summaries_min_date),
                                 end_date = as.Date(summaries_max_date),
                                 id_value = "summaries",
                                 description_string = build_description,
                                 about_string = catalog_config$about_string,
                                 about_title = catalog_config$about_title,
                                 theme_title = "Forecast Summaries",
                                 destination_path = catalog_config$summaries_path,
                                 aws_download_path = catalog_config$aws_download_path_summaries,
                                 link_items = stac4cast::generate_group_values(group_values = names(config$variable_groups)),
                                 thumbnail_link = catalog_config$summaries_thumbnail,
                                 thumbnail_title = catalog_config$summaries_thumbnail_title,
                                 group_sites = summaries_sites$site_id,
                                 model_child = FALSE)

## CREATE MODELS
variable_gsheet <- gsheet2tbl(config$target_metadata_gsheet)

# read in model metadata and filter for the relevant project
registered_model_id <- gsheet2tbl(config$model_metadata_gsheet) |>
  filter(`What forecasting challenge are you registering for?` == config$project_id)


## BUILD VARIABLE GROUPS

for (i in 1:length(config$variable_groups)){ # LOOP OVER VARIABLE GROUPS -- BUILD FUNCTION CALLED AFTER ALL VARIABLES HAVE BEEN BUILT (AFTER SECOND LOOP)
  print(names(config$variable_groups)[i])

  # check data and skip if no data found
  var_group_data_check <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'),
                                                               endpoint_override = config$endpoint, anonymous=TRUE)) |>
    filter(variable %in% c(config$variable_groups[[i]]$variable)) |>
    summarise(n = n()) |>
    collect()

  if (var_group_data_check$n == 0){
    print('No data available for group')
    next
  }


  ## REMOVE STALE OR UNUSED DIRECTORIES
  current_var_path <- paste0(catalog_config$summaries_path,names(config$variable_groups[i]))
  current_var_dirs <- list.dirs(current_var_path, recursive = FALSE, full.names = TRUE)
  unlink(current_var_dirs, recursive = TRUE)

  if (!dir.exists(file.path(catalog_config$summaries_path,names(config$variable_groups[i])))){
    dir.create(file.path(catalog_config$summaries_path,names(config$variable_groups[i])))
  }

  # match variable with full name in gsheet
  var_gsheet_arrange <- variable_gsheet |>
    arrange(duration)

  var_values <- names(config$variable_groups[[i]]$group_vars)

  var_name_full <- var_gsheet_arrange[which(var_gsheet_arrange$`"official" targets name` %in% var_values),1][[1]]

  ## CREATE VARIABLE GROUP JSONS
  group_description <- paste0('All ',names(config$variable_groups[i]),' variables for the forecasting challenge')

  ## find group sites
  find_group_sites <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'),
                                                           endpoint_override = config$endpoint, anonymous = TRUE))|>
    filter(variable %in% var_values) |>
    distinct(site_id) |>
    collect()

  ## create empty vector to track publication information
  citation_build <- c()
  doi_build <- c()

  ## create empty vector to track variable information
  variable_name_build <- c()

  for(j in 1:length(config$variable_groups[[i]]$group_vars)){ # FOR EACH VARIABLE WITHIN A MODEL GROUP

    var_name <- names(config$variable_groups[[i]]$group_vars[j])
    print(var_name)

    for (k in 1:length(config$variable_groups[[i]]$group_vars[[j]]$duration)){
      duration_value <- config$variable_groups[[i]]$group_vars[[j]]$duration[k]
      print(duration_value)

      ## save original duration name for reference
      duration_name <- config$variable_groups[[i]]$group_vars[[j]]$duration[k]

      ## create formal variable name
      duration_value[which(duration_value == 'P1D')] <- 'Daily'
      duration_value[which(duration_value == 'PT1H')] <- 'Hourly'
      duration_value[which(duration_value == 'PT30M')] <- '30min'
      duration_value[which(duration_value == 'P1W')] <- 'Weekly'

      var_formal_name <- paste0(duration_value,'_',var_name_full[j])

      # check data and skip if no data found
      var_data_check <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'),
                                                             endpoint_override = config$endpoint, anonymous = TRUE)) |>
        filter(variable == var_name, duration == duration_name) |>
        summarise(n = n()) |>
        collect()

      if (var_data_check$n == 0){
        print('No data available for variable')
        next
      }

      if (!dir.exists(file.path(catalog_config$summaries_path,names(config$variable_groups)[i],var_formal_name))){
        dir.create(file.path(catalog_config$summaries_path,names(config$variable_groups)[i],var_formal_name))
      }

      var_date_range <-  arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'), endpoint_override = config$endpoint, anonymous = TRUE)) |>
        filter(variable == var_name,
               duration == duration_name) |>
        summarize(across(all_of(c('datetime')), list(min = min, max = max))) |>
        collect()

      var_min_date <- var_date_range$datetime_min
      var_max_date <- var_date_range$datetime_max

      var_models <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'),
                                                           endpoint_override = config$endpoint, anonymous = TRUE)) |>
        filter(variable == var_name, duration == duration_name) |>
        distinct(model_id) |>
        collect() |>
        filter(model_id %in% registered_model_id$model_id,
               !grepl("example",model_id))

      find_var_sites <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'),
                                                             endpoint_override = config$endpoint, anonymous = TRUE)) |>
        filter(variable == var_name) |>
        distinct(site_id) |>
        collect()

      var_metadata <- variable_gsheet |>
        filter(`"official" targets name` == var_name,
               duration == duration_name)

      var_description <- paste0('All models for the ',var_formal_name,' variable. This variable describes the ',
                                var_metadata$Description)

      #var_path <- gsub('forecasts','scores',var_data$path[1])
      #var_path <- var_data$path[1]

      ## build lists for creating publication items
      var_citations <- config$variable_groups[[i]]$group_vars[[j]]$var_citation
      var_doi <- config$variable_groups[[i]]$group_vars[[j]]$var_doi

      #update group list of publication information
      citation_build <- append(citation_build, var_citations)
      doi_build <- append(doi_build, var_doi)

      variable_name_build <- append(variable_name_build, var_formal_name)

      stac4cast::build_group_variables(table_schema = summaries_theme_df,
                                       #theme_id = var_formal_name[j],
                                       table_description = summaries_description_create,
                                       start_date = as.Date(var_min_date),
                                       end_date = as.Date(var_max_date),
                                       id_value = var_formal_name,
                                       description_string = var_description,
                                       about_string = catalog_config$about_string,
                                       about_title = catalog_config$about_title,
                                       dashboard_string = catalog_config$dashboard_url,
                                       dashboard_title = catalog_config$dashboard_title,
                                       theme_title = var_formal_name,
                                       destination_path = file.path(catalog_config$summaries_path,names(config$variable_groups)[i],var_formal_name),
                                       aws_download_path = catalog_config$aws_download_path_summaries,
                                       group_var_items = stac4cast::generate_variable_model_items(model_list = var_models$model_id),
                                       thumbnail_link = config$variable_groups[[i]]$thumbnail_link,
                                       thumbnail_title = "Thumbnail Image",
                                       group_var_vector = NULL,
                                       single_var_name = var_name,
                                       group_duration_value = duration_name,
                                       group_sites = find_var_sites$site_id,
                                       citation_values = var_citations,
                                       doi_values = var_doi)

      forecast_sites <- c()

      ## LOOP OVER MODEL IDS AND CREATE JSONS
      for (m in var_models$model_id){

        # make model items directory
        if (!dir.exists(paste0(catalog_config$summaries_path,'/',names(config$variable_groups)[i],'/',var_formal_name,"/models"))){
          dir.create(paste0(catalog_config$summaries_path,'/',names(config$variable_groups)[i],'/',var_formal_name,"/models"))
        }

        print(m)

        model_date_range <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'), endpoint_override = config$endpoint, anonymous = TRUE)) |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name,
                 variable != 'DIC_mgL_sample') |>
          summarize(across(all_of(c('datetime','reference_date','pub_datetime')), list(min = min, max = max))) |>
          collect()

        model_min_date <- model_date_range$datetime_min
        model_max_date <- model_date_range$datetime_max

        model_reference_date <- model_date_range$reference_date_max
        model_pub_date <- model_date_range$pub_datetime_max

        model_var_duration_df <-  arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'), endpoint_override = config$endpoint, anonymous = TRUE)) |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name,
                 variable != 'DIC_mgL_sample') |>
          distinct(variable,duration, project_id) |>
          collect() |>
          mutate(duration_name = ifelse(duration == 'P1D', 'Daily', duration)) |>
          mutate(duration_name = ifelse(duration == 'PT1H', 'Hourly', duration_name)) |>
          mutate(duration_name = ifelse(duration == 'PT30M', '30min', duration_name)) |>
          mutate(duration_name = ifelse(duration == 'P1W', 'Weekly', duration_name))

        model_var_full_name <- model_var_duration_df |>
          left_join((variable_gsheet |>
                       select(variable = `"official" targets name`, full_name = `Variable name`) |>
                       distinct(variable, .keep_all = TRUE)), by = c('variable'))

        model_sites <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'),
                                                            endpoint_override = config$endpoint, anonymous=TRUE)) |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name) |>
          distinct(site_id) |>
          collect()

        model_site_text <- paste(as.character(model_sites$site_id), sep="' '", collapse=", ")

        model_vars <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'),
                                                           endpoint_override = config$endpoint, anonymous=TRUE)) |>
          filter(model_id == m,
                 variable == var_name,
                 duration == duration_name) |>
          distinct(variable) |>
          collect() |>
          left_join(model_var_full_name, by = 'variable')

        model_vars$var_duration_name <- paste0(model_vars$duration_name, " ", model_vars$full_name)

        forecast_sites <- append(forecast_sites,  stac4cast::get_site_coords(site_metadata = catalog_config$site_metadata_url,
                                                                             sites = model_sites$site_id))

        stac_id <- paste0(m,'_',var_name,'_',duration_name,'_summaries')

        idx = which(registered_model_id$model_id == m)

        if(is.na(model_pub_date)){
            model_pub_date <- model_reference_date
          }

        model_description <- paste0("This page includes summaries for the ",
                                    var_formal_name,
                                    ' variable for the ',
                                    m,
                                    ' model. Information for the model is provided as follows: ',
                                    registered_model_id[idx,"Describe your modeling approach in your own words."][[1]],
                                    '.
                                    The model predicts this variable at the following sites: ',
                                    model_site_text,
                                    '.
                                    Summaries are the forecasts statistics of the raw forecasts (i.e., mean, median, confidence intervals)')

        model_keywords <- c(list('Summaries',config$project_id, names(config$variable_groups)[i], m, var_name_full[j], var_name, duration_value, duration_name),
                            as.list(model_sites$site_id))

        model_type <- registered_model_id$`Which category best matches your modeling approach?`[idx]

        if(model_type %in% c('Empirical (a statistical model)', 'Empirical', 'empirical')){
          model_type_keyword <- "empirical"
        } else if(model_type %in% c('Machine Learning', 'ML', 'Machine learning', 'machine learning')){
          model_type_keyword <- 'machine learning'
        } else if (model_type %in% c('Process Based', 'Process based', 'process based')){
          model_type_keyword <- 'process based'
        } else{
          model_type_keyword <- NA
        }

        if (is.na(model_type_keyword)){
          model_keywords <- c(list('Summaries',config$project_id, names(config$variable_groups)[i], m, var_name_full[j], var_name, duration_value, duration_name),
                              as.list(model_sites$site_id))
        }else{
          model_keywords <- c(list('Summaries',config$project_id, names(config$variable_groups)[i], m, var_name_full[j], var_name, duration_value, duration_name),
                              as.list(model_sites$site_id), model_type_keyword)
        }

        ## build radiantearth stac and raw json link
        stac_link <- paste0('https://radiantearth.github.io/stac-browser/#/external/raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/catalog/summaries/',
                            names(config$variable_groups)[i],'/',
                            var_formal_name, '/models/',
                            m,'.json')

        json_link <- paste0('https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/catalog/summaries/',
                            names(config$variable_groups)[i],'/',
                            var_formal_name, '/models/',
                            m,'.json')

        stac4cast::build_model(model_id = m,
                               stac_id = stac_id,
                               team_name = registered_model_id$`Long name of the model (can include spaces)`[idx],
                               model_description = registered_model_id[idx,"Describe your modeling approach in your own words."][[1]],
                               start_date = as.Date(model_min_date),
                               end_date = as.Date(model_max_date),
                               pub_date = model_pub_date,
                               forecast_date = model_reference_date,
                               var_values = model_vars$var_duration_name,
                               duration_names = model_var_duration_df$duration,
                               duration_value = duration_name,
                               site_values = model_sites$site_id,
                               site_table = catalog_config$site_metadata_url,
                               model_documentation = registered_model_id,
                               destination_path = paste0(catalog_config$summaries_path,'/',names(config$variable_groups)[i],'/',var_formal_name,"/models"),
                               aws_download_path = catalog_config$aws_download_path_summaries, # USE SCORES BUCKET FOR MODELS
                               collection_name = 'summaries',
                               thumbnail_image_name = NULL,
                               table_schema = summaries_theme_df,
                               table_description = summaries_description_create,
                               full_var_df = model_vars,
                               code_web_link = registered_model_id$`Web link to model code`[idx],
                               model_keywords = model_keywords,
                               stac_web_link = stac_link,
                               raw_json_link = json_link)
      } # end model loop

    } ## end duration loop

  } ## end variable loop

  group_date_range <- arrow::open_dataset(arrow::s3_bucket(paste0(config$forecasts_bucket,'/bundled-summaries'), endpoint_override = config$endpoint, anonymous = TRUE)) |>
    filter(variable %in% names(config$variable_groups[[i]]$group_vars)) |> ## filter by
    summarize(across(all_of(c('datetime')), list(min = min, max = max))) |>
    collect()
  group_min_date <- group_date_range$datetime_min
  group_max_date <- group_date_range$datetime_max

  ## BUILD THE GROUP PAGES WITH UPDATED VAR/PUB INFORMATION
  stac4cast::build_group_variables(table_schema = summaries_theme_df,
                                   table_description = summaries_description_create,
                                   start_date = as.Date(group_min_date),
                                   end_date = as.Date(group_max_date),
                                   id_value = names(config$variable_groups)[i],
                                   description_string = group_description,
                                   about_string = catalog_config$about_string,
                                   about_title = catalog_config$about_title,
                                   dashboard_string = catalog_config$dashboard_url,
                                   dashboard_title = catalog_config$dashboard_title,
                                   theme_title = names(config$variable_groups[i]),
                                   destination_path = file.path(catalog_config$summaries_path,names(config$variable_groups)[i]),
                                   aws_download_path = catalog_config$aws_download_path_summaries,
                                   group_var_items = stac4cast::generate_group_variable_items(variables = variable_name_build),
                                   thumbnail_link = config$variable_groups[[i]]$thumbnail_link,
                                   thumbnail_title = config$variable_groups[[i]]$thumbnail_title,
                                   group_var_vector = unique(var_values),
                                   group_duration_value = NULL,
                                   single_var_name = NULL,
                                   group_sites = find_group_sites$site_id,
                                   citation_values = citation_build,
                                   doi_values = doi_build)
} # end group loop

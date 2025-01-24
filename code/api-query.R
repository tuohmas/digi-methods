# 23 January 2025
# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi

# Exercises for performing API queries

# PREPARATIONS #################################################################

# Clean the environment
rm(list = ls())

# Install and load packages necessary packages starting with pacman
if(!require("pacman")) { install.packages("pacman") }

# Load packages from CRAN using pacman that installs missing packages
pacman::p_load(dplyr,     # For data transformations
               tidyr,     # For data transformations
               purrr,     # For data transformations
               jsonlite,  # For data transformations
               httr2,     # For performing requests
               polite     # For querying web pages 'politely'
               )

sessioninfo::session_info()

# QUERYING THE SEMANTIC SCHOLAR API ############################################

# API Documentation for Academic Graph API (1.0): https://api.semanticscholar.org/api-docs/

# Be polite: wrap request function inside politely
politely_req <- politely(httr2::request, verbose = TRUE)

# Request Semantic Scholar's Academic Graph API
req <- politely_req("https://api.semanticscholar.org/graph/v1")

# Rate limits: 10 request per second for paper/citations and paper/references

# Define a function for looking up citations and reference from S2 Open Research
# that also transforms the data to a data frame

get_citations <-
  function(ids,                                            # Identifiers: DOI or S2 IDs
           k = 0,                                          # For indexing iterations
           output = vector(mode = "list", length = 1e6),   # Output list
           params = list(fields = c("paperId", "title")),  # Minimum field parameters
           include_refs = FALSE) {                          # Option for looking up refs

  # When we have multiple ids, iterate over them
  for (i in seq_along(ids)) {

    # Show progress
    message(paste0("\nRequesting ", i, " / ", length(ids), "...\n"))

    # Add to the index
    k <- k + 1

    # Build query
    query <- req %>%

      # Pass restrictions on how the request is performed
      req_progress() %>%                   # Monitor progress
      req_throttle(rate = 1 / 5) %>%       # Limit requests to 1 per 5 seconds
      req_retry(max_tries = 4, backoff = ~30) %>%

      req_url_path_append("paper", paste0("DOI:", ids[i])) %>%
      req_url_query(!!!params, .multi = "comma")

    # Try query out
    query %>% req_dry_run()

    # Perform the request
    resp <- query %>%
      req_perform()

    # Error handling
    if (resp_content_type(resp) != "application/json" |
        resp_status(resp) != 200) { next }

    # json <- resp %>% httr2::resp_body_json(simplifyVector = TRUE)
    json <- resp %>% httr2::resp_body_json(simplify = TRUE)

    # Extract ids for citations/refereces
    citation_ids <-
      json$citations$paperId[!is.na(json$citations$paperId)]
    references_ids <-
      json$references$paperId[!is.na(json$references$paperId)]

    # Unnest DOI and BibTex, discard unused values
    json$doi <- json$externalIds$DOI
    json$externalIds <- NULL

    json$citation <- json$citationStyles[[1]]
    json$citationStyles <- NULL

    json$citations <- NULL
    json$references <- NULL

    paper_details <- json %>%
      modify_if(is.null, function(x) NA) %>% # Convert NULL to NA value
      as_tibble() %>%
      mutate(type = "seed", .before = everything())

    output[[k]] <- paper_details

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    # Iterate over CITATIONS with pagination:
    # API will only return 500 batch paper details per request

    # Error handling: when no citations, skip to next
    if(length(citation_ids) > 0) {

      chunks <- split(citation_ids, ceiling(length(citation_ids)/500))

    } else { next }

    for (j in seq_along(chunks)) {

      # Add to the index
      k <- k + 1

      query <- req %>%
        req_progress() %>%                   # Monitor progress
        req_throttle(rate = 1 / 5) %>%       # Limit requests to 1 per 5 seconds
        req_retry(max_tries = 4, backoff = ~30) %>%
        req_url_query(!!!params, .multi = "comma")

      # Error handling: if more than one, use batch

      if(length(citation_ids) > 1) {

        query <- query %>%
          req_method("POST") %>%
          req_url_path_append("paper", "batch") %>% # Pass JSON using POST method
          req_body_json(list(ids = chunks[[j]]))

      } else {

        query <- query %>%
          req_url_path_append("paper", citation_ids)

      }

      # Try query out
      query %>% req_dry_run()

      # Perform the request
      resp <- query %>%
        req_perform()

      # Error handling
      if (resp_content_type(resp) != "application/json" |
          resp_status(resp) != 200) { break }

      json <- resp %>% httr2::resp_body_json(simplifyVector = TRUE)

      json$doi <- json$externalIds$DOI
      json$externalIds <- NULL

      json$citation <- json$citationStyles[[1]]
      json$citationStyles <- NULL

      json$citations <- NULL
      json$references <- NULL

      citations_details <- json %>%
        modify_if(is.null, function(x) NA) %>% # Convert NULL to NA value
        as_tibble() %>%
        mutate(type = "citation",
               citing_paper = paperId,
               cited_paper = paper_details$paperId, .before = everything())

      # Append results to output
      output[[k]] <- citations_details }

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    if(!include_refs) { next

    } else {

      # Iterate over REFERENCES with pagination:
      # API will only return 500 batch paper details per request

      # Error handling: when no citations, skip to next
      if(length(references_ids) > 0) {

        chunks <- split(references_ids, ceiling(length(references_ids)/500))

      } else { next }

      for (j in seq_along(chunks)) {

        # Add to the index
        k <- k + 1

        query <- req %>%
          req_progress() %>%                   # Monitor progress
          req_throttle(rate = 1 / 5) %>%       # Limit requests to 1 per 5 seconds
          req_retry(max_tries = 4, backoff = ~30) %>%


          req_url_query(!!!params, .multi = "comma")

        # Error handling: if more than one, use batch

        if(length(references_ids) > 1) {

          query <- query %>%
            req_method("POST") %>%
            req_url_path_append("paper", "batch") %>% # Pass JSON using POST method
            req_body_json(list(ids = chunks[[j]]))

        } else {

          query <- query %>%
            req_url_path_append("paper", references_ids) }

        # Try query out
        query %>% req_dry_run()

        # Perform the request
        resp <- query %>%
          req_perform()

        # Error handling
        if (resp_content_type(resp) != "application/json" |
            resp_status(resp) != 200) { break }

        json <- resp %>% httr2::resp_body_json(simplifyVector = TRUE)

        json$doi <- json$externalIds$DOI
        json$externalIds <- NULL

        json$citation <- json$citationStyles[[1]]
        json$citationStyles <- NULL

        json$citations <- NULL
        json$references <- NULL

        references_details <- json %>%
          modify_if(is.null, function(x) NA) %>% # Convert NULL to NA value
          as_tibble() %>%
          mutate(type = "reference",
                 citing_paper = paper_details$paperId,
                 cited_paper = paperId, .before = everything())

        # Append results to output
        output[[k]] <- references_details } # End iterate over chunks

    } # End of if include_refs

  } # End iterate over papers

  # Return compact output
  return(compact(output))

}

# Set field parameters,
params <-
  list(fields = c("paperId",  "corpusId", "title", "abstract", "venue",
                  "citationCount", "publicationDate", "externalIds",
                  "citationStyles", "citations", "references"))

# Seed DOIs
ids = c("10.1080/13183222.2018.1463047") # Farkas and Schou, 2018 (Javnost)


grepl(pattern = "^10\\.\\d{4,9}\\/[-._;()/:A-Z0-9]+", ids)
grepl(pattern = r"[/^10.1002/[^\s]+$/i]", ids[1])

# First iteration
first_citations <- get_citations(ids = ids, params = params,
                                 include_refs = TRUE)

first_citations

df_studies <- do.call("bind_rows", first_citations) %>%
  filter(publicationDate < "2024-10-01") %>%    # Include up until October 2024
  arrange(desc(publicationDate))

# STORE SECRETS FOR API AUTHENTICATION #########################################

# WikiMedia API homepage: https://api.wikimedia.org/wiki/Getting_started_with_Wikimedia_APIs

# Go to https://api.wikimedia.org/wiki/Special:AppManagement to create personal
# API key.

# Once created, store Client ID, Client secret and Access tokens as Environment
# Variables

Sys.setenv(wikimedia_client_id = "") # Uncomment and replace with your key
Sys.setenv(wikimedia_secret = "") # Uncomment and replace with your key
Sys.setenv(wikimedia_token = "") # Uncomment and replace with your Access token
# Print System variables

Sys.getenv("wikimedia_client_id")
Sys.getenv("wikimedia_secret")
Sys.getenv("wikimedia_token")

# OPTION 1: POLITELY QUERY WIKIMEDIA API #######################################

# Be polite: wrap request function inside politely
politely_req <- politely(httr2::request, verbose = TRUE)

# Start to build the query by requesting API base url
req <- politely_req("https://api.wikimedia.org/feed/v1/wikipedia/")

# Note that our rate of requests has been limited to 1 request every 5 seconds
# Rate limits of the API: https://api.wikimedia.org/wiki/Rate_limits

# As a small exercise, lets request today's featured content by following the
# tutorial slightly modified for our R and httr2 pipeline
# https://api.wikimedia.org/wiki/Getting_featured_content_from_Wikipedia_with_Python

# First we need to specify two variables that we append to the query:
# Language code: For Example "en"
# Date: Today's date in YYYY/MM/DD format

language_code <- "en"
today <- format(Sys.Date(), "%y/%m/%d")

# While building the request, we need to pass to required headers:
# Authorisation: Bearer YOUR_ACCESS_TOKEN
# User-Agent: YOUR_APP_NAME (YOUR_EMAIL_OR_CONTACT_PAGE)

# my_user_agent <- "wikipedia-api-tester (tuomas.k.heikkila@helsinki.fi)"
my_user_agent <- "tuomas.k.heikkila@helsinki.fi"

# We are now ready to build our query
query <- req %>%

  # First define "Authorisation" header
  req_headers(
    "Authorisation" = paste0("Bearer ", Sys.getenv("wikimedia_token"))) %>%

  # Next set user-agent https://api.wikimedia.org/wiki/Special:AppManagement
  req_user_agent(my_user_agent) %>%

  # Finally, append the path to today's feature
  req_url_path_append(language_code, "featured", today)

# Try query out
query %>% req_dry_run()

# Perform the request
resp <- query %>%
  req_perform()

# Error handling
if (resp_content_type(resp) != "application/json" |
    resp_status(resp) != 200) { next }

# Extract response object
json <- resp %>% httr2::resp_body_json(simplify = TRUE)

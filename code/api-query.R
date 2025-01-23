# 23 January 2025
# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi

# Exercises for API query

# PREPARATIONS #################################################################

# Clean the environment
rm(list = ls())

# Install and load packages
if(!require("pacman")) { install.packages("pacman") }

# Load packages
pacman::p_load(dplyr,
               tidyr,
               jsonlite,
               readr,
               httr2,
               polite,
               purrr,
               WikipediR)

# Additionally, it may be advisable to install the latest version of 'polite'
# straight from Github repository

install.packages("remotes")
remotes::install_github("dmi3kno/polite")
library(polite)

sessioninfo::session_info()

# STORE SECRETS FOR API AUTHENTICATION #########################################

# WikiMedia API homepage: https://api.wikimedia.org/wiki/Getting_started_with_Wikimedia_APIs

# Go to https://api.wikimedia.org/wiki/Special:AppManagement to create personal
# API key.

# Once created, store Client ID, Client secret and Access tokens as Environment
# Variables

# Sys.setenv(wikimedia_client_id = "c6a6ceb10ba86f818082c7789bc408ca") # Uncomment and replace with your key
# Sys.setenv(wikimedia_secret = "3272dc5215d4b9a6779ac75ef23f25e96a30a57a") # Uncomment and replace with your key
# Sys.setenv(wikimedia_token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJhdWQiOiJjNmE2Y2ViMTBiYTg2ZjgxODA4MmM3Nzg5YmM0MDhjYSIsImp0aSI6IjY3ZWQzN2E3MmY3NDg2MDY5ZTk0YTQwODM4ODFhMzE4NDZlYTVmYjg5NmY2MDQ4MGZiZWQxMmQyYjQ5NThlODM3MWYzZTUwZjI1YTYzZWEyIiwiaWF0IjoxNzM3NjIwMDE1LjkxMDE4NSwibmJmIjoxNzM3NjIwMDE1LjkxMDE4OCwiZXhwIjozMzI5NDUyODgxNS45MDg4MTMsInN1YiI6Ijc3NDYyNTAxIiwiaXNzIjoiaHR0cHM6Ly9tZXRhLndpa2ltZWRpYS5vcmciLCJyYXRlbGltaXQiOnsicmVxdWVzdHNfcGVyX3VuaXQiOjUwMDAsInVuaXQiOiJIT1VSIn0sInNjb3BlcyI6WyJiYXNpYyJdfQ.Kj2bvFKhw2duwIOt6QymRuTgd-OSsKSiPdtS7Z72zU18JOiUHsGpNk-RKcu7DEfSMrwYcCYmlGGkWIM9Qvbws6DjXu70QWxrBAPIk_VSuMgjzY6r4h06gT9hXSPHKiYy-5P0-ccMjLDdMpWzg-n5_PhCPl7dKhxE_YTqLkPXSpUuHwzkRw2bnZ3StHjxcmTRC-55qmkxCD2p7YRIFFb8x33tm3nNI0ie5VRP_FA8DEuHEKgsgYcN0_wwVXjiOJSTasaX8VoNdQGrd2Wt9GFTRwsGbbq1Tc1p4dELxPQRlGu66fVc4tQFG_u2xQ78ox3YnpLCJAHx7e--OQhYN3XsSYkCmz82rBOrF7EmaP4j3ETYPT70QgeZti4bXzW19-mRCgxkf5awPur8bhzAsRt1OF3ZKweRY5Q2DdKR-IFu60rOMT81DjGHsiIygeW_lU9WxKvv7UhY8M2jGPStbqV0VFLYP3rbB2x741bSPI2XuV5JNB_6SiWUfElTjAPKr6_00YFrxiqUheGyPevLsV-oqKcJmZ15hPPj5A98ddJcI2BCCIbPseZZ4LB33I-YqGfhMsVpi8GM4AH0b9UjQzgdqBvKvqxlaJk7BAgTlZVPO3iRE5IxArswPwQhUqdTSOtMqm4eQBVY-eW7_6zbMSiPswOt5M78n8QLbOuw2Zh9B1k") # Uncomment and replace with your key

# Print System variables

Sys.getenv("wikimedia_client_id")
Sys.getenv("wikimedia_secret")
Sys.getenv("wikimedia_token")

# OPTION 1: POLITELY QUERY WIKIMEDIA API #######################################

# Be polite: wrap request function inside politely
politely_req <- politely(httr2::request, verbose = TRUE)

# Start to build the query by requesting API base url
req <- politely_req("https://api.wikimedia.org/feed/v1/wikipedia")

# Note that our rate of requests has been limited to 1 request every 5 seconds
# Rate limits of the API: https://api.wikimedia.org/wiki/Rate_limits

# As a small exercise, lets request today's featured content by following the
# tutorial slightly modified for our R and httr2 pipeline
# https://api.wikimedia.org/wiki/Getting_featured_content_from_Wikipedia_with_Python

# First we need to specify two variables that we append to the query:
# Language code: For Example "en"
# Date: Today's date in YYYY/MM/DD format

language_code <- "en"
today <- Sys.Date() |> format("%y/%m/%d") # Stringing functions using R pipe

# While building the request, we need to pass to required headers:
# Authorisation: Bearer YOUR_ACCESS_TOKEN
# User-Agent: YOUR_APP_NAME (YOUR_EMAIL_OR_CONTACT_PAGE)

# We are now ready to build our query
query <- req %>%

  # First define "Authorisation" header
  req_headers(
    "Authorisation" = paste0("Bearer ", Sys.getenv("wikimedia_token"))) %>%

  # Next set user-agent https://api.wikimedia.org/wiki/Special:AppManagement
  req_user_agent("wikipedia-api-tester (tuomas.k.heikkila@helsinki.fi)") %>%

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

# TEST

# Change complete url
req <- request("http://example.com")
req |> req_url("http://google.com")

# Use a relative url
req <- request("http://example.com/a/b/c")
req |> req_url_relative("..")
req |> req_url_relative("/d/e/f")

# Change url components
req |>
  req_url_path_append("a")


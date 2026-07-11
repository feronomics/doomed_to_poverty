## ============== Setup ===============================

library(tidyverse)
library(foreign)
library(haven)
library(Hmisc)
library(yaml)

## Load log helper
## Small helper so every message has a consistent "[time] LEVEL  message" format.
source("resources/functions.R")

source("scripts/cleaning/ENEI_2016.R")

source("scripts/cleaning/ENEI_2017.R")

source("scripts/cleaning/ENEI_2018.R")

source("scripts/cleaning/ENEI_2019.R")

source("scripts/cleaning/ENEI_2021.R")

source("scripts/cleaning/ENEI_2022.R")

source("scripts/cleaning/ENCOVI_2023.R")

source("scripts/cleaning/ENEI_2024.R")
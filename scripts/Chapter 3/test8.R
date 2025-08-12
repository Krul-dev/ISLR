library(tidyverse)
library(krulRutils)
library(here)
library(GGally)
library(janitor)
library(patchwork)
library(broom)

data(who)

who %>%
  glimpse()

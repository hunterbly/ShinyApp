#!/usr/bin/env Rscript

suppressWarnings(library(HotDog))
suppressWarnings(library(dplyr))

args = commandArgs(trailingOnly=TRUE)

# If no input date, use current date
input.date = ifelse(is.na(args[1]), as.character(Sys.Date()), args[1])

df = get_hit_signal(ref.date = input.date,
                    format = 'long')

df.nz = df %>% filter(hit != 0)

# Save to db
save_hit_signal(df.signal = df.nz)

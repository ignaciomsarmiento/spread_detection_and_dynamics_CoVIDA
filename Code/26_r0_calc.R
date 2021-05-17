##########################################################
# author: Duncan Webb
# with additions from Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("dplyr","lubridate","haven","here","stringr","ggplot2")
lapply(pkg, require, character.only=T)
rm(pkg)




# Functions ---------------------------------------------------------------
# Calculate delays - function
interval_days <- function(x, y) {interval(x, y) %/% days(1)}

# Theme for graphs
theme_custom <- function(...) {
  theme_light() + 
    theme(plot.background = element_blank(),
          strip.background=element_rect(fill="#D1E0E6"), 
          strip.text = element_text(color = "black"),
          panel.grid.minor = element_blank(),
          ...)
}

# Quick histogram
hist_basic <- function(dat, x, binwidth = NULL, boundary = 0) {
  plot <- ggplot(dat, aes(x = {{x}})) + 
    geom_histogram(boundary = boundary, binwidth = binwidth, colour = "grey", fill = "lightblue")
  
  print(plot)
  
  invisible(dat)
}



# IMPORT ------------------------------------------------------------------
sds<-read_dta(here("data/sds_dta.dta"))




# Exponential growth calc -------------------------------------------------

overall_prev <- sds %>% 
  select(case_id, test_day) %>% 
  group_by(test_day) %>% 
  summarise(new_cases = n()) %>% 
  full_join(tibble(test_day = ymd("2020-03-01") + days(0:300))) %>% 
  arrange(test_day) %>% 
  mutate(new_cases = if_else(is.na(new_cases), 0L, new_cases)) %>% print(n = 500) %>% 
  mutate(days_since_march_1 = interval_days("2020-03-01", test_day)) %>% 
  mutate(ln_new_cases = log(new_cases))


# min_date <- ymd("2020-03-01") + days(30)
min_date <- ymd("2020-04-01")
max_date <- ymd("2020-04-01") + 60
# days(interval_days(data_save$start_date, data_save$lockdown_end_date[[1]]))




# REGRESSION
reg <- lm(ln_new_cases ~ days_since_march_1, data = overall_prev, 
          subset = test_day <= max_date & test_day >= min_date)

# Coefficients
r <- reg$coefficients[[2]]
r_conf <- confint(reg)[2, ]
r_conf %>% round(3)
(r_lab <- str_glue("r = {round(r, 3)}\n[{round(r_conf[[1]], 3)}, {round(r_conf[[2]], 3)}]"))

# PLOT
overall_prev %>% 
  filter(test_day >= ymd("2020-03-14")) %>% 
  filter(test_day <= ymd("2020-09-01")) %>% 
  filter(new_cases != 0) %>% 
  
  ggplot(aes(x = test_day, y = ln_new_cases))  + 
  geom_point(alpha = 0.2) + 
  geom_smooth(data = overall_prev %>% filter(test_day <= max_date & test_day >= min_date), method = "lm", fill = "skyblue", colour = "indianred", alpha = 0.4) + 
  # theme_custom(panel.grid = element_blank()) + 
  scale_x_date(breaks = "months", date_labels = "%b%est") + 
  labs(x = "Date (2020)", y = "ln(Daily Confirmed New Cases)") + 
  annotate(geom = "text", x = ymd("2020-04-01"), y = 7.5, label = r_lab, vjust = "inward", hjust = "inward") + 
  theme_custom(panel.grid = element_blank())
# geom_text(label = r_lab)
ggsave(here("views/r0_calibrate_data.pdf"), width = 6, height = 4, dpi = 75, device = cairo_pdf)







# 03b_extract_weekly.R
# Weekly surveillance (HMIS 033B) for epidemic monitoring: epidemic-prone disease cases and
# deaths, OPD attendance and 033B reporting rates, per facility and ISO week since 2020
# (summed upward to sub-county, DLG, district and Busoga in 05b_epidemic.R).
#   Rscript R/03b_extract_weekly.R            # resume
#   Rscript R/03b_extract_weekly.R --refresh  # re-pull the last 12 weeks (late reports)

source("R/00_config.R")
refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
dir.create("data/raw/weekly", recursive = TRUE, showWarnings = FALSE)

# disease, kind (seasonal = statistical threshold; notifiable = any case is an alert), cases DE, deaths DE
EPI_DE <- fread(text = "
disease,kind,cases,deaths
Malaria (confirmed),seasonal,fUflbWWhouR,IoZCByEDSnX
Suspected malaria (fever),seasonal,Nn9jPjcjg1j,
Dysentery,seasonal,ZQmTt0upgBM,iM32PqLmIPa
Typhoid fever,seasonal,gbnqdojUwmC,R9hdJy42eBV
Severe pneumonia (under 5),seasonal,tsJBUFW2Vjm,hEvz6VY4COC
Diarrhoea with dehydration (under 5),seasonal,RfixCHVUdXe,xECOmwMTL2v
Influenza-like illness,seasonal,nuZeHkDfuay,AbykqmBJSVM
SARI,seasonal,x9hL91WN0vj,mDQF18xh8e5
Animal bites (suspected rabies),seasonal,x1s0nL3MSul,xYeHFEb3RLZ
Cholera,notifiable,ubwCmJrLeYS,JnZ8l97OaX6
Measles,notifiable,i95TwhRjO0m,SILyHPYY8Lx
Acute flaccid paralysis,notifiable,G0a07K7yIiz,y16wdRU3yZT
Bacterial meningitis,notifiable,XBbLDaPUHDE,XbHJTtW2aHJ
Plague,notifiable,T2jqxyBKDnx,qYpVdbiy7Jd
Yellow fever,notifiable,OFWlKhzHh9f,sd9j28SILfA
Other viral haemorrhagic fever,notifiable,YIxjjSbiGdh,hUTVTaVmRCp
Anthrax,notifiable,Ig9GOExKMCj,ziOPuxGVvy4
Neonatal tetanus,notifiable,gfHE12yjG0D,QEixGrXMCME
Diphtheria,notifiable,JkK2hLwIkJL,aWFzTtqsA8B
Pertussis,notifiable,nNjyMm5aSCL,OPGwcWWIpsm
Dengue,notifiable,gmlHXYSmQSh,UxdBlLBgs7F
Chikungunya,notifiable,tmQu1Cj3fGA,VoSw3fI4KyA
Mpox,notifiable,JhCybNBqqey,iNVmQLdRZME
")
fwrite(EPI_DE, "data/meta/epidemic_diseases.csv")
other <- c(opd_new = "NeKm5EvaJYf", deaths_all = "ihPWCnpVnQ0")

# ISO weeks from the first week of 2020 to the last complete week
days  <- seq(as.Date("2020-01-06"), Sys.Date() - 7, by = "week")
weeks <- unique(sprintf("%sW%d", format(days, "%G"), as.integer(format(days, "%V"))))
recent <- tail(weeks, 12)
des <- unique(c(EPI_DE$cases, EPI_DE$deaths[nzchar(EPI_DE$deaths)], other))
ou_sc <- paste0("ou:", cfg$region_uid, ";LEVEL-", cfg$levels[["facility"]])

# One year of weekly values. A pull that comes back empty (DHIS2 can answer 200 with no rows when it
# is busy) is retried; if it stays empty the existing file is KEPT and the run fails loudly, so a
# silent gap can never replace good data.
pull_year <- function(wk) {
  for (try in 1:3) {
    dt <- rbindlist(lapply(chunk(des, 25), function(ch)
      d2_analytics(c(paste0("dx:", paste(ch, collapse = ";")), paste0("pe:", paste(wk, collapse = ";")), ou_sc))), fill = TRUE)
    rr <- d2_analytics(c(paste0("dx:C4oUitImBPK.REPORTING_RATE;C4oUitImBPK.ACTUAL_REPORTS;C4oUitImBPK.EXPECTED_REPORTS"),
                         paste0("pe:", paste(wk, collapse = ";")), ou_sc))
    if (nrow(dt) && nrow(rr)) return(list(values = dt, reporting = rr))
    log_msg("empty weekly pull (attempt %d of 3), waiting", try); Sys.sleep(30 * try)
  }
  NULL
}
failed <- character()
for (y in unique(substr(weeks, 1, 4))) {
  wk <- weeks[substr(weeks, 1, 4) == y]
  f <- sprintf("data/raw/weekly/w_%s.rds", y)
  if (file.exists(f) && !(refresh && any(wk %in% recent))) next
  t0 <- Sys.time()
  res <- pull_year(wk)
  if (is.null(res)) { failed <- c(failed, y); next }
  saveRDS(res, f, compress = "xz")
  pe_ <- unique(res$values$pe); last <- pe_[which.max(as.integer(sub("W.*", "", pe_)) * 100L + as.integer(sub(".*W", "", pe_)))]
  log_msg("weekly %s: %s values, %s reporting rows, latest week %s, in %.0fs", y, format(nrow(res$values), big.mark = ","),
          format(nrow(res$reporting), big.mark = ","), last, as.numeric(Sys.time() - t0, units = "secs"))
}
if (length(failed)) stop("Weekly pull returned no data for ", paste(failed, collapse = ", "), "; the existing files were kept")
# the newest week with data must be recent (reports arrive with a lag of up to about 3 weeks)
pes <- unique(unlist(lapply(list.files("data/raw/weekly", full.names = TRUE), function(p) readRDS(p)$values$pe)))
latest <- pes[which.max(as.integer(sub("W.*", "", pes)) * 100L + as.integer(sub(".*W", "", pes)))]
lw <- as.integer(sub(".*W", "", latest)); ly <- as.integer(sub("W.*", "", latest))
jan4 <- as.Date(sprintf("%d-01-04", ly)); wk1 <- jan4 - (as.integer(format(jan4, "%u")) - 1L)   # Monday of ISO week 1
age_wk <- as.numeric(Sys.Date() - (wk1 + 7 * (lw - 1))) / 7
log_msg("newest weekly data: %s (%.0f weeks ago)", latest, age_wk)
if (age_wk > 5) stop("Weekly data stop at ", latest, ": the newest week is more than 5 weeks old")
log_msg("weekly extract complete")

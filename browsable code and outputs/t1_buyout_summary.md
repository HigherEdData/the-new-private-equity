Table 1
================

``` r
imp <- readRDS("imputed_data_nopubliccap.Rds")
imp_all <- complete(imp, "all")
```

``` r
datlist <- miceadds::mids2datlist(imp) 
M       <- length(datlist)

orig    <- as.data.table(complete(imp, 0)) # original data with NA
orig[, is_mis := is.na(dealsize_2023d)]

## Known (non-missing) yearly totals: fixed, no uncertainty
known_year <- orig[!is.na(dealsize_2023d),
  .(known_volume = sum(dealsize_2023d, na.rm = TRUE)),
  by = dealyear
]

## Imputed-only yearly totals for each dataset
mis_index <- is.na(orig$dealsize_2023d)
```

``` r
## 2. Known (non-missing) yearly totals: fixed, no uncertainty
known_year_log <- orig[!is.na(dealsize_2023d),
  .(known_volume = sum(dealsize_2023d, na.rm = TRUE)),
  by = dealyear
]

## 3. Imputed-only yearly totals for each dataset 
res_mis_log <- rbindlist(
  lapply(seq_along(datlist), function(k) {
    dt <- as.data.table(datlist[[k]])
    dt[, is_mis := mis_index]

    dt[is_mis == TRUE,
       .(m = k,
         Q = sum(dealsize_2023d, na.rm = TRUE)),
       by = dealyear]
  })
)

## 4. Pool imputed totals per year in LOG space ----------------------------
pooled_mis_log <- res_mis_log[
  , {
      log_Q     <- log(Q)
      Q_bar_log <- mean(log_Q)
      B_log     <- var(log_Q)
      n_imp     <- .N
      T_log     <- (1 + 1 / n_imp) * B_log
      se_log    <- sqrt(T_log)
      df        <- n_imp - 1
      t_crit    <- qt(0.975, df = df)

      list(
        mis_volume = exp(Q_bar_log),
        mis_se     = se_log,
        mis_df     = df,
        mis_lower  = exp(Q_bar_log - t_crit * se_log),
        mis_upper  = exp(Q_bar_log + t_crit * se_log)
      )
    },
    by = dealyear
]

## 5. Combine known + imputed to get total yearly volume --------------------
pooled_year_log <- merge(known_year_log, pooled_mis_log, by = "dealyear", all = TRUE)
pooled_year_log[, `:=`(
  volume = known_volume + mis_volume,
  lower  = known_volume + mis_lower,
  upper  = known_volume + mis_upper
)]
setorder(pooled_year_log, dealyear)
pooled_year_log[, `:=`(
  volume_ra3       = frollmean(volume,       n = 3, align = "right"),
  lower_ra3        = frollmean(lower,        n = 3, align = "right"),
  upper_ra3        = frollmean(upper,        n = 3, align = "right"),
  known_volume_ra3 = frollmean(known_volume, n = 3, align = "right")
)]
pooled_year_log <- pooled_year_log[dealyear >= 1985 & dealyear <= 2019]

pooled_year_log
```

    ## Key: <dealyear>
    ##     dealyear known_volume mis_volume     mis_se mis_df  mis_lower mis_upper
    ##        <num>        <num>      <num>      <num>  <num>      <num>     <num>
    ##  1:     1985    23118.543   5651.736 0.42065236     79   2446.541  13056.03
    ##  2:     1986    35945.872  11591.896 0.39016804     79   5331.839  25201.82
    ##  3:     1987    30303.466  11812.335 0.50162942     79   4352.176  32060.12
    ##  4:     1988    37393.135  11583.101 0.26176460     79   6879.307  19503.16
    ##  5:     1989    24839.767  12773.416 0.30645691     79   6940.535  23508.29
    ##  6:     1990     8795.800  10019.852 0.32743233     79   5221.740  19226.82
    ##  7:     1991     9319.644   9008.202 0.28623471     79   5095.713  15924.70
    ##  8:     1992     7193.446  16471.718 0.25578762     79   9899.778  27406.42
    ##  9:     1993    10151.073  16349.374 0.28204367     79   9325.902  28662.32
    ## 10:     1994    18935.105  16735.505 0.22121247     79  10774.908  25993.46
    ## 11:     1995    21050.541  24297.792 0.17434818     79  17173.270  34378.00
    ## 12:     1996    48536.785  37672.278 0.25099564     79  22858.663  62085.89
    ## 13:     1997    42702.398  53096.947 0.21084048     79  34898.783  80784.64
    ## 14:     1998    85130.720  87588.593 0.14757362     79  65294.829 117494.17
    ## 15:     1999    98498.294  69583.367 0.13091142     79  53621.649  90296.46
    ## 16:     2000    76111.400  70838.714 0.11598614     79  56235.094  89234.73
    ## 17:     2001    46748.550  54403.502 0.23685168     79  33953.334  87170.85
    ## 18:     2002    61194.716  66158.885 0.13060580     79  51013.732  85800.39
    ## 19:     2003   158513.542  73041.331 0.15540723     79  53607.828  99519.72
    ## 20:     2004   187848.276 116847.856 0.15985698     79  85002.898 160623.01
    ## 21:     2005   277808.018 134293.319 0.07409766     79 115878.086 155635.08
    ## 22:     2006   409433.079 169800.231 0.08144744     79 144388.207 199684.72
    ## 23:     2007   873472.458 230475.844 0.07587565     79 198168.833 268049.79
    ## 24:     2008   214792.362 177057.444 0.07955899     79 151126.316 207437.99
    ## 25:     2009    97364.557 113719.864 0.08298272     79  96405.679 134143.63
    ## 26:     2010   200724.478 206385.316 0.06001980     79 183145.050 232574.67
    ## 27:     2011   217110.016 237810.800 0.07112284     79 206419.172 273976.38
    ## 28:     2012   233104.476 266985.226 0.05767069     79 238031.420 299460.93
    ## 29:     2013   285867.648 232746.294 0.05146387     79 210085.108 257851.87
    ## 30:     2014   277734.418 332694.341 0.07062066     79 289066.646 382906.59
    ## 31:     2015   326984.356 387246.748 0.06032293     79 343433.088 436649.96
    ## 32:     2016   426704.232 421685.387 0.06348583     79 371628.288 478485.01
    ## 33:     2017   367878.626 477110.359 0.05773257     79 425316.677 535211.31
    ## 34:     2018   360245.603 627614.116 0.06118321     79 555652.653 708895.16
    ## 35:     2019   385889.954 657450.246 0.07333028     79 568163.261 760768.70
    ##     dealyear known_volume mis_volume     mis_se mis_df  mis_lower mis_upper
    ##        <num>        <num>      <num>      <num>  <num>      <num>     <num>
    ##         volume      lower      upper volume_ra3 lower_ra3  upper_ra3
    ##          <num>      <num>      <num>      <num>     <num>      <num>
    ##  1:   28770.28   25565.08   36174.58   15888.78  12824.27   23397.58
    ##  2:   47537.77   41277.71   61147.69   28923.46  24672.30   38582.67
    ##  3:   42115.80   34655.64   62363.58   39474.62  33832.81   53228.62
    ##  4:   48976.24   44272.44   56896.30   46209.94  40068.60   60135.86
    ##  5:   37613.18   31780.30   48348.06   42901.74  36902.80   55869.31
    ##  6:   18815.65   14017.54   28022.62   35135.02  30023.43   44422.33
    ##  7:   18327.85   14415.36   25244.35   24918.89  20071.07   33871.68
    ##  8:   23665.16   17093.22   34599.87   20269.55  15175.37   29288.94
    ##  9:   26500.45   19476.97   38813.40   22831.15  16995.19   32885.87
    ## 10:   35670.61   29710.01   44928.56   28612.07  22093.40   39447.28
    ## 11:   45348.33   38223.81   55428.54   35839.80  29136.93   46390.17
    ## 12:   86209.06   71395.45  110622.68   55742.67  46443.09   70326.60
    ## 13:   95799.34   77601.18  123487.04   75785.58  62406.81   96512.75
    ## 14:  172719.31  150425.55  202624.89  118242.57  99807.39  145578.20
    ## 15:  168081.66  152119.94  188794.75  145533.44 126715.56  171635.56
    ## 16:  146950.11  132346.49  165346.13  162583.70 144963.99  185588.59
    ## 17:  101152.05   80701.88  133919.40  138727.94 121722.77  162686.76
    ## 18:  127353.60  112208.45  146995.10  125151.92 108418.94  148753.54
    ## 19:  231554.87  212121.37  258033.26  153353.51 135010.57  179649.26
    ## 20:  304696.13  272851.17  348471.29  221201.54 199060.33  251166.55
    ## 21:  412101.34  393686.10  433443.10  316117.45 292886.22  346649.22
    ## 22:  579233.31  553821.29  609117.80  432010.26 406786.19  463677.40
    ## 23: 1103948.30 1071641.29 1141522.25  698427.65 673049.56  728027.72
    ## 24:  391849.81  365918.68  422230.35  691677.14 663793.75  724290.13
    ## 25:  211084.42  193770.24  231508.18  568960.84 543776.73  598420.26
    ## 26:  407109.79  383869.53  433299.14  336681.34 314519.48  362345.89
    ## 27:  454920.82  423529.19  491086.39  357705.01 333722.98  385297.91
    ## 28:  500089.70  471135.90  532565.41  454040.10 426178.20  485650.31
    ## 29:  518613.94  495952.76  543719.52  491208.15 463539.28  522457.11
    ## 30:  610428.76  566801.06  660641.01  543044.13 511296.57  578975.31
    ## 31:  714231.10  670417.44  763634.32  614424.60 577723.75  655998.28
    ## 32:  848389.62  798332.52  905189.24  724349.83 678517.01  776488.19
    ## 33:  844988.99  793195.30  903089.93  802536.57 753981.76  857304.50
    ## 34:  987859.72  915898.26 1069140.77  893746.11 835808.69  959139.98
    ## 35: 1043340.20  954053.22 1146658.66  958729.64 887715.59 1039629.79
    ##         volume      lower      upper volume_ra3 lower_ra3  upper_ra3
    ##          <num>      <num>      <num>      <num>     <num>      <num>
    ##     known_volume_ra3
    ##                <num>
    ##  1:        10692.773
    ##  2:        21308.584
    ##  3:        29789.294
    ##  4:        34547.491
    ##  5:        30845.456
    ##  6:        23676.234
    ##  7:        14318.404
    ##  8:         8436.297
    ##  9:         8888.054
    ## 10:        12093.208
    ## 11:        16712.240
    ## 12:        29507.477
    ## 13:        37429.908
    ## 14:        58789.968
    ## 15:        75443.804
    ## 16:        86580.138
    ## 17:        73786.081
    ## 18:        61351.555
    ## 19:        88818.936
    ## 20:       135852.178
    ## 21:       208056.612
    ## 22:       291696.457
    ## 23:       520237.852
    ## 24:       499232.633
    ## 25:       395209.792
    ## 26:       170960.466
    ## 27:       171733.017
    ## 28:       216979.657
    ## 29:       245360.713
    ## 30:       265568.847
    ## 31:       296862.141
    ## 32:       343807.669
    ## 33:       373855.738
    ## 34:       384942.820
    ## 35:       371338.061
    ##     known_volume_ra3
    ##                <num>

# LBO stats (Table 1)

exporting exit stats

``` r
exit_dt <-readRDS("buyout_exits_status.RDS")
# IDate columns saved with double storage are rejected by newer vctrs; convert them to Date
for (col in names(exit_dt)[vapply(exit_dt, inherits, logical(1), "IDate")]) {
  exit_dt[[col]] <- as.Date(as.numeric(exit_dt[[col]]))
}

exit_dt <- exit_dt %>% 
  rename(dealyear = ml_dealyear)

exit_dt <- as.data.table(exit_dt)

# keep just the keys needed to align with orig; adjust key col if not companyid+dealyear
orig[exit_dt, on = .(companyid, dealyear), exit_type := i.exit_type]
```

``` r
lbo_period_bucket <- function(y) {
  fcase(
    y < 1985, "Before 1985",
    y >= 1985 & y < 1990, "1985–1989",
    y >= 1990 & y < 1995, "1990–1994",
    y >= 1995 & y < 2000, "1995–1999",
    y >= 2000 & y < 2005, "2000–2004",
    y >= 2005 & y < 2010, "2005–2009",
    y >= 2010 & y < 2015, "2010–2014",
    y >= 2015 & y < 2020, "2015–2019",
    y >= 2020 & y < 2025, "2020–2023"
  )
}

lvls <- c(
  "Before 1985","1985–1989","1990–1994","1995–1999",
  "2000–2004","2005–2009","2010–2014","2015–2019",
  "2020–2023"
)

orig[, lbo_period := lbo_period_bucket(dealyear)]
orig[, lbo_period := factor(
  lbo_period,
  levels = lvls
)]
```

``` r
datlist <- lapply(datlist, function(d) {
  setDT(d)  
  d[, lbo_period := factor(lbo_period_bucket(dealyear), levels = lvls)]
  d
})
```

``` r
sum_known <- orig[dealyear >= 1985 & dealyear < 2020, .(
  lbo_count = .N,
  target_count = uniqueN(companyid),
  exit_count = sum(!is.na(exit_type)),
  pct_kn = round((.N - sum(is_mis)) / .N, digits = 2),
  pct_kn_exit = round(sum(!is.na(exit_type)) / .N, digits = 2),
  p25_size_mn = round(quantile(dealsize_2023d, probs = c(0.25), na.rm = T), digits = 0),
  med_size_mn = round(quantile(dealsize_2023d, probs = c(0.5), na.rm = T), digits = 0),
  p75_size_mn = round(quantile(dealsize_2023d, probs = c(0.75), na.rm = T), digits = 0),
  vol_kn_bn = round(sum(dealsize_2023d, na.rm = T) / 1e3, digits = 0)
), by = lbo_period][order(lbo_period)]
```

``` r
## 2. Known (non-missing) period totals: fixed, no uncertainty -------------
known_year <- orig[!is.na(dealsize_2023d) & dealyear >= 1985 & dealyear < 2020,
  .(known_volume = sum(dealsize_2023d, na.rm = TRUE)),
  by = lbo_period
][order(lbo_period)]

## 3. Imputed-only period totals for each dataset --------------------------
mis_index <- is.na(orig$dealsize_2023d)
res_mis <- rbindlist(
  lapply(seq_along(datlist), function(k) {
    dt <- as.data.table(datlist[[k]])
    # align missingness pattern with original rows
    dt[, is_mis := mis_index]

    dt[is_mis == TRUE & dealyear >= 1985 & dealyear < 2020,
       .(m = k,
         Q = sum(dealsize_2023d, na.rm = TRUE),
         U = 0),
       by = lbo_period][order(lbo_period)]
  })
)

## 4. Pool imputed totals per year using Rubin's rules ----------------------
pooled_mis <- res_mis[
  , {
      ps     <- pool.scalar(Q = Q, U = U)  # Rubin’s rules for scalar
      se     <- sqrt(ps$t)
      t_crit <- qt(0.975, df = ps$df)

      # CI for imputed part; lower bound truncated at 0 (cannot be negative)
      mis_lower <- pmax(0, ps$qbar - t_crit * se)
      mis_upper <- ps$qbar + t_crit * se

      list(
        mis_volume = ps$qbar,
        mis_se     = se,
        mis_df     = ps$df,
        mis_lower  = mis_lower,
        mis_upper  = mis_upper
      )
    },
    by = lbo_period
]

## 5. Combine known + imputed to get total yearly volume --------------------
pooled_prd <- merge(known_year, pooled_mis, by = "lbo_period", all = TRUE)
pooled_prd[, `:=`(
  volume = known_volume + mis_volume,
  lower  = known_volume + mis_lower,
  upper  = known_volume + mis_upper
)]
setorder(pooled_prd, lbo_period)
```

``` r
## ---- TOTAL column: known summaries over full span (no period grouping) ----
sum_known_tot <- orig[dealyear >= 1985 & dealyear < 2020, .(
  lbo_period   = "1985–2019",
  lbo_count    = .N,
  target_count = uniqueN(companyid),
  exit_count   = sum(!is.na(exit_type)),
  pct_kn       = round((.N - sum(is_mis)) / .N, digits = 2),
  pct_kn_exit  = round(sum(!is.na(exit_type)) / .N, digits = 2),
  p25_size_mn  = round(quantile(dealsize_2023d, probs = 0.25, na.rm = TRUE), 0),
  med_size_mn  = round(quantile(dealsize_2023d, probs = 0.50, na.rm = TRUE), 0),
  p75_size_mn  = round(quantile(dealsize_2023d, probs = 0.75, na.rm = TRUE), 0),
  vol_kn_bn    = round(sum(dealsize_2023d, na.rm = TRUE) / 1e3, 0)
)]

## ---- TOTAL column: pooled imputed volume over full span -------------------
known_tot <- orig[!is.na(dealsize_2023d) & dealyear >= 1985 & dealyear < 2020,
  .(known_volume = sum(dealsize_2023d, na.rm = TRUE))
]$known_volume

# Each imputation's total imputed-only volume across the whole span
res_mis_tot <- rbindlist(
  lapply(seq_along(datlist), function(k) {
    dt <- as.data.table(datlist[[k]])
    dt[, is_mis := mis_index]
    dt[is_mis == TRUE & dealyear >= 1985 & dealyear < 2020,
       .(m = k, Q = sum(dealsize_2023d, na.rm = TRUE), U = 0)]
  })
)

pooled_mis_tot <- res_mis_tot[, {
  ps     <- pool.scalar(Q = Q, U = U)
  se     <- sqrt(ps$t)
  t_crit <- qt(0.975, df = ps$df)
  list(
    mis_volume = ps$qbar,
    mis_lower  = pmax(0, ps$qbar - t_crit * se),
    mis_upper  = ps$qbar + t_crit * se
  )
}]

pooled_prd_tot <- data.table(
  lbo_period = "1985–2019",
  impvol_bn  = round((known_tot + pooled_mis_tot$mis_volume) / 1e3, 0),
  impCI_bn   = paste0("[",
                      round((known_tot + pooled_mis_tot$mis_lower) / 1e3, 0), "–",
                      round((known_tot + pooled_mis_tot$mis_upper) / 1e3, 0),
                      "]")
)
```

``` r
lbo_tbl <- rbind(
  cbind(
    sum_known,
    pooled_prd[, .(
      impvol_bn = round(volume / 1e3, digits = 0),
      impCI_bn  = paste0("[",
                         round(lower / 1e3, digits = 0), "–",
                         round(upper / 1e3, digits = 0), "]")
    )]
  ),
  cbind(sum_known_tot, pooled_prd_tot[, .(impvol_bn, impCI_bn)])
)

lbo_tbl
```

    ##    lbo_period lbo_count target_count exit_count pct_kn pct_kn_exit p25_size_mn
    ##        <fctr>     <int>        <int>      <int>  <num>       <num>       <num>
    ## 1:  1985–1989       421          402        344   0.38        0.82         104
    ## 2:  1990–1994       656          648        541   0.35        0.82          21
    ## 3:  1995–1999      2820         2679       1893   0.34        0.67          27
    ## 4:  2000–2004      5268         5086       3071   0.39        0.58          24
    ## 5:  2005–2009     11261        10916       5673   0.29        0.50          22
    ## 6:  2010–2014     14135        13780       5664   0.21        0.40          22
    ## 7:  2015–2019     22413        21869       4177   0.14        0.19          26
    ## 8:  1985–2019     56974        49621      21363   0.22        0.37          24
    ##    med_size_mn p75_size_mn vol_kn_bn impvol_bn      impCI_bn
    ##          <num>       <num>     <num>     <num>        <char>
    ## 1:         266         797       152       210     [181–239]
    ## 2:          72         212        54       126     [103–148]
    ## 3:          93         291       296       573     [516–630]
    ## 4:          71         207       530       917     [851–983]
    ## 5:          80         295      1873      2701   [2630–2772]
    ## 6:          95         356      1215      2494   [2408–2579]
    ## 7:         121         505      1868      4444   [4256–4633]
    ## 8:          91         336      5987     11464 [11228–11700]

``` r
dcast(melt(lbo_tbl, id.vars = "lbo_period"), variable ~ lbo_period)
```

    ## Warning in melt.data.table(lbo_tbl, id.vars = "lbo_period"): 'measure.vars'
    ## [lbo_count, target_count, exit_count, pct_kn, ...] are not all of the same
    ## type. By order of hierarchy, the molten data value column will be of type
    ## 'character'. All measure variables not of type 'character' will be coerced too.
    ## Check DETAILS in ?melt.data.table for more on coercion.

    ## Key: <variable>
    ##         variable 1985–1989 1990–1994 1995–1999 2000–2004   2005–2009
    ##           <fctr>    <char>    <char>    <char>    <char>      <char>
    ##  1:    lbo_count       421       656      2820      5268       11261
    ##  2: target_count       402       648      2679      5086       10916
    ##  3:   exit_count       344       541      1893      3071        5673
    ##  4:       pct_kn      0.38      0.35      0.34      0.39        0.29
    ##  5:  pct_kn_exit      0.82      0.82      0.67      0.58         0.5
    ##  6:  p25_size_mn       104        21        27        24          22
    ##  7:  med_size_mn       266        72        93        71          80
    ##  8:  p75_size_mn       797       212       291       207         295
    ##  9:    vol_kn_bn       152        54       296       530        1873
    ## 10:    impvol_bn       210       126       573       917        2701
    ## 11:     impCI_bn [181–239] [103–148] [516–630] [851–983] [2630–2772]
    ##       2010–2014   2015–2019     1985–2019
    ##          <char>      <char>        <char>
    ##  1:       14135       22413         56974
    ##  2:       13780       21869         49621
    ##  3:        5664        4177         21363
    ##  4:        0.21        0.14          0.22
    ##  5:         0.4        0.19          0.37
    ##  6:          22          26            24
    ##  7:          95         121            91
    ##  8:         356         505           336
    ##  9:        1215        1868          5987
    ## 10:        2494        4444         11464
    ## 11: [2408–2579] [4256–4633] [11228–11700]

``` r
style_lbo_table <- function(lbo_tbl, title, subtitle, ci_label = "95% CI") {

  tbl <- lbo_tbl %>%
    as_tibble() %>%
    mutate(
      lbo_period = as.character(lbo_period),
      # Build a single display column for imputed volume + CI (2-line cell)
      impvol_ci = gt::md(paste0(
        formatC(impvol_bn, format = "f", digits = 0, big.mark = ","),
        "<br><span style='font-size:11px'>", impCI_bn, "</span>"
      ))
    ) %>%
    select(
      lbo_period,
      lbo_count, target_count, exit_count,
      pct_kn,
      p25_size_mn, med_size_mn, p75_size_mn,
      vol_kn_bn,
      impvol_ci
    )

  tbl %>%
    gt(rowname_col = "lbo_period") %>%
    tab_stubhead(label = "Deal window") %>%

    # ---- Title block ----
    tab_header(
      title    = md(paste0("**", title, "**")),
      subtitle = md(subtitle)
    ) %>%

    # ---- Column labels + spanners ----
    cols_label(
      lbo_count    = "LBOs",
      target_count = "Targets",
      exit_count   = "Number of exits",
      pct_kn       = "% size known",
      p25_size_mn  = "P25",
      med_size_mn  = "Median",
      p75_size_mn  = "P75",
      vol_kn_bn    = "Known",
      impvol_ci    = md(paste0("Imputed<br><span style='font-weight:400'>(", ci_label, ")</span>"))
    ) %>%
    tab_spanner("Counts", columns = c(lbo_count, target_count, exit_count)) %>%
    tab_spanner(md("Deal size (2023 USD, $M)"), columns = c(p25_size_mn, med_size_mn, p75_size_mn)) %>%
    tab_spanner(md("Total volume (2023 USD, $B)"), columns = c(vol_kn_bn, impvol_ci)) %>%

    # ---- Formatting ----
    fmt_number(columns = c(lbo_count, target_count),
               decimals = 0, sep_mark = ",") %>%
    fmt_percent(columns = pct_kn, decimals = 0) %>%
    fmt_number(columns = c(p25_size_mn, med_size_mn, p75_size_mn),
               decimals = 0, sep_mark = ",", pattern = "${x}M") %>%
    fmt_number(columns = vol_kn_bn,
               decimals = 0, sep_mark = ",", pattern = "${x}B") %>%
    fmt_markdown(columns = impvol_ci) %>%

    # ---- Layout / aesthetics ----
    cols_align("left", columns = stub()) %>%
    cols_align("right", columns = everything()) %>%
    cols_width(
      stub() ~ px(170),
      c(lbo_count, target_count, exit_count) ~ px(90),
      pct_kn ~ px(110),
      c(p25_size_mn, med_size_mn, p75_size_mn) ~ px(85),
      vol_kn_bn ~ px(95),
      impvol_ci ~ px(140)
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub(rows = everything())
    ) %>%
    opt_row_striping() %>%
    opt_table_font(font = list("Helvetica", "Arial", "sans-serif")) %>%
    tab_options(
      table.font.size            = px(13),
      heading.title.font.size    = px(18),
      heading.subtitle.font.size = px(13),
      column_labels.font.size    = px(12),
      data_row.padding           = px(6),
      table.border.top.style     = "solid",
      table.border.bottom.style  = "solid",
      column_labels.border.bottom.style = "solid"
    ) %>%
    tab_source_note(md(
      "Notes: Deal sizes and volumes are in 2023 dollars. Size percentiles are computed on deals with known sizes. Imputed totals are pooled across imputations; confidence intervals reflect imputation uncertainty."
    ))
}

lbo_gt <- style_lbo_table(
  lbo_tbl,
  title    = "Leveraged-Buyout (LBO) Activity (1985–2019)",
  subtitle = "Counts, coverage rates, and deal-value aggregates by deal windows, in 2023 dollars",
  ci_label = "95% CI"
)
lbo_gt
```

<div id="aarcijkqpl" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#aarcijkqpl table {
  font-family: Helvetica, Arial, sans-serif, system-ui, 'Segoe UI', Roboto, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#aarcijkqpl thead, #aarcijkqpl tbody, #aarcijkqpl tfoot, #aarcijkqpl tr, #aarcijkqpl td, #aarcijkqpl th {
  border-style: none;
}

#aarcijkqpl p {
  margin: 0;
  padding: 0;
}

#aarcijkqpl .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 13px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#aarcijkqpl .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#aarcijkqpl .gt_title {
  color: #333333;
  font-size: 18px;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#aarcijkqpl .gt_subtitle {
  color: #333333;
  font-size: 13px;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#aarcijkqpl .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#aarcijkqpl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#aarcijkqpl .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#aarcijkqpl .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 12px;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#aarcijkqpl .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 12px;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#aarcijkqpl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#aarcijkqpl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#aarcijkqpl .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#aarcijkqpl .gt_spanner_row {
  border-bottom-style: hidden;
}

#aarcijkqpl .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#aarcijkqpl .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#aarcijkqpl .gt_from_md > :first-child {
  margin-top: 0;
}

#aarcijkqpl .gt_from_md > :last-child {
  margin-bottom: 0;
}

#aarcijkqpl .gt_row {
  padding-top: 6px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#aarcijkqpl .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#aarcijkqpl .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#aarcijkqpl .gt_row_group_first td {
  border-top-width: 2px;
}

#aarcijkqpl .gt_row_group_first th {
  border-top-width: 2px;
}

#aarcijkqpl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#aarcijkqpl .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#aarcijkqpl .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#aarcijkqpl .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#aarcijkqpl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#aarcijkqpl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#aarcijkqpl .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#aarcijkqpl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#aarcijkqpl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#aarcijkqpl .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#aarcijkqpl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#aarcijkqpl .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#aarcijkqpl .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#aarcijkqpl .gt_left {
  text-align: left;
}

#aarcijkqpl .gt_center {
  text-align: center;
}

#aarcijkqpl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#aarcijkqpl .gt_font_normal {
  font-weight: normal;
}

#aarcijkqpl .gt_font_bold {
  font-weight: bold;
}

#aarcijkqpl .gt_font_italic {
  font-style: italic;
}

#aarcijkqpl .gt_super {
  font-size: 65%;
}

#aarcijkqpl .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#aarcijkqpl .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#aarcijkqpl .gt_indent_1 {
  text-indent: 5px;
}

#aarcijkqpl .gt_indent_2 {
  text-indent: 10px;
}

#aarcijkqpl .gt_indent_3 {
  text-indent: 15px;
}

#aarcijkqpl .gt_indent_4 {
  text-indent: 20px;
}

#aarcijkqpl .gt_indent_5 {
  text-indent: 25px;
}

#aarcijkqpl .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#aarcijkqpl div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" style="table-layout:fixed;width:0px;" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <colgroup>
    <col style="width:170px;"/>
    <col style="width:90px;"/>
    <col style="width:90px;"/>
    <col style="width:90px;"/>
    <col style="width:110px;"/>
    <col style="width:85px;"/>
    <col style="width:85px;"/>
    <col style="width:85px;"/>
    <col style="width:95px;"/>
    <col style="width:140px;"/>
  </colgroup>
  <thead>
    <tr class="gt_heading">
      <td colspan="10" class="gt_heading gt_title gt_font_normal" style><span class='gt_from_md'><strong>Leveraged-Buyout (LBO) Activity (1985–2019)</strong></span></td>
    </tr>
    <tr class="gt_heading">
      <td colspan="10" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style><span class='gt_from_md'>Counts, coverage rates, and deal-value aggregates by deal windows, in 2023 dollars</span></td>
    </tr>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="a::stub">Deal window</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="3" scope="colgroup" id="Counts">
        <div class="gt_column_spanner">Counts</div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="pct_kn">% size known</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="3" scope="colgroup" id="Deal size (2023 USD, $M)">
        <div class="gt_column_spanner"><span class='gt_from_md'>Deal size (2023 USD, $M)</span></div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="Total volume (2023 USD, $B)">
        <div class="gt_column_spanner"><span class='gt_from_md'>Total volume (2023 USD, $B)</span></div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="lbo_count">LBOs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="target_count">Targets</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="exit_count">Number of exits</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p25_size_mn">P25</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="med_size_mn">Median</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p75_size_mn">P75</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="vol_kn_bn">Known</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="impvol_ci"><span class='gt_from_md'>Imputed<br><span style='font-weight:400'>(95% CI)</span></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;">1985–1989</th>
<td headers="stub_1_1 lbo_count" class="gt_row gt_right">421</td>
<td headers="stub_1_1 target_count" class="gt_row gt_right">402</td>
<td headers="stub_1_1 exit_count" class="gt_row gt_right">344</td>
<td headers="stub_1_1 pct_kn" class="gt_row gt_right">38%</td>
<td headers="stub_1_1 p25_size_mn" class="gt_row gt_right">$104M</td>
<td headers="stub_1_1 med_size_mn" class="gt_row gt_right">$266M</td>
<td headers="stub_1_1 p75_size_mn" class="gt_row gt_right">$797M</td>
<td headers="stub_1_1 vol_kn_bn" class="gt_row gt_right">$152B</td>
<td headers="stub_1_1 impvol_ci" class="gt_row gt_right"><span class='gt_from_md'>210<br><span style='font-size:11px'>[181–239]</span></span></td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;">1990–1994</th>
<td headers="stub_1_2 lbo_count" class="gt_row gt_right gt_striped">656</td>
<td headers="stub_1_2 target_count" class="gt_row gt_right gt_striped">648</td>
<td headers="stub_1_2 exit_count" class="gt_row gt_right gt_striped">541</td>
<td headers="stub_1_2 pct_kn" class="gt_row gt_right gt_striped">35%</td>
<td headers="stub_1_2 p25_size_mn" class="gt_row gt_right gt_striped">$21M</td>
<td headers="stub_1_2 med_size_mn" class="gt_row gt_right gt_striped">$72M</td>
<td headers="stub_1_2 p75_size_mn" class="gt_row gt_right gt_striped">$212M</td>
<td headers="stub_1_2 vol_kn_bn" class="gt_row gt_right gt_striped">$54B</td>
<td headers="stub_1_2 impvol_ci" class="gt_row gt_right gt_striped"><span class='gt_from_md'>126<br><span style='font-size:11px'>[103–148]</span></span></td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;">1995–1999</th>
<td headers="stub_1_3 lbo_count" class="gt_row gt_right">2,820</td>
<td headers="stub_1_3 target_count" class="gt_row gt_right">2,679</td>
<td headers="stub_1_3 exit_count" class="gt_row gt_right">1893</td>
<td headers="stub_1_3 pct_kn" class="gt_row gt_right">34%</td>
<td headers="stub_1_3 p25_size_mn" class="gt_row gt_right">$27M</td>
<td headers="stub_1_3 med_size_mn" class="gt_row gt_right">$93M</td>
<td headers="stub_1_3 p75_size_mn" class="gt_row gt_right">$291M</td>
<td headers="stub_1_3 vol_kn_bn" class="gt_row gt_right">$296B</td>
<td headers="stub_1_3 impvol_ci" class="gt_row gt_right"><span class='gt_from_md'>573<br><span style='font-size:11px'>[516–630]</span></span></td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;">2000–2004</th>
<td headers="stub_1_4 lbo_count" class="gt_row gt_right gt_striped">5,268</td>
<td headers="stub_1_4 target_count" class="gt_row gt_right gt_striped">5,086</td>
<td headers="stub_1_4 exit_count" class="gt_row gt_right gt_striped">3071</td>
<td headers="stub_1_4 pct_kn" class="gt_row gt_right gt_striped">39%</td>
<td headers="stub_1_4 p25_size_mn" class="gt_row gt_right gt_striped">$24M</td>
<td headers="stub_1_4 med_size_mn" class="gt_row gt_right gt_striped">$71M</td>
<td headers="stub_1_4 p75_size_mn" class="gt_row gt_right gt_striped">$207M</td>
<td headers="stub_1_4 vol_kn_bn" class="gt_row gt_right gt_striped">$530B</td>
<td headers="stub_1_4 impvol_ci" class="gt_row gt_right gt_striped"><span class='gt_from_md'>917<br><span style='font-size:11px'>[851–983]</span></span></td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;">2005–2009</th>
<td headers="stub_1_5 lbo_count" class="gt_row gt_right">11,261</td>
<td headers="stub_1_5 target_count" class="gt_row gt_right">10,916</td>
<td headers="stub_1_5 exit_count" class="gt_row gt_right">5673</td>
<td headers="stub_1_5 pct_kn" class="gt_row gt_right">29%</td>
<td headers="stub_1_5 p25_size_mn" class="gt_row gt_right">$22M</td>
<td headers="stub_1_5 med_size_mn" class="gt_row gt_right">$80M</td>
<td headers="stub_1_5 p75_size_mn" class="gt_row gt_right">$295M</td>
<td headers="stub_1_5 vol_kn_bn" class="gt_row gt_right">$1,873B</td>
<td headers="stub_1_5 impvol_ci" class="gt_row gt_right"><span class='gt_from_md'>2,701<br><span style='font-size:11px'>[2630–2772]</span></span></td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;">2010–2014</th>
<td headers="stub_1_6 lbo_count" class="gt_row gt_right gt_striped">14,135</td>
<td headers="stub_1_6 target_count" class="gt_row gt_right gt_striped">13,780</td>
<td headers="stub_1_6 exit_count" class="gt_row gt_right gt_striped">5664</td>
<td headers="stub_1_6 pct_kn" class="gt_row gt_right gt_striped">21%</td>
<td headers="stub_1_6 p25_size_mn" class="gt_row gt_right gt_striped">$22M</td>
<td headers="stub_1_6 med_size_mn" class="gt_row gt_right gt_striped">$95M</td>
<td headers="stub_1_6 p75_size_mn" class="gt_row gt_right gt_striped">$356M</td>
<td headers="stub_1_6 vol_kn_bn" class="gt_row gt_right gt_striped">$1,215B</td>
<td headers="stub_1_6 impvol_ci" class="gt_row gt_right gt_striped"><span class='gt_from_md'>2,494<br><span style='font-size:11px'>[2408–2579]</span></span></td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;">2015–2019</th>
<td headers="stub_1_7 lbo_count" class="gt_row gt_right">22,413</td>
<td headers="stub_1_7 target_count" class="gt_row gt_right">21,869</td>
<td headers="stub_1_7 exit_count" class="gt_row gt_right">4177</td>
<td headers="stub_1_7 pct_kn" class="gt_row gt_right">14%</td>
<td headers="stub_1_7 p25_size_mn" class="gt_row gt_right">$26M</td>
<td headers="stub_1_7 med_size_mn" class="gt_row gt_right">$121M</td>
<td headers="stub_1_7 p75_size_mn" class="gt_row gt_right">$505M</td>
<td headers="stub_1_7 vol_kn_bn" class="gt_row gt_right">$1,868B</td>
<td headers="stub_1_7 impvol_ci" class="gt_row gt_right"><span class='gt_from_md'>4,444<br><span style='font-size:11px'>[4256–4633]</span></span></td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;">1985–2019</th>
<td headers="stub_1_8 lbo_count" class="gt_row gt_right gt_striped">56,974</td>
<td headers="stub_1_8 target_count" class="gt_row gt_right gt_striped">49,621</td>
<td headers="stub_1_8 exit_count" class="gt_row gt_right gt_striped">21363</td>
<td headers="stub_1_8 pct_kn" class="gt_row gt_right gt_striped">22%</td>
<td headers="stub_1_8 p25_size_mn" class="gt_row gt_right gt_striped">$24M</td>
<td headers="stub_1_8 med_size_mn" class="gt_row gt_right gt_striped">$91M</td>
<td headers="stub_1_8 p75_size_mn" class="gt_row gt_right gt_striped">$336M</td>
<td headers="stub_1_8 vol_kn_bn" class="gt_row gt_right gt_striped">$5,987B</td>
<td headers="stub_1_8 impvol_ci" class="gt_row gt_right gt_striped"><span class='gt_from_md'>11,464<br><span style='font-size:11px'>[11228–11700]</span></span></td></tr>
  </tbody>
  <tfoot>
    <tr class="gt_sourcenotes">
      <td class="gt_sourcenote" colspan="10"><span class='gt_from_md'>Notes: Deal sizes and volumes are in 2023 dollars. Size percentiles are computed on deals with known sizes. Imputed totals are pooled across imputations; confidence intervals reflect imputation uncertainty.</span></td>
    </tr>
  </tfoot>
</table>
</div>

``` r
style_lbo_table_period_cols <- function(lbo_tbl, title, subtitle, ci_label = "95% CI") {

  period_levels <- as.character(lbo_tbl$lbo_period)
  num <- function(x) suppressWarnings(as.numeric(x))
  fmt_int <- function(x) formatC(num(x), format = "f", digits = 0, big.mark = ",")
  fmt_pct <- function(x) paste0(round(num(x) * 100, 0), "%")
  fmt_usd_m <- function(x) paste0("$", fmt_int(x), "M")
  fmt_usd_b <- function(x) paste0("$", fmt_int(x), "B")

  wide <- lbo_tbl %>%
    as_tibble() %>%
    mutate(
      lbo_period = factor(as.character(lbo_period),
                          levels = period_levels, ordered = TRUE),
      impvol_ci = paste0(fmt_usd_b(impvol_bn), "  \n", "*", impCI_bn, "*")
    ) %>%
    select(
      lbo_period, lbo_count, target_count, exit_count, pct_kn, pct_kn_exit,
      p25_size_mn, med_size_mn, p75_size_mn, vol_kn_bn, impvol_ci
    ) %>%
    # make everything character so pivot_longer can combine them
    mutate(across(-lbo_period, as.character)) %>%
    pivot_longer(-lbo_period, names_to = "stat", values_to = "value") %>%
    mutate(
      variable = recode(
        stat,
        lbo_count    = "Number of LBOs",
        target_count = "Number of targets",
        exit_count = "Number of exits",
        pct_kn       = "Prop. of LBOs with known deal size (%)",
        pct_kn_exit  = "Prop. of LBOs with known exit type (%)",
        p25_size_mn  = "Deal size P25 (2023 USD, $M)",
        med_size_mn  = "Deal size median (2023 USD, $M)",
        p75_size_mn  = "Deal size P75 (2023 USD, $M)",
        vol_kn_bn    = "Known total volume (2023 USD, $B)",
        impvol_ci    = paste0("Imputed total volume (2023 USD, $B)  \n*[", ci_label, "]*")
      ),
      group = case_when(
        stat %in% c("lbo_count", "target_count", "exit_count") ~ "Counts",
        stat == "pct_kn"                         ~ "Coverage",
        stat == "pct_kn_exit"                    ~ "Coverage",
        stat %in% c("p25_size_mn", "med_size_mn", "p75_size_mn") ~ "Deal size distribution",
        stat %in% c("vol_kn_bn", "impvol_ci")     ~ "Total volume",
        TRUE                                      ~ "Other"
      ),
      display = case_when(
        stat %in% c("lbo_count", "target_count", "exit_count") ~ fmt_int(value),
        stat == "pct_kn"                         ~ fmt_pct(value),
        stat == "pct_kn_exit"                    ~ fmt_pct(value),
        stat %in% c("p25_size_mn", "med_size_mn", "p75_size_mn") ~ fmt_usd_m(value),
        stat == "vol_kn_bn"                      ~ fmt_usd_b(value),
        stat == "impvol_ci"                      ~ value,  # already HTML (estimate + CI)
        TRUE                                     ~ value
      ),
      variable = factor(
        variable,
        levels = c(
          "Number of LBOs",
          "Number of targets",
          "Number of exits",
          "Prop. of LBOs with known deal size (%)",
          "Prop. of LBOs with known exit type (%)",
          "Deal size P25 (2023 USD, $M)",
          "Deal size median (2023 USD, $M)",
          "Deal size P75 (2023 USD, $M)",
          "Known total volume (2023 USD, $B)",
          paste0("Imputed total volume (2023 USD, $B)  \n*[", ci_label, "]*")
        )
      )
    ) %>%
    arrange(variable) %>%
    select(group, variable, lbo_period, display) %>%
    pivot_wider(names_from = lbo_period, values_from = display)

  period_cols <- setdiff(names(wide), c("group", "variable"))

  wide %>%
    gt(rowname_col = "variable", groupname_col = "group") %>%
    tab_header(
      title    = md(paste0("**", title, "**")), subtitle = md(subtitle)
    ) %>%
    tab_stubhead(label = "") %>%
    fmt_markdown(columns = everything()) %>%     # formats the period columns
    cols_align("left",  columns = stub()) %>%
    cols_align("right", columns = everything()) %>%
    cols_width(
      stub() ~ px(280), everything() ~ px(90)    # period columns
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"), locations = cells_stub()
    ) %>%
    tab_style(
      style = cell_text(style = "italic"), locations = cells_row_groups()
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = "1985–2019")
    ) %>%
    opt_row_striping() %>%
    opt_table_font(font = list("Helvetica", "Arial", "sans-serif")) %>%
    tab_options(
      table.font.size            = px(14),
      heading.title.font.size    = px(18),
      heading.subtitle.font.size = px(14),
      row_group.font.size        = px(12),
      data_row.padding           = px(6),
      column_labels.border.bottom.style = "solid"
    )
}

lbo_gt <- style_lbo_table_period_cols(
  lbo_tbl,
  title    = "Leveraged-buyout (LBO) Activity (1985–2019)",
  subtitle = "Counts, coverage rates, and deal-value aggregates by deal windows, in 2023 dollars",
  ci_label = "95% CI"
)

lbo_gt
```

<div id="schiyrilej" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#schiyrilej table {
  font-family: Helvetica, Arial, sans-serif, system-ui, 'Segoe UI', Roboto, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#schiyrilej thead, #schiyrilej tbody, #schiyrilej tfoot, #schiyrilej tr, #schiyrilej td, #schiyrilej th {
  border-style: none;
}

#schiyrilej p {
  margin: 0;
  padding: 0;
}

#schiyrilej .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#schiyrilej .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#schiyrilej .gt_title {
  color: #333333;
  font-size: 18px;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#schiyrilej .gt_subtitle {
  color: #333333;
  font-size: 14px;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#schiyrilej .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#schiyrilej .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#schiyrilej .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#schiyrilej .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#schiyrilej .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#schiyrilej .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#schiyrilej .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#schiyrilej .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#schiyrilej .gt_spanner_row {
  border-bottom-style: hidden;
}

#schiyrilej .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 12px;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#schiyrilej .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 12px;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#schiyrilej .gt_from_md > :first-child {
  margin-top: 0;
}

#schiyrilej .gt_from_md > :last-child {
  margin-bottom: 0;
}

#schiyrilej .gt_row {
  padding-top: 6px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#schiyrilej .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#schiyrilej .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#schiyrilej .gt_row_group_first td {
  border-top-width: 2px;
}

#schiyrilej .gt_row_group_first th {
  border-top-width: 2px;
}

#schiyrilej .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#schiyrilej .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#schiyrilej .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#schiyrilej .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#schiyrilej .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#schiyrilej .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#schiyrilej .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#schiyrilej .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#schiyrilej .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#schiyrilej .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#schiyrilej .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#schiyrilej .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#schiyrilej .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#schiyrilej .gt_left {
  text-align: left;
}

#schiyrilej .gt_center {
  text-align: center;
}

#schiyrilej .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#schiyrilej .gt_font_normal {
  font-weight: normal;
}

#schiyrilej .gt_font_bold {
  font-weight: bold;
}

#schiyrilej .gt_font_italic {
  font-style: italic;
}

#schiyrilej .gt_super {
  font-size: 65%;
}

#schiyrilej .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#schiyrilej .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#schiyrilej .gt_indent_1 {
  text-indent: 5px;
}

#schiyrilej .gt_indent_2 {
  text-indent: 10px;
}

#schiyrilej .gt_indent_3 {
  text-indent: 15px;
}

#schiyrilej .gt_indent_4 {
  text-indent: 20px;
}

#schiyrilej .gt_indent_5 {
  text-indent: 25px;
}

#schiyrilej .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#schiyrilej div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" style="table-layout:fixed;width:0px;" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <colgroup>
    <col style="width:280px;"/>
    <col style="width:90px;"/>
    <col style="width:90px;"/>
    <col style="width:90px;"/>
    <col style="width:90px;"/>
    <col style="width:90px;"/>
    <col style="width:90px;"/>
    <col style="width:90px;"/>
    <col style="width:90px;"/>
  </colgroup>
  <thead>
    <tr class="gt_heading">
      <td colspan="9" class="gt_heading gt_title gt_font_normal" style><span class='gt_from_md'><strong>Leveraged-buyout (LBO) Activity (1985–2019)</strong></span></td>
    </tr>
    <tr class="gt_heading">
      <td colspan="9" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style><span class='gt_from_md'>Counts, coverage rates, and deal-value aggregates by deal windows, in 2023 dollars</span></td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a1985–1989">1985–1989</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a1990–1994">1990–1994</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a1995–1999">1995–1999</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a2000–2004">2000–2004</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a2005–2009">2005–2009</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a2010–2014">2010–2014</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a2015–2019">2015–2019</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="a1985–2019">1985–2019</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="9" class="gt_group_heading" style="font-style: italic;" scope="colgroup" id="Counts">Counts</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_1" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;"><span class='gt_from_md'>Number of LBOs</span></th>
<td headers="Counts stub_1_1 1985–1989" class="gt_row gt_right"><span class='gt_from_md'>421</span></td>
<td headers="Counts stub_1_1 1990–1994" class="gt_row gt_right"><span class='gt_from_md'>656</span></td>
<td headers="Counts stub_1_1 1995–1999" class="gt_row gt_right"><span class='gt_from_md'>2,820</span></td>
<td headers="Counts stub_1_1 2000–2004" class="gt_row gt_right"><span class='gt_from_md'>5,268</span></td>
<td headers="Counts stub_1_1 2005–2009" class="gt_row gt_right"><span class='gt_from_md'>11,261</span></td>
<td headers="Counts stub_1_1 2010–2014" class="gt_row gt_right"><span class='gt_from_md'>14,135</span></td>
<td headers="Counts stub_1_1 2015–2019" class="gt_row gt_right"><span class='gt_from_md'>22,413</span></td>
<td headers="Counts stub_1_1 1985–2019" class="gt_row gt_right"><span class='gt_from_md'>56,974</span></td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;"><span class='gt_from_md'>Number of targets</span></th>
<td headers="Counts stub_1_2 1985–1989" class="gt_row gt_right gt_striped"><span class='gt_from_md'>402</span></td>
<td headers="Counts stub_1_2 1990–1994" class="gt_row gt_right gt_striped"><span class='gt_from_md'>648</span></td>
<td headers="Counts stub_1_2 1995–1999" class="gt_row gt_right gt_striped"><span class='gt_from_md'>2,679</span></td>
<td headers="Counts stub_1_2 2000–2004" class="gt_row gt_right gt_striped"><span class='gt_from_md'>5,086</span></td>
<td headers="Counts stub_1_2 2005–2009" class="gt_row gt_right gt_striped"><span class='gt_from_md'>10,916</span></td>
<td headers="Counts stub_1_2 2010–2014" class="gt_row gt_right gt_striped"><span class='gt_from_md'>13,780</span></td>
<td headers="Counts stub_1_2 2015–2019" class="gt_row gt_right gt_striped"><span class='gt_from_md'>21,869</span></td>
<td headers="Counts stub_1_2 1985–2019" class="gt_row gt_right gt_striped"><span class='gt_from_md'>49,621</span></td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;"><span class='gt_from_md'>Number of exits</span></th>
<td headers="Counts stub_1_3 1985–1989" class="gt_row gt_right"><span class='gt_from_md'>344</span></td>
<td headers="Counts stub_1_3 1990–1994" class="gt_row gt_right"><span class='gt_from_md'>541</span></td>
<td headers="Counts stub_1_3 1995–1999" class="gt_row gt_right"><span class='gt_from_md'>1,893</span></td>
<td headers="Counts stub_1_3 2000–2004" class="gt_row gt_right"><span class='gt_from_md'>3,071</span></td>
<td headers="Counts stub_1_3 2005–2009" class="gt_row gt_right"><span class='gt_from_md'>5,673</span></td>
<td headers="Counts stub_1_3 2010–2014" class="gt_row gt_right"><span class='gt_from_md'>5,664</span></td>
<td headers="Counts stub_1_3 2015–2019" class="gt_row gt_right"><span class='gt_from_md'>4,177</span></td>
<td headers="Counts stub_1_3 1985–2019" class="gt_row gt_right"><span class='gt_from_md'>21,363</span></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="9" class="gt_group_heading" style="font-style: italic;" scope="colgroup" id="Coverage">Coverage</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_4" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;"><span class='gt_from_md'>Prop. of LBOs with known deal size (%)</span></th>
<td headers="Coverage stub_1_4 1985–1989" class="gt_row gt_right gt_striped"><span class='gt_from_md'>38%</span></td>
<td headers="Coverage stub_1_4 1990–1994" class="gt_row gt_right gt_striped"><span class='gt_from_md'>35%</span></td>
<td headers="Coverage stub_1_4 1995–1999" class="gt_row gt_right gt_striped"><span class='gt_from_md'>34%</span></td>
<td headers="Coverage stub_1_4 2000–2004" class="gt_row gt_right gt_striped"><span class='gt_from_md'>39%</span></td>
<td headers="Coverage stub_1_4 2005–2009" class="gt_row gt_right gt_striped"><span class='gt_from_md'>29%</span></td>
<td headers="Coverage stub_1_4 2010–2014" class="gt_row gt_right gt_striped"><span class='gt_from_md'>21%</span></td>
<td headers="Coverage stub_1_4 2015–2019" class="gt_row gt_right gt_striped"><span class='gt_from_md'>14%</span></td>
<td headers="Coverage stub_1_4 1985–2019" class="gt_row gt_right gt_striped"><span class='gt_from_md'>22%</span></td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;"><span class='gt_from_md'>Prop. of LBOs with known exit type (%)</span></th>
<td headers="Coverage stub_1_5 1985–1989" class="gt_row gt_right"><span class='gt_from_md'>82%</span></td>
<td headers="Coverage stub_1_5 1990–1994" class="gt_row gt_right"><span class='gt_from_md'>82%</span></td>
<td headers="Coverage stub_1_5 1995–1999" class="gt_row gt_right"><span class='gt_from_md'>67%</span></td>
<td headers="Coverage stub_1_5 2000–2004" class="gt_row gt_right"><span class='gt_from_md'>58%</span></td>
<td headers="Coverage stub_1_5 2005–2009" class="gt_row gt_right"><span class='gt_from_md'>50%</span></td>
<td headers="Coverage stub_1_5 2010–2014" class="gt_row gt_right"><span class='gt_from_md'>40%</span></td>
<td headers="Coverage stub_1_5 2015–2019" class="gt_row gt_right"><span class='gt_from_md'>19%</span></td>
<td headers="Coverage stub_1_5 1985–2019" class="gt_row gt_right"><span class='gt_from_md'>37%</span></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="9" class="gt_group_heading" style="font-style: italic;" scope="colgroup" id="Deal size distribution">Deal size distribution</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_6" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;"><span class='gt_from_md'>Deal size P25 (2023 USD, $M)</span></th>
<td headers="Deal size distribution stub_1_6 1985–1989" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$104M</span></td>
<td headers="Deal size distribution stub_1_6 1990–1994" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$21M</span></td>
<td headers="Deal size distribution stub_1_6 1995–1999" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$27M</span></td>
<td headers="Deal size distribution stub_1_6 2000–2004" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$24M</span></td>
<td headers="Deal size distribution stub_1_6 2005–2009" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$22M</span></td>
<td headers="Deal size distribution stub_1_6 2010–2014" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$22M</span></td>
<td headers="Deal size distribution stub_1_6 2015–2019" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$26M</span></td>
<td headers="Deal size distribution stub_1_6 1985–2019" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$24M</span></td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;"><span class='gt_from_md'>Deal size median (2023 USD, $M)</span></th>
<td headers="Deal size distribution stub_1_7 1985–1989" class="gt_row gt_right"><span class='gt_from_md'>$266M</span></td>
<td headers="Deal size distribution stub_1_7 1990–1994" class="gt_row gt_right"><span class='gt_from_md'>$72M</span></td>
<td headers="Deal size distribution stub_1_7 1995–1999" class="gt_row gt_right"><span class='gt_from_md'>$93M</span></td>
<td headers="Deal size distribution stub_1_7 2000–2004" class="gt_row gt_right"><span class='gt_from_md'>$71M</span></td>
<td headers="Deal size distribution stub_1_7 2005–2009" class="gt_row gt_right"><span class='gt_from_md'>$80M</span></td>
<td headers="Deal size distribution stub_1_7 2010–2014" class="gt_row gt_right"><span class='gt_from_md'>$95M</span></td>
<td headers="Deal size distribution stub_1_7 2015–2019" class="gt_row gt_right"><span class='gt_from_md'>$121M</span></td>
<td headers="Deal size distribution stub_1_7 1985–2019" class="gt_row gt_right"><span class='gt_from_md'>$91M</span></td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;"><span class='gt_from_md'>Deal size P75 (2023 USD, $M)</span></th>
<td headers="Deal size distribution stub_1_8 1985–1989" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$797M</span></td>
<td headers="Deal size distribution stub_1_8 1990–1994" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$212M</span></td>
<td headers="Deal size distribution stub_1_8 1995–1999" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$291M</span></td>
<td headers="Deal size distribution stub_1_8 2000–2004" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$207M</span></td>
<td headers="Deal size distribution stub_1_8 2005–2009" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$295M</span></td>
<td headers="Deal size distribution stub_1_8 2010–2014" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$356M</span></td>
<td headers="Deal size distribution stub_1_8 2015–2019" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$505M</span></td>
<td headers="Deal size distribution stub_1_8 1985–2019" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$336M</span></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="9" class="gt_group_heading" style="font-style: italic;" scope="colgroup" id="Total volume">Total volume</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_9" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;"><span class='gt_from_md'>Known total volume (2023 USD, $B)</span></th>
<td headers="Total volume stub_1_9 1985–1989" class="gt_row gt_right"><span class='gt_from_md'>$152B</span></td>
<td headers="Total volume stub_1_9 1990–1994" class="gt_row gt_right"><span class='gt_from_md'>$54B</span></td>
<td headers="Total volume stub_1_9 1995–1999" class="gt_row gt_right"><span class='gt_from_md'>$296B</span></td>
<td headers="Total volume stub_1_9 2000–2004" class="gt_row gt_right"><span class='gt_from_md'>$530B</span></td>
<td headers="Total volume stub_1_9 2005–2009" class="gt_row gt_right"><span class='gt_from_md'>$1,873B</span></td>
<td headers="Total volume stub_1_9 2010–2014" class="gt_row gt_right"><span class='gt_from_md'>$1,215B</span></td>
<td headers="Total volume stub_1_9 2015–2019" class="gt_row gt_right"><span class='gt_from_md'>$1,868B</span></td>
<td headers="Total volume stub_1_9 1985–2019" class="gt_row gt_right"><span class='gt_from_md'>$5,987B</span></td></tr>
    <tr><th id="stub_1_10" scope="row" class="gt_row gt_right gt_stub" style="font-weight: bold;"><span class='gt_from_md'>Imputed total volume (2023 USD, $B)<br />
<em>[95% CI]</em></span></th>
<td headers="Total volume stub_1_10 1985–1989" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$210B<br />
<em>[181–239]</em></span></td>
<td headers="Total volume stub_1_10 1990–1994" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$126B<br />
<em>[103–148]</em></span></td>
<td headers="Total volume stub_1_10 1995–1999" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$573B<br />
<em>[516–630]</em></span></td>
<td headers="Total volume stub_1_10 2000–2004" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$917B<br />
<em>[851–983]</em></span></td>
<td headers="Total volume stub_1_10 2005–2009" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$2,701B<br />
<em>[2630–2772]</em></span></td>
<td headers="Total volume stub_1_10 2010–2014" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$2,494B<br />
<em>[2408–2579]</em></span></td>
<td headers="Total volume stub_1_10 2015–2019" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$4,444B<br />
<em>[4256–4633]</em></span></td>
<td headers="Total volume stub_1_10 1985–2019" class="gt_row gt_right gt_striped"><span class='gt_from_md'>$11,464B<br />
<em>[11228–11700]</em></span></td></tr>
  </tbody>
  
</table>
</div>

``` r
gtsave(lbo_gt, "tables/t1_buyout_summary.docx")
gtsave(lbo_gt, "tables/t1_buyout_summary.png", zoom = 2, expand = 5, vwidth = 8 * 90 + 300)  # 2× DPI
```

    ## file:////var/folders/gp/hc9k1h4j3bv9z3zk3wrnz3mw0000gn/T//RtmpHUZ8SV/filea0f41d8af9ee.html screenshot completed

``` r
knitr::include_graphics("../tables/t1_buyout_summary.png", error = FALSE)
```

![](../tables/t1_buyout_summary.png)<!-- -->

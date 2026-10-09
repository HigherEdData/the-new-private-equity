Figure 3
================

# Figure 3 (log-space pooling)

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

Updated Figure 3

``` r
# Compute shared y-axis limits
y_max <- 1125000
y_min <- 0

p_B <- ggplot(pooled_year_log, aes(x = dealyear)) +
  geom_line(aes(y = known_volume, colour = "Known portion only"),
            na.rm = TRUE) +
  geom_point(aes(y = known_volume, colour = "Known portion only"),
             na.rm = TRUE, shape = 17) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2, fill = "darkblue") +
  geom_line(aes(y = volume, colour = "Aggregate (known and imputed)"),
            na.rm = TRUE) +
  geom_point(aes(y = volume, colour = "Aggregate (known and imputed)"),
             na.rm = TRUE) +
  scale_colour_manual(
    name   = "",
    values = c("Aggregate (known and imputed)" = "darkblue",
               "Known portion only"            = "grey20")) +
  labs(
    y     = "Deal volume (2023 dollars)",
    title = "LBO deal volume") +
  scale_y_continuous(
    breaks = c(0, 0.125, 0.25, 0.375, 0.50, 0.625, 0.75, 0.875, 1, 1.125) * 1e6,
    labels = c("$0", "$125B", "$250B", "$375B", "$500B",
               "$625B", "$750B", "$875B", "$1.00T", "$1.125T")) +
  scale_x_continuous(
     breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
    )+
  coord_cartesian(ylim = c(y_min, y_max)) +
  theme_minimal() +
  theme(legend.position    = "bottom",
        panel.grid.minor = element_blank(),
        axis.title.x       = element_blank(),
        legend.text        = element_text(size = 12),
        plot.title         = element_text(size = 13, hjust = 0.5),
        plot.title.position = "panel",
        axis.title.y       = element_text(size = 11),
        axis.text.x        = element_text(size = 11),
        axis.text.y        = element_text(size = 10))

ggsave("figures/f3_lbo_deal_volume.png", p_B,
       width = 8, height = 5, units = "in", dpi = 320)
knitr::include_graphics("../figures/f3_lbo_deal_volume.png", error = FALSE)
```

![](../figures/f3_lbo_deal_volume.png)<!-- -->

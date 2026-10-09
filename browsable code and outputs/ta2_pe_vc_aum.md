Table A2: Private Equity and Venture Fund Assets Under Management in
Billions (2023 Dollars)
================

# Load data

``` r
# PitchBook dry powder and investments (NAV) and SEC Form PF gross assets
# (Q1 of each year) for US PE and VC funds, in millions of nominal dollars.
# Built by "Data prep for Figures 2, A2, A5.Rmd".
aum_nominal <- read.csv("d_net_worth_listed_firms.csv") %>%
  select(Year, PE_DryPowder, PE_Investments, VC_DryPowder, VC_Investments,
         PE_Gross = GAV_PE, VC_Gross = GAV_VC) %>%
  filter(Year >= 1997, Year <= 2023)

# --- CPI adjustment to 2023 dollars (January CPI-U index) ---
data("cu_main", package = "blscrapeR")

cpi_jan <- cu_main %>%
  mutate(date = as.Date(date)) %>%
  filter(format(date, "%m") == "01") %>%
  transmute(
    Year = as.integer(format(date, "%Y")),
    cpi = as.numeric(value)
  )

cpi_2023 <- cpi_jan %>% filter(Year == 2023) %>% pull(cpi)
```

# Convert to billions of 2023 dollars

``` r
aum <- aum_nominal %>%
  left_join(cpi_jan, by = "Year") %>%
  mutate(
    cpi_factor = cpi_2023 / cpi,
    across(c(PE_DryPowder, PE_Investments, VC_DryPowder, VC_Investments, PE_Gross, VC_Gross),
           ~ .x / 1000 * cpi_factor),
    # AUM = Dry Powder + Investments
    PE_AUM = PE_DryPowder + PE_Investments,
    VC_AUM = VC_DryPowder + VC_Investments
  ) %>%
  select(Year, PE_DryPowder, PE_Investments, PE_AUM,
         VC_DryPowder, VC_Investments, VC_AUM, PE_Gross, VC_Gross) %>%
  arrange(Year)

aum
```

    ##    Year PE_DryPowder PE_Investments    PE_AUM VC_DryPowder VC_Investments
    ## 1  1997     172.0358       7.725593  179.7614     52.94858       8.856168
    ## 2  1998     277.3658      48.761499  326.1273     61.36903      30.591815
    ## 3  1999     325.5224     131.850266  457.3727     91.18276     120.361239
    ## 4  2000     374.3362     201.006112  575.3423    131.46119     175.813819
    ## 5  2001     364.4981     198.925984  563.4241    152.91473     118.021435
    ## 6  2002     345.4855     210.942199  556.4277    140.79716      94.484527
    ## 7  2003     298.8756     270.090116  568.9657    124.35331     101.982870
    ## 8  2004     289.2317     332.277894  621.5096    112.04907     123.334589
    ## 9  2005     317.7566     405.229781  722.9864    111.45778     143.907520
    ## 10 2006     459.6517     519.029636  978.6813    114.23475     170.447886
    ## 11 2007     501.5358     662.316597 1163.8524    113.38813     196.657536
    ## 12 2008     484.2808     605.740253 1090.0210    109.28522     183.746401
    ## 13 2009     477.4619     703.508743 1180.9707    107.14194     197.985841
    ## 14 2010     427.0124     819.775443 1246.7879    101.78142     219.720810
    ## 15 2011     418.2418     860.111536 1278.3533     96.95605     248.636600
    ## 16 2012     411.1667     890.223957 1301.3906     90.69659     254.556858
    ## 17 2013     465.1597     905.427900 1370.5876     88.28700     292.993564
    ## 18 2014     431.3450     913.113490 1344.4584     93.31553     325.136315
    ## 19 2015     547.3650     905.749647 1453.1147    112.21111     348.916413
    ## 20 2016     595.6516     960.777234 1556.4288    132.19850     346.926270
    ## 21 2017     693.7514    1048.949112 1742.7005    136.35845     375.293970
    ## 22 2018     798.8686    1114.843495 1913.7121    159.07370     454.892837
    ## 23 2019     870.8815    1322.079677 2192.9612    165.66133     546.337504
    ## 24 2020     937.3581    1644.089064 2581.4472    197.21644     725.756505
    ## 25 2021    1006.6099    2264.814992 3271.4249    260.29068    1160.953661
    ## 26 2022    1071.4920    2181.168063 3252.6601    304.72748     982.466933
    ## 27 2023    1056.2000    2354.200000 3410.4000    322.40000     908.800000
    ##        VC_AUM PE_Gross  VC_Gross
    ## 1    61.80475       NA        NA
    ## 2    91.96085       NA        NA
    ## 3   211.54400       NA        NA
    ## 4   307.27501       NA        NA
    ## 5   270.93616       NA        NA
    ## 6   235.28168       NA        NA
    ## 7   226.33618       NA        NA
    ## 8   235.38366       NA        NA
    ## 9   255.36530       NA        NA
    ## 10  284.68263       NA        NA
    ## 11  310.04567       NA        NA
    ## 12  293.03162       NA        NA
    ## 13  305.12778       NA        NA
    ## 14  321.50223       NA        NA
    ## 15  345.59265       NA        NA
    ## 16  345.25345       NA        NA
    ## 17  381.28056 2079.476  31.11436
    ## 18  418.45184 2342.462  38.29639
    ## 19  461.12752 2423.351  49.90004
    ## 20  479.12477 2600.999  69.51164
    ## 21  511.65242 2861.555  76.43964
    ## 22  613.96653 3338.375  98.96846
    ## 23  711.99883 3875.738 132.00580
    ## 24  922.97295 4421.129 167.05393
    ## 25 1421.24435 5517.018 255.14208
    ## 26 1287.19442 6790.158 354.18587
    ## 27 1231.20000 6641.000 374.00000

# Formatted table

``` r
dollars <- function(x) ifelse(is.na(x), "", paste0("$", formatC(round(x), format = "d", big.mark = ",")))

ta2 <- aum %>%
  mutate(Year = as.character(Year),
         across(-Year, dollars)) %>%
  flextable() %>%
  set_header_labels(
    Year = "",
    PE_DryPowder = "PE Dry Powder", PE_Investments = "PE Investment", PE_AUM = "PE AUM",
    VC_DryPowder = "VC Dry Powder", VC_Investments = "VC Investment", VC_AUM = "VC AUM",
    PE_Gross = "PE Gross Assets", VC_Gross = "VC Gross Assets"
  ) %>%
  add_header_row(values = c("", "PitchBook", "SEC Form PF"), colwidths = c(1, 6, 2)) %>%
  set_caption("Table A2: Private Equity and Venture Fund Assets Under Management in Billions (2023 Dollars)") %>%
  add_footer_lines("Note: Data are from the SEC Form PF (Gross Assets) for private equity and venture capital funds and PitchBook (Investments) for private equity and venture capital funds. AUM = Dry Powder + Investments.") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  bold(part = "header") %>%
  align(j = -1, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  # Booktabs-style horizontal rules only (no vertical borders)
  border_remove() %>%
  hline_top(part = "header", border = fp_border(width = 1.5)) %>%
  hline(i = 1, j = 2:9, part = "header", border = fp_border(width = 0.75)) %>%
  hline_bottom(part = "header", border = fp_border(width = 0.75)) %>%
  hline_bottom(part = "body", border = fp_border(width = 1.5)) %>%
  # Fixed widths that fit a portrait page with 1-inch margins (6.5 in)
  width(j = 1, width = 0.5) %>%
  width(j = 2:9, width = 0.74) %>%
  set_table_properties(layout = "fixed")

invisible(save_as_image(ta2, "tables/ta2_pe_vc_aum.png", res = 200))
knitr::include_graphics("../tables/ta2_pe_vc_aum.png", error = FALSE)
```

![](../tables/ta2_pe_vc_aum.png)<!-- -->

# Export to Word

``` r
save_as_docx(
  ta2,
  path = output_docx,
  pr_section = prop_section(
    page_size = page_size(width = 8.5, height = 11, orient = "portrait"),  # US Letter
    page_margins = page_mar(top = 1, bottom = 1, left = 1, right = 1),
    type = "continuous"
  )
)
```

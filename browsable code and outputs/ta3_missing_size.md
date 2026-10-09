Table A3: Missing Deal Size Percentages by Deal Type and Time Period
================

# Load and prepare data

``` r
buyout_data <- readRDS(input_rds)
# IDate columns saved with double storage are rejected by newer vctrs; convert them to Date
for (col in names(buyout_data)[vapply(buyout_data, inherits, logical(1), "IDate")]) {
  buyout_data[[col]] <- as.Date(as.numeric(buyout_data[[col]]))
}

deals <- buyout_data %>%
  as_tibble() %>%
  mutate(
    # dealdate carries IDate class with double storage (dplyr round-trip
    # artifact); rebuild as a plain Date before extracting the year
    dealdate = as.Date(as.numeric(dealdate)),
    dealyear = year(dealdate),
    size_missing = is.na(dealsize_2023d),
    # Deals with missing deal type are treated as Private to PE
    transfertype = coalesce(transfertype, "Private to PE")
  ) %>%
  # Drop deals where the target remains or becomes publicly traded
  filter(!transfertype %in% c("Public to public", "Private to public")) %>%
  # Restrict to the 1985-2019 window and assign 5-year periods
  filter(dealyear >= 1985, dealyear <= 2019) %>%
  mutate(
    period = paste(
      floor((dealyear - 1985) / 5) * 5 + 1985,
      floor((dealyear - 1985) / 5) * 5 + 1989,
      sep = "-"
    )
  )
```

# Percent missing by deal type and period

``` r
# Percent missing within each period x deal type cell
by_type <- deals %>%
  group_by(period, transfertype) %>%
  summarise(pct_missing = 100 * mean(size_missing), .groups = "drop")

# Percent missing across all deal types within each period
all_types <- deals %>%
  group_by(period) %>%
  summarise(pct_missing = 100 * mean(size_missing), .groups = "drop") %>%
  mutate(transfertype = "All deals")

missing_wide <- bind_rows(by_type, all_types) %>%
  pivot_wider(names_from = transfertype, values_from = pct_missing) %>%
  arrange(period) %>%
  relocate(period, `Public to PE`,
           sort(setdiff(names(.), c("period", "Public to PE", "All deals"))),
           `All deals`) %>%
  rename(Period = period)

missing_wide
```

    ## # A tibble: 7 × 6
    ##   Period    `Public to PE` `Private to PE` Secondary `VC to PE` `All deals`
    ##   <chr>              <dbl>           <dbl>     <dbl>      <dbl>       <dbl>
    ## 1 1985-1989           10.5            70.8      66.7       80          62.5
    ## 2 1990-1994           25.7            68.6      51.4       55          64.9
    ## 3 1995-1999           20.9            69.5      63.3       73.4        66.0
    ## 4 2000-2004           24.5            65.8      50.2       67.2        61.4
    ## 5 2005-2009           22.5            75.6      61.1       69.7        70.9
    ## 6 2010-2014           23.7            83.6      70.1       76.8        78.9
    ## 7 2015-2019           28.3            90.0      75.4       82.0        85.9

# Formatted table

``` r
ta3 <- missing_wide %>%
  flextable() %>%
  colformat_double(digits = 1) %>%
  set_caption("Table A3: Missing Deal Size Percentages by Deal Type and Time Period") %>%
  add_footer_lines("Note: Cells report the percentage of deals with missing transaction size (2023 dollars). Source: PitchBook buyout data.") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  align(j = -1, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  # Booktabs-style horizontal rules only (no vertical borders)
  border_remove() %>%
  hline_top(part = "header", border = fp_border(width = 1.5)) %>%
  hline_bottom(part = "header", border = fp_border(width = 0.75)) %>%
  hline_bottom(part = "body", border = fp_border(width = 1.5)) %>%
  autofit()

invisible(save_as_image(ta3, "tables/ta3_missing_size.png", res = 200))
knitr::include_graphics("../tables/ta3_missing_size.png", error = FALSE)
```

![](../tables/ta3_missing_size.png)<!-- -->

# Export to Word

``` r
save_as_docx(
  ta3,
  path = output_docx,
  pr_section = prop_section(
    page_size = page_size(width = 8.5, height = 11, orient = "portrait"),  # US Letter
    page_margins = page_mar(top = 1, bottom = 1, left = 1, right = 1),
    type = "continuous"
  )
)
```

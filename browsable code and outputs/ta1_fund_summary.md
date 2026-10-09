Table Appendix 1
================

Appendix Table 1 (top part)

``` r
# ---- 1. Read pivot, take the All/All top-line rows ----------------
raw <- read_csv("PitchBook_Pivot_US_Funds.csv",
                show_col_types = FALSE)
names(raw) <- str_trim(names(raw))

clean_num <- function(x) {
  x <- str_replace_all(as.character(x), "[^0-9.\\-]", "")
  x[x %in% c("", "-")] <- NA_character_
  as.numeric(x)
}

agg <- raw %>%
  filter(`Fund Category` == "All", `Fund Type` == "All") %>%
  mutate(across(c(`Closed Year`, `Fund Count`, `Fund Size Sum`,
                  `Fund Size Median`, `Fund Size Max`,
                  `Fund Size 75th`, `Fund Size Count`), clean_num)) %>%
  filter(`Closed Year` >= 1985, `Closed Year` <= 2019)

# ---- 2. CPI-adjust to 2023 dollars (Jan CPI-U) --------------------
data("cu_main", package = "blscrapeR")
cpi_jan <- cu_main %>%
  mutate(date = as.Date(date)) %>%
  filter(format(date, "%m") == "01") %>%
  transmute(year = as.integer(format(date, "%Y")), cpi = as.numeric(value))
cpi_2023 <- cpi_jan %>% filter(year == 2023) %>% pull(cpi)

agg <- agg %>%
  left_join(cpi_jan, by = c("Closed Year" = "year")) %>%
  mutate(f = cpi_2023 / cpi,
         across(c(`Fund Size Sum`, `Fund Size Median`,
                  `Fund Size Max`, `Fund Size 75th`), ~ .x * f))

# ---- 3. Collapse into 5-year windows + all-period total -----------
# Sums/counts pool across years; mean = pooled total/count;
# median/75th use the FIRST year of the span; max is the overall max.
agg <- agg %>%
  mutate(win_start = ((`Closed Year` - 1985) %/% 5) * 5 + 1985,
         window    = paste0(win_start, "-", win_start + 4))

summ <- function(g) {
  g <- g %>% arrange(`Closed Year`)
  size_count <- sum(g$`Fund Size Count`, na.rm = TRUE)
  tibble(
    `Fund Count`        = sum(g$`Fund Count`, na.rm = TRUE),
    `Fund Size Sum B`   = sum(g$`Fund Size Sum`, na.rm = TRUE) / 1000,        # m -> bn
    `Fund Size Mean`    = sum(g$`Fund Size Sum`, na.rm = TRUE) / size_count,  # pooled mean
    `Fund Size Median`  = first(g$`Fund Size Median`),                        # first year
    `Fund Size 75th`    = first(g$`Fund Size 75th`),                          # first year
    `Fund Size Max`     = max(g$`Fund Size Max`, na.rm = TRUE),
    `Prop missing size` = round(100 * (1 - size_count / sum(g$`Fund Count`, na.rm = TRUE)))
  )
}

windowed <- agg %>%
  group_by(window) %>%
  group_modify(~ summ(.x)) %>%
  ungroup()

total_col <- summ(agg) %>%
  mutate(window = "1985-2019")

windowed <- bind_rows(windowed, total_col)

# ---- 4. Pivot to metrics-as-rows, windows-as-columns --------------
metric_order <- c("Fund Count", "Prop missing size", "Fund Size Sum B",
                  "Fund Size Mean", "Fund Size Median",
                  "Fund Size 75th", "Fund Size Max")

win_levels <- c(sort(setdiff(unique(windowed$window), "1985-2019")),
                "1985-2019")

funds <- windowed %>%
  pivot_longer(-window, names_to = "Metric", values_to = "val") %>%
  pivot_wider(names_from = window, values_from = val) %>%
  mutate(Metric = factor(Metric, levels = metric_order)) %>%
  arrange(Metric) %>%
  select(Metric, all_of(win_levels)) %>%
  mutate(Metric = dplyr::recode(as.character(Metric),
    "Fund Count"        = "Number of funds",
    "Prop missing size" = "Prop. of funds with missing $ raised (%)",
    "Fund Size Sum B"   = "Total $ raised by all funds (bn USD)",
    "Fund Size Mean"    = "Mean $ raised per fund (m USD)",
    "Fund Size Median"  = "Median of $ raised in 1st yr of period (m USD)",
    "Fund Size 75th"    = "75th-pct of $ raised in 1st yr of period (m USD)",
    "Fund Size Max"     = "$ raised by largest fund (m USD)"
  ))

fund_gt <- funds |>
  gt(rowname_col = "Metric") |>
  # ----- titles -----------------------------------------------------
  tab_header(
    title    = md("**Private Funds Statistics (1985–2019)**"),
    subtitle = md("Counts, coverage rates, and capital raised by capital commitment windows, in 2023 dollars")
  ) |>
  # ----- numeric formatting ----------------------------------------
  fmt_number(
    rows    = Metric == "Number of funds",
    columns = where(is.numeric),
    decimals = 0, sep_mark = ","
  ) |>
  fmt_number(
    rows    = Metric == "Prop. of funds with missing $ raised (%)",
    columns = where(is.numeric),
    decimals = 0, pattern = "{x}%"
  ) |>
  fmt_number(
    rows    = Metric == "Total $ raised by all funds (bn USD)",
    columns = where(is.numeric),
    decimals = 0, sep_mark = ",", pattern = "$ {x} B"
  ) |>
  fmt_number(
    rows    = Metric %in% c(
      "Mean $ raised per fund (m USD)",
      "Median of $ raised in 1st yr of period (m USD)",
      "75th-pct of $ raised in 1st yr of period (m USD)",
      "$ raised by largest fund (m USD)"
    ),
    columns = where(is.numeric),
    decimals = 0, sep_mark = ",", pattern = "$ {x} M"
  ) |>
  # ----- aesthetics -------------------------------------------------
  tab_style(                                 # bold stub labels
    style = cell_text(weight = "bold"),
    locations = cells_stub()
  ) |>
  tab_style(                                 # bold the total column header
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = `1985-2019`)
  ) |>
  opt_table_font(font = list("Helvetica", "Arial", "sans-serif")) |>
  tab_options(
    table.font.size            = px(14),
    heading.title.font.size    = px(18),
    heading.subtitle.font.size = px(14),
    data_row.padding           = px(6)
  )
fund_gt
```

<div id="ieflsrquyr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ieflsrquyr table {
  font-family: Helvetica, Arial, sans-serif, system-ui, 'Segoe UI', Roboto, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ieflsrquyr thead, #ieflsrquyr tbody, #ieflsrquyr tfoot, #ieflsrquyr tr, #ieflsrquyr td, #ieflsrquyr th {
  border-style: none;
}

#ieflsrquyr p {
  margin: 0;
  padding: 0;
}

#ieflsrquyr .gt_table {
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

#ieflsrquyr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ieflsrquyr .gt_title {
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

#ieflsrquyr .gt_subtitle {
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

#ieflsrquyr .gt_heading {
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

#ieflsrquyr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ieflsrquyr .gt_col_headings {
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

#ieflsrquyr .gt_col_heading {
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

#ieflsrquyr .gt_column_spanner_outer {
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

#ieflsrquyr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ieflsrquyr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ieflsrquyr .gt_column_spanner {
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

#ieflsrquyr .gt_spanner_row {
  border-bottom-style: hidden;
}

#ieflsrquyr .gt_group_heading {
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

#ieflsrquyr .gt_empty_group_heading {
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

#ieflsrquyr .gt_from_md > :first-child {
  margin-top: 0;
}

#ieflsrquyr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ieflsrquyr .gt_row {
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

#ieflsrquyr .gt_stub {
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

#ieflsrquyr .gt_stub_row_group {
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

#ieflsrquyr .gt_row_group_first td {
  border-top-width: 2px;
}

#ieflsrquyr .gt_row_group_first th {
  border-top-width: 2px;
}

#ieflsrquyr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ieflsrquyr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ieflsrquyr .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ieflsrquyr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ieflsrquyr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ieflsrquyr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ieflsrquyr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ieflsrquyr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ieflsrquyr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ieflsrquyr .gt_footnotes {
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

#ieflsrquyr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ieflsrquyr .gt_sourcenotes {
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

#ieflsrquyr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ieflsrquyr .gt_left {
  text-align: left;
}

#ieflsrquyr .gt_center {
  text-align: center;
}

#ieflsrquyr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ieflsrquyr .gt_font_normal {
  font-weight: normal;
}

#ieflsrquyr .gt_font_bold {
  font-weight: bold;
}

#ieflsrquyr .gt_font_italic {
  font-style: italic;
}

#ieflsrquyr .gt_super {
  font-size: 65%;
}

#ieflsrquyr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ieflsrquyr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ieflsrquyr .gt_indent_1 {
  text-indent: 5px;
}

#ieflsrquyr .gt_indent_2 {
  text-indent: 10px;
}

#ieflsrquyr .gt_indent_3 {
  text-indent: 15px;
}

#ieflsrquyr .gt_indent_4 {
  text-indent: 20px;
}

#ieflsrquyr .gt_indent_5 {
  text-indent: 25px;
}

#ieflsrquyr .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ieflsrquyr div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="9" class="gt_heading gt_title gt_font_normal" style><span class='gt_from_md'><strong>Private Funds Statistics (1985–2019)</strong></span></td>
    </tr>
    <tr class="gt_heading">
      <td colspan="9" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style><span class='gt_from_md'>Counts, coverage rates, and capital raised by capital commitment windows, in 2023 dollars</span></td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a1985-1989">1985-1989</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a1990-1994">1990-1994</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a1995-1999">1995-1999</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a2000-2004">2000-2004</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a2005-2009">2005-2009</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a2010-2014">2010-2014</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a2015-2019">2015-2019</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="a1985-2019">1985-2019</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub" style="font-weight: bold;">Number of funds</th>
<td headers="stub_1_1 1985-1989" class="gt_row gt_right">337</td>
<td headers="stub_1_1 1990-1994" class="gt_row gt_right">489</td>
<td headers="stub_1_1 1995-1999" class="gt_row gt_right">1,774</td>
<td headers="stub_1_1 2000-2004" class="gt_row gt_right">2,431</td>
<td headers="stub_1_1 2005-2009" class="gt_row gt_right">4,103</td>
<td headers="stub_1_1 2010-2014" class="gt_row gt_right">6,107</td>
<td headers="stub_1_1 2015-2019" class="gt_row gt_right">11,523</td>
<td headers="stub_1_1 1985-2019" class="gt_row gt_right">26,764</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub" style="font-weight: bold;">Prop. of funds with missing $ raised (%)</th>
<td headers="stub_1_2 1985-1989" class="gt_row gt_right">10%</td>
<td headers="stub_1_2 1990-1994" class="gt_row gt_right">10%</td>
<td headers="stub_1_2 1995-1999" class="gt_row gt_right">5%</td>
<td headers="stub_1_2 2000-2004" class="gt_row gt_right">7%</td>
<td headers="stub_1_2 2005-2009" class="gt_row gt_right">11%</td>
<td headers="stub_1_2 2010-2014" class="gt_row gt_right">15%</td>
<td headers="stub_1_2 2015-2019" class="gt_row gt_right">17%</td>
<td headers="stub_1_2 1985-2019" class="gt_row gt_right">14%</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub" style="font-weight: bold;">Total $ raised by all funds (bn USD)</th>
<td headers="stub_1_3 1985-1989" class="gt_row gt_right">$ 122 B</td>
<td headers="stub_1_3 1990-1994" class="gt_row gt_right">$ 163 B</td>
<td headers="stub_1_3 1995-1999" class="gt_row gt_right">$ 772 B</td>
<td headers="stub_1_3 2000-2004" class="gt_row gt_right">$ 1,122 B</td>
<td headers="stub_1_3 2005-2009" class="gt_row gt_right">$ 2,589 B</td>
<td headers="stub_1_3 2010-2014" class="gt_row gt_right">$ 2,206 B</td>
<td headers="stub_1_3 2015-2019" class="gt_row gt_right">$ 4,258 B</td>
<td headers="stub_1_3 1985-2019" class="gt_row gt_right">$ 11,233 B</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub" style="font-weight: bold;">Mean $ raised per fund (m USD)</th>
<td headers="stub_1_4 1985-1989" class="gt_row gt_right">$ 402 M</td>
<td headers="stub_1_4 1990-1994" class="gt_row gt_right">$ 370 M</td>
<td headers="stub_1_4 1995-1999" class="gt_row gt_right">$ 459 M</td>
<td headers="stub_1_4 2000-2004" class="gt_row gt_right">$ 496 M</td>
<td headers="stub_1_4 2005-2009" class="gt_row gt_right">$ 710 M</td>
<td headers="stub_1_4 2010-2014" class="gt_row gt_right">$ 427 M</td>
<td headers="stub_1_4 2015-2019" class="gt_row gt_right">$ 446 M</td>
<td headers="stub_1_4 1985-2019" class="gt_row gt_right">$ 488 M</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub" style="font-weight: bold;">Median of $ raised in 1st yr of period (m USD)</th>
<td headers="stub_1_5 1985-1989" class="gt_row gt_right">$ 85 M</td>
<td headers="stub_1_5 1990-1994" class="gt_row gt_right">$ 146 M</td>
<td headers="stub_1_5 1995-1999" class="gt_row gt_right">$ 139 M</td>
<td headers="stub_1_5 2000-2004" class="gt_row gt_right">$ 186 M</td>
<td headers="stub_1_5 2005-2009" class="gt_row gt_right">$ 243 M</td>
<td headers="stub_1_5 2010-2014" class="gt_row gt_right">$ 138 M</td>
<td headers="stub_1_5 2015-2019" class="gt_row gt_right">$ 95 M</td>
<td headers="stub_1_5 1985-2019" class="gt_row gt_right">$ 85 M</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub" style="font-weight: bold;">75th-pct of $ raised in 1st yr of period (m USD)</th>
<td headers="stub_1_6 1985-1989" class="gt_row gt_right">$ 189 M</td>
<td headers="stub_1_6 1990-1994" class="gt_row gt_right">$ 420 M</td>
<td headers="stub_1_6 1995-1999" class="gt_row gt_right">$ 282 M</td>
<td headers="stub_1_6 2000-2004" class="gt_row gt_right">$ 523 M</td>
<td headers="stub_1_6 2005-2009" class="gt_row gt_right">$ 595 M</td>
<td headers="stub_1_6 2010-2014" class="gt_row gt_right">$ 414 M</td>
<td headers="stub_1_6 2015-2019" class="gt_row gt_right">$ 395 M</td>
<td headers="stub_1_6 1985-2019" class="gt_row gt_right">$ 189 M</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub" style="font-weight: bold;">$ raised by largest fund (m USD)</th>
<td headers="stub_1_7 1985-1989" class="gt_row gt_right">$ 16,528 M</td>
<td headers="stub_1_7 1990-1994" class="gt_row gt_right">$ 4,622 M</td>
<td headers="stub_1_7 1995-1999" class="gt_row gt_right">$ 11,328 M</td>
<td headers="stub_1_7 2000-2004" class="gt_row gt_right">$ 11,448 M</td>
<td headers="stub_1_7 2005-2009" class="gt_row gt_right">$ 32,038 M</td>
<td headers="stub_1_7 2010-2014" class="gt_row gt_right">$ 23,828 M</td>
<td headers="stub_1_7 2015-2019" class="gt_row gt_right">$ 30,824 M</td>
<td headers="stub_1_7 1985-2019" class="gt_row gt_right">$ 32,038 M</td></tr>
  </tbody>
  
</table>
</div>

``` r
gtsave(fund_gt, "tables/ta1a_fund_summary.docx")
gtsave(fund_gt, "tables/ta1a_fund_summary.png", zoom = 2, expand = 5, vwidth = 8 * 90 + 300)  # 2× DPI
```

    ## file:////var/folders/gp/hc9k1h4j3bv9z3zk3wrnz3mw0000gn/T//RtmpcLFNx2/filea475169fad74.html screenshot completed

``` r
knitr::include_graphics("../tables/ta1a_fund_summary.png", error = FALSE)
```

![](../tables/ta1a_fund_summary.png)<!-- -->

Appendix Table 1 (bottom part)

``` r
companies <- fread("allcompanies.csv")
deals_investors <- fread("alldealswithinvestors.csv")
funds <- fread("allfunds.csv")
investor_fund <- fread("investorfundrel_redundant.csv")

# --- CPI adjustment to 2023 dollars (January CPI-U index) ---
data("cu_main", package = "blscrapeR")
cpi_jan <- cu_main %>%
  mutate(date = as.Date(date)) %>%
  filter(format(date, "%m") == "01") %>%
  transmute(year = as.integer(format(date, "%Y")), cpi = as.numeric(value))
cpi_2023 <- cpi_jan %>% filter(year == 2023) %>% pull(cpi)

# --- 0. Fund -> group lookup ---
fund_groups <- funds %>%
  filter(fd_hd_fundcountry == "UNITED STATES") %>%
  transmute(
    fundid,
    fund_group = case_when(
      fd_hd_fundcategory %in% c("Infrastructure",
                                "Real Assets - Real Estate",
                                "Real Assets & Natural Resources") ~ "Real Assets",
      fd_hd_fundcategory %in% c("Co-Investment - General",
                                "Secondaries - General", "Fund of Funds - General",
                                "Other")                            ~ "Other",
      fd_hd_fundcategory == "Private Equity"                        ~ "Private Equity",
      fd_hd_fundcategory == "Private Debt"                          ~ "Private Debt",
      fd_hd_fundcategory == "Venture Capital"                       ~ "Venture Capital",
      TRUE                                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(fund_group))

# --- 1. Per-fund deal stats ---
deals_base <- deals_investors %>%
  left_join(companies %>% select(companyid, hd_hqcountry), by = "companyid") %>%
  filter(hd_hqcountry == "UNITED STATES") %>%
  filter(ml_dealyear > 1984 & ml_dealyear < 2020) %>%
  filter(if_investorfundid != "") %>%
  mutate(
    investment = ml_dealsize2023dollars / investors,
    is_lbo     = dealtype == "Buyout/LBO"
  )

deal_fund <- deals_base %>%
  group_by(if_investorfundid) %>%
  summarise(
    deal_count_total = n_distinct(dealid),
    deal_vol_total   = sum(investment, na.rm = TRUE),
    deal_count_lbo   = n_distinct(dealid[is_lbo]),
    deal_vol_lbo     = sum(investment[is_lbo], na.rm = TRUE),
    .groups = "drop"
  )

# --- 1b. Deduplicated UNIQUE LBO deals per fund group ---
lbo_unique_by_group <- deals_base %>%
  filter(is_lbo) %>%
  left_join(fund_groups, by = c("if_investorfundid" = "fundid")) %>%
  filter(!is.na(fund_group)) %>%
  group_by(fund_group) %>%
  summarise(unique_lbo_deals = n_distinct(dealid), .groups = "drop")

total_unique_lbo_deals <- deals_base %>%
  filter(is_lbo) %>%
  semi_join(fund_groups, by = c("if_investorfundid" = "fundid")) %>%
  summarise(n = n_distinct(dealid)) %>% pull(n)

# --- 1c. Deduplicated UNIQUE deals (all types) per fund group ---
alldeal_unique_by_group <- deals_base %>%
  left_join(fund_groups, by = c("if_investorfundid" = "fundid")) %>%
  filter(!is.na(fund_group)) %>%
  group_by(fund_group) %>%
  summarise(unique_all_deals = n_distinct(dealid), .groups = "drop")

total_unique_all_deals <- deals_base %>%
  semi_join(fund_groups, by = c("if_investorfundid" = "fundid")) %>%
  summarise(n = n_distinct(dealid)) %>% pull(n)

# --- 2. Join to funds, deflate fund size, classify groups ---
lbo_funds <- left_join(funds, deal_fund, by = c("fundid" = "if_investorfundid")) %>%
  filter(fd_hd_fundcountry == "UNITED STATES") %>%
  mutate(adj_year = coalesce(fd_vintage,
                             as.integer(format(as.Date(fd_closedate), "%Y")))) %>%
  left_join(cpi_jan, by = c("adj_year" = "year")) %>%
  mutate(cpi_factor = cpi_2023 / cpi,
         fundsize_2023 = fd_hd_fundsize * cpi_factor) %>%
  left_join(fund_groups, by = "fundid") %>%
  filter(!is.na(fund_group)) %>%
  mutate(lbo_present = ifelse(is.na(deal_count_lbo) | deal_count_lbo == 0, 0, 1))

# --- 3. Category-level rollup ---
lbo_summary <- lbo_funds %>%
  group_by(fund_group) %>%
  summarise(
    n_funds          = n(),
    pct_funds_lbo    = 100 * mean(lbo_present),
    funds_raised_2023 = sum(fundsize_2023, na.rm = TRUE),
    capital_total    = sum(fundsize_2023, na.rm = TRUE),
    capital_lbo      = sum(fundsize_2023[lbo_present == 1], na.rm = TRUE),
    pct_capital_lbo  = 100 * capital_lbo / capital_total,
    deal_count_lbo   = sum(deal_count_lbo,   na.rm = TRUE),
    deal_count_total = sum(deal_count_total, na.rm = TRUE),
    deal_vol_lbo     = sum(deal_vol_lbo,   na.rm = TRUE),
    deal_vol_total   = sum(deal_vol_total, na.rm = TRUE),
    pct_deals_lbo    = 100 * sum(deal_count_lbo, na.rm = TRUE) / sum(deal_count_total, na.rm = TRUE),
    pct_volume_lbo   = 100 * sum(deal_vol_lbo,   na.rm = TRUE) / sum(deal_vol_total,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(lbo_unique_by_group,     by = "fund_group") %>%
  left_join(alldeal_unique_by_group, by = "fund_group") %>%
  mutate(
    pct_of_all_lbo_count    = 100 * unique_lbo_deals / total_unique_lbo_deals,
    pct_of_all_lbo_volume   = 100 * deal_vol_lbo / sum(deal_vol_lbo),
    pct_of_alldeal_count    = 100 * unique_all_deals / total_unique_all_deals,
    pct_of_alldeal_volume   = 100 * deal_vol_total / sum(deal_vol_total)
  ) %>%
  arrange(desc(deal_vol_lbo))

# --- 4. Overall column ---
overall <- lbo_summary %>%
  summarise(
    fund_group       = "All Funds",
    n_funds          = sum(n_funds),
    pct_funds_lbo    = 100 * sum(lbo_funds$lbo_present) / nrow(lbo_funds),
    funds_raised_2023 = sum(funds_raised_2023),
    capital_total    = sum(capital_total),
    capital_lbo      = sum(capital_lbo),
    pct_capital_lbo  = 100 * sum(capital_lbo) / sum(capital_total),
    deal_count_lbo   = sum(deal_count_lbo),
    deal_count_total = sum(deal_count_total),
    deal_vol_lbo     = sum(deal_vol_lbo),
    deal_vol_total   = sum(deal_vol_total),
    pct_deals_lbo    = 100 * sum(deal_count_lbo) / sum(deal_count_total),
    pct_volume_lbo   = 100 * sum(deal_vol_lbo) / sum(deal_vol_total),
    unique_lbo_deals = total_unique_lbo_deals,
    unique_all_deals = total_unique_all_deals,
    pct_of_all_lbo_count    = 100,
    pct_of_all_lbo_volume   = 100,
    pct_of_alldeal_count    = 100,
    pct_of_alldeal_volume   = 100
  )

lbo_all <- bind_rows(lbo_summary, overall)

# --- 5. Build labeled, formatted ROWS, then pivot wide ---
fmt_int <- function(x) format(round(x), big.mark = ",", trim = TRUE)
fmt_bil <- function(x) paste0("$", format(round(x / 1000), big.mark = ",", trim = TRUE), "B")
fmt_pct <- function(x) paste0(round(x), "%")

stat_rows <- lbo_all %>%
  transmute(
    fund_group,
    # (1) Fund-based summaries
    `Number of funds`                        = fmt_int(n_funds),
    `Total funds raised ($2023)`             = fmt_bil(funds_raised_2023),
    `% of funds engaging in LBOs (by fund count)`   = fmt_pct(pct_funds_lbo),
    `% of funds engaging in LBOs (by funds raised)` = fmt_pct(pct_capital_lbo),
    # (2) Deal-based summaries
    `Total number of deals`                  = fmt_int(deal_count_total),
    `Number of LBO deals`                    = fmt_int(deal_count_lbo),
    `LBO as % of total deal count`           = fmt_pct(pct_deals_lbo),
    `Total known deal volume ($2023)`        = fmt_bil(deal_vol_total),
    `Known LBO deal volume ($2023)`          = fmt_bil(deal_vol_lbo),
    `LBO as % of known deal volume`          = fmt_pct(pct_volume_lbo),
    # (3) Distribution across fund types
    `% of total deal count (all deals)*`         = fmt_pct(pct_of_alldeal_count),
    `% of total known deal volume (all deals)`   = fmt_pct(pct_of_alldeal_volume),
    `% of total LBO deal count*`                 = fmt_pct(pct_of_all_lbo_count),
    `% of known LBO deal volume`                 = fmt_pct(pct_of_all_lbo_volume)
  ) %>%
  pivot_longer(-fund_group, names_to = "Statistic", values_to = "value") %>%
  pivot_wider(names_from = fund_group, values_from = value)

row_levels <- c(
  "Number of funds", "Total funds raised ($2023)",
  "% of funds engaging in LBOs (by fund count)",
  "% of funds engaging in LBOs (by funds raised)",
  "Total number of deals", "Number of LBO deals", "LBO as % of total deal count",
  "Total known deal volume ($2023)", "Known LBO deal volume ($2023)",
  "LBO as % of known deal volume",
  "% of total deal count (all deals)*", "% of total known deal volume (all deals)",
  "% of total LBO deal count*", "% of known LBO deal volume"
)

stat_rows <- stat_rows %>%
  mutate(Statistic = factor(Statistic, levels = row_levels)) %>%
  arrange(Statistic) %>%
  mutate(Statistic = as.character(Statistic)) %>%
  select(Statistic, `Private Equity`, `Real Assets`, `Private Debt`,
         `Venture Capital`, `Other`, `All Funds`)

# --- 6. Presentation table with three grouped sections ---
fund_table <- stat_rows %>%
  gt(rowname_col = "Statistic") %>%
  tab_header(
    title    = "U.S. Private Fund Participation in U.S. LBO Deals (1985–2019)",
    subtitle = "Counts, coverage rates, and capital by fund type, in 2023 dollars"
  ) %>%
  tab_row_group(
    label = "Distribution across fund types",
    rows  = c("% of total deal count (all deals)*", "% of total known deal volume (all deals)",
              "% of total LBO deal count*", "% of known LBO deal volume")
  ) %>%
  tab_row_group(
    label = "Deal-based summaries",
    rows  = c("Total number of deals", "Number of LBO deals", "LBO as % of total deal count",
              "Total known deal volume ($2023)", "Known LBO deal volume ($2023)",
              "LBO as % of known deal volume")
  ) %>%
  tab_row_group(
    label = "Fund-based summaries",
    rows  = c("Number of funds", "Total funds raised ($2023)",
              "% of funds engaging in LBOs (by fund count)",
              "% of funds engaging in LBOs (by funds raised)")
  ) %>%
  cols_align(align = "right", columns = -1) %>%
  tab_source_note(
    source_note = "*Deal counts are deduplicated by dealid; deals with funds from multiple fund types are counted once per type, so these shares do not sum to 100%."
  ) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups()) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(columns = `All Funds`)) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_title(groups = "title"))

# --- 7. Save as image ---
gtsave(fund_table, "tables/ta1b_fund_participation.docx")
gtsave(fund_table, "tables/ta1b_fund_participation.png", vwidth = 1600, expand = 10)
```

    ## file:////var/folders/gp/hc9k1h4j3bv9z3zk3wrnz3mw0000gn/T//RtmpcLFNx2/filea4754545297d.html screenshot completed

``` r
knitr::include_graphics("../tables/ta1b_fund_participation.png", error = FALSE)
```

![](../tables/ta1b_fund_participation.png)<!-- -->

``` r
#Note: LBO deal counts are deduplicated by dealid: a buyout with multiple participating funds is counted once per fund type. Because a single deal can involve funds from more than one type, '% of total LBO deal count does not sum to 100%. Volume is split across participating funds, so % of known LBO deal volume' does sum to 100%.
```

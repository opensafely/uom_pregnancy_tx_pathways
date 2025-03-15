tbl_delivery_selected_snomed_code_usage <- delivery_selected_snomed_code_usage |>
  filter(!snomed_concept_id %in% delivery_unused_codes) |>
  left_join(snomed_code_dict) |>
  select(start_date, end_date, snomed_concept_id, description, usage) |>
  group_by(snomed_concept_id) |>
  mutate(
    sum_usage_total = sum(usage, na.rm = TRUE),
    min_start_date = min(start_date, na.rm = TRUE),
    max_end_date = max(end_date, na.rm = TRUE)
  ) |>
  filter(start_date == "2022-08-01") |>
  select(start_date, snomed_concept_id, description, sum_usage_22to23 = usage, sum_usage_total, min_start_date, max_end_date) |>
  ungroup() |>
  mutate(
    pct_usage_total = sum_usage_total / sum(sum_usage_total),
    pct_usage_22to23 = sum_usage_22to23 / sum(sum_usage_22to23),
    active_date_range = paste0(year(min_start_date), " to ", year(max_end_date))
  ) |>
  select(snomed_concept_id, description, active_date_range, sum_usage_22to23, pct_usage_22to23, sum_usage_total, pct_usage_total)




## Unused codes

```{r}
#| label: tbl-unused-codes-codelist-1
#| tbl-cap: Unused codes from 'pregnancy_delivery_snomed_reviewed_2'

delivery_codelist |>
  filter(code %in% delivery_unused_codes) |>
  select(-codelist) |>
  gt() |>
  cols_label(
    code = "SNOMED Code",
    term = "Description"
  ) |>
  cols_width(
    code ~ px(160),
    term ~ px(550)
  ) |>
  opt_interactive(
    use_filters = TRUE,
    use_highlight = TRUE,
    use_compact_mode = TRUE,
    use_resizers= TRUE,
    use_page_size_select = TRUE
    )

```

## Codes with changing descriptions

```{r}
#| include: false

snomed_codes_diff_desc <- delivery_selected_snomed_code_usage |>
  filter(!snomed_concept_id %in% delivery_unused_codes) |>
  left_join(snomed_code_dict) |> 
  select(snomed_concept_id, description) |> 
  distinct() |> 
  group_by(snomed_concept_id) |> 
  count() |> 
  filter(n > 1) |> 
  pull(snomed_concept_id)
```

There is a total of n = `r length(snomed_codes_diff_desc)` SNOMED codes in the selected codelist with codes that have changing descriptions.

```{r}
#| label: tbl-diff-codes-desc-1
#| tbl-cap: Codes from 'pregnancy_delivery_snomed_reviewed_2' with changing descriptions between 1st Aug 2011 and 31 Jul 2023


delivery_selected_snomed_code_usage |>
  left_join(snomed_code_dict) |> 
  filter(snomed_concept_id %in% snomed_codes_diff_desc) |> 
  select(snomed_concept_id, description) |> 
  distinct() |> 
  arrange(snomed_concept_id) |> 
  gt() |>
  cols_label(
    snomed_concept_id = "SNOMED Code",
    description = "Description"
  ) |>
  cols_width(
    snomed_concept_id ~ px(160),
    description ~ px(550)
  ) |>
  opt_interactive(
    use_filters = TRUE,
    use_highlight = TRUE,
    use_compact_mode = TRUE,
    use_resizers= TRUE,
    use_page_size_select = TRUE
    )
```

## Exploration of active and inactive codes

```{r}
#| label: tbl-codes-active-inactive-1
#| tbl-cap: Number of codes from 'pregnancy_delivery_snomed_reviewed_2' with changing status (active / inactive) between 1st Aug 2011 and 31 Jul 2023

delivery_selected_snomed_code_usage |>
  filter(!snomed_concept_id %in% delivery_unused_codes) |>
  # left_join(snomed_code_dict) |> 
  select(snomed_concept_id, active_at_start, active_at_end) |> 
  group_by(active_at_start, active_at_end) |> 
  count() |> 
  ungroup() |> 
  mutate(active_at_start = factor(active_at_start, labels = c("No", "Yes")),
         active_at_end = factor(active_at_end, labels = c("No", "Yes"))) |> 
  gt() |>
  cols_label(
    active_at_start = "Active at start",
    active_at_end = "Active at end",
    n = "Count"
  ) |>
  cols_width(
    active_at_start ~ px(250),
    active_at_end ~ px(250),
    n ~ px(100)
  ) |>
  opt_interactive(
    use_filters = FALSE,
    use_highlight = TRUE,
    use_compact_mode = TRUE,
    use_resizers= FALSE,
    use_page_size_select = FALSE
    )
```

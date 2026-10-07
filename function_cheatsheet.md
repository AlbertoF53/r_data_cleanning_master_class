# Data Cleaning & Regex Recall Sheet — organized by "what do I want to do?"

Look up by task, not by function name — find the row that matches what you're
trying to accomplish, then use the function/pattern listed. Sources: the
lesson files in this folder (`slice_function.R`, `automate_with_fs_copy.qmd`,
`{fs}_file_sytem_organizer.qmd`), `scripts/part_1_mutate_summarise.qmd` and `scripts/part_2_data_formats_csv_excel.qmd`, `scripts/part_4_times_dates.qmd` and `scripts/part_5_functional_programing.qmd` (Parts 1, 2, 4, 5 added 2026-09-20), `Documents/R-Books/regular_expressions_for_R.qmd`,
and `Documents/R-Books/working_with_strings_in_R.qmd`.

---

## Inspect and count values

| I want to... | Use | What it delivers |
|---|---|---|
| Standardize messy column names (spaces, caps, dots → snake_case) | `janitor::clean_names()` | Clean, consistent column names in one call. |
| Count how many rows fall into each category | `count(var, sort = TRUE)` | A tidy frequency table, most common first. |
| Count combinations of two variables | `count(var1, var2, sort = TRUE)` | Cross-tab of counts, still tidy (one row per combo). |

---

## Select specific rows

| I want to... | Use | What it delivers |
|---|---|---|
| Grab a specific row (or drop it) | `slice(n)` / `slice(-n)` | Row `n` only, or everything except row `n`. |
| Grab a range of rows | `slice(a:b)` | Rows `a` through `b`. |
| Grab the first/last N rows | `slice_head(n = )` / `slice_tail(n = )` | Top or bottom N rows as they appear in the data. |
| Grab the N rows with smallest/largest value of a column | `slice_min(var, n = )` / `slice_max(var, n = )` | Rows ranked by `var`; ties are kept by default. |
| Break ties so you get exactly N rows | `slice_min(var, n = , with_ties = FALSE)` | Drops extra tied rows to hit the exact count. |
| Take a random sample of rows | `slice_sample(n = )` | N random rows, no replacement by default. |
| Random sample with replacement | `slice_sample(n = , replace = TRUE)` | Same row can be picked more than once. |
| Weighted random sample | `slice_sample(n = , weight_by = var, replace = TRUE)` | Rows with higher `var` are more likely to be picked. |
| Do any of the above *per group* (e.g. top row per country) | `group_by(...) \|> slice_max(var, n = 1)` (or `slice_head`, `slice_min`, etc.) | Repeats the slice independently within each group. |
| Grab the first N% of rows per group | `group_by(...) \|> slice_head(prop = 0.1)` | Proportional slice instead of a fixed count. |

---

## Sort rows (arrange)

| I want to... | Use | What it delivers |
|---|---|---|
| Sort ascending (default) | `arrange(var)` | Smallest/first to largest/last. |
| Sort a numeric column descending | `arrange(-var)` | Negates the number to reverse order — numeric columns only. |
| Sort any column descending (numeric, character, factor, date) | `arrange(desc(var))` | Explicit descending sort; works on types `-var` can't handle, e.g. `arrange(desc(motive))` for reverse-alphabetical, or a date column. |
| Sort by multiple columns, mixing directions | `arrange(col1, desc(col2))` | Each column can independently be ascending (bare) or descending (`desc()`); `-` can't mix into the same call as cleanly for non-numeric columns. |

**When to use `desc()` over `-`:** `-var` only works because negating a number reverses its order — it fails (or does something nonsensical) on character/factor/date columns. `desc()` works on any sortable type, so default to it unless you're quickly reordering a single numeric column. Example: `Documents/R-Books/working_with_categorical_data_in_R.qmd` line 120 uses `arrange(-n)`, which works there because `n` (a count) is numeric.

---

## Explore a dataset quickly (Part 1, lesson 1)

| I want to... | Use | What it delivers |
|---|---|---|
| Get the number of rows and columns | `dim(df)` | Rows, then columns. |
| Get a full profile of every column (type, missing, mean, histogram) | `skimr::skim(df)` (one column: `skim(df, col)`) | A summary table by column type. |
| Get a compact visual summary of a few numeric columns | `df \|> select(a, b) \|> gtExtras::gt_plt_summary()` | A small table with a mini distribution plot per column. |
| Clean column names before exploring | `janitor::clean_names()` | e.g. `Lot_Area` becomes `lot_area`, so names match what you type later. |

---

## Look inside a list or unfamiliar object: `str()`

`str()` = **structure**. Like `glimpse()`, but works on *any* R object — including
lists and tidymodels objects (`rsplit`, recipes, fitted models) that aren't data frames.

| I want to... | Use | What it delivers |
|---|---|---|
| See the parts of an object, top layer only | `str(x, max.level = 1)` | One line per part: its name, type, and a preview of values. |
| Open the parts one level deeper | `str(x, max.level = 2)` | Also shows what's inside each part (e.g. the columns of a stored data frame). |
| Just list the part names | `names(x)` | e.g. `"data" "in_id" "out_id" "id"`. Each part is reachable with `x$part`. |
| Point and click instead | Positron **Variables** pane → click the arrow next to the object | Same information, expandable. |

**How to read the output** (example: `split <- initial_split(College)`, then `str(split, max.level = 1)`):

```
List of 4                                           ← a list with 4 parts
 $ data  :'data.frame':	777 obs. of  18 variables   ← the full data set
 $ in_id : int [1:582] 398 679 129 509 471 ...      ← 582 whole numbers (training row numbers)
 $ out_id: logi NA                                  ← empty
 $ id    : tibble [1 × 1]                           ← a small label
 - attr(*, "class")= chr [1:3] "initial_split" "mc_split" "rsplit"
 - attr(*, "prop")= num 0.75                        ← settings used to create it
```

- `$` = a part you can reach with `x$name`; `..$` = a part *inside* a part (appears with `max.level = 2`).
- `- attr` lines = **attributes**: extra information attached to the object, such as its `class` (what kind of object it is) and the settings that created it.
- Type abbreviations: `int` whole numbers, `num` decimals, `chr` text, `logi` TRUE/FALSE/NA, `Factor` categories.

**Rule of thumb:** with an unfamiliar object, start with `max.level = 1`; go deeper only if needed. Source: `Documents/R-Books/tidymodels_all_about.qmd` ("Splitting the data").

---

## Handle missing values (Part 1, lessons 2 and 7)

| I want to... | Use | What it delivers |
|---|---|---|
| Drop rows where one column is missing | `filter(!is.na(sex))` | Keeps rows with a value. Base R: `df[!is.na(df$sex), ]`. |
| Same, written the other way round: say what to *drop* | `filter_out(is.na(sex))` | Drops the rows that match. Newer dplyr function (the course uses it; installed dplyr is 1.2.1). |
| Drop every row that has any missing value | `na.omit()` | Aggressive: one `NA` anywhere removes the whole row. |
| Ignore `NA` inside one calculation | `mean(x, na.rm = TRUE)` | Otherwise a single `NA` makes the result `NA`. |
| Fill in group combinations that are absent | `complete(species, island, fill = list(mean_x = 0))` | Adds the missing combinations as rows; `fill = list(col = value)` sets what goes in each column. |

**Gotchas from the lessons:**
- **`na.rm = TRUE` placed inside `mutate()` is not an argument to the math.** `mutate(ratio = a / b, na.rm = TRUE)` silently creates an extra column named `na.rm`. Put it inside `mean()`/`sum()`.
- **A misspelled argument to `mean()` is silently ignored.** `mean(x, na.omit = TRUE)` runs without error and does not remove the `NA`s; the argument is `na.rm`.

---

## Apply your own function to every row: `map()` (Part 1, lesson 2)

| I want to... | Use | What it delivers |
|---|---|---|
| Understand why my function fails inside `mutate()` | `if (x > 39)` expects **one** value, but `mutate()` passes the whole column | Errors on a vector (R 4.2+ requires a length-1 condition). |
| Fix it by calling the function once per value | `mutate(flag = map_lgl(bill_length_mm, large_quantity))` | `map_lgl()` returns a TRUE/FALSE column. `map_dbl()` returns numbers; plain `map()` returns a list column. |
| Or make it work on the whole vector | Use vectorized code: `if_else(x > 39, TRUE, FALSE)` or just `x > 39` | Vectorized functions need no `map()` and are faster. |

---

## Summarize by group (Part 1, lessons 3 to 7)

| I want to... | Use | What it delivers |
|---|---|---|
| Summarize the whole table into one row | `summarise(mean_x = mean(x, na.rm = TRUE), sd_x = sd(x, na.rm = TRUE))` | One row; each argument creates a column. |
| Summarize **per group**, one row per group | `summarise(..., .by = species)` | Groups inside the call; the result comes back **ungrouped**. Several groups: `.by = c(species, island)`. |
| The older way to do the same | `group_by(species) \|> summarise(...)` | Result may stay grouped (see next row). |
| Stop `summarise()` leaving a leftover grouping | `summarise(..., .groups = "drop")` or `ungroup()` | Otherwise a second `summarise()` works on the leftover groups and can give surprising numbers. Using `.by =` avoids the problem. |
| Add a **per-group calculation to every row** without collapsing | `mutate(mean_x = mean(x, na.rm = TRUE), .by = species)` | Keeps all rows; each row gets its group's value. |
| Collapse those repeated values to one per group | `... \|> select(species, mean_x) \|> unique()` (or `distinct()`) | One row per group. `summarise(.by = )` does this directly. |
| Center a variable within each group | `mutate(x = scale(x, center = TRUE), .by = species)` | Subtracts each group's own mean. Without `.by =` it centers on the overall mean. |
| Keep all values of a group as a vector | `summarise(values = list(x), .by = species)` | A **list column** (see the list-column section below). |

---

## Repeat a calculation over many columns: `across()` (Part 1, lessons 8 to 11)

| I want to... | Use | What it delivers |
|---|---|---|
| Apply one function to several named columns | `summarise(across(.cols = c(a, b, c), .fns = mean))` | One summary per column, instead of writing each line by hand. |
| Choose columns by position | `across(.cols = 3:6, ...)` | Columns 3 to 6. |
| Choose columns by type | `across(.cols = where(is.numeric), ...)` | All numeric columns. Exclude one: `c(where(is.numeric), -year)`. |
| Choose columns by name pattern | `across(.cols = contains("bill"), ...)` or `starts_with("flipper")` | See the tidyselect section below. |
| Pass a function that needs arguments | `.fns = function(x) mean(x, na.rm = TRUE)` or `.fns = \(x) mean(x, na.rm = TRUE)` | An anonymous (unnamed) function; `\(x)` is the short form. |
| Name the new columns | `.names = "mean_{.col}"` | `{.col}` is the original column name. |
| Strip a piece from the new names | `.names = 'mean_{.col \|> str_remove("_mm")}'` | Code inside the braces `{}` is run for each name. |
| Put the function's label in the name | `.fns = list(avg = mean)`, `.names = "{.fn}_{.col}"` | `list(avg = mean)` gives the function a label, so names become `avg_bill_length_mm`. |
| Compute several summaries at once | `.fns = list(avg = mean, sd = sd)` | One column per column-and-function pair. |
| Add a one-off calculation next to `across()` | `summarise(across(...), sd_bill = sd(bill_length_mm))` | Extra named arguments can sit after `across()`. |
| Group the whole `across()` result | `summarise(across(...), .by = c(species, island))` | The `.by =` goes after `across()`. |
| Turn text with numbers into numbers, for many columns | `mutate(across(everything(), parse_number))` | Reads "I have 10 apples" as `10`. Add `.names = "number_{.col}"` to keep the original columns. |

---

## Return more than one row per group: `reframe()` (Part 1, lesson 12)

| I want to... | Use | What it delivers |
|---|---|---|
| Get min and max together | `reframe(range_x = range(x))` | Two rows. `summarise()` is meant for one row per group and warns/errors here. |
| Label those rows | `reframe(type = c("min", "max"), range_x = range(x))` | A `type` column telling which row is which. |
| Do it per group | `reframe(type = c("min", "max"), range_x = range(x), .by = species)` | Two rows per group. |
| Do it for many columns | `reframe(type = c("min", "max"), across(c(a, b), range), .by = species)` | `across()` works inside `reframe()`. |
| Reshape into a report table | `... \|> pivot_longer(cols = -c(1:2), names_to = "attributes", values_to = "value") \|> pivot_wider(names_from = c(species, type), values_from = value) \|> gt()` | Long to wide; `gt()` prints a formatted table. |

---

## Select columns with tidyselect helpers (Part 1, lessons 13 to 16)

| I want to... | Use | What it delivers |
|---|---|---|
| Select by type | `select(where(is.numeric))`, `where(is.factor)`, `where(is.character)` | All columns of that type. Combine: `select(where(is.factor), where(is.character))`. |
| Select by a condition on the column's values | `select(where(function(x) is.numeric(x) && mean(x) > 200))` | Use `&&` / `\|\|` inside the test; test `is.numeric()` first, so `mean()` isn't run on text. |
| Select columns with many missing values | `select(where(function(x) mean(is.na(x)) > 0.01))` | Columns with more than 1% `NA`. |
| Give that test a name for reuse | `more_than_one_percent_missing <- function(x) mean(is.na(x)) > 0.01` then `select(where(more_than_one_percent_missing))` | Note there are no parentheses after the name inside `where()`. |
| Select a run of columns | `select(bill_length_mm:sex)` | From one column to another, inclusive. |
| Select by name pattern | `ends_with("_mm")`, `starts_with("bill")`, `contains("length")` | Plain text matches. |
| Select by regex | `select(matches("^[fb].*(_mm)$"))` | Names starting with f or b and ending in `_mm`. |
| Select from a saved vector of names | `select(all_of(my_cols))` | **Errors** if a name is missing. |
| Same, but tolerate missing names | `select(any_of(my_cols))` | Silently skips names that don't exist. |
| Detect count-like columns | `select(where(function(x) all(x %in% 0:10)))` | Columns whose values are all whole numbers 0 to 10. Negate with `!all(...)` to get the continuous ones. |
| Check a derived column by hand | `filter(gr_liv_area != first_flr_sf + second_flr_sf)` | Zero rows means the two agree. |

---

## Group and reshape categories for plots (Part 1, lesson 17)

| I want to... | Use | What it delivers |
|---|---|---|
| Keep the top N factor levels and lump the rest | `mutate(neighborhood = fct_lump_n(neighborhood, n = 5))` | The 5 most frequent levels plus an "Other" level. |
| See every level of a long count table | `count(neighborhood) \|> print(n = Inf)` | Prints all rows instead of the first 10. |
| Cap a count variable at "2+" | `mutate(across(-sale_price, \(x) if_else(x < 2, as.character(x), "2+") \|> factor(levels = c(0:1, "2+"))))` | Values 0, 1, "2+"; `factor(levels = )` keeps them in the right order. |
| Standardize then compare distributions per variable | `pivot_longer(cols = -sale_price) \|> mutate(sale_price = scale(sale_price), .by = name)` | Each variable's values centered on its own mean. |
| Reduce skew of a price | `scale(log(x))` | Log first, then center. Skewness check: `e1071::skewness`. |
| Show min and max as labels on facets | Build a `reframe(type = c("min", "max"), ..., .by = group)` table and pass it as `geom_text(data = ranges, ...)` | A second data source for the labels layer. |

---

## Prep columns before cleaning

| I want to... | Use | What it delivers |
|---|---|---|
| Add a row-number ID column in a specific spot | `mutate(row_id = row_number(), .before = some_col)` | An ID column placed exactly where you want it, not just tacked on at the end. |
| Turn row names into a real column | `rownames_to_column(var = "name")` | Row names (e.g. `mtcars`'s car names) become a normal data column you can manipulate. |

---

## Recode factor levels

| I want to... | Use | What it delivers |
|---|---|---|
| Rename the levels of a factor (e.g. `0`/`1` → `"Automatic"`/`"Manual"`) | `fct_recode(var, "New label" = "old level")` | Relabels factor levels in place. |

**Gotcha — the old-value side must always be a quoted string, even if the factor started out numeric.** Once a column is converted with `as.factor()` (e.g. `mutate(am = as.factor(am))` on numeric 0/1), its levels are stored as character strings — `"0"`, `"1"` — never as numbers, no matter what the original type was. `fct_recode()` matches against those string labels, so passing bare `0`/`1` (unquoted) won't match anything and the recode silently fails. Trace back to the specific `as.factor()` line that created the factor to see why quoting is required. Example in `Documents/R-Books/working_with_categorical_data_in_R.qmd` (lines 76-94).

---

## Clean and standardize text values

| I want to... | Use | What it delivers |
|---|---|---|
| Lower-case a string/vector | `str_to_lower()` | Consistent casing, e.g. before comparing or renaming files. |
| Change case another way | base R `tolower()` / `toupper()`, or `str_to_sentence()` / `str_to_title()` | Sentence case or Title Case, when lower/upper isn't what you want. |
| Replace only the first match per element | `str_replace(string, pattern, replacement)` | Case-sensitive; stops after the first hit in each string. |
| Replace every occurrence of a pattern | `str_replace_all(string, pattern, replacement)` | E.g. standardizing delimiters — spaces and underscores → dashes. |
| Build a string by inserting variable values | `str_glue("{folder}/{subdir}")` | Glue-style interpolation instead of `paste0()`. |

**`str_replace()`/`str_replace_all()` are useful for recoding missing-value codes stored as text** — e.g., replacing a text-coded `99` with `-9999` when preparing a data file for Mplus. When the code must match the entire string, anchor it (e.g., `^99$`) so values merely containing `99` are not changed.

---

## Split strings into pieces

| I want to... | Use | What it delivers |
|---|---|---|
| Break every string in a vector at a character (e.g. a space) | `str_split(vector, " ")` | A **list**, one element per input string, each holding its pieces. |
| Break a single string and skip the list wrapper | `str_split_1(string, " ")` | A plain character vector — use this instead of `str_split() \|> unlist()` for one string. |
| Pull out just the *k*th word of a string | `word(string, start = k)` | Works on one string at a time — wrap in `map_chr()` (below) to apply across a vector or a tibble column. |

## Combine strings

| I want to... | Use | What it delivers |
|---|---|---|
| Glue pieces together, optionally with a separator | `paste("a", "b")` / `paste0("a", "b")` / `str_c("a", "b", sep = ".")` | `paste()` defaults to a space separator, `paste0()` to none, `str_c()` (stringr) works like `paste0()` but takes `sep =` explicitly. |
| Apply a single-string function (like `word()`) across a vector | `map_chr(vector, word, start = k)` | `{purrr}`'s `map_chr()` vectorizes it; inside a tibble, target the column explicitly, e.g. `map_chr(df$col, word, start = 2)`. |

---

## Detect, subset, or extract with regex (stringr)

| I want to... | Use | What it delivers |
|---|---|---|
| Keep only the vector elements that match a pattern | `str_subset(vector, pattern)` | A shorter vector, matches only. |
| Test whether each element matches (TRUE/FALSE) | `str_detect(vector, pattern)` | A logical vector — use this inside `filter()`. |
| Pull out the actual matched text | `str_extract(vector, pattern)` | The matched substring, or `NA` where nothing matched — use this inside `mutate()`. |
| Match strings *ending* in a specific letter | `str_extract(vector, "s$")` — **not** `"s."` | `$` anchors to the end of the string. `.` means "any character," so `"s."` matches an s followed by *anything*, not "s at the end." |
| Search for a literal special character (e.g. a period) | `str_detect(vector, "s\\.")` | Escape it with `\\` (a double backslash in an R string) so `.` is treated literally instead of "any character." |
| Match a word whether or not it's capitalized (e.g. start vs. middle of a sentence) | `str_subset(vector, "[Tt]he")` | The character class `[Tt]` matches either letter — handles case variants without a case-insensitive flag. |

---

## Use regex together with dplyr verbs

| I want to... | Use | What it delivers |
|---|---|---|
| Keep only rows whose text matches a pattern | `filter(str_detect(col, pattern))` | Filters directly on the regex test. |
| Create a column holding what matched (or `NA`) | `mutate(match = str_extract(col, pattern))` | Lets you inspect matches before deciding what to do with them. |
| See where the regex succeeded / failed | `filter(!is.na(match))` / `filter(is.na(match))` | Isolates hits vs. misses for debugging a pattern. |

---

## Regex syntax reference (the patterns themselves)

| Symbol | Matches | Notes |
|---|---|---|
| `.` | any single character | |
| `\\d` / `\\D` | a digit / anything but a digit | double backslash in R strings |
| `\\w` / `\\W` | a word character (a-z, A-Z, 0-9, _) / anything but | |
| `\\s` `\\t` `\\n` | whitespace / tab / newline | |
| `^` | start of string | |
| `$` | end of string | |
| `[oe]` | one character from the set (o or e) | |
| `[a-j]` | one character in the range a–j | |
| `[^a-j]` | one character **not** in that range | `^` inside `[]` means negation |
| `{n}` | exactly n repeats of the preceding item | |
| `+` | one or more of the preceding item | |
| `?` | **two roles depending on placement**: (1) right after a single character/group → that item is optional (0 or 1); (2) right after another quantifier (`*` or `+`) → makes *that* quantifier lazy ("as few characters as possible" instead of "as many as possible") | (1) e.g. `"apples?"` matches both "apple" and "apples"; (2) e.g. `.+?` / `.*?` below |
| `*` | zero or more of the preceding item | |
| `.+?` | **one or more** of any character, **as few as possible** | `?` here is the *lazy modifier* role, not "optional" — it makes `.+` stop early; e.g. `str_extract(sentences, ".+?(the\|The)")` grabs up to the *first* "the"/"The", requiring at least 1 char before it |
| `.*?` | **zero or more** of any character, **as few as possible** | same lazy-modifier role as above; e.g. `str_extract(sentences, ".*?(the\|The)")` — also matches when "the"/"The" starts the string (0 chars before it) |
| `\|` | OR / alternation | e.g. `"^re\|er$"` |
| `()` | grouping | e.g. `"(chicken\|hog)s"` |
| `\\.` `\\|` `\\(` etc. | a literal metacharacter | precede with `\\` to match it literally |

**Gotcha — greedy vs. lazy can silently mis-extract data.** `str_extract()`/`str_extract_all()` never error on a "wrong" match — they just return whatever the pattern actually matched. Grab the wrong flavor of `*`/`+` (greedy when you needed lazy, or vice versa) and the function will happily hand back a substring that's too long (greedy overshoots past your intended stop point) or too short/truncated (lazy stops at the first opportunity, even if that's too early). Since there's no error, this is exactly how incomplete or malformed data entries slip into a column unnoticed — always eyeball a few extracted values against the source string before trusting the column.

Full source with runnable examples: `Documents/R-Books/regular_expressions_for_R.qmd`.

---

## Read text files: csv, tsv, txt (Part 2, lessons 1 to 4)

| I want to... | Use | What it delivers |
|---|---|---|
| Read a standard csv | `read_csv("data/file.csv")` | A tibble; column types are guessed. |
| Read a csv **with no header row** | `read_csv(file, col_names = FALSE)` | Columns named `X1`, `X2`, ... instead of treating row 1 as names. |
| Supply my own column names | `read_csv(file, col_names = c("a", "b"), skip = 1)` | `skip = 1` drops the old header so it isn't read as data. |
| Skip junk lines at the top (metadata) | `read_csv(file, skip = 2)` | Starts reading at line 3. |
| Read European csv (`;` separator, `,` decimals) | `read_csv2(file)` | `read_csv()` would put everything in one column. |
| Read a tab-separated file | `read_tsv(file)` | Same as `read_csv()` with a tab delimiter. |
| Read a file with any separator | `read_delim(file, delim = "\|")` | You name the delimiter. |
| Handle quotes inside text | `read_delim(file, delim = ",", escape_backslash = TRUE)` for `\"`; `escape_double = TRUE` for doubled `""` | Stops a quote in the text from breaking the columns. |

---

## Read Excel and JSON (Part 2, lessons 5 and 6)

| I want to... | Use | What it delivers |
|---|---|---|
| Read an Excel sheet | `openxlsx::read.xlsx("file.xlsx") \|> as_tibble()` | A data frame; `as_tibble()` makes it a tibble. |
| Skip title rows above the real header | `read.xlsx(file, startRow = 2)` | Header is read from row 2. |
| Fix dates that arrive as numbers | `mutate(dates = janitor::excel_numeric_to_date(dates))` | Converts Excel's serial day numbers. `lubridate::as_date()` on a raw number counts from 1970, not Excel's origin, so check the result against the spreadsheet. |
| Read a JSON file | `jsonlite::read_json("file.json")` | A nested list. Inspect it with `str()` or `glimpse()`. |
| Turn a list of records into rows | `read_json(file) \|> bind_rows()` | One row per record. |
| Dig into a nested piece | `pluck("information", "origin")` | Follows the path of names inside the list. |
| Spread list columns into rows | `as_tibble() \|> unnest(cols = everything())` | `unnest()` unpacks columns that hold lists. |

---

## Turn odd missing-value labels into real `NA` (Part 2, lesson 7)

| I want to... | Use | What it delivers |
|---|---|---|
| Recode labels while reading a csv | `read_csv(file, na = c("", "(null)", "Unknown"))` | These strings become `NA`, so columns stay numeric or date. |
| Same for Excel | `read.xlsx(file, na.strings = c("", "(null)", "Unknown"))` | Note the different argument name: `na.strings`, not `na`. |
| Recode after reading, per column | `parse_number(x, na = c("", "NA", "(null)", "Unknown"))` | Numbers with the listed strings as `NA`. |
| Why to bother | A column with text labels reads as character; arithmetic on it fails | e.g. `missing_NULL + 2` errors until the labels are recoded. |

---

## Parse numbers, dates and date-times (Part 2, lessons 8 to 10)

| I want to... | Use | What it delivers |
|---|---|---|
| Pull the number out of text | `parse_number("I have 10 apples")` | `10`. Works on many columns with `across(everything(), parse_number)`. |
| Parse a date-time | `lubridate::ymd_hms(x, quiet = TRUE)` | Anything that doesn't match becomes `NA` without a warning. `lubridate` has no `na =` argument. |
| Parse with an exact format and allowed `NA` labels | `parse_datetime(x, format = "%Y-%m-%d %H:%M:%S", na = "Unknown")` | readr's version; also `parse_date()`. |
| Parse a column with **mixed formats** | `parse_date_time(x, orders = c("dmyHM", "dmy", "ymd"), quiet = TRUE)` | Tries each pattern in order; failures become `NA`. Letters: d day, m month, y year, H hour, M minute. |

---

## Stack or glue tables together (Part 2, lesson 11)

| I want to... | Use | What it delivers |
|---|---|---|
| Add rows from another table | `bind_rows(tib1, tib2)` | Matches columns **by name**, not position. A column missing in one table is filled with `NA`. |
| Add columns from another table | `bind_cols(tib1, tib2)` | Matches by **position** only; tables must have the same number of rows. To match by a key, use a join. |

---

## Join tables by a key (Part 2, lessons 12 to 14)

Mental model: the **left** table keeps its rows; the join looks up extra columns from the **right** table using a shared key.

| I want to... | Use | What it delivers |
|---|---|---|
| Keep every row of the left table, add columns from the right | `left_join(x, y, by = join_by(carrier))` | Unmatched rows get `NA`. The most common join. |
| Keep every row of the right table | `right_join(x, y, by = join_by(carrier))` | Mirror image of `left_join()`. |
| Keep rows from both tables | `full_join(x, y, by = join_by(faa == origin))` | All rows from both, `NA` where there is no match. |
| Find rows in `x` that have **no** match in `y` | `anti_join(x, y, by = join_by(...))` | Only the unmatched rows of `x`. |
| Join when the key columns have **different names** | `join_by(origin == faa)` | Left name first, then right name. |
| Join on **several columns** | `join_by(year, month, day, hour, origin)` | All must match. With different names: `join_by(year, month, day, hour, dep_airport == origin)`. |
| Avoid guessing the key | Always write `by = join_by(...)` | Without `by`, dplyr joins on **every** column name the tables share and only prints a message; if none are shared it errors. |
| Rename a key first | `rename(supplier_id = product_supplier_id)` | Makes the names match so `join_by(supplier_id)` works. |
| Keep one row per group for a smaller example | `slice(1, .by = origin)` | First row of each group. |

---

## Reshape: `pivot_longer()` and `pivot_wider()` (Part 2, lessons 15 to 17)

| I want to... | Use | What it delivers |
|---|---|---|
| Make wide columns into rows | `pivot_longer(cols = 2:5)` | Two new columns, `name` and `value`. |
| Choose the columns in different ways | `cols = 2:5`, `cols = contains("score_")`, `cols = -date` | By position, by name pattern, or "everything except". |
| Name the new columns | `names_to = "test", values_to = "score"` | Meaningful names instead of `name` and `value`. |
| Drop a prefix from the old column names | `names_prefix = "score_"` | `score_math` becomes `math`. |
| Keep only part of the old names | `names_pattern = "(.+)_payment_date"` | The part in parentheses (a regex group) is kept. Alternatives: `"(last\|next\|final)_payment_date"`. |
| Summarize after reshaping | `... \|> summarise(mean_score = mean(score), .by = score_category)` | Long format makes grouped summaries easy. |
| Make long data wide again | `pivot_wider(names_from = "payment_date", values_from = "date")` | One column per value of `names_from`. |
| Control the new column names | `names_glue = "{payment_date}_payment_date"` | Text inside `{}` is a column name; other text is copied. |
| Say which columns identify a row | `id_cols = c("utility", "payment")` | Rows with the same ids are combined into one. Leave a real identifier out and rows get merged or duplicated. |

---

## Clean messy Excel pivot tables (Part 2, lessons 18 to 20)

Idea: read every cell with its position, then peel off each label ("behead") into its own column.

| I want to... | Use | What it delivers |
|---|---|---|
| Read every cell with its row, column and formatting | `tidyxl::xlsx_cells("file.xlsx")` | One row per cell (`row`, `col`, `character`, `numeric`, ...). |
| See the sheet laid out as a grid | `unpivotr::rectify()` | Rebuilds the layout so you can check what each cell is. |
| Remove label cells you don't want | `filter(!(character %in% c("Gender", "Age")))` | Or `filter(col > 1)` to drop a label column. |
| Turn a header cell into a column | `unpivotr::behead("up-left", name = "city")` | Directions: `up`, `left`, `up-left`, `left-up`. One `behead()` per header level. |
| Behead only cells that meet a condition | `unpivotr::behead_if(condition, direction = "left", name = "region")` | The condition can come from cell **formatting**, e.g. background color found with `tidyxl::xlsx_formats(...)`. |
| Merge text and number cells into one column | `mutate(cell_values = coalesce(character, as.character(numeric)))` | Takes the first non-missing value. |
| Fill missing region labels | `mutate(region = if_else(is.na(region), "Total", region))` | Names the total rows. |

---

## Put it together on a messy workbook (Part 2, lesson 21)

| I want to... | Use | What it delivers |
|---|---|---|
| Ignore a decorative title row in Excel | `read.xlsx(file, startRow = 2) \|> janitor::clean_names()` | Header from row 2, tidy names. |
| Days between two dates | `interval(start = shipped, end = delivered) / days(x = 1)` | A number of days (lubridate). |
| Fix duplicate column names from a sheet | `as_tibble(.name_repair = function(x) paste0(prefix, x))` | You write the renaming rule; e.g. `case_when(seq_along(x) <= 2 ~ "warehouse_", ...)` builds a prefix by column position. |
| Join two summaries whose keys differ | `left_join(a, b, by = join_by(origin_country == supplier_country))` | Combines summaries from different tables for one plot. |

---

## Get today's date, create dates, format them (Part 4, lessons 1 to 2)

Needs `library(lubridate)` (part of the tidyverse install, but not loaded by `library(tidyverse)`).

| I want to... | Use | What it delivers |
|---|---|---|
| Current date-time / current date | `now()` / `today()` | Base R equivalents: `Sys.time()` / `Sys.Date()`. |
| Current time in another time zone | `now(tzone = "EST")` | Same moment, shown in that zone. |
| Build a date from parts | `make_date(2023, 2, 2)` | A `Date`. |
| Build a date-time from parts | `make_datetime(2025, 2, 7, 13, 6, tz = "EST")` | Year, month, day, hour, minute (and seconds). |
| Write a date the way I want | `format(x, "%d.%m.%Y %H:%M:%S")` | Text. `%d` day, `%m` month number, `%B` month name, `%Y` year, `%H` hour, `%M` **minutes**, `%S` seconds, `%F` = `%Y-%m-%d`. |
| Keep only the date part of a date-time | `date(x)` | Drops the time. |
| Get the year as a decimal | `decimal_date(x)` | e.g. 2025.10. |

**Gotcha:** `%m` is the month and `%M` is minutes. Swapping them gives a valid-looking but wrong result with no error.

---

## Pull parts out of a date (Part 4, lesson 3)

| I want to... | Use | What it delivers |
|---|---|---|
| Year, month, day | `year(x)`, `month(x)`, `day(x)` | Works on a whole vector of dates. |
| Hour, minute, second | `hour(x)`, `minute(x)`, `second(x)` | Time parts. |
| Semester or quarter | `semester(x)`, `quarter(x)` | 1 or 2; 1 to 4. |
| Weekday | `wday(x, label = TRUE, abbr = FALSE, week_start = 1)` | Name of the day; `week_start = 1` makes Monday day 1. |

---

## Add or subtract time: periods vs durations (Part 4, lessons 4 to 7)

| I want to... | Use | What it delivers |
|---|---|---|
| Add "calendar" time (2 days, 1 month) | `x + days(2)`, `x + months(1)`, `x + hours(2)` | A **period**: follows the calendar, so it handles month lengths and daylight saving. |
| Build a custom lapse | `hours(2) + minutes(21) + seconds(6)` | Periods can be added, subtracted and multiplied (`2 * period`). |
| Add an exact number of seconds | `x + ddays(1)`, `dmonths(1)`, `dyears(1)` | A **duration**: fixed seconds. `dmonths(1)` is 2,629,800 s (about 4.35 weeks), not "one calendar month". |
| Add a month without landing on an invalid date | `jan31 %m+% months(1)` or `add_with_rollback(jan31, months(1))` | Rolls back to the last valid day (Feb 29). `jan31 + months(1)` gives `NA` because Feb 31 doesn't exist. Subtract with `%m-%`. |
| Roll a date back to the end of the previous month | `rollback(x)` (options `preserve_hms = FALSE`, `roll_to_first = TRUE`) | Last day of the previous month (or the first day of this month). |
| Convert a period to a number | `period / minutes(1)` | e.g. `(hours(2) + minutes(30)) / minutes(1)` is 150. |
| Compare exact lengths | `dyears(1) / dmonths(1)`, `dmonths(1) / ddays(1)` | Divide a duration by a duration. |

**Gotchas:**
- `minute(period)` **extracts the minutes component** and doesn't convert; use `period / minutes(1)` for the total.
- Use periods for calendar logic ("same day next month") and durations for exact elapsed time.

---

## Time between two points (Part 4, lessons 8 to 10)

| I want to... | Use | What it delivers |
|---|---|---|
| Subtract two date-times | `b - a` | A `difftime`; awkward for later math. |
| Get a clean number | `as.period(b - a) / hours(1)` or `as.duration(b - a) / dhours(1)` | Numbers you can compute with. |
| Create an interval | `interval(start, end, tzone = "EST")` | An object holding both endpoints. |
| Convert an interval to a number of units | `interval_ab / days(1)`, `/ hours(1)`, `/ dseconds(1)` | Length in the chosen unit. |
| Move an interval | `int_shift(int, hours(1))` | Same length, shifted. |
| Start, end, length | `int_start(int)`, `int_end(int)`, `int_length(int)` | Length is in seconds. |
| Fix or reverse the order | `int_flip(int)`, `int_standardize(int)` | Makes the start come before the end. |
| Do two intervals overlap? | `int_overlaps(a, b)` | `TRUE` / `FALSE`. |
| Do they share a start or end? | `int_aligns(a, b)` | `TRUE` / `FALSE`. |
| Gaps between consecutive timestamps | `int_diff(timepoints)` | One interval per consecutive pair; divide by `dminutes(1)` for minutes, e.g. average time between arrivals. |
| Is a date inside an interval? | `x %within% interval(start, end)` | `TRUE` / `FALSE`. Use inside `filter()`. |

---

## Round dates and group by week, month, quarter (Part 4, lesson 11)

| I want to... | Use | What it delivers |
|---|---|---|
| Round to the nearest unit | `round_date(x, unit = "day")` | Also `"week"`, `"month"`, `"quarter"`. |
| Always round down / up | `floor_date(x, unit = "week", week_start = 1)` / `ceiling_date(...)` | Start of the period (Monday-based week with `week_start = 1`), or the next period start. |
| Total by week, month or quarter | `mutate(week = floor_date(date, "week", week_start = 1)) \|> summarise(total = sum(x), .by = week)` | One row per period. |

---

## Parse date text (Part 4, lessons 12 to 14)

| I want to... | Use | What it delivers |
|---|---|---|
| Turn "04/23/2024"-style text into a date | `mdy(x)`; also `dmy()`, `ymd()` | The function name gives the order of month/day/year. |
| Convert several date columns at once | `mutate(across(contains("date"), mdy))` | Each matching column becomes a date. |
| Keep rows from the recent years | `filter(year(date) >= 2023)` | Uses `year()` on the date column. |
| Read a sheet with repeated header names | `as_tibble(.name_repair = "unique")` then `janitor::clean_names()` | Duplicated names get numbers (e.g. `post_url_1`, `post_url_4`), then `rename()` them. |

---

## Rolling averages and gaps in time series (Part 4, lessons 12 to 14)

| I want to... | Use | What it delivers |
|---|---|---|
| Moving total over k rows | `zoo::rollsum(x, k = 7, fill = NA, align = "left")` | Sum of each window of 7. |
| Moving average over k rows | `zoo::rollmean(x, k = 7, fill = NA, align = "left")` | `fill = NA` pads where a full window isn't available; `align` says where the window sits (`"left"`, `"center"`, `"right"`). |
| Get the best row and read its date | `slice_max(order_by = followers) \|> pull(week_start)` | The peak week. |
| Show every day, including days with no data | `complete(date = seq(make_date(2024), make_date(2025), by = "day"))` | Adds missing days as rows with `NA`. |
| Make a rolling window mean what I think | Run `complete()` **and** `arrange(date)` **before** `rollmean()` | Rolling functions count rows, not days. If days are missing, a "7-day" window covers more than 7 days. |
| Turn totals into per-day rates | `mutate(hours_per_day = watch_time_hours / days_since_published)` | A rate per day. The lesson builds `days_since_published` earlier in the same lesson; I did not check how. |

---

## Replace `for` loops with `map()` (Part 5, lessons 1 to 6)

| I want to... | Use | What it delivers |
|---|---|---|
| Apply a function to every item of a vector or list | `map(x, f)` | A **list**, one result per item. |
| Get a plain vector back instead | `map_dbl()` (numbers), `map_chr()` (text), `map_lgl()` (TRUE/FALSE), `map_int()` | Errors if a result isn't length 1 of that type (e.g. `map_chr(1:20, function(x) c(x, 2 * x))` fails). |
| Pass extra arguments to the function | `map(files, read_csv, show_col_types = FALSE)` | Extra arguments go after the function name. |
| Read many files and stack them | `fs::dir_ls("data", regexp = "fake_data_.+\\.csv$") \|> map(read_csv) \|> bind_rows()` | One tibble. `map_dfr(files, read_csv)` does both steps, but it is superseded in newer purrr (the equivalent there is `map(...) \|> list_rbind()`). |
| Force the column types when files disagree | `read_csv(x, col_types = cols(column_a = col_integer(), column_b = col_double()))` | Prevents one file reading a column as text. |
| Write the function inline | `map(x, function(x) ...)`, `map(x, \(x) ...)`, or formula shortcut `map(x, ~ .x * 2)` | `.x` is the current item in the formula form. |
| Pull one element out of each item | `map(list_of_lists, "lifeExp")` or `map_dbl(coefs, 2)` | A name or a position works as a shortcut function. |
| Fit one model per group | `df \|> nest(data = -continent) \|> mutate(mod = map(data, \(x) lm(lifeExp ~ year, data = x)), slope = map_dbl(mod, \(m) coef(m)[2]))` | A list column of models, then a numeric column of slopes. `nest(data = -continent)` packs every other column into a list column named `data`. |

---

## Two or more inputs, and side effects (Part 5, lessons 8, 11 to 13)

| I want to... | Use | What it delivers |
|---|---|---|
| Run code for its side effect (save files, print) | `walk(files, function(x) { ...; ggsave(...) })` | Returns its input invisibly; nothing gets printed as output. |
| Loop over **two** inputs in step | `map2(lin_mod, data, function(model, tib) predict(model, tib))` | Uses the i-th item of each. Formula form: `~ .x - .y$lifeExp`. |
| Loop over **many** inputs (rows of a tibble) | `pmap(tib, function(x, y, ...) ...)` | Columns matched by **position** as `..1`, `..2`, or by name via `list(...)`. |
| Use argument names, not positions | `pmap(tib, ~{ l <- list(...); l$x })` | `l$x`, `l$curvature` read like columns. `pwalk()` is the side-effect version. |
| Combine a list of plot layers into one plot | ``reduce(layers, `+`)`` | Adds all layers to a plot. |

---

## Vectorized vs one-at-a-time (Part 5, lesson 9)

| I want to... | Use | What it delivers |
|---|---|---|
| Write a function that works on a whole vector | Use `if_else()`, `case_when()` instead of `if (...) { } else { }` | `if` handles **one** value at a time; `if_else()`/`case_when()` handle every element. |
| Choose between two outcomes | `if_else(condition, value_if_true, value_if_false)` | Both outcomes must be the same type (e.g. text with `as.character(x)`). |
| Choose among many outcomes | `case_when(x %% 3 == 0 & x %% 5 == 0 ~ "fizzbuzz", x %% 3 == 0 ~ "fizz", .default = as.character(x))` | Conditions checked top to bottom; first match wins; `.default` catches the rest. |

**Gotcha:** `&` compares element by element (use inside vectorized code); `&&` looks at one value (use inside `if`).

---

## Dig into nested data (Part 5, lesson 10)

| I want to... | Use | What it delivers |
|---|---|---|
| Get one item from a list column | `tib \|> pluck("data", 1)` | First tibble in `data`. Same as `tib$data[[1]]`. |
| Go deeper | `pluck("data", 1, "lifeExp", 391)` | Column `lifeExp` of the first tibble, its 391st value. |
| Compute an error measure per group | `mutate(predictions = map2(...), errors = map2(...), mse = map_dbl(errors, \(x) mean(x^2)))` | One MSE number per group. |

---

## Build up a result step by step: `reduce()` and `accumulate()` (Part 5, lesson 14)

| I want to... | Use | What it delivers |
|---|---|---|
| Combine everything into one final result | ``reduce(1:20, `+`)`` | One value (`210`). |
| Keep every intermediate result | ``accumulate(1:20, `+`)`` | A running total, like `cumsum()`. |
| Start from a given object | `accumulate(list, bind_cols, .init = tibble(id = letters[1:5]))` | `.init` is the first value the function receives. |
| Join many files on a key | `files \|> map(read_csv) \|> accumulate(function(x, y) full_join(x, y, by = "id"))` | Joins each new table onto the running result. Use `reduce()` to get just the last one. |
| Bind many small tibbles side by side | `map_dfc(...)` or `reduce(bind_cols)` | Columns added one after another. |

---

## Make code fail safely (Part 5, lessons 15 to 18)

| I want to... | Use | What it delivers |
|---|---|---|
| Return a default instead of an error | `possibly(f, otherwise = 10)` | A new function that never stops the loop. |
| Keep the error details too | `safe_f <- safely(f, otherwise = 10)`; `res <- map(x, safe_f)` | Each result is `list(result, error)`. Extract with `map_dbl(res, "result")`, and flag successes with `map_lgl(res, \(r) is.null(r$error))`. |
| Retry a flaky call | `insistently(f, rate_delay(pause = 0.1, max_times = 10))` | Tries again after a pause, up to `max_times`. |
| Wait longer after each failure | `insistently(f, rate_backoff(pause_base = 0.1, pause_cap = 60, max_times = 10))` | Exponential backoff. |
| Run a fallback when something errors | `tryCatch({ risky_code }, error = function(e) { message(e$message); fallback_value })` | The value of the handler becomes the result. |
| Print clear status messages | `cli::cli_alert_info("...")`, `cli_alert_danger()`, `cli_alert_success()` | Icons and colors. Inside text: `{.strong {x}}`, `{.var name}`, `{.val {value}}`. |

---

## Write your own tidyverse-style function (Part 5, lesson 19)

| I want to... | Use | What it delivers |
|---|---|---|
| Let a user pass **unquoted column names** to my function | `f <- function(df, x, y) { df \|> ggplot(aes(x = {{ x }}, y = {{ y }})) + geom_point() }` | "Curly-curly" `{{ }}` passes the column through. Call: `f(penguins, x = flipper_length_mm, y = bill_length_mm)`. |
| Do arithmetic on such columns | `mutate(ratio = {{ num }} / {{ denom }})` | Same idea inside `mutate()`. |
| Name the new column from an argument | `mutate({{ name }} := {{ num }} / {{ denom }})` | The walrus `:=` lets the left side be computed, not literal. |
| Build a column name from the arguments | `name <- glue::glue("ratio_{ensym(num)}_{ensym(denom)}")` | Turns the unquoted arguments into text for the name. |

---

## Call a web API (Part 5, lesson 20)

| I want to... | Use | What it delivers |
|---|---|---|
| Send a request and read the JSON | `httr2::request(url) \|> req_url_path_append("points", coords) \|> req_perform() \|> resp_body_json()` | A nested list. |
| Walk the list to the piece I need | `pluck("properties", "periods")` | Follows the names down (inspect first with `glimpse()`). |
| Turn a list of records into a tibble | `map_dfr(records, function(r) tibble(time = r \|> pluck("startTime"), ...))` | One row per record; `pluck()` handles nested values like `pluck("probabilityOfPrecipitation", "value")`. |
| Keep only rows whose nested value is numeric | `filter(map_lgl(rain_prob, is.numeric))` then `mutate(rain_prob = map_dbl(rain_prob, 1))` | Some entries are `NULL`, so test each one before extracting. |
| Reuse it | Wrap the steps in a function with a default argument (e.g. coordinates) | One call per location. |

---

## Build reliable file paths (`here` package)

| I want to... | Use | What it delivers |
|---|---|---|
| Load a file with a path that works no matter where R's working directory happens to be | `here::here("subfolder", "file.csv")` | Builds the path from the *project root*, not the current working directory — safer than a relative path like `read_csv("subfolder/file.csv")` or calling `setwd()`. |
| Anchor the project root so `here()` finds it reliably | Add an empty `.here` file at the project root (`file.create(".here")` in R, or `touch .here` in the terminal) | `here()` stops guessing via `.Rproj`/git-root heuristics and always resolves relative to that folder instead. Confirmed in `Documents/R-Books`: just opening the project folder in the editor is enough to activate it — no `setwd()` or `here::i_am()` call needed. |
| Create the `.here` marker from R | `here::set_here("path/to/project")` | Writes the empty `.here` file for you. |
| Fix `here::here()` still showing the old root right after adding `.here` | Restart R, or run `here::i_am("script_name.R")` from a script in the project folder | `here` works out the root **once, when it loads**, and keeps that answer for the session — a `.here` file added mid-session isn't seen until R restarts. (Awesome PDF Automation: `here()` kept showing the git root `.../GitHub` until restart.) |
| Tell `here()` apart from `getwd()` | `getwd()` = where R is running now; `here()` = project root found from a marker file | They can differ (e.g. git root is a parent folder). When a script can't find a file, print both. |

---

## Manage files and folders ({fs} package)

| I want to... | Use | What it delivers |
|---|---|---|
| Make / check / delete a folder | `dir_create(path)` / `dir_exists(path)` / `dir_delete(path)` | Folder-level operations; delete is recursive. |
| Create empty files / delete files | `file_create(path, files)` / `file_delete(path)` | File-level equivalents of the `dir_` functions. |
| List the files in a folder as a vector | `dir_ls(path)` | Named `fs_path` character vector of full paths. |
| Filter that listing to one file type (wildcard) | `dir_ls(path, glob = "*.csv")` | Case-sensitive; combine with `\|` for multiple extensions, e.g. `"*.r\|*.R"`. |
| Filter that listing with a regex, optionally into subfolders | `dir_ls(path, regexp = "fake_data_.+\\.csv", recurse = TRUE)` | More flexible than `glob`; `recurse = TRUE` searches subfolders too. |
| See a folder's structure at a glance | `dir_tree(path)` | Pretty-printed tree — good sanity check before/after cleanup. |
| Get metadata (size, type, etc.) for files in a folder | `dir_info(path)` | Returns a tibble instead of a bare vector. |
| Rename or move file(s) | `file_move(path, new_path)` | Vectorized — pass multiple old/new paths at once. |
| Build a path by joining pieces | `path(...)` | Relative path from parts, e.g. `path(str_glue("{folder}/R_script"))`. |
| Build an absolute path from the working directory | `path_wd(...)` | Same idea as `path()`, anchored to the working directory. |
| Change a file's extension | `path_ext_set(path, ext)` | E.g. turning a `.csv` path into a `.png` output path. |
| Build a path *and* add the extension in one go | `path(dir, name, ext = "svg")` | E.g. `path(out_dir, "UA", ext = "svg")` gives `.../UA.svg`; handy inside a loop over names. |
| Create a nested folder in one call | `dir_create(here::here("a/b/c"))` | Also creates any missing parent folders. Pair with `dir_exists()` to check (`TRUE`/`FALSE`). |
| Avoid "cannot open file" when saving | Run `dir_create()` **before** `write_csv()` / `ggsave()` | Writing functions do **not** create the folder for you; they fail if it's missing. Keep the folder path identical in both places (a `awesome_pdf_automation/...` vs `code_snippets/...` mismatch made `dir_exists()` print `FALSE`). |

---

## Batch-process many files at once

| I want to... | Use | What it delivers |
|---|---|---|
| Read/iterate over every file matched above | `dir_ls(...) \|> map(read_csv)` | `purrr::map()` applies a function across the whole file vector. |
| Reuse a cleanup routine across projects | Wrap the `dir_`/`file_`/`str_` steps into a function, e.g. `clean_file_names(folder)` or `organize_files(folder)` | Turns a one-off script into a repeatable tool — see `automate_with_fs_copy.qmd`. |
| Run code once per row **for its side effect** (save a file, print), several columns at a time | `df \|> select(a, b) \|> pwalk(\(a, b) { ... })` | Columns are matched to the function's arguments **by name** (`a`, `b`), so the names in `\(a, b)` must equal the column names. Returns nothing useful; you use it to write files. `walk()` = one input, `walk2()` = two, `pwalk()` = any number. |
| Same idea but you want the results back (e.g. a plot per row) | `mutate(plot = map(dep_delays, create_histogram))` | `map()` returns a list, one item per row, stored in a new list column. Use `walk`-family when you only care about the side effect, `map`-family when you want the output. |
| Write `function(x) ...` more briefly | `\(x) ...` | R's shorthand for an anonymous function, e.g. `\(carrier, plot) { ... }`. |

---

## Keep a whole vector per group (list columns)

| I want to... | Use | What it delivers |
|---|---|---|
| Summarize per group and keep every raw value | `summarize(n = n(), delays = list(dep_delay), .by = c(carrier, name))` | One row per group; `delays` is a **list column** whose cell holds the whole vector. `.by =` groups inside the call, no `group_by()`/`ungroup()` needed. |
| Pull one group's vector back out | `filter(carrier == "UA") \|> pull(delays) \|> pluck(1)` | `filter()` keeps the row, `pull()` extracts the column (a list), `pluck(1)` takes its first item — same as `[[1]]`. Naming the group is safer than relying on row position. |
| Save the table to CSV | `select(-delays)` first | A CSV can't hold a vector in one cell, so drop the list column before `write_csv()`. |
| Sanity-check a group's vector | `length(x)` vs the `n` column | They should match exactly, as long as the same rows were filtered before `n()` was computed. |

---

## Use computed values inside ggplot2 `aes()`

| I want to... | Use | What it delivers |
|---|---|---|
| Use something the statistical layer just computed | `after_stat()` | Refers to a value the stat calculated (e.g. `after_stat(count)` or `after_stat(density)` in a histogram). |
| Use something the aesthetic scale just produced | `after_scale()` | Refers to the value after the scale mapped it (e.g. the actual fill color), so another aesthetic can be derived from it. |

## Label a mean on a ggplot

| I want to... | Use | What it delivers |
|---|---|---|
| Add one label at a known spot | `annotate("label", x = , y = , label = "Mean = 42")` | A single label; `"text"` gives plain text without a box. |
| Label a mean computed from the data | `geom_label(data = means, aes(label = round(mean_x, 1)))` | `means` comes from `summarise()`; the label layer uses it as its own data source. |
| Label a mean drawn with `stat_summary()` | `stat_summary(fun = mean, geom = "label", aes(label = round(after_stat(y))), vjust = -0.5)` | The stat computes the mean as `y`; `after_stat(y)` lets the label use it, so no separate table is needed. |
| Avoid overlapping labels | `ggrepel::geom_label_repel()` | Pushes labels apart. |

---

*Add a row whenever a function or regex pattern trips you up mid-project — the "I want to..." phrasing is the point: search this file by what you're trying to accomplish, not by a name you're trying to recall.*

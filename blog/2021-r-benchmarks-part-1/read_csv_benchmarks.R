if (!file.exists("read_csv_benchmarks_2021_02_12.parquet")) {
  library(arrowbench)
  # get arrow over time versions
  res_csv <- run_benchmark(
    read_csv,
    lib_path = c("devel", "3.0", "2.0", "1.0", "0.17"),
    reader = "arrow",
    n_iter = 10,
    mem_alloc = c("jemalloc", "mimalloc", "system")
  )

  # get all package (devel + release as of arrow 3.0)
  res_csv <- run_benchmark(
    read_csv,
    lib_path = c("devel", "3.0"),
    mem_alloc = NA, # must have this for the file names to line up
    reader =  c("vroom", "data.table", "readr"),
    n_iter = 2
  )

  # for setting the read benchmarks to something other than the actual max for the
  # system that's reading them. (e.g. I ran them on a system that has 8, but am
  # reading them on a system with 12)
  max_cpus <- 8

  # read them in together
  res_csv <- run_benchmark(
    read_csv,
    cpu_count = c(1, max_cpus),
    lib_path = c("devel", "3.0", "2.0", "1.0", "0.17"),
    n_iter = 5, # this might not actually be needed
    mem_alloc = c("jemalloc", "mimalloc", "system"),
    read_only = TRUE
  )

  ##### Save the benchamrkdata
  bench_df <- as.data.frame(res_csv)
  arrow::write_parquet(bench_df, "read_csv_benchmarks_2021_02_12.parquet")
} else {
  bench_df <- arrow::read_parquet("read_csv_benchmarks_2021_02_12.parquet")
}

##### Plots
library(ggplot2)
library(dplyr)

### Themeing
pkg_colors <- c(
  "arrow" = "#F58A65",
  "data.table" = "#7DF595",
  "readr" = "#4CC0F5",
  "vroom" = "#F5586F"
)
malloc_colors <- c(
  "system" = "#FA5A71",
  "mimalloc" = "#FAB15A",
  "jemalloc" = "#F58A65" # the (current) default
)
one_color <- "#57b894"
two_colors <- c(one_color, "#c0c0c0")

fig_width <- 9
fig_height <- 5

second_labels <- function() { scale_y_continuous(labels = function(x) paste0(x, "s")) }

theme_set(
  theme_minimal(base_family = "Lato") +
    theme(
      legend.position="bottom",
      legend.box = "horizontal",
      legend.text = element_text(margin = margin(r = 30, unit = "pt"))
    )
)

### Release versions
bench_df %>%
  filter(iteration <= 2) %>%
  filter(lib_path %in% c("3.0")) %>%
  filter(output == "data_frame") %>%
  mutate(compression=factor(compression, levels=c("uncompressed", "gzip"))) %>%
  ggplot(.) +
  aes(x = reorder(reader, desc(reader)), y = real, fill = reader, group = iteration) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(cols = vars(source, compression), rows = vars(cpu_count), scales = "free_x") +
  ggtitle(
    "Reading csvs by package (released versions as of 2021-02-04)",
    subtitle = "cols: dataset, compression · rows: cpu count · the lower bar is the first iteration, the upper bar is the second"
  ) +
  xlab("package") +
  ylab("time (real)") +
  guides(fill = FALSE) +
  scale_fill_manual(breaks = names(pkg_colors), values = pkg_colors) +
  second_labels()

ggsave("release-versions.png", width = fig_width, height = fig_height)

### limited
bench_df %>%
  filter(iteration == 1) %>%
  filter(lib_path %in% c("3.0")) %>%
  filter(output == "data_frame") %>%
  filter(source == "nyctaxi_2010-01" & compression == "uncompressed" & cpu_count == max_cpus) %>%
  mutate(compression=factor(compression, levels=c("uncompressed", "gzip"))) %>%
  ggplot(.) +
  aes(x = reorder(reader, desc(reader)), y = real, group = iteration) +
  geom_bar(stat = "identity", position = "dodge", fill = one_color) +
  coord_flip() +
  ggtitle(
    "Reading csvs by package (released versions as of 2021-02-04)",
    subtitle = "nyctaxi_2010-01, uncompressed on 8 cores"
  ) +
  xlab("package") +
  ylab("time (real)") +
  guides(fill = FALSE) +
  second_labels()

ggsave("release-versions-limited.png", width = fig_width, height = fig_height)


### versions over time

### full
bench_df %>%
  filter(iteration == 1) %>%
  filter(is.na(mem_alloc) | mem_alloc == "jemalloc") %>%
  filter(reader == "arrow") %>%
  mutate(compression=factor(compression, levels=c("uncompressed", "gzip"))) %>%
  ggplot(.) +
  aes(x = lib_path, y = real, fill = output, group = output) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_grid(cols = vars(source, compression), rows = vars(cpu_count), scales = "free_x") +
  ggtitle(
    "Reading csvs by Arrow version",
    subtitle = "cols: dataset, compression · rows: cpu count"
  ) +
  xlab("arrow version") +
  ylab("time (real)") +
  # guides(fill = FALSE) +
  scale_fill_manual(values = rev(two_colors)) +
  second_labels()

ggsave("arrow-versions-over-time-full.png", width = fig_width, height = fig_height)

### limited
bench_df %>%
  filter(iteration == 1 ) %>%
  filter(is.na(mem_alloc) | mem_alloc == "jemalloc") %>%
  filter(reader == "arrow") %>%
  filter(source == "nyctaxi_2010-01" & compression == "uncompressed" & cpu_count == max_cpus) %>%
  mutate(
    compression=factor(compression, levels=c("uncompressed", "gzip")),
    output = factor(output, levels = c("arrow_table", "data_frame"))
  ) %>%
  arrange(desc(output)) %>%
  ggplot(.) +
  aes(x = lib_path, y = real, fill = output) +
  geom_col(position = "identity") +
  coord_flip() +
  ggtitle(
    "Reading csvs by Arrow version",
    subtitle = "nyctaxi_2010-01, uncompressed on 8 cores"
  ) +
  xlab("arrow version") +
  ylab("time (real)") +
  guides(fill = guide_legend(
    title = NULL,

  )) +
  scale_fill_manual(values = rev(two_colors), labels = c("csv → arrow", "arrow → r")) +
  second_labels()

ggsave("arrow-versions-over-time-limited.png", width = fig_width, height = fig_height)

bench_df %>%
  filter(iteration == 1 ) %>%
  filter(is.na(mem_alloc) | mem_alloc == "jemalloc") %>%
  filter(reader == "arrow") %>%
  filter(source == "nyctaxi_2010-01" & compression == "uncompressed" & cpu_count == max_cpus) %>%
  mutate(
    compression=factor(compression, levels=c("uncompressed", "gzip")),
    output = factor(output, levels = c("arrow_table", "data_frame"))
  ) %>%
  arrange(desc(output)) %>%
  ggplot(.) +
  aes(x = lib_path, y = real, fill = output, group = output) +
  geom_col(position = "dodge") +
  coord_flip() +
  ggtitle(
    "Reading csvs by Arrow version",
    subtitle = "nyctaxi_2010-01, uncompressed on 8 cores"
  ) +
  xlab("arrow version") +
  ylab("time (real)") +
  # guides(fill = FALSE) +
  scale_fill_manual(values = rev(two_colors)) +
  second_labels()

ggsave("arrow-versions-over-time-limited-gropued.png", width = fig_width, height = fig_height)

### Memory allocators

### full
bench_df %>%
  filter(lib_path %in% c("devel") & reader == "arrow") %>%
  mutate(compression=factor(compression, levels=c("uncompressed", "gzip"))) %>%
  mutate(ex = case_when(
    !is.na(mem_alloc) ~ paste(reader, mem_alloc, sep = " - "),
    TRUE ~ reader
  )) %>%
  ggplot(.) +
  aes(x = reorder(ex, desc(ex)), y = real, fill = mem_alloc, group = iteration) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(cols = vars(source, compression), rows = vars(cpu_count, output), scales = "free_x") +
  ggtitle(
    "Reading csvs with different memory allocators",
    subtitle = "cols: dataset, compression · rows: cpu count · the lowest bar is the first iteration, the upper bar is the tenth"
  ) +
  xlab("package - memory allocator") +
  ylab("time (real)") +
  guides(fill = FALSE) +
  scale_fill_manual(breaks = names(malloc_colors), values = malloc_colors) +
  second_labels()

ggsave("memory-allocators-full.png", width = fig_width, height = fig_height)



### Limited
bench_df %>%
  filter(lib_path %in% c("devel") & reader == "arrow") %>%
  mutate(compression=factor(compression, levels=c("uncompressed", "gzip"))) %>%
  filter(source == "nyctaxi_2010-01" & compression == "uncompressed" & cpu_count == max_cpus) %>%
  mutate(ex = case_when(
    !is.na(mem_alloc) ~ paste(reader, mem_alloc, sep = " - "),
    TRUE ~ reader
  )) %>%
  ggplot(.) +
  aes(x = reorder(ex, desc(ex)), y = real, fill = mem_alloc, group = iteration) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(rows = vars(output), scales = "free_x") +
  ggtitle(
    "Reading csvs with different memory allocators",
    subtitle = "nyctaxi_2010-01, uncompressed on 8 cores \nthe lowest bar is the first iteration, the upper bar is the tenth"
  ) +
  xlab("package - memory allocator") +
  ylab("time (real)") +
  guides(fill = FALSE) +
  scale_fill_manual(breaks = names(malloc_colors), values = malloc_colors) +
  second_labels()

ggsave("memory-allocators-limited.png", width = fig_width, height = fig_height)


### jemalloc + system
bench_df %>%
  filter(lib_path %in% c("devel") & reader == "arrow") %>%
  filter(mem_alloc != "mimalloc") %>%
  mutate(compression=factor(compression, levels=c("uncompressed", "gzip"))) %>%
  filter(source == "nyctaxi_2010-01" & compression == "uncompressed" & cpu_count == max_cpus) %>%
  mutate(ex = case_when(
    !is.na(mem_alloc) ~ paste(reader, mem_alloc, sep = " - "),
    TRUE ~ reader
  )) %>%
  ggplot(.) +
  aes(x = reorder(ex, desc(ex)), y = real, fill = mem_alloc, group = iteration) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(rows = vars(output), scales = "free_x") +
  ggtitle(
    "Reading csvs with different memory allocators",
    subtitle = "nyctaxi_2010-01, uncompressed on 8 cores \nthe lowest bar is the first iteration, the upper bar is the tenth"
  ) +
  xlab("package - memory allocator") +
  ylab("time (real)") +
  guides(fill = FALSE) +
  scale_fill_manual(breaks = names(malloc_colors), values = malloc_colors) +
  second_labels()

ggsave("memory-allocators-jemalloc-system.png", width = fig_width, height = fig_height)

### jemalloc + system data_frame
bench_df %>%
  filter(lib_path %in% c("devel") & reader == "arrow") %>%
  filter(mem_alloc != "mimalloc") %>%
  filter(output == "data_frame") %>%
  mutate(compression=factor(compression, levels=c("uncompressed", "gzip"))) %>%
  filter(source == "nyctaxi_2010-01" & compression == "uncompressed" & cpu_count == max_cpus) %>%
  mutate(ex = case_when(
    !is.na(mem_alloc) ~ paste(reader, mem_alloc, sep = " - "),
    TRUE ~ reader
  )) %>%
  ggplot(.) +
  aes(x = reorder(ex, desc(ex)), y = real, fill = mem_alloc, group = iteration) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ggtitle(
    "Reading csvs with different memory allocators to data.frame",
    subtitle = "nyctaxi_2010-01, uncompressed on 8 cores \nthe lowest bar is the first iteration, the upper bar is the tenth"
  ) +
  xlab("package - memory allocator") +
  ylab("time (real)") +
  guides(fill = FALSE) +
  scale_fill_manual(breaks = names(malloc_colors), values = malloc_colors) +
  second_labels()

ggsave("memory-allocators-jemalloc-system_data_frame.png", width = fig_width, height = fig_height)

### jemalloc + system arrow_table
bench_df %>%
  filter(lib_path %in% c("devel") & reader == "arrow") %>%
  filter(mem_alloc != "mimalloc") %>%
  filter(output == "arrow_table") %>%
  mutate(compression=factor(compression, levels=c("uncompressed", "gzip"))) %>%
  filter(source == "nyctaxi_2010-01" & compression == "uncompressed" & cpu_count == max_cpus) %>%
  mutate(ex = case_when(
    !is.na(mem_alloc) ~ paste(reader, mem_alloc, sep = " - "),
    TRUE ~ reader
  )) %>%
  ggplot(.) +
  aes(x = reorder(ex, desc(ex)), y = real, fill = mem_alloc, group = iteration) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ggtitle(
    "Reading csvs with different memory allocators to arrow table",
    subtitle = "nyctaxi_2010-01, uncompressed on 8 cores \nthe lowest bar is the first iteration, the upper bar is the tenth"
  ) +
  xlab("package - memory allocator") +
  ylab("time (real)") +
  guides(fill = FALSE) +
  scale_fill_manual(breaks = names(malloc_colors), values = malloc_colors) +
  second_labels()

ggsave("memory-allocators-jemalloc-system_arrow_table.png", width = fig_width, height = fig_height)



### Devel versions
bench_df %>%
  filter(iteration <= 2) %>%
  filter(lib_path %in% c("devel", "3.0")) %>%
  filter(output == "data_frame") %>%
  filter(mem_alloc != "system" & mem_alloc != "jemalloc" | is.na(mem_alloc)) %>%
  mutate(compression=factor(compression, levels=c("uncompressed", "gzip"))) %>%
  mutate(ex = case_when(
    reader == "arrow" & lib_path == "devel" ~ paste(reader, lib_path, mem_alloc, sep = " - "),
    lib_path == "devel" ~ paste(reader, lib_path, sep = " - "),
    lib_path == "3.0" ~ paste(reader, "release", sep = " - "),
  )) %>%
  arrange(ex) %>%
  ggplot(.) +
  aes(x = reorder(ex, -order(ex)), y = real, fill = reader, group = iteration) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(cols = vars(source, compression), rows = vars(cpu_count)) +
  ggtitle(
    "Reading csvs by package (development and released versions as of 2021-02-04)",
    subtitle = "cols: dataset, compression · rows: cpu count · the lower bar is the first iteration, the upper bar is the second"
  ) +
  xlab("package") +
  ylab("time (real)") +
  guides(fill = FALSE) +
  scale_fill_manual(breaks = names(pkg_colors), values = pkg_colors) +
  second_labels()

ggsave("devel-versions.png", width = fig_width, height = fig_height)

### without vroom release
bench_df %>%
  filter(iteration <= 2) %>%
  filter(lib_path %in% c("devel", "3.0")) %>%
  filter(output == "data_frame") %>%
  filter(!(reader == "vroom" & lib_path == "3.0")) %>%
  filter(mem_alloc != "system" & mem_alloc != "jemalloc" | is.na(mem_alloc)) %>%
  mutate(compression=factor(compression, levels=c("uncompressed", "gzip"))) %>%
  mutate(ex = case_when(
    reader == "arrow" & lib_path == "devel" ~ paste(reader, lib_path, mem_alloc, sep = " - "),
    lib_path == "devel" ~ paste(reader, lib_path, sep = " - "),
    lib_path == "3.0" ~ paste(reader, "release", sep = " - "),
  )) %>%
  arrange(ex) %>%
  ggplot(.) +
  aes(x = reorder(ex, -order(ex)), y = real, fill = reader, group = iteration) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(cols = vars(source, compression), rows = vars(cpu_count)) +
  ggtitle(
    "Reading csvs by package (development and released versions as of 2021-02-04)",
    subtitle = "cols: dataset, compression · rows: cpu count · the lower bar is the first iteration, the upper bar is the second"
  ) +
  xlab("package") +
  ylab("time (real)") +
  guides(fill = FALSE) +
  scale_fill_manual(breaks = names(pkg_colors), values = pkg_colors) +
  second_labels()

ggsave("devel-versions-sans-vroom.png", width = fig_width, height = fig_height)

## limited
bench_df %>%
  filter(iteration == 1) %>%
  filter(source == "nyctaxi_2010-01" & compression == "uncompressed" & cpu_count == max_cpus) %>%
  filter(lib_path %in% c("devel", "3.0")) %>%
  filter(output == "data_frame") %>%
  filter(reader != "readr") %>%
  filter(mem_alloc != "system" & mem_alloc != "jemalloc" | is.na(mem_alloc)) %>%
  mutate(compression=factor(compression, levels=c("uncompressed", "gzip"))) %>%
  mutate(ex = case_when(
    reader == "arrow" & lib_path == "devel" ~ paste(reader, lib_path, mem_alloc, sep = " - "),
    lib_path == "devel" ~ paste(reader, lib_path, sep = " - "),
    lib_path == "3.0" ~ paste(reader, "release", sep = " - "),
  )) %>%
  arrange(ex) %>%
  ggplot(.) +
  aes(x = reorder(ex, -order(ex)), y = real, fill = reader, group = iteration) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ggtitle(
    "Reading csvs by package (development and released versions as of 2021-02-04)",
    subtitle = "nyctaxi_2010-01, uncompressed on 8 cores"
  ) +
  xlab("package") +
  ylab("time (real)") +
  guides(fill = FALSE) +
  scale_fill_manual(breaks = names(pkg_colors), values = pkg_colors) +
  second_labels()

ggsave("devel-versions-limited.png", width = fig_width, height = fig_height)



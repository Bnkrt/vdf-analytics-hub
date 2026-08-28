library(shiny)
library(bslib)
library(bsicons)
library(htmltools)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(rvest)
library(stringr)
library(purrr)


finance <- readr::read_csv("data/finance/master_financial_data.csv", show_col_types = FALSE)
fleet <- readr::read_csv("data/fleet/fleet_master.csv", show_col_types = FALSE)
fleet_employees <- readr::read_csv("data/fleet/employees.csv", show_col_types = FALSE)

factoring_path <- "data/factoring/factoring_master.csv"

factoring_metric_catalog <- tibble::tribble(
  ~metric_id,                         ~group,           ~display_name,                                ~pattern,
  "total_assets",                     "Genel",          "Toplam Aktif",                               "^(VARLIKLAR TOPLAMI|VARLIK TOPLAMI|TOPLAM AKTIF)$",
  "factoring_receivables",            "Faktoring",      "Faktoring Alacakları",                       "^FAKTORING ALACAKLARI$",
  "discounted_factoring_receivables", "Faktoring",      "İskontolu Faktoring Alacakları",             "^ISKONTOLU FAKTORING ALACAKLARI( NET)?$",
  "other_factoring_receivables",      "Faktoring",      "Diğer Faktoring Alacakları",                 "^DIGER FAKTORING ALACAKLARI( NET)?$",
  "loans_received",                   "Fonlama",        "Alınan Krediler",                            "^ALINAN KREDILER$",
  "equity",                           "Genel",          "Özkaynaklar",                                "^(OZKAYNAKLAR|OZKAYNAK TOPLAMI)$",
  "npl",                              "Risk & Teminat", "Takipteki Alacaklar",                        "^(TAKIPTEKI ALACAKLAR|TAKIPTEKI FAKTORING ALACAKLARI).*$",
  "loss_provisions",                  "Risk & Teminat", "Özel / Beklenen Zarar Karşılıkları",         ".*(OZEL KARSILIK|BEKLENEN ZARAR KARSILIK|ZARAR KARSILIGI).*",
  "risk_assumed",                     "Nazım Hesaplar", "Riski Üstlenilen Faktoring İşlemleri",       ".*RISKI USTLENILEN FAKTORING.*",
  "risk_not_assumed",                 "Nazım Hesaplar", "Riski Üstlenilmeyen Faktoring İşlemleri",    ".*RISKI USTLENILMEYEN FAKTORING.*",
  "operating_income",                 "Kârlılık",       "Esas Faaliyet Gelirleri",                    "^ESAS FAALIYET GELIRLERI$",
  "net_income",                       "Kârlılık",       "Net Dönem Kârı",                             "^(DONEM NET KARI VEYA ZARARI|DONEM NET KARI/ZARARI|DONEM NET KARI|NET DONEM KARI).*$"
)

fac_ascii_upper <- function(x) {
  x <- as.character(x)
  x <- stringr::str_to_upper(x, locale = "tr")
  x <- chartr(
    "ÇĞİÖŞÜÂÊÎÔÛ",
    "CGIOSUAEIOU",
    x
  )
  x <- stringr::str_replace_all(x, "[[:punct:]]+", " ")
  x <- stringr::str_squish(x)
  x
}

fac_clean_colnames <- function(x) {
  x <- as.character(x)
  x <- stringr::str_to_lower(x, locale = "tr")
  x <- chartr(
    "çğıöşüâêîôû",
    "cgiosuaeiou",
    x
  )
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "^_|_$", "")
  make.unique(x, sep = "_")
}

fac_first_existing <- function(nms, candidates) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

fac_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  
  y <- as.character(x)
  y <- stringr::str_squish(y)
  y[y %in% c("", "-", "—", "NA", "N/A")] <- NA_character_
  
  # Turkish / financial-statement number handling:
  # 1.234.567,89 -> 1234567.89
  # 1,234,567.89 -> 1234567.89
  both <- stringr::str_detect(y, "\\.") & stringr::str_detect(y, ",")
  
  turkish_style <- both &
    stringr::str_detect(y, ",[0-9]{1,2}$")
  
  y[turkish_style] <- stringr::str_replace_all(
    y[turkish_style], "\\.", ""
  )
  y[turkish_style] <- stringr::str_replace(
    y[turkish_style], ",", "."
  )
  
  international_style <- both & !turkish_style
  y[international_style] <- stringr::str_replace_all(
    y[international_style], ",", ""
  )
  
  comma_only <- !stringr::str_detect(y, "\\.") &
    stringr::str_detect(y, ",")
  
  y[comma_only] <- stringr::str_replace(
    y[comma_only], ",", "."
  )
  
  dot_thousands <- stringr::str_detect(
    y,
    "^-?[0-9]{1,3}(\\.[0-9]{3})+$"
  )
  y[dot_thousands] <- stringr::str_replace_all(
    y[dot_thousands], "\\.", ""
  )
  
  suppressWarnings(as.numeric(y))
}

fac_enrich_long <- function(raw) {
  
  names(raw) <- fac_clean_colnames(names(raw))
  nms <- names(raw)
  
  # Already dashboard-ready data: just standardise/enrich it.
  metric_col <- fac_first_existing(
    nms,
    c("metric_id", "metricid", "metric", "id")
  )
  
  company_col <- fac_first_existing(
    nms,
    c("company", "firma", "sirket", "company_id")
  )
  
  period_col <- fac_first_existing(
    nms,
    c("period", "donem", "tarih", "date")
  )
  
  value_col <- fac_first_existing(
    nms,
    c("value", "deger", "amount", "tutar", "value_tl")
  )
  
  item_col <- fac_first_existing(
    nms,
    c(
      "item", "kalem", "line_item", "line",
      "hesap", "account", "label", "aciklama",
      "description", "name"
    )
  )
  
  if (!is.na(metric_col) &&
      !is.na(company_col) &&
      !is.na(period_col) &&
      !is.na(value_col)) {
    
    out <- raw |>
      dplyr::transmute(
        company = as.character(.data[[company_col]]),
        period = as.character(.data[[period_col]]),
        metric_id = as.character(.data[[metric_col]]),
        value = fac_number(.data[[value_col]]),
        item = if (!is.na(item_col)) {
          as.character(.data[[item_col]])
        } else {
          as.character(.data[[metric_col]])
        }
      ) |>
      dplyr::left_join(
        factoring_metric_catalog |>
          dplyr::select(
            .data$metric_id,
            .data$group,
            .data$display_name
          ),
        by = "metric_id"
      ) |>
      dplyr::mutate(
        group = dplyr::coalesce(.data$group, "Genel"),
        display_name = dplyr::coalesce(
          .data$display_name,
          .data$item,
          .data$metric_id
        )
      )
    
    return(out)
  }
  
  # Raw long master from the old parser.
  if (!is.na(company_col) &&
      !is.na(period_col) &&
      !is.na(value_col) &&
      !is.na(item_col)) {
    
    base <- raw |>
      dplyr::transmute(
        company = as.character(.data[[company_col]]),
        period = as.character(.data[[period_col]]),
        item = as.character(.data[[item_col]]),
        value = fac_number(.data[[value_col]]),
        item_norm = fac_ascii_upper(.data[[item_col]])
      )
    
    return(
      purrr::pmap_dfr(
        factoring_metric_catalog,
        function(metric_id, group, display_name, pattern) {
          
          d <- base |>
            dplyr::filter(
              stringr::str_detect(
                .data$item_norm,
                stringr::regex(pattern, ignore_case = TRUE)
              )
            )
          
          if (nrow(d) == 0) {
            return(tibble::tibble())
          }
          
          d |>
            dplyr::transmute(
              company = .data$company,
              period = .data$period,
              metric_id = metric_id,
              group = group,
              display_name = display_name,
              value = .data$value,
              item = .data$item
            )
        }
      )
    )
  }
  
  if (!is.na(company_col) && !is.na(period_col)) {
    
    id_cols <- c(company_col, period_col)
    value_cols <- setdiff(names(raw), id_cols)
    
    if (length(value_cols) > 0) {
      
      long <- raw |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(value_cols),
          names_to = "item",
          values_to = "raw_value"
        ) |>
        dplyr::transmute(
          company = as.character(.data[[company_col]]),
          period = as.character(.data[[period_col]]),
          item = as.character(.data$item),
          value = fac_number(.data$raw_value),
          item_norm = fac_ascii_upper(.data$item)
        )
      
      out <- purrr::pmap_dfr(
        factoring_metric_catalog,
        function(metric_id, group, display_name, pattern) {
          
          d <- long |>
            dplyr::filter(
              stringr::str_detect(
                .data$item_norm,
                stringr::regex(pattern, ignore_case = TRUE)
              )
            )
          
          if (nrow(d) == 0) {
            return(tibble::tibble())
          }
          
          d |>
            dplyr::transmute(
              company = .data$company,
              period = .data$period,
              metric_id = metric_id,
              group = group,
              display_name = display_name,
              value = .data$value,
              item = .data$item
            )
        }
      )
      
      if (nrow(out) > 0) return(out)
    }
  }
  
  attr(raw, "fac_unrecognised_columns") <- names(raw)
  
  tibble::tibble(
    company = character(),
    period = character(),
    metric_id = character(),
    group = character(),
    display_name = character(),
    value = double(),
    item = character()
  )
}

factoring_raw <- if (file.exists(factoring_path)) {
  readr::read_csv(
    factoring_path,
    show_col_types = FALSE,
    name_repair = "unique"
  )
} else {
  tibble::tibble()
}

factoring_data <- if (nrow(factoring_raw) > 0) {
  fac_enrich_long(factoring_raw)
} else {
  tibble::tibble(
    company = character(),
    period = character(),
    metric_id = character(),
    group = character(),
    display_name = character(),
    value = double(),
    item = character()
  )
}

factoring_loaded <- nrow(factoring_data) > 0

factoring_data_problem <- if (
  file.exists(factoring_path) &&
  nrow(factoring_raw) > 0 &&
  !factoring_loaded
) {
  paste0(
    "Factoring CSV okundu ama yapısı otomatik eşleştirilemedi. ",
    "Bulunan sütunlar: ",
    paste(names(factoring_raw), collapse = ", ")
  )
} else {
  NULL
}

factoring_companies <- c(
  "VDF", "GARANTI", "IS", "YAPI_KREDI", "TEB", "TAM_FINANS", "MNG"
)

factoring_employee_data <- tibble::tibble(
  company = c("VDF", "GARANTI", "TEB", "IS", "YAPI_KREDI"),
  employees = c(25, 122, 113, 135, 142)
)

fac_company_label <- function(x) {
  dplyr::recode(
    x,
    "VDF" = "VDF",
    "GARANTI" = "Garanti",
    "IS" = "İş",
    "YAPI_KREDI" = "Yapı Kredi",
    "TEB" = "TEB",
    "TAM_FINANS" = "Tam Finans",
    "MNG" = "MNG",
    .default = x
  )
}

fac_format_bn <- function(x) {
  ifelse(
    is.na(x),
    "—",
    paste0(
      format(
        round(x / 1e6, 2),
        nsmall = 2,
        big.mark = ".",
        decimal.mark = ","
      ),
      " Mlr TL"
    )
  )
}

fac_metric_label <- function(metric_id_value) {
  if (!factoring_loaded) return(metric_id_value)
  
  x <- factoring_data |>
    dplyr::filter(.data$metric_id == .env$metric_id_value) |>
    dplyr::pull(.data$display_name) |>
    unique()
  
  if (length(x) == 0) metric_id_value else x[[1]]
}

fac_metric_choices <- function(group_value) {
  if (!factoring_loaded) {
    return(stats::setNames(character(), character()))
  }
  
  d <- factoring_data |>
    dplyr::filter(.data$group == .env$group_value) |>
    dplyr::distinct(.data$metric_id, .data$display_name)
  
  if (nrow(d) == 0) {
    return(stats::setNames(character(), character()))
  }
  
  stats::setNames(d$metric_id, d$display_name)
}

fac_plot_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      text = ggplot2::element_text(color = "#CFE7E5"),
      axis.text = ggplot2::element_text(color = "#89AAA7"),
      axis.title = ggplot2::element_text(color = "#89AAA7"),
      plot.title = ggplot2::element_text(color = "#EAF7F6", face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "#789B98"),
      legend.text = ggplot2::element_text(color = "#A8C7C4"),
      legend.title = ggplot2::element_text(color = "#A8C7C4"),
      legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#18383C", linewidth = .35),
      panel.grid.minor = ggplot2::element_blank()
    )
}

fac_plot_company_comparison <- function(
    data,
    metric_id_value,
    source_id,
    highlight_company = NULL
) {
  d <- data |>
    dplyr::filter(
      .data$period == "2026/06",
      .data$metric_id == .env$metric_id_value
    ) |>
    dplyr::group_by(.data$company) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      company_name = fac_company_label(.data$company),
      is_highlight = !is.null(highlight_company) &
        .data$company == highlight_company
    ) |>
    dplyr::arrange(.data$value)
  
  shiny::validate(
    shiny::need(nrow(d) > 0, "Bu kalem için 2026/06 verisi bulunamadı.")
  )
  
  title_text <- fac_metric_label(metric_id_value)
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = stats::reorder(.data$company_name, .data$value),
      y = .data$value,
      text = paste0(
        "<b>", .data$company_name, "</b>",
        "<br>", title_text, ": ", fac_format_bn(.data$value)
      ),
      key = .data$company,
      fill = .data$is_highlight
    )
  ) +
    ggplot2::geom_col(width = .56) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#25C7C9", "FALSE" = "#65858A"),
      guide = "none"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(
        format(round(x / 1e6, 1), decimal.mark = ","),
        " Mlr"
      ),
      n.breaks = 5
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = paste0(title_text, " | 2026/06")
    ) +
    fac_plot_theme(13) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  
  plotly::ggplotly(p, tooltip = "text", source = source_id) |>
    plotly::event_register("plotly_click") |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 145, r = 25, t = 55, b = 45)
    ) |>
    plotly::config(displayModeBar = FALSE)
}

fac_plot_company_detail <- function(
    data,
    company_id,
    group_value,
    source_id
) {
  d <- data |>
    dplyr::filter(
      .data$period == "2026/06",
      .data$company == .env$company_id,
      .data$group == .env$group_value
    ) |>
    dplyr::group_by(.data$metric_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(plot_value = abs(.data$value)) |>
    dplyr::arrange(.data$plot_value)
  
  shiny::validate(
    shiny::need(nrow(d) > 0, "Bu şirket için detay verisi bulunamadı.")
  )
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = stats::reorder(.data$display_name, .data$plot_value),
      y = .data$plot_value,
      text = paste0(
        "<b>", .data$display_name, "</b><br>",
        fac_format_bn(.data$value)
      ),
      key = .data$metric_id
    )
  ) +
    ggplot2::geom_col(width = .56, fill = "#63C9C8") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(
        format(round(x / 1e6, 1), decimal.mark = ","),
        " Mlr"
      ),
      n.breaks = 4
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = paste0(fac_company_label(company_id), " | Detay")
    ) +
    fac_plot_theme(12) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  
  plotly::ggplotly(p, tooltip = "text", source = source_id) |>
    plotly::event_register("plotly_click") |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 190, r = 25, t = 55, b = 50)
    ) |>
    plotly::config(displayModeBar = FALSE)
}

fac_comparison_ui <- function(prefix, title, group_value, default_metric) {
  metric_choices_now <- fac_metric_choices(group_value)
  
  bslib::nav_panel(
    title,
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::selectInput(
          paste0("fac_", prefix, "_metric"),
          "Ana karşılaştırma kalemi",
          choices = metric_choices_now,
          selected = if (default_metric %in% unname(metric_choices_now)) {
            default_metric
          } else {
            if (length(metric_choices_now)) unname(metric_choices_now)[1] else NULL
          }
        ),
        shiny::selectInput(
          paste0("fac_", prefix, "_company"),
          "Detay şirketi",
          choices = stats::setNames(
            factoring_companies,
            fac_company_label(factoring_companies)
          ),
          selected = "VDF"
        ),
        shiny::helpText(
          "Üst grafikte bir şirkete tıklarsan detay şirketi değişir. Alt soldaki bir kaleme tıklarsan sağdaki karşılaştırma o kaleme geçer."
        )
      ),
      bslib::card(
        class = "metric-card",
        full_screen = TRUE,
        bslib::card_header(paste0("2026/06 ", title, " Karşılaştırması")),
        plotly::plotlyOutput(paste0("fac_", prefix, "_top"), height = "380px")
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          plotly::plotlyOutput(paste0("fac_", prefix, "_detail"), height = "410px")
        ),
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          plotly::plotlyOutput(paste0("fac_", prefix, "_right"), height = "410px")
        )
      )
    )
  )
}

finance_metrics <- c(
  "Varlıklar Toplamı",
  "Özkaynaklar",
  "Alınan Krediler",
  "İhraç Edilen Menkul Kıymetler (Net)",
  "Esas Faaliyet Gelirleri",
  "Finansman Giderleri",
  "Brüt Kâr (Zarar)",
  "Net Faaliyet Kârı (Zararı)"
)

fleet_metric_lookup <- fleet |>
  distinct(metric_id, metric) |>
  arrange(metric)

fleet_metric_choices <- stats::setNames(
  fleet_metric_lookup$metric_id,
  fleet_metric_lookup$metric
)

finance_company_colors <- c(
  "VDF" = "#25C7C9",
  "KOC" = "#5577E8",
  "KOC_STELLANTIS" = "#8F69CF",
  "MERCEDES" = "#6BAE75",
  "ALJ" = "#E6A34A",
  "TEB" = "#D96D8A",
  "ORFIN" = "#7F98A8"
)

fleet_company_colors <- c(
  "VDF Filo" = "#149C9C",
  "Garanti" = "#4F81D9",
  "Hedef" = "#E3A94D",
  "TEB Arval" = "#9874C8"
)

fmt_mn <- function(x) {
  ifelse(
    is.na(x), "—",
    ifelse(
      abs(x) >= 1000,
      paste0(format(round(x / 1000, 2), nsmall = 2, big.mark = ".", decimal.mark = ","), " Mlr TL"),
      paste0(format(round(x, 2), nsmall = 2, big.mark = ".", decimal.mark = ","), " Mn TL")
    )
  )
}

fmt_tl <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "—",
    abs(x) >= 1e9 ~ paste0(format(round(x / 1e9, 2), nsmall = 2, big.mark = ".", decimal.mark = ","), " Mlr TL"),
    abs(x) >= 1e6 ~ paste0(format(round(x / 1e6, 2), nsmall = 2, big.mark = ".", decimal.mark = ","), " Mn TL"),
    abs(x) >= 1e3 ~ paste0(format(round(x / 1e3, 2), nsmall = 2, big.mark = ".", decimal.mark = ","), " Bin TL"),
    TRUE ~ paste0(format(round(x, 2), nsmall = 2, big.mark = ".", decimal.mark = ","), " TL")
  )
}

finance_wide <- finance |>
  select(company, period, item, value_mn_tl) |>
  pivot_wider(names_from = item, values_from = value_mn_tl) |>
  mutate(
    equity_ratio = `Özkaynaklar` / `Varlıklar Toplamı` * 100,
    funding_to_assets = (`Alınan Krediler` + coalesce(`İhraç Edilen Menkul Kıymetler (Net)`, 0)) /
      `Varlıklar Toplamı` * 100,
    gross_margin = `Brüt Kâr (Zarar)` / `Esas Faaliyet Gelirleri` * 100,
    operating_margin = `Net Faaliyet Kârı (Zararı)` / `Esas Faaliyet Gelirleri` * 100,
    financing_cost_ratio = abs(`Finansman Giderleri`) / `Esas Faaliyet Gelirleri` * 100
  )

ratio_labels <- c(
  equity_ratio = "Equity Ratio",
  funding_to_assets = "Funding / Assets",
  gross_margin = "Gross Margin",
  operating_margin = "Operating Margin",
  financing_cost_ratio = "Financing Cost / Revenue"
)

pnl_map <- c(
  "Bildirimler.xls" = "VDF",
  "Bildirimler-2.xls" = "ORFIN",
  "Bildirimler-3.xls" = "ALJ",
  "Bildirimler-4.xls" = "KOC",
  "Bildirimler-5.xls" = "KOC_STELLANTIS",
  "Bildirimler-6.xls" = "MERCEDES"
)

extract_expenses <- function(file_path, company_name) {
  target_labels <- c(
    "ESAS FAALİYET GİDERLERİ (-)",
    "Personel Giderleri",
    "Kıdem Tazminatı Karşılığı Gideri",
    "Genel İşletme Giderleri"
  )
  
  html <- read_html(file_path)
  
  map_dfr(target_labels, function(label) {
    node <- html |>
      html_element(xpath = paste0("//table[normalize-space(.)='", label, "']"))
    
    if (is.na(node)) {
      return(tibble(company = company_name, item = label, value = NA_real_))
    }
    
    row_text <- node |>
      html_element(xpath = "./ancestor::tr[1]") |>
      html_text2()
    
    nums <- stringr::str_extract_all(row_text, "-?[0-9]+(?:\\.[0-9]{3})*")[[1]]
    nums <- suppressWarnings(as.numeric(gsub("\\.", "", nums)))
    
    if (length(nums) >= 1 && abs(nums[1]) < 100) nums <- nums[-1]
    
    val <- if (length(nums) >= 1) abs(nums[1]) else NA_real_
    
    tibble(company = company_name, item = label, value = val)
  })
}

pnl_files <- list.files("data/finance/pnl", pattern = "\\.xls$", full.names = TRUE)

finance_expenses <- map_dfr(pnl_files, function(f) {
  nm <- pnl_map[[basename(f)]]
  extract_expenses(f, nm)
})


theme_hub <- bs_theme(
  version = 5,
  bg = "#071418",
  fg = "#EAF7F6",
  primary = "#25C7C9",
  secondary = "#7EE6DF",
  base_font = font_google("Inter")
)

hub_css <- "
html, body {
  background: #071418;
  color: #EAF7F6;
}

.hub-shell {
  min-height: 100vh;
  position: relative;
  overflow: hidden;
  padding: 34px 42px 46px;
  background:
    radial-gradient(circle at 78% 18%, rgba(37,199,201,.18), transparent 30%),
    radial-gradient(circle at 12% 82%, rgba(126,230,223,.10), transparent 28%),
    linear-gradient(135deg,#071418 0%,#0A1D22 50%,#081317 100%);
}

.hub-shell::before {
  content: '';
  position: absolute;
  inset: 0;
  opacity: .13;
  pointer-events: none;
  background-image:
    linear-gradient(rgba(255,255,255,.05) 1px, transparent 1px),
    linear-gradient(90deg,rgba(255,255,255,.05) 1px, transparent 1px);
  background-size: 34px 34px;
}

.hub-topline {
  position: relative;
  z-index: 2;
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 72px;
}

.hub-brand {
  display: flex;
  align-items: center;
  gap: 12px;
  letter-spacing: .12em;
  font-size: 12px;
  font-weight: 700;
  color: #B8D8D6;
  text-transform: uppercase;
}

.brand-dot {
  width: 10px;
  height: 10px;
  border-radius: 50%;
  background: #25C7C9;
  box-shadow: 0 0 18px rgba(37,199,201,.95);
}

.hub-period {
  font-size: 12px;
  color: #7FA4A3;
  border: 1px solid rgba(126,230,223,.18);
  padding: 8px 12px;
  border-radius: 999px;
  background: rgba(255,255,255,.025);
}

.hero-wrap {
  position: relative;
  z-index: 2;
  max-width: 1080px;
  margin: 0 auto;
}

.eyebrow {
  color: #60D7D5;
  text-transform: uppercase;
  letter-spacing: .18em;
  font-size: 12px;
  font-weight: 700;
  margin-bottom: 14px;
}

.hero-title {
  font-size: clamp(42px,6.5vw,78px);
  line-height: .98;
  letter-spacing: -.055em;
  font-weight: 750;
  max-width: 900px;
  color: #F1FCFB;
  margin-bottom: 22px;
}

.hero-title .accent { color: #62D9D8; }

.hero-subtitle {
  color: #9AB8B6;
  font-size: 16px;
  line-height: 1.65;
  max-width: 720px;
  margin-bottom: 46px;
}

.sector-grid {
  display: grid;
  grid-template-columns: repeat(3,1fr);
  gap: 18px;
}

.sector-card {
  position: relative;
  min-height: 260px;
  border: 1px solid rgba(126,230,223,.14);
  border-radius: 22px;
  padding: 24px;
  background: linear-gradient(180deg,rgba(255,255,255,.052),rgba(255,255,255,.022));
  transition: .2s ease;
  overflow: hidden;
}

.sector-card:hover {
  transform: translateY(-5px);
  border-color: rgba(98,217,216,.58);
  background: linear-gradient(180deg,rgba(37,199,201,.11),rgba(255,255,255,.025));
}

.sector-index {
  color: #537876;
  font-size: 11px;
  letter-spacing: .16em;
  margin-bottom: 32px;
}

.sector-icon {
  width: 46px;
  height: 46px;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 13px;
  color: #63DAD8;
  background: rgba(37,199,201,.09);
  border: 1px solid rgba(37,199,201,.16);
  margin-bottom: 20px;
}

.sector-title {
  color: #F2FBFA;
  font-size: 24px;
  font-weight: 700;
  margin-bottom: 8px;
}

.sector-copy {
  color: #88AAA8;
  font-size: 13px;
  line-height: 1.55;
  min-height: 62px;
}

.sector-meta {
  color: #5E817F;
  font-size: 11px;
  margin-top: 18px;
  letter-spacing: .04em;
}

.sector-card .action-button {
  position: absolute;
  inset: 0;
  opacity: 0;
  width: 100%;
  height: 100%;
  cursor: pointer;
}

.hub-footer {
  position: relative;
  z-index: 2;
  max-width: 1080px;
  margin: 30px auto 0;
  display: flex;
  justify-content: space-between;
  color: #4F7270;
  font-size: 11px;
}

.analysis-shell {
  min-height: 100vh;
  padding: 20px 24px 30px;
  color: #DDF3F1;
  background:
    radial-gradient(circle at 85% 8%,rgba(37,199,201,.11),transparent 28%),
    linear-gradient(135deg,#071418 0%,#0A1D22 55%,#081317 100%);
}

.analysis-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 18px;
}

.analysis-title {
  font-size: 27px;
  font-weight: 750;
  letter-spacing: -.035em;
  color: #F2FBFA;
}

.analysis-sub {
  font-size: 12px;
  color: #7FA4A3;
}

.home-btn {
  border-radius: 999px !important;
  color: #A8CFCC !important;
  border-color: rgba(126,230,223,.28) !important;
  background: rgba(255,255,255,.025) !important;
}

.home-btn:hover {
  color: #F2FBFA !important;
  border-color: #58D3D2 !important;
  background: rgba(37,199,201,.10) !important;
}

/* SIDEBAR */
.analysis-shell .sidebar {
  background: #09191E !important;
  color: #CFE7E5 !important;
  border: 1px solid rgba(126,230,223,.14) !important;
  border-radius: 20px !important;
}

.analysis-shell .sidebar label {
  color: #92B5B2 !important;
}

.analysis-shell .form-select,
.analysis-shell .selectize-input,
.analysis-shell .form-control {
  background: #0D2328 !important;
  color: #EAF7F6 !important;
  border: 1px solid rgba(126,230,223,.17) !important;
  border-radius: 11px !important;
  box-shadow: none !important;
}

.analysis-shell .selectize-dropdown {
  background: #0D2328 !important;
  color: #EAF7F6 !important;
  border-color: rgba(126,230,223,.18) !important;
}

.analysis-shell .selectize-dropdown .active {
  background: #12383D !important;
  color: white !important;
}

.analysis-shell .text-muted {
  color: #739895 !important;
}

/* MAIN TAB CONTAINER - THIS WAS THE WHITE AREA */
.analysis-shell .navset-card-tab,
.analysis-shell .card:has(> .card-header > .nav-tabs) {
  background: #0A1C21 !important;
  border: 1px solid rgba(126,230,223,.14) !important;
  border-radius: 20px !important;
  overflow: hidden;
}

.analysis-shell .navset-card-tab > .card-header,
.analysis-shell .card-header:has(.nav-tabs) {
  background: #0D2328 !important;
  border-bottom: 1px solid rgba(126,230,223,.12) !important;
}

.analysis-shell .tab-content,
.analysis-shell .tab-pane {
  background: #0A1C21 !important;
  color: #DDF3F1 !important;
}

/* TABS */
.analysis-shell .nav-tabs {
  border-bottom: 1px solid rgba(126,230,223,.10) !important;
}

.analysis-shell .nav-tabs .nav-link {
  color: #86AAA7 !important;
  border: none !important;
  background: transparent !important;
  font-weight: 600;
}

.analysis-shell .nav-tabs .nav-link.active {
  color: #62D9D8 !important;
  border-bottom: 2px solid #25C7C9 !important;
  background: rgba(37,199,201,.08) !important;
}

.kpi-box {
  background: #0D2328 !important;
  border: 1px solid rgba(126,230,223,.15) !important;
  border-radius: 18px;
  padding: 18px;
  min-height: 112px;
}

.kpi-label {
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: .08em;
  color: #79A09D !important;
  margin-bottom: 8px;
}

.kpi-value {
  font-size: 25px;
  font-weight: 750;
  color: #F2FBFA !important;
  line-height: 1.2;
}

.kpi-note {
  font-size: 11px;
  color: #8AAEAB !important;
  margin-top: 6px;
}


.metric-card {
  border: 1px solid rgba(126,230,223,.14) !important;
  border-radius: 18px !important;
  background: #0D2025 !important;
  box-shadow: none !important;
}

.metric-card .card-header {
  background: #10282D !important;
  color: #CDE9E6 !important;
  border-bottom: 1px solid rgba(126,230,223,.11) !important;
  font-weight: 650;
}

.metric-card .card-body {
  background: #0D2025 !important;
}

.analysis-shell .js-plotly-plot,
.analysis-shell .plot-container,
.analysis-shell .svg-container {
  border-radius: 14px !important;
}

.note-strip {
  font-size: 12px;
  color: #91B7B4;
  background: rgba(37,199,201,.055);
  border: 1px solid rgba(126,230,223,.13);
  border-radius: 13px;
  padding: 10px 12px;
  margin-bottom: 12px;
}

.fact-frame {
  width: 100%;
  height: calc(100vh - 105px);
  border: 1px solid rgba(126,230,223,.14);
  border-radius: 18px;
  background: #0D2025;
}

.analysis-shell .dataTables_wrapper {
  color: #CFE7E5 !important;
}

.analysis-shell table.dataTable {
  color: #DDF3F1 !important;
  background: #0D2025 !important;
}

.analysis-shell table.dataTable thead th {
  color: #A8CFCC !important;
  background: #10282D !important;
}

.analysis-shell table.dataTable tbody tr,
.analysis-shell table.dataTable tbody td {
  color: #DDF3F1 !important;
  background: #0D2025 !important;
}

.analysis-shell table.dataTable tbody tr:nth-child(even) td {
  background: #10262B !important;
}

.analysis-shell .dataTables_filter input,
.analysis-shell .dataTables_length select {
  background: #0C2024 !important;
  color: #DFF4F2 !important;
  border: 1px solid rgba(126,230,223,.14) !important;
}

@media(max-width:900px) {
  .hub-shell { padding: 24px; }
  .hub-topline { margin-bottom: 44px; }
  .sector-grid { grid-template-columns: 1fr; }
  .sector-card { min-height: 220px; }
  .hub-footer { display: none; }
  .analysis-shell { padding: 16px; }
}
"

sector_card <- function(index, icon, title, copy, meta, input_id) {
  div(
    class = "sector-card",
    div(class = "sector-index", index),
    div(class = "sector-icon", bs_icon(icon, size = "24px")),
    div(class = "sector-title", title),
    div(class = "sector-copy", copy),
    div(class = "sector-meta", meta),
    actionButton(input_id, "", class = "action-button")
  )
}

landing_ui <- div(
  class = "hub-shell",
  div(
    class = "hub-topline",
    div(class = "hub-period", "Finance • Factoring • Fleet")
  ),
  div(
    class = "hero-wrap",
    div(class = "hero-title", HTML("One view.<br><span class='accent'>Three financial lenses.</span>")),
    div(class = "hero-subtitle",
        "A unified analytics environment for financing, factoring and fleet-leasing peer analysis."),
    div(
      class = "sector-grid",
      sector_card(
        "01 / FINANCE", "bank", "Finance",
        "Financial position, profitability, funding structure, benchmark scorecards and operating-expense analysis.",
        "2023/12 • 2024/12 • 2025/12 • 2026/06", "go_finance"
      ),
      sector_card(
        "02 / FACTORING", "diagram-3", "Factoring",
        "Company comparison, balance-sheet metrics, trend analysis and employee productivity across factoring peers.",
        "Existing live peer-analysis dashboard", "go_factoring"
      ),
      sector_card(
        "03 / FLEET", "car-front", "Fleet",
        "Fleet-leasing peer comparison with leased assets, profitability, funding, used-car metrics and productivity.",
        "2024/12 • 2025/06 • 2025/12 • 2026/06", "go_fleet"
      )
    )
  ),
  div(class = "hub-footer", span("R + Shiny"))
)

analysis_header <- function(title, subtitle, back_id) {
  div(
    class = "analysis-header",
    div(
      div(class = "analysis-title", title),
      div(class = "analysis-sub", subtitle)
    ),
    actionButton(back_id, "← Hub", class = "btn btn-outline-secondary home-btn")
  )
}

finance_ui <- div(
  class = "analysis-shell",
  analysis_header("Finance Analytics", "Financing companies peer analysis", "finance_home"),
  page_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      width = 255,
      selectInput("fin_period", "Period",
                  choices = sort(unique(finance$period), decreasing = TRUE),
                  selected = "2026/06"),
      selectInput("fin_metric", "Metric",
                  choices = finance_metrics,
                  selected = "Varlıklar Toplamı"),
      hr(),
      div(class = "small text-muted",
          "Financial statement values are shown in Mn TL / Bn TL format.")
    ),
    navset_card_tab(
      nav_panel(
        "Overview",
        layout_column_wrap(
          width = 1/3,
          div(class = "kpi-box",
              div(class = "kpi-label", "Companies"),
              div(class = "kpi-value", textOutput("fin_kpi_companies"))),
          div(class = "kpi-box",
              div(class = "kpi-label", "Leader"),
              div(class = "kpi-value", textOutput("fin_kpi_leader")),
              div(class = "kpi-note", textOutput("fin_kpi_metric"))),
          div(class = "kpi-box",
              div(class = "kpi-label", "VDF Position"),
              div(class = "kpi-value", textOutput("fin_kpi_vdf")))
        ),
        card(class = "metric-card",
             card_header("Peer Snapshot"),
             plotlyOutput("fin_overview_plot", height = 430))
      ),
      nav_panel(
        "Trends",
        card(class = "metric-card",
             card_header("Metric Trend"),
             plotlyOutput("fin_trend_plot", height = 500))
      ),
      nav_panel(
        "Ratios",
        card(class = "metric-card",
             card_header("2026/06 Ratio Benchmark"),
             plotlyOutput("fin_ratio_plot", height = 520))
      ),
      nav_panel(
        "Expenses",
        div(class = "note-strip",
            "Expense data uses the latest 2026/06 values available in the uploaded P&L files."),
        card(class = "metric-card",
             card_header("Operating Expense Comparison"),
             plotlyOutput("fin_expense_plot", height = 490))
      ),
      nav_panel(
        "Data",
        card(class = "metric-card",
             card_header("Finance Data"),
             DTOutput("fin_table"))
      )
    )
  )
)

factoring_ui <- div(
  class = "analysis-shell",
  analysis_header(
    "Factoring Peer Analysis",
    "Factoring companies peer analysis",
    "factoring_home"
  ),
  
  if (!factoring_loaded) {
    div(
      class = "note-strip",
      if (!file.exists(factoring_path)) {
        HTML(
          "<b>Factoring data file is missing.</b><br>
          Copy <code>factoring_master.csv</code> to
          <code>data/factoring/factoring_master.csv</code>."
        )
      } else {
        HTML(
          paste0(
            "<b>Factoring CSV was found, but its structure could not be mapped.</b><br>",
            htmltools::htmlEscape(factoring_data_problem)
          )
        )
      }
    )
  } else {
    bslib::navset_card_tab(
      
      bslib::nav_panel(
        "Genel Bakış",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            shiny::selectInput(
              "fac_overview_company",
              "Şirket",
              choices = stats::setNames(
                factoring_companies,
                fac_company_label(factoring_companies)
              ),
              selected = "VDF"
            ),
            shiny::selectInput(
              "fac_overview_metric",
              "Karşılaştırma kalemi",
              choices = c(
                "Toplam Aktif" = "total_assets",
                "Faktoring Alacakları" = "factoring_receivables",
                "Alınan Krediler" = "loans_received",
                "Özkaynaklar" = "equity",
                "Net Dönem Kârı" = "net_income"
              ),
              selected = "factoring_receivables"
            )
          ),
          
          bslib::layout_columns(
            col_widths = c(3, 3, 3, 3),
            
            div(
              class = "kpi-box",
              div(class = "kpi-label", "Toplam Aktif"),
              div(class = "kpi-value", shiny::textOutput("fac_vb_assets"))
            ),
            div(
              class = "kpi-box",
              div(class = "kpi-label", "Faktoring Alacakları"),
              div(class = "kpi-value", shiny::textOutput("fac_vb_receivables"))
            ),
            div(
              class = "kpi-box",
              div(class = "kpi-label", "Özkaynaklar"),
              div(class = "kpi-value", shiny::textOutput("fac_vb_equity"))
            ),
            div(
              class = "kpi-box",
              div(class = "kpi-label", "Net Dönem Kârı"),
              div(class = "kpi-value", shiny::textOutput("fac_vb_profit"))
            )
          ),
          
          bslib::card(
            class = "metric-card",
            full_screen = TRUE,
            bslib::card_header(shiny::textOutput("fac_overview_title")),
            plotly::plotlyOutput("fac_overview_plot", height = "460px")
          )
        )
      ),
      
      bslib::nav_panel(
        "Operasyonel Verimlilik",
        
        bslib::layout_columns(
          col_widths = c(4, 8),
          
          bslib::card(
            class = "metric-card",
            full_screen = TRUE,
            bslib::card_header("Çalışan Sayısı"),
            plotly::plotlyOutput("fac_employee_count_plot", height = "420px")
          ),
          
          bslib::card(
            class = "metric-card",
            full_screen = TRUE,
            bslib::card_header("Çalışan Başına Faktoring Alacağı"),
            plotly::plotlyOutput("fac_employee_efficiency_plot", height = "420px")
          )
        ),
        
        div(
          class = "note-strip",
          "Çalışan başına faktoring alacağı operasyonel ölçeği çalışan sayısıyla birlikte değerlendirmek için kullanılır. Tam Finans ve MNG çalışan sayısı karşılaştırılabilir biçimde doğrulanamadığı için bu bölümde yer almamaktadır."
        )
      ),
      
      fac_comparison_ui(
        "fact",
        "Faktoring Faaliyetleri",
        "Faktoring",
        "factoring_receivables"
      ),
      
      fac_comparison_ui(
        "fund",
        "Fonlama",
        "Fonlama",
        "loans_received"
      ),
      
      fac_comparison_ui(
        "off",
        "Nazım Hesaplar",
        "Nazım Hesaplar",
        "risk_assumed"
      ),
      
      fac_comparison_ui(
        "pnl",
        "Kârlılık",
        "Kârlılık",
        "operating_income"
      ),
      
      bslib::nav_panel(
        "Dönemsel Değişim",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            shiny::selectInput(
              "fac_change_metric",
              "Karşılaştırma kalemi",
              choices = c(
                "Faktoring Alacakları" = "factoring_receivables",
                "Toplam Aktif" = "total_assets",
                "Alınan Krediler" = "loans_received",
                "Özkaynaklar" = "equity"
              ),
              selected = "factoring_receivables"
            ),
            shiny::helpText(
              "31.12.2025 ve 30.06.2026 değerlerini doğrudan karşılaştırır."
            )
          ),
          
          bslib::card(
            class = "metric-card",
            full_screen = TRUE,
            bslib::card_header("31.12.2025 → 30.06.2026 Karşılaştırması"),
            plotly::plotlyOutput("fac_change_plot", height = "410px")
          ),
          
          bslib::card(
            class = "metric-card",
            full_screen = TRUE,
            bslib::card_header("Şirket Bazında Mutlak Değişim"),
            plotly::plotlyOutput("fac_absolute_change_plot", height = "390px")
          )
        )
      ),
      
      bslib::nav_panel(
        "Veri Kontrol",
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          bslib::card_header("Kullanılan Master Veri"),
          DT::DTOutput("fac_data_table")
        )
      )
    )
  }
)

fleet_ui <- div(
  class = "analysis-shell",
  analysis_header("Fleet Peer Analysis", "Fleet-leasing peer comparison", "fleet_home"),
  page_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      width = 255,
      selectInput("fleet_period", "Period",
                  choices = sort(unique(fleet$period), decreasing = TRUE),
                  selected = "2026/06"),
      selectInput("fleet_metric", "Metric",
                  choices = fleet_metric_choices,
                  selected = "leased_assets_net"),
      hr(),
      div(class = "small text-muted",
          "TEB Arval 2026/06 is excluded because the report is not published.")
    ),
    navset_card_tab(
      nav_panel(
        "Overview",
        layout_column_wrap(
          width = 1/3,
          div(class = "kpi-box",
              div(class = "kpi-label", "Companies"),
              div(class = "kpi-value", textOutput("fleet_kpi_companies"))),
          div(class = "kpi-box",
              div(class = "kpi-label", "Leader"),
              div(class = "kpi-value", textOutput("fleet_kpi_leader"))),
          div(class = "kpi-box",
              div(class = "kpi-label", "Metric"),
              div(class = "kpi-value", textOutput("fleet_kpi_metric")))
        ),
        card(class = "metric-card",
             card_header("Peer Snapshot"),
             plotlyOutput("fleet_overview_plot", height = 430))
      ),
      nav_panel(
        "Comparison",
        card(class = "metric-card",
             card_header("Selected Metric by Company"),
             plotlyOutput("fleet_compare_plot", height = 500))
      ),
      nav_panel(
        "Trends",
        card(class = "metric-card",
             card_header("Metric Trend"),
             plotlyOutput("fleet_trend_plot", height = 500))
      ),
      nav_panel(
        "Employee Productivity",
        div(class = "note-strip",
            "Period-specific employee counts are used where available. Otherwise the nearest known count is used as an approximation."),
        card(class = "metric-card",
             card_header("Metric per Employee"),
             plotlyOutput("fleet_employee_plot", height = 490))
      ),
      nav_panel(
        "Data",
        card(class = "metric-card",
             card_header("Fleet Data"),
             DTOutput("fleet_table"))
      )
    )
  )
)

ui <- tagList(
  tags$head(tags$style(HTML(hub_css))),
  navset_hidden(
    id = "main_view",
    nav_panel("Home", value = "home", landing_ui),
    nav_panel("Finance", value = "finance", finance_ui),
    nav_panel("Factoring", value = "factoring", factoring_ui),
    nav_panel("Fleet", value = "fleet", fleet_ui)
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$go_finance, nav_select("main_view", selected = "finance"))
  observeEvent(input$go_factoring, nav_select("main_view", selected = "factoring"))
  observeEvent(input$go_fleet, nav_select("main_view", selected = "fleet"))
  observeEvent(input$finance_home, nav_select("main_view", selected = "home"))
  observeEvent(input$factoring_home, nav_select("main_view", selected = "home"))
  observeEvent(input$fleet_home, nav_select("main_view", selected = "home"))
  
 
  fin_period_data <- reactive({
    finance |>
      filter(period == input$fin_period, item == input$fin_metric)
  })
  
  output$fin_kpi_companies <- renderText({
    n_distinct(fin_period_data()$company)
  })
  
  output$fin_kpi_metric <- renderText(input$fin_metric)
  
  output$fin_kpi_leader <- renderText({
    x <- fin_period_data() |>
      arrange(desc(value_mn_tl)) |>
      slice(1)
    
    if (nrow(x) == 0) return("—")
    paste0(x$company, " · ", fmt_mn(x$value_mn_tl))
  })
  
  output$fin_kpi_vdf <- renderText({
    x <- fin_period_data() |>
      arrange(desc(value_mn_tl)) |>
      mutate(rank = row_number()) |>
      filter(company == "VDF")
    
    if (nrow(x) == 0) return("—")
    paste0("#", x$rank, " · ", fmt_mn(x$value_mn_tl))
  })
  
  output$fin_overview_plot <- renderPlotly({
    x <- fin_period_data() |>
      arrange(value_mn_tl)
    
    p <- ggplot(
      x,
      aes(
        x = value_mn_tl,
        y = reorder(company, value_mn_tl),
        fill = company,
        text = paste0("<b>", company, "</b><br>", input$fin_metric, ": ", fmt_mn(value_mn_tl))
      )
    ) +
      geom_col(width = .52, show.legend = FALSE) +
      scale_fill_manual(values = finance_company_colors) +
      scale_x_continuous(labels = function(v) fmt_mn(v)) +
      labs(x = NULL, y = NULL, title = paste(input$fin_metric, "|", input$fin_period)) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$fin_trend_plot <- renderPlotly({
    x <- finance |>
      filter(item == input$fin_metric)
    
    p <- ggplot(
      x,
      aes(
        x = period,
        y = value_mn_tl,
        group = company,
        color = company,
        text = paste0("<b>", company, "</b><br>Period: ", period, "<br>", input$fin_metric, ": ", fmt_mn(value_mn_tl))
      )
    ) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      scale_color_manual(values = finance_company_colors) +
      scale_y_continuous(labels = function(v) fmt_mn(v)) +
      labs(x = NULL, y = NULL, color = NULL, title = paste(input$fin_metric, "Trend")) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$fin_ratio_plot <- renderPlotly({
    x <- finance_wide |>
      filter(period == "2026/06") |>
      select(company, all_of(names(ratio_labels))) |>
      pivot_longer(-company, names_to = "ratio", values_to = "value") |>
      mutate(
        ratio_label = recode(ratio, !!!ratio_labels),
        text = paste0("<b>", company, "</b><br>", ratio_label, ": ", round(value, 1), "%")
      )
    
    p <- ggplot(x, aes(x = ratio_label, y = company, fill = value, text = text)) +
      geom_tile(color = "white", linewidth = 1) +
      geom_text(aes(label = paste0(round(value, 1), "%")), size = 3.6) +
      scale_fill_gradient(low = "#E8F7F5", high = "#2CBFC0") +
      labs(x = NULL, y = NULL, fill = "%", title = "Financial Ratio Comparison | 2026/06") +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 25, hjust = 1),
            plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$fin_expense_plot <- renderPlotly({
    x <- finance_expenses |>
      filter(item == "ESAS FAALİYET GİDERLERİ (-)") |>
      arrange(value)
    
    p <- ggplot(
      x,
      aes(
        x = value,
        y = reorder(company, value),
        fill = company,
        text = paste0("<b>", company, "</b><br>Operating Expenses: ", fmt_mn(value / 1000))
      )
    ) +
      geom_col(width = .5, show.legend = FALSE) +
      scale_fill_manual(values = finance_company_colors) +
      scale_x_continuous(labels = function(v) fmt_mn(v / 1000)) +
      labs(x = NULL, y = NULL, title = "Operating Expenses | 2026/06") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$fin_table <- renderDT({
    datatable(
      finance |>
        arrange(company, desc(period), item) |>
        mutate(display_value = fmt_mn(value_mn_tl)) |>
        select(company, period, item, display_value),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  if (factoring_loaded) {
    
    fac_get_overview_value <- function(company_id, metric_id_value) {
      x <- factoring_data |>
        dplyr::filter(
          .data$company == .env$company_id,
          .data$period == "2026/06",
          .data$metric_id == .env$metric_id_value
        ) |>
        dplyr::pull(.data$value)
      
      if (length(x) == 0) return("—")
      fac_format_bn(x[[1]])
    }
    
    output$fac_vb_assets <- shiny::renderText({
      shiny::req(input$fac_overview_company)
      fac_get_overview_value(input$fac_overview_company, "total_assets")
    })
    
    output$fac_vb_receivables <- shiny::renderText({
      shiny::req(input$fac_overview_company)
      fac_get_overview_value(input$fac_overview_company, "factoring_receivables")
    })
    
    output$fac_vb_equity <- shiny::renderText({
      shiny::req(input$fac_overview_company)
      fac_get_overview_value(input$fac_overview_company, "equity")
    })
    
    output$fac_vb_profit <- shiny::renderText({
      shiny::req(input$fac_overview_company)
      fac_get_overview_value(input$fac_overview_company, "net_income")
    })
    
    output$fac_overview_title <- shiny::renderText({
      shiny::req(input$fac_overview_metric)
      paste0(fac_metric_label(input$fac_overview_metric), " | Şirket Karşılaştırması")
    })
    
    output$fac_overview_plot <- plotly::renderPlotly({
      shiny::req(input$fac_overview_metric, input$fac_overview_company)
      
      fac_plot_company_comparison(
        factoring_data,
        metric_id_value = input$fac_overview_metric,
        source_id = "fac_overview",
        highlight_company = input$fac_overview_company
      )
    })
    
    output$fac_employee_count_plot <- plotly::renderPlotly({
      d <- factoring_employee_data |>
        dplyr::mutate(
          company_name = fac_company_label(.data$company),
          is_vdf = .data$company == "VDF"
        ) |>
        dplyr::arrange(.data$employees)
      
      p <- ggplot2::ggplot(
        d,
        ggplot2::aes(
          x = stats::reorder(.data$company_name, .data$employees),
          y = .data$employees,
          fill = .data$is_vdf,
          text = paste0(
            "<b>", .data$company_name, "</b>",
            "<br>Çalışan Sayısı: ", .data$employees
          )
        )
      ) +
        ggplot2::geom_col(width = .56) +
        ggplot2::scale_fill_manual(
          values = c("TRUE" = "#25C7C9", "FALSE" = "#65858A"),
          guide = "none"
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = NULL) +
        fac_plot_theme(13) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 145, r = 25, t = 25, b = 45)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    output$fac_employee_efficiency_plot <- plotly::renderPlotly({
      d <- factoring_data |>
        dplyr::filter(
          .data$period == "2026/06",
          .data$metric_id == "factoring_receivables",
          .data$company %in% factoring_employee_data$company
        ) |>
        dplyr::group_by(.data$company) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::left_join(factoring_employee_data, by = "company") |>
        dplyr::mutate(
          company_name = fac_company_label(.data$company),
          receivables_per_employee = .data$value / .data$employees,
          is_vdf = .data$company == "VDF"
        ) |>
        dplyr::arrange(.data$receivables_per_employee)
      
      shiny::validate(
        shiny::need(nrow(d) > 0, "Çalışan başına faktoring alacağı hesaplanamadı.")
      )
      
      p <- ggplot2::ggplot(
        d,
        ggplot2::aes(
          x = stats::reorder(
            .data$company_name,
            .data$receivables_per_employee
          ),
          y = .data$receivables_per_employee,
          fill = .data$is_vdf,
          text = paste0(
            "<b>", .data$company_name, "</b>",
            "<br>Çalışan Sayısı: ", .data$employees,
            "<br>Faktoring Alacakları: ", fac_format_bn(.data$value),
            "<br>Çalışan Başına: ",
            format(
              round(.data$receivables_per_employee / 1e3, 2),
              decimal.mark = ",",
              big.mark = "."
            ),
            " Mn TL"
          )
        )
      ) +
        ggplot2::geom_col(width = .56) +
        ggplot2::scale_fill_manual(
          values = c("TRUE" = "#25C7C9", "FALSE" = "#65858A"),
          guide = "none"
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Mn TL / Çalışan") +
        fac_plot_theme(13) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 145, r = 25, t = 25, b = 55)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    fac_wire_comparison <- function(prefix, group_value) {
      metric_input <- paste0("fac_", prefix, "_metric")
      company_input <- paste0("fac_", prefix, "_company")
      top_source <- paste0("fac_", prefix, "_top")
      detail_source <- paste0("fac_", prefix, "_detail")
      right_source <- paste0("fac_", prefix, "_right")
      
      clicked_metric <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(input[[metric_input]], {
        clicked_metric(NULL)
      }, ignoreInit = TRUE)
      
      shiny::observeEvent(
        plotly::event_data("plotly_click", source = top_source),
        {
          click <- plotly::event_data("plotly_click", source = top_source)
          
          if (!is.null(click$key) && click$key %in% factoring_companies) {
            shiny::updateSelectInput(
              session,
              company_input,
              selected = click$key
            )
          }
        }
      )
      
      shiny::observeEvent(
        plotly::event_data("plotly_click", source = detail_source),
        {
          click <- plotly::event_data("plotly_click", source = detail_source)
          if (!is.null(click$key)) clicked_metric(click$key)
        }
      )
      
      output[[top_source]] <- plotly::renderPlotly({
        shiny::req(input[[metric_input]], input[[company_input]])
        
        fac_plot_company_comparison(
          factoring_data,
          metric_id_value = input[[metric_input]],
          source_id = top_source,
          highlight_company = input[[company_input]]
        )
      })
      
      output[[detail_source]] <- plotly::renderPlotly({
        shiny::req(input[[company_input]])
        
        fac_plot_company_detail(
          factoring_data,
          company_id = input[[company_input]],
          group_value = group_value,
          source_id = detail_source
        )
      })
      
      output[[right_source]] <- plotly::renderPlotly({
        shiny::req(input[[metric_input]], input[[company_input]])
        
        metric_id_value <- clicked_metric()
        
        if (is.null(metric_id_value)) {
          metric_id_value <- input[[metric_input]]
        }
        
        fac_plot_company_comparison(
          factoring_data,
          metric_id_value = metric_id_value,
          source_id = right_source,
          highlight_company = input[[company_input]]
        )
      })
    }
    
    fac_wire_comparison("fact", "Faktoring")
    fac_wire_comparison("fund", "Fonlama")
    fac_wire_comparison("off", "Nazım Hesaplar")
    fac_wire_comparison("pnl", "Kârlılık")
    
    output$fac_change_plot <- plotly::renderPlotly({
      shiny::req(input$fac_change_metric)
      
      d <- factoring_data |>
        dplyr::filter(
          .data$metric_id == input$fac_change_metric,
          .data$period %in% c("2025/12", "2026/06")
        ) |>
        dplyr::group_by(.data$company, .data$period) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          company_name = fac_company_label(.data$company),
          period = factor(
            .data$period,
            levels = c("2025/12", "2026/06")
          )
        )
      
      shiny::validate(
        shiny::need(nrow(d) > 0, "Bu kalem için dönemsel veri bulunamadı.")
      )
      
      metric_title <- fac_metric_label(input$fac_change_metric)
      
      p <- ggplot2::ggplot(
        d,
        ggplot2::aes(
          x = .data$company_name,
          y = abs(.data$value),
          fill = .data$period,
          text = paste0(
            "<b>", .data$company_name, "</b>",
            "<br>Dönem: ", .data$period,
            "<br>", metric_title, ": ", fac_format_bn(.data$value)
          )
        )
      ) +
        ggplot2::geom_col(position = "dodge", width = .66) +
        ggplot2::scale_fill_manual(
          values = c("2025/12" = "#65858A", "2026/06" = "#25C7C9")
        ) +
        ggplot2::labs(
          title = paste0(metric_title, " | Dönemsel Karşılaştırma"),
          x = NULL,
          y = NULL,
          fill = "Dönem"
        ) +
        ggplot2::scale_y_continuous(
          labels = function(x) paste0(
            format(round(x / 1e6, 1), decimal.mark = ","),
            " Mlr"
          )
        ) +
        fac_plot_theme(13) +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)
        )
      
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    output$fac_absolute_change_plot <- plotly::renderPlotly({
      shiny::req(input$fac_change_metric)
      
      d <- factoring_data |>
        dplyr::filter(
          .data$metric_id == input$fac_change_metric,
          .data$period %in% c("2025/12", "2026/06")
        ) |>
        dplyr::group_by(.data$company, .data$period) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::select(.data$company, .data$period, .data$value) |>
        tidyr::pivot_wider(
          names_from = period,
          values_from = value
        ) |>
        dplyr::filter(
          !is.na(`2025/12`),
          !is.na(`2026/06`)
        ) |>
        dplyr::mutate(
          change = `2026/06` - `2025/12`,
          company_name = fac_company_label(.data$company)
        ) |>
        dplyr::arrange(.data$change)
      
      shiny::validate(
        shiny::need(nrow(d) > 0, "İki dönemi de bulunan şirket yok.")
      )
      
      metric_title <- fac_metric_label(input$fac_change_metric)
      
      p <- ggplot2::ggplot(
        d,
        ggplot2::aes(
          x = stats::reorder(.data$company_name, .data$change),
          y = .data$change,
          text = paste0(
            "<b>", .data$company_name, "</b>",
            "<br>2025/12: ", fac_format_bn(`2025/12`),
            "<br>2026/06: ", fac_format_bn(`2026/06`),
            "<br>Değişim: ", fac_format_bn(.data$change)
          )
        )
      ) +
        ggplot2::geom_col(width = .56, fill = "#63C9C8") +
        ggplot2::geom_hline(
          yintercept = 0,
          linewidth = .4,
          color = "#668B88"
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste0(metric_title, " | Mutlak Değişim"),
          subtitle = "30.06.2026 değeri − 31.12.2025 değeri",
          x = NULL,
          y = NULL
        ) +
        ggplot2::scale_y_continuous(
          labels = function(x) paste0(
            format(round(x / 1e6, 1), decimal.mark = ","),
            " Mlr"
          )
        ) +
        fac_plot_theme(13) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 145, r = 30, t = 70, b = 45)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    output$fac_data_table <- DT::renderDT({
      factoring_data |>
        dplyr::mutate(
          company = fac_company_label(.data$company),
          display_value = fac_format_bn(.data$value)
        ) |>
        dplyr::select(
          .data$company,
          .data$period,
          .data$group,
          .data$display_name,
          .data$display_value,
          .data$item
        ) |>
        DT::datatable(
          rownames = FALSE,
          filter = "top",
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            autoWidth = TRUE
          )
        )
    }, server = FALSE)
  }
  
  # -----------------------------
  # Fleet
  # -----------------------------
  fleet_period_data <- reactive({
    fleet |>
      filter(period == input$fleet_period, metric_id == input$fleet_metric)
  })
  
  fleet_metric_name <- reactive({
    fleet |>
      filter(metric_id == input$fleet_metric) |>
      distinct(metric) |>
      pull(metric) |>
      first()
  })
  
  output$fleet_kpi_companies <- renderText(n_distinct(fleet_period_data()$company))
  output$fleet_kpi_metric <- renderText(fleet_metric_name())
  
  output$fleet_kpi_leader <- renderText({
    x <- fleet_period_data() |>
      arrange(desc(value)) |>
      slice(1)
    
    if (nrow(x) == 0) return("—")
    paste0(x$company, " · ", fmt_tl(x$value))
  })
  
  output$fleet_overview_plot <- renderPlotly({
    x <- fleet_period_data() |> arrange(value)
    
    p <- ggplot(
      x,
      aes(
        x = value,
        y = reorder(company, value),
        text = paste0("<b>", company, "</b><br>", fleet_metric_name(), ": ", fmt_tl(value))
      )
    ) +
      geom_col(width = .48, fill = "#9ADDDD") +
      scale_x_continuous(labels = function(v) fmt_tl(v)) +
      labs(x = NULL, y = NULL, title = paste(fleet_metric_name(), "|", input$fleet_period)) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$fleet_compare_plot <- renderPlotly({
    x <- fleet_period_data() |> arrange(value)
    
    p <- ggplot(
      x,
      aes(
        x = value,
        y = reorder(company, value),
        text = paste0("<b>", company, "</b><br>", fleet_metric_name(), ": ", fmt_tl(value))
      )
    ) +
      geom_col(width = .48, fill = "#9ADDDD") +
      scale_x_continuous(labels = function(v) fmt_tl(v)) +
      labs(x = NULL, y = NULL, title = paste(fleet_metric_name(), "|", input$fleet_period)) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$fleet_trend_plot <- renderPlotly({
    x <- fleet |>
      filter(metric_id == input$fleet_metric)
    
    p <- ggplot(
      x,
      aes(
        x = period,
        y = value,
        group = company,
        color = company,
        text = paste0("<b>", company, "</b><br>Period: ", period, "<br>", fleet_metric_name(), ": ", fmt_tl(value))
      )
    ) +
      geom_line(linewidth = 1.6) +
      geom_point(size = 3.2) +
      scale_color_manual(values = fleet_company_colors) +
      scale_y_continuous(labels = function(v) fmt_tl(v)) +
      labs(x = NULL, y = NULL, color = NULL, title = paste(fleet_metric_name(), "Trend")) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$fleet_employee_plot <- renderPlotly({
    x <- fleet |>
      filter(period == input$fleet_period, metric_id == input$fleet_metric) |>
      select(company, period, value) |>
      left_join(fleet_employees, by = c("company", "period")) |>
      mutate(
        value_per_employee = value / employees,
        source_note = case_when(
          employee_source == "reported" ~ "Reported employee count",
          employee_source == "current" ~ "Current employee count",
          employee_source == "fallback_2025_12" ~ "Approximation using 2025/12 employee count",
          TRUE ~ "Employee count unavailable"
        )
      ) |>
      filter(!is.na(value_per_employee)) |>
      arrange(value_per_employee)
    
    p <- ggplot(
      x,
      aes(
        x = value_per_employee,
        y = reorder(company, value_per_employee),
        text = paste0(
          "<b>", company, "</b><br>",
          "Employees: ", employees, "<br>",
          source_note, "<br>",
          fleet_metric_name(), " / Employee: ", fmt_tl(value_per_employee)
        )
      )
    ) +
      geom_col(width = .48, fill = "#A6E7E7") +
      scale_x_continuous(labels = function(v) fmt_tl(v)) +
      labs(x = paste(fleet_metric_name(), "per Employee"), y = NULL,
           title = paste(fleet_metric_name(), "per Employee |", input$fleet_period)) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$fleet_table <- renderDT({
    datatable(
      fleet |>
        arrange(company, desc(period), metric) |>
        mutate(display_value = fmt_tl(value)) |>
        select(company, period, metric, display_value),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
}

shinyApp(ui = ui, server = server)

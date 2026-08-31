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
    "Faktoring CSV okundu ama yapısı otomatik eşleştirilemedi. ",
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
    dplyr::mutate(
      plot_value = abs(.data$value)
    ) |>
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
          "Üst grafikte bir şirkete tıklarsan aşağıdaki şirket detayı otomatik değişir."
        )
      ),
      bslib::card(
        class = "metric-card",
        full_screen = TRUE,
        bslib::card_header(paste0("2026/06 ", title, " Karşılaştırması")),
        plotly::plotlyOutput(paste0("fac_", prefix, "_top"), height = "380px")
      ),
      bslib::card(
        class = "metric-card",
        full_screen = TRUE,
        bslib::card_header("Seçili Şirket Detayı"),
        plotly::plotlyOutput(paste0("fac_", prefix, "_detail"), height = "410px")
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

fleet_metric_tr <- c(
  "Used Car Sales" = "İkinci El Araç Satışları",
  "Cost of UC Sales" = "İkinci El Araç Satış Maliyeti",
  "UC Sale Profit" = "İkinci El Araç Satış Kârı",
  "Leased Assets - Net" = "Kiralanan Varlıklar - Net",
  "Impairment" = "Değer Düşüklüğü",
  "Shareholders' Equity" = "Özkaynaklar",
  "Profit Before Tax" = "Vergi Öncesi Kâr",
  "Profit After Tax" = "Vergi Sonrası Kâr",
  "Borrowings" = "Borçlanmalar",
  "Interest Expense" = "Faiz Gideri",
  "Depreciation" = "Amortisman",
  "Operational Profit" = "Faaliyet Kârı"
)

fleet_metric_label <- function(x) {
  out <- unname(fleet_metric_tr[as.character(x)])
  out[is.na(out)] <- as.character(x)[is.na(out)]
  out
}

fleet_metric_lookup <- fleet |>
  distinct(metric_id, metric) |>
  arrange(metric) |>
  mutate(metric_label = fleet_metric_label(metric))

fleet_metric_choices <- stats::setNames(
  fleet_metric_lookup$metric_id,
  fleet_metric_lookup$metric_label
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
  equity_ratio = "Özkaynak Oranı",
  funding_to_assets = "Fonlama / Varlık",
  gross_margin = "Brüt Kâr Marjı",
  operating_margin = "Net Faaliyet Kâr Marjı",
  financing_cost_ratio = "Finansman Gideri / Gelir"
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

get_finance_expenses <- local({
  cache <- NULL
  
  function() {
    if (!is.null(cache)) return(cache)
    
    pnl_files <- list.files("data/finance/pnl", pattern = "\\.xls$", full.names = TRUE)
    cache <<- purrr::map_dfr(pnl_files, function(f) {
      nm <- pnl_map[[basename(f)]]
      extract_expenses(f, nm)
    })
    
    cache
  }
})


finance_companies <- sort(unique(finance$company))

fin_metric_choices_all <- stats::setNames(
  finance_metrics,
  finance_metrics
)

fin_structure_metrics <- c(
  "Varlıklar Toplamı",
  "Özkaynaklar",
  "Alınan Krediler",
  "İhraç Edilen Menkul Kıymetler (Net)"
)
fin_structure_metrics <- fin_structure_metrics[
  fin_structure_metrics %in% unique(finance$item)
]

fin_profit_metrics <- c(
  "Esas Faaliyet Gelirleri",
  "Finansman Giderleri",
  "Brüt Kâr (Zarar)",
  "Net Faaliyet Kârı (Zararı)"
)
fin_profit_metrics <- fin_profit_metrics[
  fin_profit_metrics %in% unique(finance$item)
]

fin_plot_company_comparison <- function(
    metric_value,
    period_value = "2026/06",
    highlight_company = NULL,
    source_id = NULL
) {
  d <- finance |>
    dplyr::filter(
      .data$period == .env$period_value,
      .data$item == .env$metric_value
    ) |>
    dplyr::group_by(.data$company) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      is_highlight = !is.null(highlight_company) &
        .data$company == highlight_company
    ) |>
    dplyr::arrange(.data$value_mn_tl)
  
  shiny::validate(
    shiny::need(nrow(d) > 0, "Bu kalem için veri bulunamadı.")
  )
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = stats::reorder(.data$company, .data$value_mn_tl),
      y = .data$value_mn_tl,
      fill = .data$is_highlight,
      key = .data$company,
      text = paste0(
        "<b>", .data$company, "</b><br>",
        metric_value, ": ", fmt_mn(.data$value_mn_tl)
      )
    )
  ) +
    ggplot2::geom_col(width = .56) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#25C7C9", "FALSE" = "#65858A"),
      guide = "none"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = function(x) {
        ifelse(
          abs(x) >= 1000,
          paste0(format(round(x / 1000, 1), decimal.mark = ","), " Mlr"),
          paste0(format(round(x, 0), decimal.mark = ","), " Mn")
        )
      }
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = paste0(metric_value, " | ", period_value)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
  
  w <- plotly::ggplotly(p, tooltip = "text", source = source_id)
  if (!is.null(source_id)) w <- plotly::event_register(w, "plotly_click")
  w |> plotly::config(displayModeBar = FALSE)
}

fin_plot_company_detail <- function(
    company_value,
    metrics,
    period_value = "2026/06",
    source_id = NULL
) {
  d <- finance |>
    dplyr::filter(
      .data$company == .env$company_value,
      .data$period == .env$period_value,
      .data$item %in% .env$metrics
    ) |>
    dplyr::mutate(plot_value = abs(.data$value_mn_tl)) |>
    dplyr::arrange(.data$plot_value)
  
  shiny::validate(
    shiny::need(nrow(d) > 0, "Bu şirket için detay verisi bulunamadı.")
  )
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = stats::reorder(.data$item, .data$plot_value),
      y = .data$plot_value,
      key = .data$item,
      text = paste0(
        "<b>", .data$item, "</b><br>",
        fmt_mn(.data$value_mn_tl)
      )
    )
  ) +
    ggplot2::geom_col(width = .56, fill = "#63C9C8") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = function(x) {
        ifelse(
          abs(x) >= 1000,
          paste0(format(round(x / 1000, 1), decimal.mark = ","), " Mlr"),
          paste0(format(round(x, 0), decimal.mark = ","), " Mn")
        )
      }
    ) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = paste0(company_value, " | Detay")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
  
  w <- plotly::ggplotly(p, tooltip = "text", source = source_id)
  if (!is.null(source_id)) w <- plotly::event_register(w, "plotly_click")
  w |> plotly::config(displayModeBar = FALSE)
}

finance_comparison_ui <- function(prefix, title, metrics, default_metric) {
  bslib::nav_panel(
    title,
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::selectInput(
          paste0("fin_", prefix, "_metric"),
          "Karşılaştırma kalemi",
          choices = stats::setNames(metrics, metrics),
          selected = default_metric
        ),
        shiny::selectInput(
          paste0("fin_", prefix, "_company"),
          "Detay şirketi",
          choices = finance_companies,
          selected = "VDF"
        ),
        shiny::helpText(
          "Üst grafikte bir şirkete tıklarsan aşağıdaki şirket detayı otomatik değişir."
        )
      ),
      bslib::card(
        class = "metric-card",
        full_screen = TRUE,
        bslib::card_header(paste0("2026/06 ", title, " Karşılaştırması")),
        plotly::plotlyOutput(paste0("fin_", prefix, "_top"), height = "380px")
      ),
      bslib::card(
        class = "metric-card",
        full_screen = TRUE,
        bslib::card_header("Seçili Şirket Detayı"),
        plotly::plotlyOutput(paste0("fin_", prefix, "_detail"), height = "410px")
      )
    )
  )
}

fleet_companies <- sort(unique(fleet$company))

fleet_choices_by_pattern <- function(pattern) {
  d <- fleet |>
    dplyr::distinct(.data$metric_id, .data$metric) |>
    dplyr::filter(
      stringr::str_detect(
        stringr::str_to_lower(.data$metric),
        stringr::regex(pattern, ignore_case = TRUE)
      )
    )
  
  if (nrow(d) == 0) {
    d <- fleet |>
      dplyr::distinct(.data$metric_id, .data$metric)
  }
  
  stats::setNames(d$metric_id, fleet_metric_label(d$metric))
}

fleet_asset_choices <- fleet_choices_by_pattern(
  "aktif|varlık|kirala|lease|araç|arac|used|uc|filo"
)

fleet_performance_choices <- fleet_choices_by_pattern(
  "kâr|kar|profit|gelir|revenue|income|özkaynak|ozkaynak|borç|borc|loan|fund|finans"
)

fleet_plot_company_comparison <- function(
    metric_id_value,
    period_value = "2026/06",
    highlight_company = NULL,
    source_id = NULL
) {
  d <- fleet |>
    dplyr::filter(
      .data$period == .env$period_value,
      .data$metric_id == .env$metric_id_value
    ) |>
    dplyr::group_by(.data$company) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      is_highlight = !is.null(highlight_company) &
        .data$company == highlight_company
    ) |>
    dplyr::arrange(.data$value)
  
  shiny::validate(
    shiny::need(nrow(d) > 0, "Bu kalem için veri bulunamadı.")
  )
  
  metric_title <- fleet |>
    dplyr::filter(.data$metric_id == .env$metric_id_value) |>
    dplyr::pull(.data$metric) |>
    unique()
  metric_title <- if (length(metric_title)) fleet_metric_label(metric_title[[1]]) else metric_id_value
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = stats::reorder(.data$company, .data$value),
      y = .data$value,
      fill = .data$is_highlight,
      key = .data$company,
      text = paste0(
        "<b>", .data$company, "</b><br>",
        metric_title, ": ", fmt_tl(.data$value)
      )
    )
  ) +
    ggplot2::geom_col(width = .56) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#25C7C9", "FALSE" = "#65858A"),
      guide = "none"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = function(x) {
        ifelse(
          abs(x) >= 1e9,
          paste0(format(round(x / 1e9, 1), decimal.mark = ","), " Mlr"),
          paste0(format(round(x / 1e6, 0), decimal.mark = ","), " Mn")
        )
      }
    ) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = paste0(metric_title, " | ", period_value)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
  
  w <- plotly::ggplotly(p, tooltip = "text", source = source_id)
  if (!is.null(source_id)) w <- plotly::event_register(w, "plotly_click")
  w |> plotly::config(displayModeBar = FALSE)
}

fleet_plot_company_detail <- function(
    company_value,
    metric_choices,
    period_value = "2026/06",
    source_id = NULL
) {
  ids <- unname(metric_choices)
  
  d <- fleet |>
    dplyr::filter(
      .data$company == .env$company_value,
      .data$period == .env$period_value,
      .data$metric_id %in% .env$ids
    ) |>
    dplyr::group_by(.data$metric_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      plot_value = abs(.data$value),
      metric_label = fleet_metric_label(.data$metric)
    ) |>
    dplyr::arrange(.data$plot_value)
  
  shiny::validate(
    shiny::need(nrow(d) > 0, "Bu şirket için detay verisi bulunamadı.")
  )
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = stats::reorder(.data$metric_label, .data$plot_value),
      y = .data$plot_value,
      key = .data$metric_id,
      text = paste0(
        "<b>", .data$metric_label, "</b><br>",
        fmt_tl(.data$value)
      )
    )
  ) +
    ggplot2::geom_col(width = .56, fill = "#63C9C8") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = paste0(company_value, " | Detay")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
  
  w <- plotly::ggplotly(p, tooltip = "text", source = source_id)
  if (!is.null(source_id)) w <- plotly::event_register(w, "plotly_click")
  w |> plotly::config(displayModeBar = FALSE)
}

fleet_comparison_ui <- function(prefix, title, choices, default_metric) {
  bslib::nav_panel(
    title,
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::selectInput(
          paste0("fleet_", prefix, "_metric"),
          "Karşılaştırma kalemi",
          choices = choices,
          selected = if (default_metric %in% unname(choices)) {
            default_metric
          } else {
            unname(choices)[1]
          }
        ),
        shiny::selectInput(
          paste0("fleet_", prefix, "_company"),
          "Detay şirketi",
          choices = fleet_companies,
          selected = if ("VDF Filo" %in% fleet_companies) "VDF Filo" else fleet_companies[1]
        ),
        shiny::helpText(
          "Üst grafikte bir şirkete tıklarsan aşağıdaki şirket detayı otomatik değişir."
        )
      ),
      bslib::card(
        class = "metric-card",
        full_screen = TRUE,
        bslib::card_header(paste0("2026/06 ", title, " Karşılaştırması")),
        plotly::plotlyOutput(paste0("fleet_", prefix, "_top"), height = "380px")
      ),
      bslib::card(
        class = "metric-card",
        full_screen = TRUE,
        bslib::card_header("Seçili Şirket Detayı"),
        plotly::plotlyOutput(paste0("fleet_", prefix, "_detail"), height = "410px")
      )
    )
  )
}


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

/* HAREKETLİ LAVA ARKA PLAN */
.lava-background {
  position: absolute;
  inset: 0;
  overflow: hidden;
  pointer-events: none;
  z-index: 0;
}

.lava-blob {
  position: absolute;
  border-radius: 50%;
  filter: blur(35px);
  opacity: .55;
  will-change: transform, border-radius;
}

.lava-1 {
  width: 420px;
  height: 520px;
  left: -80px;
  top: 20%;
  background:
    radial-gradient(circle at 35% 30%, rgba(81,255,190,.55), rgba(20,180,155,.22) 55%, transparent 75%);
  animation: lava1 13s ease-in-out infinite alternate;
}

.lava-2 {
  width: 520px;
  height: 430px;
  right: -100px;
  top: -80px;
  background:
    radial-gradient(circle at 50% 50%, rgba(36,220,190,.55), rgba(17,135,140,.25) 58%, transparent 76%);
  animation: lava2 17s ease-in-out infinite alternate;
}

.lava-3 {
  width: 380px;
  height: 460px;
  left: 38%;
  bottom: -250px;
  background:
    radial-gradient(circle, rgba(59,235,180,.42), rgba(16,145,130,.20) 55%, transparent 75%);
  animation: lava3 15s ease-in-out infinite alternate;
}

.lava-4 {
  width: 300px;
  height: 360px;
  right: 22%;
  top: 42%;
  background:
    radial-gradient(circle, rgba(75,255,210,.35), rgba(18,150,150,.15) 55%, transparent 75%);
  animation: lava4 11s ease-in-out infinite alternate;
}

@keyframes lava1 {
  0% { transform: translate(0,0) rotate(0deg) scale(1); border-radius: 48% 52% 63% 37% / 42% 38% 62% 58%; }
  50% { transform: translate(180px,-90px) rotate(45deg) scale(1.25); border-radius: 65% 35% 42% 58% / 55% 63% 37% 45%; }
  100% { transform: translate(80px,180px) rotate(90deg) scale(.9); border-radius: 38% 62% 58% 42% / 62% 38% 57% 43%; }
}

@keyframes lava2 {
  0% { transform: translate(0,0) rotate(0deg) scale(1); border-radius: 62% 38% 47% 53% / 38% 58% 42% 62%; }
  50% { transform: translate(-220px,130px) rotate(-55deg) scale(.85); border-radius: 42% 58% 65% 35% / 60% 35% 65% 40%; }
  100% { transform: translate(-80px,300px) rotate(-100deg) scale(1.2); border-radius: 55% 45% 35% 65% / 43% 63% 37% 57%; }
}

@keyframes lava3 {
  0% { transform: translate(0,0) rotate(0deg) scale(.8); }
  50% { transform: translate(-180px,-260px) rotate(70deg) scale(1.3); }
  100% { transform: translate(230px,-380px) rotate(140deg) scale(1); }
}

@keyframes lava4 {
  0% { transform: translate(0,0) scale(.8); }
  50% { transform: translate(-180px,100px) scale(1.35); }
  100% { transform: translate(100px,-180px) scale(.9); }
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
  max-width: 1280px;
  margin: 0 auto;
}

.hero-subtitle {
  font-size: 17px;
  color: rgba(220, 240, 240, 0.60);
  margin-top: 10px;
  margin-bottom: 32px;
  max-width: 700px;
  line-height: 1.6;
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
  grid-template-columns: repeat(3, 1fr);
  gap: 24px;
}

.sector-card {
  position: relative;
  cursor: pointer;
  min-height: 340px;
  border: 1px solid rgba(126,230,223,.14);
  border-radius: 24px;
  padding: 32px;
  background: linear-gradient(
    180deg,
    rgba(255,255,255,.052),
    rgba(255,255,255,.022)
  );
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

/* TABLE */
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
  target_value <- dplyr::recode(
    input_id,
    "go_finance" = "finance",
    "go_factoring" = "factoring",
    "go_fleet" = "fleet"
  )
  
  div(
    class = "sector-card",
    role = "button",
    tabindex = "0",
    onclick = sprintf(
      "var el=document.querySelector('#main_view [data-value=\"%s\"]'); if(el){el.click();}",
      target_value
    ),
    onkeydown = sprintf(
      "if(event.key==='Enter'||event.key===' '){event.preventDefault(); var el=document.querySelector('#main_view [data-value=\"%s\"]'); if(el){el.click();}}",
      target_value
    ),
    div(class = "sector-index", index),
    div(class = "sector-icon", bs_icon(icon, size = "24px")),
    div(class = "sector-title", title),
    div(class = "sector-copy", copy),
    div(class = "sector-meta", meta)
  )
}

landing_ui <- div(
  class = "hub-shell",
  
  div(
    class = "lava-background",
    div(class = "lava-blob lava-1"),
    div(class = "lava-blob lava-2"),
    div(class = "lava-blob lava-3"),
    div(class = "lava-blob lava-4")
  ),
  
  div(
    class = "hub-topline",
    div(class = "hub-period", "Finans • Faktoring • Filo")
  ),
  
  div(
    class = "hero-wrap",
    
    div(
      class = "hero-title",
      HTML("Üç finansal bakış.")
    ),
    
    div(
      class = "hero-subtitle",
      "Finansal performansı, sektörel karşılaştırmaları ve dönemsel değişimleri tek bakışta keşfetmek."
    ),
    
    div(
      class = "sector-grid",
      
      sector_card(
        "01 / FİNANS",
        "bank",
        "Finans",
        "Finansal yapı, kârlılık, fonlama, dönemsel değişim ve faaliyet gideri karşılaştırmaları.",
        "2023/12 • 2024/12 • 2025/12 • 2026/06",
        "go_finance"
      ),
      
      sector_card(
        "02 / FAKTORİNG",
        "diagram-3",
        "Faktoring",
        "Faktoring şirketleri için faaliyet, fonlama, kârlılık, verimlilik ve dönemsel değişim analizi.",
        "Emsal şirket karşılaştırma analizi",
        "go_factoring"
      ),
      
      sector_card(
        "03 / FİLO",
        "car-front",
        "Filo",
        "Filo kiralama şirketleri için varlık, performans, dönemsel değişim ve çalışan verimliliği analizi.",
        "2024/12 • 2025/06 • 2025/12 • 2026/06",
        "go_fleet"
      )
    )
  ),
  
  div(
    class = "hub-footer",
    span("R + Shiny")
  )
)
analysis_header <- function(title, subtitle, back_id) {
  div(
    class = "analysis-header",
    div(
      div(class = "analysis-title", title),
      div(class = "analysis-sub", subtitle)
    ),
    actionButton(
      back_id,
      "← Ana Sayfa",
      class = "btn btn-outline-secondary home-btn",
      onclick = "var el=document.querySelector('#main_view [data-value=\"home\"]'); if(el){el.click();}"
    )
  )
}

finance_ui <- div(
  class = "analysis-shell",
  analysis_header(
    "Finans Emsal Analizi",
    "Finansman şirketleri karşılaştırmalı analizi",
    "finance_home"
  ),
  
  bslib::navset_card_tab(
    
    bslib::nav_panel(
      "Genel Bakış",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput(
            "fin_overview_company",
            "Şirket",
            choices = finance_companies,
            selected = "VDF"
          ),
          shiny::selectInput(
            "fin_overview_metric",
            "Karşılaştırma kalemi",
            choices = fin_metric_choices_all,
            selected = "Varlıklar Toplamı"
          ),
          shiny::selectInput(
            "fin_overview_period",
            "Dönem",
            choices = sort(unique(finance$period), decreasing = TRUE),
            selected = "2026/06"
          )
        ),
        
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          
          div(
            class = "kpi-box",
            div(class = "kpi-label", "Toplam Aktif"),
            div(class = "kpi-value", shiny::textOutput("fin_vb_assets"))
          ),
          div(
            class = "kpi-box",
            div(class = "kpi-label", "Özkaynaklar"),
            div(class = "kpi-value", shiny::textOutput("fin_vb_equity"))
          ),
          div(
            class = "kpi-box",
            div(class = "kpi-label", "Esas Faaliyet Gelirleri"),
            div(class = "kpi-value", shiny::textOutput("fin_vb_income"))
          ),
          div(
            class = "kpi-box",
            div(class = "kpi-label", "Net Faaliyet Kârı"),
            div(class = "kpi-value", shiny::textOutput("fin_vb_profit"))
          )
        ),
        
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          bslib::card_header(shiny::textOutput("fin_overview_title")),
          plotly::plotlyOutput("fin_overview_plot2", height = "460px")
        )
      )
    ),
    
    finance_comparison_ui(
      "structure",
      "Finansal Yapı",
      fin_structure_metrics,
      "Varlıklar Toplamı"
    ),
    
    finance_comparison_ui(
      "profit",
      "Kârlılık",
      fin_profit_metrics,
      "Esas Faaliyet Gelirleri"
    ),
    
    bslib::nav_panel(
      "Finansal Oranlar",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput(
            "fin_ratio_metric2",
            "Karşılaştırma kalemi",
            choices = stats::setNames(names(ratio_labels), ratio_labels),
            selected = "equity_ratio"
          ),
          shiny::selectInput(
            "fin_ratio_company2",
            "Detay şirketi",
            choices = finance_companies,
            selected = "VDF"
          )
        ),
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          bslib::card_header("2026/06 Finansal Oran Karşılaştırması"),
          plotly::plotlyOutput("fin_ratio_compare2", height = "430px")
        ),
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          bslib::card_header("Seçilen Şirket | Finansal Oran Profili"),
          plotly::plotlyOutput("fin_ratio_detail2", height = "390px")
        )
      )
    ),
    
    bslib::nav_panel(
      "Faaliyet Giderleri",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput(
            "fin_expense_metric2",
            "Karşılaştırma kalemi",
            choices = c(
              "Esas Faaliyet Giderleri" = "ESAS FAALİYET GİDERLERİ (-)",
              "Personel Giderleri" = "Personel Giderleri",
              "Genel İşletme Giderleri" = "Genel İşletme Giderleri",
              "Kıdem Tazminatı" = "Kıdem Tazminatı Karşılığı Gideri"
            ),
            selected = "ESAS FAALİYET GİDERLERİ (-)"
          )
        ),
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          bslib::card_header("2026/06 Faaliyet Gideri Karşılaştırması"),
          plotly::plotlyOutput("fin_expense_plot2", height = "470px")
        )
      )
    ),
    
    bslib::nav_panel(
      "Dönemsel Değişim",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput(
            "fin_change_metric2",
            "Karşılaştırma kalemi",
            choices = fin_metric_choices_all,
            selected = "Varlıklar Toplamı"
          ),
          shiny::helpText(
            "31.12.2025 ve 30.06.2026 değerlerini doğrudan karşılaştırır."
          )
        ),
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          bslib::card_header("31.12.2025 → 30.06.2026 Karşılaştırması"),
          plotly::plotlyOutput("fin_change_plot2", height = "410px")
        ),
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          bslib::card_header("Şirket Bazında Mutlak Değişim"),
          plotly::plotlyOutput("fin_absolute_change_plot2", height = "390px")
        )
      )
    ),
    
    bslib::nav_panel(
      "Veri Kontrol",
      bslib::card(
        class = "metric-card",
        full_screen = TRUE,
        bslib::card_header("Kullanılan Finans Master Veri"),
        DT::DTOutput("fin_table2")
      )
    )
  )
)

factoring_ui <- div(
  class = "analysis-shell",
  analysis_header(
    "Faktoring Emsal Analizi",
    "Faktoring şirketleri karşılaştırmalı analizi",
    "factoring_home"
  ),
  
  if (!factoring_loaded) {
    div(
      class = "note-strip",
      if (!file.exists(factoring_path)) {
        HTML(
          "<b>Faktoring veri dosyası bulunamadı.</b><br>
          <code>factoring_master.csv</code> dosyasını
          <code>data/factoring/factoring_master.csv</code> konumuna kopyalayın."
        )
      } else {
        HTML(
          paste0(
            "<b>Faktoring CSV dosyası bulundu ancak yapısı otomatik olarak eşleştirilemedi.</b><br>",
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
  analysis_header(
    "Filo Emsal Analizi",
    "Filo kiralama şirketleri karşılaştırmalı analizi",
    "fleet_home"
  ),
  
  bslib::navset_card_tab(
    
    bslib::nav_panel(
      "Genel Bakış",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput(
            "fleet_overview_company2",
            "Şirket",
            choices = fleet_companies,
            selected = if ("VDF Filo" %in% fleet_companies) "VDF Filo" else fleet_companies[1]
          ),
          shiny::selectInput(
            "fleet_overview_metric2",
            "Karşılaştırma kalemi",
            choices = fleet_metric_choices,
            selected = if ("leased_assets_net" %in% unname(fleet_metric_choices)) {
              "leased_assets_net"
            } else {
              unname(fleet_metric_choices)[1]
            }
          ),
          shiny::selectInput(
            "fleet_overview_period2",
            "Dönem",
            choices = sort(unique(fleet$period), decreasing = TRUE),
            selected = "2026/06"
          ),
          shiny::helpText(
            "TEB Arval 2026/06 raporu yayımlanmadığı için bu dönemde veri bulunmayabilir."
          )
        ),
        
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          div(
            class = "kpi-box",
            div(class = "kpi-label", "Şirket"),
            div(class = "kpi-value", shiny::textOutput("fleet_vb_company2"))
          ),
          div(
            class = "kpi-box",
            div(class = "kpi-label", "Seçilen Kalem"),
            div(class = "kpi-value", shiny::textOutput("fleet_vb_metric2"))
          ),
          div(
            class = "kpi-box",
            div(class = "kpi-label", "Şirket Değeri"),
            div(class = "kpi-value", shiny::textOutput("fleet_vb_value2"))
          ),
          div(
            class = "kpi-box",
            div(class = "kpi-label", "Şirket Sırası"),
            div(class = "kpi-value", shiny::textOutput("fleet_vb_rank2"))
          )
        ),
        
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          bslib::card_header(shiny::textOutput("fleet_overview_title2")),
          plotly::plotlyOutput("fleet_overview_plot2", height = "460px")
        )
      )
    ),
    
    fleet_comparison_ui(
      "assets",
      "Filo Varlıkları",
      fleet_asset_choices,
      "leased_assets_net"
    ),
    
    fleet_comparison_ui(
      "performance",
      "Finansman ve Performans",
      fleet_performance_choices,
      unname(fleet_performance_choices)[1]
    ),
    
    bslib::nav_panel(
      "Operasyonel Verimlilik",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput(
            "fleet_employee_metric2",
            "Karşılaştırma kalemi",
            choices = fleet_metric_choices,
            selected = if ("leased_assets_net" %in% unname(fleet_metric_choices)) {
              "leased_assets_net"
            } else {
              unname(fleet_metric_choices)[1]
            }
          ),
          shiny::selectInput(
            "fleet_employee_period2",
            "Dönem",
            choices = sort(unique(fleet$period), decreasing = TRUE),
            selected = "2026/06"
          )
        ),
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          bslib::card_header("Çalışan Başına Seçilen Kalem"),
          plotly::plotlyOutput("fleet_employee_plot2", height = "450px")
        ),
        div(
          class = "note-strip",
          "Döneme ait çalışan sayısı varsa doğrudan kullanılır; yoksa veri setindeki fallback çalışan sayısı kullanılır."
        )
      )
    ),
    
    bslib::nav_panel(
      "Dönemsel Değişim",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput(
            "fleet_change_metric2",
            "Karşılaştırma kalemi",
            choices = fleet_metric_choices,
            selected = if ("leased_assets_net" %in% unname(fleet_metric_choices)) {
              "leased_assets_net"
            } else {
              unname(fleet_metric_choices)[1]
            }
          ),
          shiny::helpText(
            "31.12.2025 ve 30.06.2026 değerlerini doğrudan karşılaştırır."
          )
        ),
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          bslib::card_header("31.12.2025 → 30.06.2026 Karşılaştırması"),
          plotly::plotlyOutput("fleet_change_plot2", height = "410px")
        ),
        bslib::card(
          class = "metric-card",
          full_screen = TRUE,
          bslib::card_header("Şirket Bazında Mutlak Değişim"),
          plotly::plotlyOutput("fleet_absolute_change_plot2", height = "390px")
        )
      )
    ),
    
    bslib::nav_panel(
      "Veri Kontrol",
      bslib::card(
        class = "metric-card",
        full_screen = TRUE,
        bslib::card_header("Kullanılan Filo Master Veri"),
        DT::DTOutput("fleet_table2")
      )
    )
  )
)


ui <- bslib::page_fillable(
  theme = theme_hub,
  padding = 0,
  tags$head(tags$style(HTML(hub_css))),
  navset_hidden(
    id = "main_view",
    selected = "home",
    nav_panel("Ana Sayfa", value = "home", landing_ui),
    nav_panel("Finans", value = "finance", finance_ui),
    nav_panel("Faktoring", value = "factoring", factoring_ui),
    nav_panel("Filo", value = "fleet", fleet_ui)
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$go_finance, nav_select("main_view", selected = "finance"))
  observeEvent(input$go_factoring, nav_select("main_view", selected = "factoring"))
  observeEvent(input$go_fleet, nav_select("main_view", selected = "fleet"))
  observeEvent(input$finance_home, nav_select("main_view", selected = "home"))
  observeEvent(input$factoring_home, nav_select("main_view", selected = "home"))
  observeEvent(input$fleet_home, nav_select("main_view", selected = "home"))
  
  
  fin_get_value2 <- function(company_value, item_value, period_value = "2026/06") {
    x <- finance |>
      dplyr::filter(
        .data$company == .env$company_value,
        .data$period == .env$period_value,
        .data$item == .env$item_value
      ) |>
      dplyr::pull(.data$value_mn_tl)
    if (length(x) == 0) return("—")
    fmt_mn(x[[1]])
  }
  
  output$fin_vb_assets <- renderText({
    req(input$fin_overview_company, input$fin_overview_period)
    fin_get_value2(input$fin_overview_company, "Varlıklar Toplamı", input$fin_overview_period)
  })
  output$fin_vb_equity <- renderText({
    req(input$fin_overview_company, input$fin_overview_period)
    fin_get_value2(input$fin_overview_company, "Özkaynaklar", input$fin_overview_period)
  })
  output$fin_vb_income <- renderText({
    req(input$fin_overview_company, input$fin_overview_period)
    fin_get_value2(input$fin_overview_company, "Esas Faaliyet Gelirleri", input$fin_overview_period)
  })
  output$fin_vb_profit <- renderText({
    req(input$fin_overview_company, input$fin_overview_period)
    fin_get_value2(input$fin_overview_company, "Net Faaliyet Kârı (Zararı)", input$fin_overview_period)
  })
  output$fin_overview_title <- renderText({
    req(input$fin_overview_metric, input$fin_overview_period)
    paste0(input$fin_overview_metric, " | Şirket Karşılaştırması")
  })
  output$fin_overview_plot2 <- renderPlotly({
    req(input$fin_overview_metric, input$fin_overview_period, input$fin_overview_company)
    fin_plot_company_comparison(
      input$fin_overview_metric,
      input$fin_overview_period,
      input$fin_overview_company
    )
  })
  
  fin_wire_comparison2 <- function(prefix, metrics) {
    metric_input <- paste0("fin_", prefix, "_metric")
    company_input <- paste0("fin_", prefix, "_company")
    top_source <- paste0("fin_", prefix, "_top")
    detail_source <- paste0("fin_", prefix, "_detail")
    
    observeEvent(plotly::event_data("plotly_click", source = top_source), {
      click <- plotly::event_data("plotly_click", source = top_source)
      if (!is.null(click$key) && click$key %in% finance_companies) {
        updateSelectInput(session, company_input, selected = click$key)
      }
    })
    
    output[[top_source]] <- renderPlotly({
      req(input[[metric_input]], input[[company_input]])
      fin_plot_company_comparison(
        input[[metric_input]], "2026/06", input[[company_input]], top_source
      )
    })
    
    output[[detail_source]] <- renderPlotly({
      req(input[[company_input]])
      fin_plot_company_detail(
        input[[company_input]], metrics, "2026/06", detail_source
      )
    })
    
  }
  
  fin_wire_comparison2("structure", fin_structure_metrics)
  fin_wire_comparison2("profit", fin_profit_metrics)
  
  output$fin_ratio_compare2 <- renderPlotly({
    req(input$fin_ratio_metric2, input$fin_ratio_company2)
    lbl <- ratio_labels[[input$fin_ratio_metric2]]
    d <- finance_wide |>
      dplyr::filter(.data$period == "2026/06") |>
      dplyr::transmute(
        company = .data$company,
        value = .data[[input$fin_ratio_metric2]],
        highlight = .data$company == input$fin_ratio_company2
      ) |>
      dplyr::arrange(.data$value)
    
    p <- ggplot(d, aes(
      x = reorder(company, value), y = value,
      fill = highlight,
      text = paste0("<b>", company, "</b><br>", lbl, ": ", round(value, 1), "%")
    )) +
      geom_col(width = .56) +
      scale_fill_manual(values = c("TRUE"="#25C7C9","FALSE"="#65858A"), guide="none") +
      coord_flip() +
      labs(x=NULL,y=NULL,title=paste0(lbl," | 2026/06")) +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank(),
            plot.title=element_text(face="bold"))
    ggplotly(p, tooltip="text") |> config(displayModeBar=FALSE)
  })
  
  output$fin_ratio_detail2 <- renderPlotly({
    req(input$fin_ratio_company2)
    d <- finance_wide |>
      dplyr::filter(
        .data$period == "2026/06",
        .data$company == input$fin_ratio_company2
      ) |>
      dplyr::select(dplyr::all_of(names(ratio_labels))) |>
      tidyr::pivot_longer(everything(), names_to="ratio", values_to="value") |>
      dplyr::mutate(label = unname(ratio_labels[ratio]))
    
    p <- ggplot(d, aes(
      x = reorder(label, value), y = value,
      text = paste0("<b>",label,"</b><br>",round(value,1),"%")
    )) +
      geom_col(width=.56, fill="#63C9C8") +
      coord_flip() +
      labs(x=NULL,y=NULL,title=paste0(input$fin_ratio_company2," | Finansal Oran Profili")) +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank(),
            plot.title=element_text(face="bold"))
    ggplotly(p, tooltip="text") |> config(displayModeBar=FALSE)
  })
  
  output$fin_expense_plot2 <- renderPlotly({
    req(input$fin_expense_metric2)
    d <- get_finance_expenses() |>
      dplyr::filter(.data$item == input$fin_expense_metric2) |>
      dplyr::filter(!is.na(.data$value)) |>
      dplyr::arrange(.data$value)
    
    p <- ggplot(d, aes(
      x = reorder(company,value), y=value,
      text=paste0("<b>",company,"</b><br>",fmt_mn(value/1000))
    )) +
      geom_col(width=.56, fill="#63C9C8") +
      coord_flip() +
      labs(x=NULL,y=NULL,title=paste0(input$fin_expense_metric2," | 2026/06")) +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank(),
            plot.title=element_text(face="bold"))
    ggplotly(p, tooltip="text") |> config(displayModeBar=FALSE)
  })
  
  output$fin_change_plot2 <- renderPlotly({
    req(input$fin_change_metric2)
    d <- finance |>
      dplyr::filter(
        .data$item == input$fin_change_metric2,
        .data$period %in% c("2025/12","2026/06")
      ) |>
      dplyr::mutate(period=factor(.data$period,levels=c("2025/12","2026/06")))
    
    p <- ggplot(d,aes(
      x=company,y=abs(value_mn_tl),fill=period,
      text=paste0("<b>",company,"</b><br>Dönem: ",period,"<br>",fmt_mn(value_mn_tl))
    )) +
      geom_col(position="dodge",width=.66) +
      scale_fill_manual(values=c("2025/12"="#65858A","2026/06"="#25C7C9")) +
      labs(x=NULL,y=NULL,fill="Dönem",
           title=paste0(input$fin_change_metric2," | Dönemsel Karşılaştırma")) +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.x=element_blank(),axis.text.x=element_text(angle=25,hjust=1),
            plot.title=element_text(face="bold"))
    ggplotly(p,tooltip="text") |> config(displayModeBar=FALSE)
  })
  
  output$fin_absolute_change_plot2 <- renderPlotly({
    req(input$fin_change_metric2)
    d <- finance |>
      dplyr::filter(
        .data$item == input$fin_change_metric2,
        .data$period %in% c("2025/12","2026/06")
      ) |>
      dplyr::select(company,period,value_mn_tl) |>
      tidyr::pivot_wider(names_from=period,values_from=value_mn_tl) |>
      dplyr::filter(!is.na(`2025/12`),!is.na(`2026/06`)) |>
      dplyr::mutate(change=`2026/06`-`2025/12`) |>
      dplyr::arrange(change)
    
    p <- ggplot(d,aes(
      x=reorder(company,change),y=change,
      text=paste0("<b>",company,"</b><br>2025/12: ",fmt_mn(`2025/12`),
                  "<br>2026/06: ",fmt_mn(`2026/06`),"<br>Değişim: ",fmt_mn(change))
    )) +
      geom_col(width=.56,fill="#63C9C8") +
      geom_hline(yintercept=0,linewidth=.4) +
      coord_flip() +
      labs(x=NULL,y=NULL,title=paste0(input$fin_change_metric2," | Mutlak Değişim")) +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank(),
            plot.title=element_text(face="bold"))
    ggplotly(p,tooltip="text") |> config(displayModeBar=FALSE)
  })
  
  output$fin_table2 <- renderDT({
    finance |>
      dplyr::arrange(company,desc(period),item) |>
      dplyr::mutate(value = fmt_mn(value_mn_tl)) |>
      dplyr::select(company,period,item,value) |>
      DT::datatable(rownames=FALSE,filter="top",
                    options=list(pageLength=20,scrollX=TRUE))
  })
  
  
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
        text = paste0("<b>", company, "</b><br>Dönem: ", period, "<br>", input$fin_metric, ": ", fmt_mn(value_mn_tl))
      )
    ) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      scale_color_manual(values = finance_company_colors) +
      scale_y_continuous(labels = function(v) fmt_mn(v)) +
      labs(x = NULL, y = NULL, color = NULL, title = paste(input$fin_metric, "Eğilimi")) +
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
      labs(x = NULL, y = NULL, fill = "%", title = "Finansal Oran Karşılaştırması | 2026/06") +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 25, hjust = 1),
            plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$fin_expense_plot <- renderPlotly({
    x <- get_finance_expenses() |>
      filter(item == "ESAS FAALİYET GİDERLERİ (-)") |>
      arrange(value)
    
    p <- ggplot(
      x,
      aes(
        x = value,
        y = reorder(company, value),
        fill = company,
        text = paste0("<b>", company, "</b><br>Faaliyet Giderleri: ", fmt_mn(value / 1000))
      )
    ) +
      geom_col(width = .5, show.legend = FALSE) +
      scale_fill_manual(values = finance_company_colors) +
      scale_x_continuous(labels = function(v) fmt_mn(v / 1000)) +
      labs(x = NULL, y = NULL, title = "Faaliyet Giderleri | 2026/06") +
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
  
  
  output$fleet_vb_company2 <- renderText({
    req(input$fleet_overview_company2)
    input$fleet_overview_company2
  })
  output$fleet_vb_metric2 <- renderText({
    req(input$fleet_overview_metric2)
    x <- fleet |>
      filter(metric_id == input$fleet_overview_metric2) |>
      pull(metric) |>
      unique()
    if(length(x)) fleet_metric_label(x[[1]]) else input$fleet_overview_metric2
  })
  output$fleet_vb_value2 <- renderText({
    req(input$fleet_overview_company2,input$fleet_overview_metric2,input$fleet_overview_period2)
    x <- fleet |>
      filter(
        company == input$fleet_overview_company2,
        metric_id == input$fleet_overview_metric2,
        period == input$fleet_overview_period2
      ) |>
      pull(value)
    if(length(x)==0) return("—")
    fmt_tl(x[[1]])
  })
  output$fleet_vb_rank2 <- renderText({
    req(input$fleet_overview_company2,input$fleet_overview_metric2,input$fleet_overview_period2)
    d <- fleet |>
      filter(
        metric_id == input$fleet_overview_metric2,
        period == input$fleet_overview_period2
      ) |>
      arrange(desc(value)) |>
      mutate(rank=row_number())
    x <- d |> filter(company==input$fleet_overview_company2)
    if(nrow(x)==0) return("—")
    paste0("#",x$rank," / ",nrow(d))
  })
  output$fleet_overview_title2 <- renderText({
    req(input$fleet_overview_metric2)
    m <- fleet |>
      filter(metric_id==input$fleet_overview_metric2) |>
      pull(metric) |>
      unique()
    paste0(if(length(m)) fleet_metric_label(m[[1]]) else input$fleet_overview_metric2,
           " | Şirket Karşılaştırması")
  })
  output$fleet_overview_plot2 <- renderPlotly({
    req(input$fleet_overview_metric2,input$fleet_overview_period2,input$fleet_overview_company2)
    fleet_plot_company_comparison(
      input$fleet_overview_metric2,
      input$fleet_overview_period2,
      input$fleet_overview_company2
    )
  })
  
  fleet_wire_comparison2 <- function(prefix, choices) {
    metric_input <- paste0("fleet_",prefix,"_metric")
    company_input <- paste0("fleet_",prefix,"_company")
    top_source <- paste0("fleet_",prefix,"_top")
    detail_source <- paste0("fleet_",prefix,"_detail")
    
    observeEvent(plotly::event_data("plotly_click",source=top_source),{
      click <- plotly::event_data("plotly_click",source=top_source)
      if(!is.null(click$key) && click$key %in% fleet_companies){
        updateSelectInput(session,company_input,selected=click$key)
      }
    })
    
    output[[top_source]] <- renderPlotly({
      req(input[[metric_input]],input[[company_input]])
      fleet_plot_company_comparison(
        input[[metric_input]],"2026/06",input[[company_input]],top_source
      )
    })
    output[[detail_source]] <- renderPlotly({
      req(input[[company_input]])
      fleet_plot_company_detail(
        input[[company_input]],choices,"2026/06",detail_source
      )
    })
  }
  
  fleet_wire_comparison2("assets",fleet_asset_choices)
  fleet_wire_comparison2("performance",fleet_performance_choices)
  
  output$fleet_employee_plot2 <- renderPlotly({
    req(input$fleet_employee_metric2,input$fleet_employee_period2)
    d <- fleet |>
      filter(
        period==input$fleet_employee_period2,
        metric_id==input$fleet_employee_metric2
      ) |>
      select(company,period,value) |>
      left_join(fleet_employees,by=c("company","period")) |>
      mutate(value_per_employee=value/employees) |>
      filter(!is.na(value_per_employee)) |>
      arrange(value_per_employee)
    
    m <- fleet |>
      filter(metric_id==input$fleet_employee_metric2) |>
      pull(metric) |>
      unique()
    mt <- if(length(m)) m[[1]] else input$fleet_employee_metric2
    
    p <- ggplot(d,aes(
      x=reorder(company,value_per_employee),y=value_per_employee,
      text=paste0("<b>",company,"</b><br>Çalışan: ",employees,
                  "<br>",mt," / Çalışan: ",fmt_tl(value_per_employee))
    )) +
      geom_col(width=.56,fill="#63C9C8") +
      coord_flip() +
      labs(x=NULL,y=NULL,title=paste0(mt," | Çalışan Başına")) +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank(),
            plot.title=element_text(face="bold"))
    ggplotly(p,tooltip="text") |> config(displayModeBar=FALSE)
  })
  
  output$fleet_change_plot2 <- renderPlotly({
    req(input$fleet_change_metric2)
    d <- fleet |>
      filter(
        metric_id==input$fleet_change_metric2,
        period %in% c("2025/12","2026/06")
      ) |>
      mutate(period=factor(period,levels=c("2025/12","2026/06")))
    m <- unique(d$metric); mt <- if(length(m)) m[[1]] else input$fleet_change_metric2
    
    p <- ggplot(d,aes(
      x=company,y=abs(value),fill=period,
      text=paste0("<b>",company,"</b><br>Dönem: ",period,"<br>",fmt_tl(value))
    )) +
      geom_col(position="dodge",width=.66) +
      scale_fill_manual(values=c("2025/12"="#65858A","2026/06"="#25C7C9")) +
      labs(x=NULL,y=NULL,fill="Dönem",title=paste0(mt," | Dönemsel Karşılaştırma")) +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.x=element_blank(),axis.text.x=element_text(angle=25,hjust=1),
            plot.title=element_text(face="bold"))
    ggplotly(p,tooltip="text") |> config(displayModeBar=FALSE)
  })
  
  output$fleet_absolute_change_plot2 <- renderPlotly({
    req(input$fleet_change_metric2)
    d0 <- fleet |>
      filter(
        metric_id==input$fleet_change_metric2,
        period %in% c("2025/12","2026/06")
      )
    mt <- if(nrow(d0)) unique(d0$metric)[[1]] else input$fleet_change_metric2
    d <- d0 |>
      select(company,period,value) |>
      tidyr::pivot_wider(names_from=period,values_from=value) |>
      filter(!is.na(`2025/12`),!is.na(`2026/06`)) |>
      mutate(change=`2026/06`-`2025/12`) |>
      arrange(change)
    
    p <- ggplot(d,aes(
      x=reorder(company,change),y=change,
      text=paste0("<b>",company,"</b><br>2025/12: ",fmt_tl(`2025/12`),
                  "<br>2026/06: ",fmt_tl(`2026/06`),"<br>Değişim: ",fmt_tl(change))
    )) +
      geom_col(width=.56,fill="#63C9C8") +
      geom_hline(yintercept=0,linewidth=.4) +
      coord_flip() +
      labs(x=NULL,y=NULL,title=paste0(mt," | Mutlak Değişim")) +
      theme_minimal(base_size=13) +
      theme(panel.grid.major.y=element_blank(),panel.grid.minor=element_blank(),
            plot.title=element_text(face="bold"))
    ggplotly(p,tooltip="text") |> config(displayModeBar=FALSE)
  })
  
  output$fleet_table2 <- renderDT({
    fleet |>
      arrange(company,desc(period),metric) |>
      mutate(value_display=fmt_tl(value)) |>
      select(company,period,metric,value_display) |>
      DT::datatable(rownames=FALSE,filter="top",
                    options=list(pageLength=20,scrollX=TRUE))
  })
  
  
  fleet_period_data <- reactive({
    fleet |>
      filter(period == input$fleet_period, metric_id == input$fleet_metric)
  })
  
  fleet_metric_name <- reactive({
    x <- fleet |>
      filter(metric_id == input$fleet_metric) |>
      distinct(metric) |>
      pull(metric) |>
      first()
    fleet_metric_label(x)
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
        text = paste0("<b>", company, "</b><br>Dönem: ", period, "<br>", fleet_metric_name(), ": ", fmt_tl(value))
      )
    ) +
      geom_line(linewidth = 1.6) +
      geom_point(size = 3.2) +
      scale_color_manual(values = fleet_company_colors) +
      scale_y_continuous(labels = function(v) fmt_tl(v)) +
      labs(x = NULL, y = NULL, color = NULL, title = paste(fleet_metric_name(), "Eğilimi")) +
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
          employee_source == "reported" ~ "Raporlanan çalışan sayısı",
          employee_source == "current" ~ "Güncel çalışan sayısı",
          employee_source == "fallback_2025_12" ~ "2025/12 çalışan sayısı kullanılarak yaklaşık değer",
          TRUE ~ "Çalışan sayısı mevcut değil"
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
          "Çalışan Sayısı: ", employees, "<br>",
          source_note, "<br>",
          fleet_metric_name(), " / Çalışan: ", fmt_tl(value_per_employee)
        )
      )
    ) +
      geom_col(width = .48, fill = "#A6E7E7") +
      scale_x_continuous(labels = function(v) fmt_tl(v)) +
      labs(x = paste(fleet_metric_name(), "çalışan başına"), y = NULL,
           title = paste(fleet_metric_name(), "çalışan başına |", input$fleet_period)) +
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

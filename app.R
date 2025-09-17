# app.R — Fundação 1Bi • Recomendador Top-5 (ALS Implícito BM25)
# Versão: 2025-09-17 — prioriza RDS p/ fatores, evita segfaults de NPY

# =====================
# Pacotes
# =====================
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(readr)
  library(arrow)
  library(stringr)
  library(tibble)
  library(tidyr)
  # opcional: reticulate só será usado se existir, não é obrigatório
})

# =====================
# Tema (CSS)
# =====================
THEME_CSS <- "\n:root{\n  --bi-primary:#6E29F2;\n  --bi-secondary:#B5179E;\n  --bi-accent:#FF4D9D;\n  --bi-bg:#0E0A17;\n  --bi-surface:#1A1430;\n  --bi-surface-2:#221B44;\n  --bi-border:#3A2F63;\n  --bi-text:#FFFFFF;\n  --bi-muted:#E4D8FF;\n  --bi-muted-2:#CDBEF7;\n}\n\n*{ box-sizing:border-box; }\nhtml,body{ margin:0; padding:0; background:var(--bi-bg)!important; color:var(--bi-text)!important; font-family: ui-sans-serif, system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial;}\n\n.header{ position:sticky; top:0; z-index:1000; background:linear-gradient(92deg,var(--bi-primary),var(--bi-secondary))!important; color:#fff!important; padding:18px 24px; box-shadow:0 10px 30px rgba(0,0,0,.28);}\n.header .brand{ font-weight:900; font-size:22px; letter-spacing:.2px; }\n.header .subtitle{ color:#FFF; opacity:.98; font-size:14px; margin-top:2px; }\n\n.container{ padding:28px 24px 36px; max-width:1360px; margin:0 auto; }\n.grid{ display:grid; gap:22px; grid-template-columns:repeat(12,1fr);}\n.col-6{ grid-column:span 6; }\n.col-7{ grid-column:span 7; }\n.col-5{ grid-column:span 5; }\n@media(max-width:1000px){ .col-7,.col-5,.col-6{ grid-column:span 12; }}\n\n.card{ background:linear-gradient(180deg,var(--bi-surface),var(--bi-surface-2)); border:1px solid var(--bi-border); border-radius:18px; padding:18px; box-shadow:0 18px 40px rgba(0,0,0,.35); overflow:hidden;}\n.card-header{ font-weight:900; font-size:28px; line-height:1.15; margin:6px 0 12px; color:#FFFFFF; text-shadow:0 2px 6px rgba(0,0,0,.5); letter-spacing:.2px;}\n\n.shiny-input-container{ width:100%!important; max-width:100%!important; }\n.shiny-input-container label{ color:var(--bi-muted)!important; font-weight:700; letter-spacing:.2px; }\ninput.form-control,input[type='text']{ width:100%!important; background:#140F29!important; border:1px solid var(--bi-border)!important; color:var(--bi-text)!important; padding:14px 16px; border-radius:12px; outline:none; font-size:18px;}\ninput.form-control::placeholder{ color:var(--bi-muted-2)!important; }\nbutton,.btn{ background:linear-gradient(92deg,var(--bi-primary),var(--bi-accent))!important; border:0; color:#fff!important; padding:12px 16px; border-radius:14px; font-weight:900; font-size:18px; box-shadow:0 10px 22px rgba(110,41,242,.45);}\n\n.table-zone{ width:100%; margin-top:12px; overflow-x:auto;}\n.table-zone table{ width:100%; border-collapse:collapse; font-size:16px; color:var(--bi-text); table-layout:fixed;}\n.table-zone th,.table-zone td{ border-bottom:1px solid var(--bi-border); padding:10px 12px;}\n.table-zone thead th{ text-align:left; font-weight:800; background:var(--bi-surface-2)!important; color:#FFF;}\n.table-zone table th:nth-child(1),.table-zone table td:nth-child(1){ width:64px;}\n.table-zone table th:nth-child(3),.table-zone table td:nth-child(3){ width:160px; text-align:right; white-space:nowrap; font-variant-numeric: tabular-nums;}\n.table-zone table th:nth-child(2),.table-zone table td:nth-child(2){ width:calc(100% - 64px - 160px - 72px); max-width:100%; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;}\n@media (max-width: 900px){ .table-zone table th:nth-child(3), .table-zone table td:nth-child(3){ width:138px; } .table-zone table th:nth-child(2), .table-zone table td:nth-child(2){ width:calc(100% - 64px - 138px - 72px);} }\n.notice{ margin-top:22px; padding:14px 16px; background:rgba(255,255,255,.05); border:1px dashed var(--bi-border); border-radius:14px;}\n.footer{ color:var(--bi-muted); font-size:12px; margin-top:10px;}\n"

# =====================
# Constantes e utilitários
# =====================
TOPK <- 5L
MAX_ITEM_CHARS <- 64L
CIRCLED <- setNames(intToUtf8(9311 + 1:10, multiple = TRUE), as.character(1:10))
ARTIFACTS_DIR <- "artifacts"
SCORE_PRESENTATION <- "bucket"  # "softmax" | "bucket" | "hide"
SOFTMAX_BETA <- 3
`%||%` <- function(a, b) if (!is.null(a)) a else b

norm_key <- function(x){
  x <- as.character(x)
  y <- tolower(trimws(x))
  y <- gsub("^item[_-]*", "", y)
  y <- gsub("[^0-9a-z]+", "", y)
  y <- sub("^0+", "", y)
  y[y == ""] <- x[y == ""]
  y
}

score_presentation <- function(scores, mode = SCORE_PRESENTATION, beta = SOFTMAX_BETA){
  s <- as.numeric(scores)
  if (length(s) == 0 || all(!is.finite(s))) return(list(text = rep("", length(s)), prob = rep(NA_real_, length(s))))
  if (mode == "hide") return(list(text = rep("", length(s)), prob = rep(NA_real_, length(s))))
  if (mode == "softmax"){
    m <- max(s, na.rm = TRUE); z <- s - m
    p <- exp(beta * z); p <- p / sum(p, na.rm = TRUE)
    txt <- paste0(round(100 * p), "%")
    return(list(text = txt, prob = p))
  }
  if (mode == "bucket"){
    rng <- range(s[is.finite(s)])
    denom <- rng[2] - rng[1]; if (!is.finite(denom) || denom == 0) denom <- 1
    z <- (s - rng[1]) / denom
    lab <- ifelse(z >= 0.75, "Alta", ifelse(z >= 0.45, "Média", "Baixa"))
    return(list(text = lab, prob = z))
  }
  list(text = as.character(round(s, 4)), prob = s)
}

score_col_name <- function(){
  switch(SCORE_PRESENTATION,
         softmax = "Relevância",
         bucket  = "Confiança",
         hide    = NULL,
         "Score")
}

add_score_column <- function(df){
  nm <- score_col_name()
  if (is.null(nm) || !"score" %in% names(df)) return(df)
  if ("userId" %in% names(df)){
    df %>% group_by(userId) %>% group_modify(function(.x, .k){
      sp <- score_presentation(.x$score); .x[[nm]] <- sp$text; .x
    }) %>% ungroup()
  } else {
    sp <- score_presentation(df$score); df[[nm]] <- sp$text; df
  }
}

.has_required <- function(d){
  file.exists(file.path(d, "item_factors.rds")) || file.exists(file.path(d, "item_factors.parquet")) ||
    file.exists(file.path(d, "item_factors.npy")) &&
    (file.exists(file.path(d, "item_index.parquet")) || file.exists(file.path(d, "item_index.csv")))
}

.read_item_index <- function(champ){
  p_parq <- file.path(champ, "item_index.parquet")
  p_csv  <- file.path(champ, "item_index.csv")
  if (file.exists(p_parq)) return(as.data.frame(arrow::read_parquet(p_parq)))
  if (file.exists(p_csv))  return(suppressMessages(readr::read_csv(p_csv, show_col_types = FALSE)))
  stop(sprintf("Nem 'item_index.parquet' nem 'item_index.csv' em %s", champ))
}

resolve_champ_dir <- function(){
  ch <- Sys.getenv("CHAMP_DIR", unset = "")
  if (nzchar(ch)){
    cand <- if (dir.exists(ch)) ch else file.path(ARTIFACTS_DIR, ch)
    if (.has_required(cand)) return(cand)
  }
  ptr <- file.path(ARTIFACTS_DIR, "CHAMP_POINTER.txt")
  if (file.exists(ptr)){
    target <- trimws(readLines(ptr, warn = FALSE)[1])
    cand <- if (dir.exists(target)) target else file.path(ARTIFACTS_DIR, target)
    if (.has_required(cand)) return(cand)
  }
  dirs <- list.dirs(ARTIFACTS_DIR, recursive = FALSE, full.names = TRUE)
  champs <- dirs[grepl("^champ_", basename(dirs))]
  valid <- Filter(.has_required, champs)
  if (!length(valid)) stop("Nenhum champ_* válido com item_index.(parquet|csv) + item_factors.{rds|parquet|npy} em 'artifacts/'.")
  valid[which.max(file.info(valid)$mtime)]
}

# ---------- NOVO: loader que PRIORIZA .RDS, depois .parquet e por fim .npy ----------
safe_load_matrix <- function(champ, base){
  p_rds  <- file.path(champ, paste0(base, ".rds"))
  if (file.exists(p_rds)) return(readRDS(p_rds))

  p_parq <- file.path(champ, paste0(base, ".parquet"))
  if (file.exists(p_parq)) return(as.matrix(arrow::read_parquet(p_parq)))

  p_npy  <- file.path(champ, paste0(base, ".npy"))
  if (file.exists(p_npy) && requireNamespace("reticulate", quietly = TRUE)){
    np <- reticulate::import("numpy", delay_load = TRUE)
    arr <- np$load(p_npy, allow_pickle = FALSE)
    return(reticulate::py_to_r(arr))
  }

  stop(sprintf("Não consegui carregar %s.{rds|parquet|npy} em %s", base, champ))
}

load_catalog <- function(champ_dir){
  candidates <- c(
    file.path(champ_dir, "catalog_items.parquet"),
    file.path(champ_dir, "catalog_items.csv"),
    file.path(champ_dir, "aulas_aprendizap.parquet"),
    file.path(champ_dir, "aulas_aprendizap.csv"),
    file.path("data", "aulas_aprendizap.parquet"),
    file.path("data", "aulas_aprendizap.csv"),
    "aulas_aprendizap.parquet",
    "aulas_aprendizap.csv",
    "stg_formation.csv"
  )
  read_any <- function(p){
    ext <- tolower(tools::file_ext(p))
    if (ext == "parquet") as.data.frame(arrow::read_parquet(p))
    else suppressMessages(readr::read_csv(p, show_col_types = FALSE, progress = FALSE))
  }
  first_present <- function(cands, pool){
    pool_l <- tolower(pool)
    for (c in cands){
      pos <- match(tolower(c), pool_l); if (!is.na(pos)) return(pool[pos])
    }
    NULL
  }
  for (p in candidates){
    if (!file.exists(p)) next
    raw <- tryCatch(read_any(p), error = function(e) NULL)
    if (is.null(raw) || nrow(raw) == 0) next
    cols <- colnames(raw)
    col_item <- first_present(c("itemId","lesson_id","lessonId","id_aula","conteudo_id","content_id","id","lesson","lessonid"), cols)
    col_name <- first_present(c("itemName","lesson_title","lessonTitle","titulo","nome","name","title","descricao","description"), cols)
    col_url  <- first_present(c("itemUrl","url","link","href"), cols)
    col_disc <- first_present(c("disciplina","subject","materia","disciplina_nome","subject_name"), cols)
    if (is.null(col_item) || is.null(col_name)) next
    keep <- c(col_item, col_name, col_url, col_disc)
    keep <- keep[!sapply(keep, is.null)]
    out <- raw[, keep, drop = FALSE]
    names(out)[match(col_item, names(out))] <- "itemId"
    names(out)[match(col_name, names(out))] <- "itemName"
    if (!is.null(col_url))  names(out)[match(col_url, names(out))]  <- "itemUrl"
    if (!is.null(col_disc)) names(out)[match(col_disc, names(out))] <- "disciplina"
    out$itemId   <- as.character(out$itemId)
    out$itemName <- as.character(out$itemName)
    if ("itemUrl" %in% names(out))    out$itemUrl <- as.character(out$itemUrl)
    if ("disciplina" %in% names(out)) out$disciplina <- as.character(out$disciplina)
    out <- distinct(drop_na(out, itemId, itemName), itemId, .keep_all = TRUE)
    out$join_key <- norm_key(out$itemId)
    message(sprintf("Catálogo carregado de: %s | linhas: %d", p, nrow(out)))
    return(out)
  }
  tibble(itemId = character(), itemName = character(), itemUrl = character(), join_key = character())
}

load_artifacts <- function(){
  champ <- resolve_champ_dir()
  message("Usando champ: ", champ)

  item_index   <- .read_item_index(champ)
  item_factors <- safe_load_matrix(champ, "item_factors")

  user_index <- NULL; user_factors <- NULL
  p_uidx <- file.path(champ, "user_index.parquet")
  if (file.exists(p_uidx)){
    user_index <- as.data.frame(arrow::read_parquet(p_uidx))
    if (file.exists(file.path(champ, "user_factors.rds")) ||
        file.exists(file.path(champ, "user_factors.parquet")) ||
        file.exists(file.path(champ, "user_factors.npy"))){
      user_factors <- safe_load_matrix(champ, "user_factors")
    }
  }

  topn_df <- NULL
  p_topn <- file.path(champ, "topN_user.parquet")
  if (file.exists(p_topn)) topn_df <- as.data.frame(arrow::read_parquet(p_topn))

  if (!is.null(topn_df) && all(c("itemId","rank") %in% names(topn_df))){
    pop <- topn_df %>%
      mutate(itemId = as.character(.data$itemId)) %>%
      count(.data$itemId, name = "freq") %>%
      arrange(desc(.data$freq))
  } else {
    norms <- sqrt(rowSums(item_factors^2))
    pop <- tibble(i_idx = seq_len(nrow(item_factors)), score = norms) %>%
      inner_join(item_index, by = "i_idx") %>%
      transmute(itemId = as.character(.data$itemId), freq = .data$score) %>%
      arrange(desc(.data$freq))
  }

  catalog <- load_catalog(champ)

  list(topn_df = topn_df,
       item_index = item_index,
       item_factors = item_factors,
       user_index = user_index,
       user_factors = user_factors,
       popularity = pop,
       catalog = catalog)
}

ART <- load_artifacts()
ITEMID_TO_IIDX <- setNames(ART$item_index$i_idx, as.character(ART$item_index$itemId))
USERID_TO_UIDX <- if (is.null(ART$user_index)) character(0) else setNames(ART$user_index$u_idx, as.character(ART$user_index$userId))
assign("ART", ART, envir = .GlobalEnv)
assign("ITEMID_TO_IIDX", ITEMID_TO_IIDX, envir = .GlobalEnv)
assign("USERID_TO_UIDX", USERID_TO_UIDX, envir = .GlobalEnv)

# Cobertura de nomes (log informativo)
try({
  if (!is.null(ART$catalog) && nrow(ART$catalog) > 0){
    idx <- ART$item_index %>% mutate(join_key = norm_key(as.character(itemId)))
    cat_tbl <- ART$catalog
    if (!"join_key" %in% names(cat_tbl)) cat_tbl$join_key <- norm_key(as.character(cat_tbl$itemId))
    cov <- idx %>% left_join(cat_tbl %>% select(join_key, itemName), by = "join_key")
    coverage <- mean(!is.na(cov$itemName)) * 100
    message(sprintf("Cobertura de nomes no catálogo: %.1f%%", coverage))
  } else {
    message("Catálogo AUSENTE — nomes cairão em itemId.")
  }
}, silent = TRUE)

enrich_with_names <- function(df){
  if (is.null(df) || nrow(df) == 0) return(df)
  if (is.null(ART$catalog) || nrow(ART$catalog) == 0){
    df$itemName <- as.character(df$itemId); return(df)
  }
  out <- df %>% mutate(itemId = as.character(itemId), join_key = norm_key(itemId))
  cat_tbl <- ART$catalog
  if (!"join_key" %in% names(cat_tbl)){
    if (!"itemId" %in% names(cat_tbl)){
      possible <- intersect(c("lesson_id","id_aula","conteudo_id","content_id","id","lesson","lessonid"), names(cat_tbl))
      if (length(possible)) cat_tbl$itemId <- as.character(cat_tbl[[possible[1]]]) else {
        out$itemName <- out$itemId; out$join_key <- NULL; return(out)
      }
    }
    cat_tbl$itemId <- as.character(cat_tbl$itemId)
    cat_tbl$join_key <- norm_key(cat_tbl$itemId)
  }
  joined <- out %>% left_join(cat_tbl %>% select(join_key, itemName, any_of(c("itemUrl","disciplina"))), by = "join_key")
  need_fill <- is.na(joined$itemName) | joined$itemName == ""
  if (any(need_fill)){
    joined2 <- joined %>% left_join(cat_tbl %>% transmute(itemId = as.character(itemId), itemName_fallback = as.character(itemName)), by = "itemId")
    joined2$itemName <- ifelse(need_fill, joined2$itemName_fallback, joined2$itemName)
    joined <- joined2 %>% select(-itemName_fallback)
  }
  joined$itemName <- ifelse(is.na(joined$itemName) | joined$itemName == "", as.character(joined$itemId), joined$itemName)
  joined$join_key <- NULL
  joined
}

topk_from_scores <- function(scores, k = TOPK){
  k <- max(1L, min(as.integer(k), length(scores)))
  idx <- head(order(scores, decreasing = TRUE), k)
  out <- tibble(i_idx = idx, score = as.numeric(scores[idx])) %>%
    inner_join(ART$item_index, by = "i_idx") %>%
    transmute(itemId = as.character(itemId), score) %>%
    mutate(rank = dplyr::row_number())
  enrich_with_names(out)
}

recommend_known_user <- function(user_id, k = TOPK){
  uid <- str_trim(as.character(user_id))

  if (!is.null(ART$topn_df) && nrow(ART$topn_df) > 0){
    sub <- ART$topn_df %>% filter(as.character(userId) == uid)
    if (nrow(sub) > 0){
      out <- sub %>% arrange(rank) %>% head(k)
      if (!"score" %in% names(out)) out$score <- NA_real_
      out <- out %>% select(itemId, rank, score)
      return(enrich_with_names(out))
    }
  }

  if (uid %in% names(USERID_TO_UIDX) && !is.null(ART$user_factors)){
    u_idx <- USERID_TO_UIDX[[uid]]
    u_vec <- ART$user_factors[u_idx + 1, , drop = TRUE]
    denom <- sqrt(sum(u_vec^2)); if (!is.finite(denom) || denom == 0) denom <- 1
    scores <- as.numeric(ART$item_factors %*% (u_vec / denom))
    return(topk_from_scores(scores, k = k))
  }

  pop <- ART$popularity %>% head(k) %>% transmute(itemId, score = as.numeric(freq)) %>% mutate(rank = dplyr::row_number())
  enrich_with_names(pop)
}

recommend_user_batch <- function(user_ids, k = TOPK){
  uids <- unique(str_trim(as.character(user_ids)))
  rows <- lapply(uids, function(uid){
    recs <- recommend_known_user(uid, k = k)
    if (is.null(recs) || nrow(recs) == 0){
      return(tibble(userId = uid, rank = integer(), itemId = character(), itemName = character(), score = numeric()))
    }
    recs %>% mutate(userId = uid, .before = 1) %>% select(userId, rank, itemId, itemName, score, dplyr::any_of(c("disciplina")))
  })
  bind_rows(rows)
}

format_table <- function(df){
  if (is.null(df) || nrow(df) == 0) return(tibble())
  out <- df %>% mutate(rank = as.integer(rank))
  rank_chr <- as.character(out$rank)
  out$Rank <- ifelse(rank_chr %in% names(CIRCLED), CIRCLED[rank_chr], rank_chr)
  base_name <- ifelse(is.na(out$itemName) | out$itemName == "", as.character(out$itemId), out$itemName)
  disc_suf <- if ("disciplina" %in% names(out)) ifelse(is.na(out$disciplina) | out$disciplina == "", "", paste0(" — ", out$disciplina)) else ""
  txt <- paste0(base_name, disc_suf)
  too_long <- nchar(txt, type = "width") > MAX_ITEM_CHARS
  txt[too_long] <- paste0(str_sub(txt[too_long], 1, MAX_ITEM_CHARS - 1), "…")
  out$Item <- txt
  out <- add_score_column(out)
  score_nm <- score_col_name()
  cols <- c("Rank", "Item", score_nm); cols <- cols[!sapply(cols, is.null)]
  out[, intersect(cols, names(out)), drop = FALSE]
}

# =====================
# UI
# =====================
ui <- fluidPage(
  tags$head(tags$style(HTML(THEME_CSS))),
  div(
    div("Fundação 1Bi", class = "brand"),
    div("Recomendações Top-5 • ALS Implícito (BM25)", class = "subtitle"),
    class = "header"
  ),
  div(
    div(
      div(
        div("Consulta por usuário", class = "card-header"),
        textInput("user_id", "userId", placeholder = "ex.: 12345"),
        actionButton("btn_user", "Buscar Top-5", class = "btn"),
        div(tableOutput("tbl_user"), class = "table-zone"),
        class = "card"
      ),
      class = "col-7"
    ),
    div(
      div(
        div("Upload de usuários (CSV)", class = "card-header"),
        fileInput("csv_users", "CSV com uma coluna: userId", accept = c(".csv")),
        actionButton("btn_batch", "Processar CSV", class = "btn"),
        tableOutput("tbl_preview"),
        downloadButton("dl_csv", "Baixar CSV com recomendações"),
        class = "card"
      ),
      class = "col-5"
    ),
    class = "grid"
  ),
  uiOutput("catalog_notice"),
  div("© Fundação 1Bi — MVP de Recomendação", class = "footer"),
  class = "container"
)

# =====================
# Server
# =====================
server <- function(input, output, session){
  cache <- reactiveVal(tibble())

  output$tbl_user <- renderTable({
    req(input$btn_user, cancelOutput = TRUE)
    uid <- str_trim(input$user_id %||% "")
    if (uid == "") return(tibble())
    recs <- recommend_known_user(uid, k = TOPK)
    format_table(recs)
  })

  output$tbl_preview <- renderTable({
    req(input$btn_batch, cancelOutput = TRUE)
    fileinfo <- input$csv_users
    if (is.null(fileinfo)) return(tibble())
    path <- fileinfo$datapath
    df <- tryCatch(suppressMessages(readr::read_csv(path, show_col_types = FALSE, progress = FALSE)), error = function(e) NULL)
    if (is.null(df)) return(tibble(`Erro ao ler CSV` = "Arquivo inválido."))
    if (!"userId" %in% names(df)) return(tibble(erro = "CSV deve conter coluna 'userId'"))
    uids <- df$userId %>% as.character() %>% str_trim()
    uids <- unique(uids[uids != ""])
    if (length(uids) == 0) return(tibble(erro = "Nenhum userId válido encontrado."))
    out <- recommend_user_batch(uids, k = TOPK)
    cache(out)
    prev <- out %>% group_by(userId) %>% slice_head(n = TOPK) %>% ungroup()
    prev$Rank <- ifelse(as.character(prev$rank) %in% names(CIRCLED), CIRCLED[as.character(prev$rank)], as.character(prev$rank))
    base_name <- ifelse(is.na(prev$itemName) | prev$itemName == "", as.character(prev$itemId), prev$itemName)
    disc_suf <- if ("disciplina" %in% names(prev)) ifelse(is.na(prev$disciplina) | prev$disciplina == "", "", paste0(" — ", prev$disciplina)) else ""
    txt <- paste0(base_name, disc_suf)
    too_long <- nchar(txt, type = "width") > MAX_ITEM_CHARS
    txt[too_long] <- paste0(str_sub(txt[too_long], 1, MAX_ITEM_CHARS - 1), "…")
    prev$Item <- txt
    prev <- add_score_column(prev)
    score_nm <- score_col_name()
    cols <- c("userId", "Rank", "Item", score_nm); cols <- cols[cols %in% names(prev)]
    prev %>% select(dplyr::all_of(cols))
  })

  output$dl_csv <- downloadHandler(
    filename = function(){ "recs_top5.csv" },
    content  = function(file){
      df <- cache()
      if (is.null(df) || nrow(df) == 0){
        df <- tibble(userId = character(), rank = integer(), itemId = character(), itemName = character(), score = numeric())
      }
      readr::write_csv(df, file)
    }
  )

  output$catalog_notice <- renderUI({
    if (is.null(ART$catalog) || nrow(ART$catalog) == 0){
      div(
        "Aviso: catálogo não encontrado. Coloque ", tags$code("aulas_aprendizap.csv"),
        " com colunas ", tags$code("lesson_id, lesson_title, disciplina"),
        " na pasta do campeão (", tags$code("artifacts/champ_*/"), "), em ", tags$code("data/"), " ou na raiz.",
        class = "notice"
      )
    } else {
      idx <- ART$item_index %>% mutate(join_key = norm_key(as.character(itemId)))
      cat <- ART$catalog
      if (!"join_key" %in% names(cat)) cat$join_key <- norm_key(cat$itemId)
      cov <- idx %>% left_join(cat %>% select(join_key, itemName), by = "join_key")
      coverage <- mean(!is.na(cov$itemName)) * 100
      div(sprintf("Catálogo carregado (%d itens). Cobertura de nomes: %.1f%%.", nrow(ART$catalog), coverage), class = "notice")
    }
  })
}

# =====================
# Run
# =====================
shinyApp(ui, server)


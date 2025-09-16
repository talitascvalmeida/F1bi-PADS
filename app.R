# app.R — Fundação 1Bi • Top-5 Recomendações (ALS Implícito BM25)
# Requer pacotes: shiny, dplyr, readr, arrow, RcppCNPy, stringr, tibble

library(shiny)
library(dplyr)
library(readr)
library(arrow)
library(RcppCNPy)
library(stringr)
library(tibble)

# =======================
# TEMA (ajustes de tabela p/ caber no card)
# =======================
THEME_CSS <- "
:root{
  --bi-primary:#6E29F2;
  --bi-secondary:#B5179E;
  --bi-accent:#FF4D9D;

  --bi-bg:#0E0A17;
  --bi-surface:#1A1430;
  --bi-surface-2:#221B44;

  --bi-border:#3A2F63;
  --bi-text:#FFFFFF;
  --bi-muted:#E4D8FF;
  --bi-muted-2:#CDBEF7;
}

*{ box-sizing:border-box; }
html,body{
  margin:0; padding:0;
  background:var(--bi-bg)!important;
  color:var(--bi-text)!important;
  font-family: ui-sans-serif, system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial;
}

/* Header */
.header{
  position:sticky; top:0; z-index:1000;
  background:linear-gradient(92deg,var(--bi-primary),var(--bi-secondary))!important;
  color:#fff!important; padding:18px 24px;
  box-shadow:0 10px 30px rgba(0,0,0,.28);
}
.header .brand{ font-weight:900; font-size:22px; letter-spacing:.2px; }
.header .subtitle{ color:#FFF; opacity:.98; font-size:14px; margin-top:2px; }

/* Layout */
.container{ padding:28px 24px 36px; max-width:1360px; margin:0 auto; }
.grid{ display:grid; gap:22px; grid-template-columns:repeat(12,1fr); }
.col-6{ grid-column:span 6; }
.col-7{ grid-column:span 7; }
.col-5{ grid-column:span 5; }
@media(max-width:1000px){
  .col-7,.col-5,.col-6{ grid-column:span 12; }
}

/* Cards */
.card{
  background:linear-gradient(180deg,var(--bi-surface),var(--bi-surface-2));
  border:1px solid var(--bi-border);
  border-radius:18px;
  padding:18px;
  box-shadow:0 18px 40px rgba(0,0,0,.35);
  overflow:hidden;
}
.card-header{
  font-weight:900; font-size:28px; line-height:1.15; margin:6px 0 12px;
  color:#FFFFFF;
  text-shadow:0 2px 6px rgba(0,0,0,.5);
  letter-spacing:.2px;
}

/* Inputs & Buttons */
.shiny-input-container{ width:100%!important; max-width:100%!important; }
.shiny-input-container label{ color:var(--bi-muted)!important; font-weight:700; letter-spacing:.2px; }
input.form-control,input[type='text']{
  width:100%!important;
  background:#140F29!important;
  border:1px solid var(--bi-border)!important;
  color:var(--bi-text)!important;
  padding:14px 16px; border-radius:12px; outline:none;
  font-size:20px;
}
input.form-control::placeholder{ color:var(--bi-muted-2)!important; }

button,.btn{
  background:linear-gradient(92deg,var(--bi-primary),var(--bi-accent))!important;
  border:0; color:#fff!important; padding:14px 18px;
  border-radius:14px; font-weight:900; font-size:22px;
  box-shadow:0 10px 22px rgba(110,41,242,.45);
}

/* ---------- TABELA do BLOCO ESQUERDO ---------- */
.table-zone{ width:100%; margin-top:12px; overflow-x:auto; }
.table-zone table{
  width:100%;
  border-collapse:collapse;
  font-size:16px;
  color:var(--bi-text);
  table-layout:fixed;
}
.table-zone th,.table-zone td{
  border-bottom:1px solid var(--bi-border);
  padding:10px 12px;
}
.table-zone thead th{
  text-align:left; font-weight:800;
  background:var(--bi-surface-2)!important; color:#FFF;
}

/* Rank (coluna 1) */
.table-zone table th:nth-child(1),
.table-zone table td:nth-child(1){ width:64px; }

/* Score (coluna 3) */
.table-zone table th:nth-child(3),
.table-zone table td:nth-child(3){
  width:160px;
  text-align:right;
  white-space:nowrap;
  font-variant-numeric: tabular-nums;
}

/* Item (coluna 2) */
.table-zone table th:nth-child(2),
.table-zone table td:nth-child(2){
  width:calc(100% - 64px - 160px - 72px);
  max-width:100%;
  white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
}

/* linhas sem zebra */
.table-zone tbody tr td{ background:transparent!important; color:var(--bi-text)!important; }

/* Responsivo */
@media (max-width: 900px){
  .table-zone table th:nth-child(3), .table-zone table td:nth-child(3){ width:138px; }
  .table-zone table th:nth-child(2), .table-zone table td:nth-child(2){
    width:calc(100% - 64px - 138px - 72px);
  }
}

/* Avisos & Rodapé */
.notice{
  margin-top:22px; padding:14px 16px;
  background:rgba(255,255,255,.05);
  border:1px dashed var(--bi-border);
  border-radius:14px;
}
.footer{ color:var(--bi-muted); font-size:12px; margin-top:10px; }
"

# =======================
# Constantes
# =======================
TOPK <- 5L
MAX_ITEM_CHARS <- 64L
CIRCLED <- setNames(intToUtf8(9311 + 1:10, multiple = TRUE), as.character(1:10))

# =======================
# Utilidades / Artefatos
# =======================
ARTIFACTS_DIR <- "artifacts"

.has_required <- function(d){
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
  # 1) ENV override (configure também no Connect se quiser)
  ch <- Sys.getenv("CHAMP_DIR", unset = "")
  if (nzchar(ch)) {
    cand <- if (dir.exists(ch)) ch else file.path(ARTIFACTS_DIR, ch)
    if (.has_required(cand)) return(cand)
  }
  # 2) Ponteiro em artifacts/CHAMP_POINTER.txt (opcional)
  ptr <- file.path(ARTIFACTS_DIR, "CHAMP_POINTER.txt")
  if (file.exists(ptr)) {
    target <- trimws(readLines(ptr, warn = FALSE)[1])
    cand <- if (dir.exists(target)) target else file.path(ARTIFACTS_DIR, target)
    if (.has_required(cand)) return(cand)
  }
  # 3) Fallback: pick champ_* válido mais recente
  dirs <- list.dirs(ARTIFACTS_DIR, recursive = FALSE, full.names = TRUE)
  champs <- dirs[grepl("^champ_", basename(dirs))]
  valid <- Filter(.has_required, champs)
  if (!length(valid)) stop("Nenhum champ_* válido com item_index.(parquet|csv) + item_factors.npy em 'artifacts/'.")
  valid[which.max(file.info(valid)$mtime)]
}

# ======== NOVO: carrega catálogo priorizando aulas_aprendizap.csv ========
load_catalog <- function(champ_dir){
  candidates <- c(
    file.path(champ_dir, "catalog_items.parquet"),
    file.path(champ_dir, "catalog_items.csv"),
    file.path(champ_dir, "aulas_aprendizap.parquet"),
    file.path(champ_dir, "aulas_aprendizap.csv"),
    file.path("data",      "aulas_aprendizap.parquet"),
    file.path("data",      "aulas_aprendizap.csv"),
    "aulas_aprendizap.parquet",
    "aulas_aprendizap.csv",
    "stg_formation.csv"
  )

  read_any <- function(p){
    ext <- tolower(tools::file_ext(p))
    if (ext == "parquet") as.data.frame(arrow::read_parquet(p)) else
      suppressMessages(readr::read_csv(p, show_col_types = FALSE, progress = FALSE))
  }

  first_present <- function(cands, pool){
    pool_l <- tolower(pool)
    for (c in cands){
      pos <- match(tolower(c), pool_l)
      if (!is.na(pos)) return(pool[pos])
    }
    NULL
  }

  for (p in candidates){
    if (!file.exists(p)) next
    raw <- tryCatch(read_any(p), error = function(e) NULL)
    if (is.null(raw) || nrow(raw) == 0) next

    cols <- colnames(raw)

    # mapeamento específico: lesson_id -> itemId | lesson_title -> itemName | disciplina
    col_item <- first_present(c("itemId","lesson_id","id_aula","conteudo_id","content_id","lesson_id","id"), cols)
    col_name <- first_present(c("itemName","lesson_title","titulo","name","title","descricao","description"), cols)
    col_url  <- first_present(c("itemUrl","url","link","href"), cols)
    col_disc <- first_present(c("disciplina","subject","materia"), cols)

    if (is.null(col_item) || is.null(col_name)) next

    keep <- c(col_item, col_name, col_url, col_disc)
    keep <- keep[!sapply(keep, is.null)]
    out  <- raw[, keep, drop = FALSE]

    names(out)[match(col_item, names(out))] <- "itemId"
    names(out)[match(col_name, names(out))] <- "itemName"
    if (!is.null(col_url))  names(out)[match(col_url,  names(out))] <- "itemUrl"
    if (!is.null(col_disc)) names(out)[match(col_disc, names(out))] <- "disciplina"

    out$itemId   <- as.character(out$itemId)
    out$itemName <- as.character(out$itemName)
    if ("itemUrl"    %in% names(out)) out$itemUrl    <- as.character(out$itemUrl)
    if ("disciplina" %in% names(out)) out$disciplina <- as.character(out$disciplina)

    out <- out |>
      tidyr::drop_na(itemId, itemName) |>
      dplyr::distinct(itemId, .keep_all = TRUE)

    if (nrow(out) > 0) return(out)
  }

  # fallback vazio (app mostrará IDs cru se não achar catálogo)
  tibble::tibble(itemId = character(), itemName = character(), itemUrl = character())
}

load_artifacts <- function(){
  champ <- resolve_champ_dir()
  message("Usando champ: ", champ)

  item_index   <- .read_item_index(champ)
  item_factors <- RcppCNPy::npyLoad(file.path(champ, "item_factors.npy"))

  user_index <- user_factors <- NULL
  if (file.exists(file.path(champ,"user_index.parquet")) && file.exists(file.path(champ,"user_factors.npy"))){
    user_index   <- as.data.frame(arrow::read_parquet(file.path(champ,"user_index.parquet")))
    user_factors <- RcppCNPy::npyLoad(file.path(champ,"user_factors.npy"))
  }

  topn_df <- NULL
  topn_path <- file.path(champ, "topN_user.parquet")
  if (file.exists(topn_path)) topn_df <- as.data.frame(arrow::read_parquet(topn_path))

  if (!is.null(topn_df) && all(c("itemId","rank") %in% names(topn_df))){
    pop <- topn_df |>
      dplyr::mutate(itemId = as.character(.data$itemId)) |>
      dplyr::count(.data$itemId, name = "freq") |>
      dplyr::arrange(dplyr::desc(.data$freq))
  } else {
    norms <- sqrt(rowSums(item_factors^2))
    pop <- tibble::tibble(i_idx = seq_len(nrow(item_factors)), score = norms) |>
      dplyr::inner_join(item_index, by = "i_idx") |>
      dplyr::transmute(itemId = as.character(.data$itemId), freq = .data$score) |>
      dplyr::arrange(dplyr::desc(.data$freq))
  }

  catalog <- load_catalog(champ)
  list(
    topn_df = topn_df,
    item_index = item_index,
    item_factors = item_factors,
    user_index = user_index,
    user_factors = user_factors,
    popularity = pop,
    catalog = catalog
  )
}

ART <- load_artifacts()
ITEMID_TO_IIDX <- setNames(ART$item_index$i_idx, as.character(ART$item_index$itemId))
USERID_TO_UIDX <- if (is.null(ART$user_index)) character(0) else setNames(ART$user_index$u_idx, as.character(ART$user_index$userId))

# =======================
# Recomendação
# =======================
enrich_with_names <- function(df){
  if (is.null(df) || nrow(df) == 0) return(df)
  if (is.null(ART$catalog) || nrow(ART$catalog) == 0){
    df$itemName <- as.character(df$itemId)
    return(df)
  }
  out <- df %>%
    mutate(itemId = as.character(itemId)) %>%
    left_join(ART$catalog, by = "itemId")
  out$itemName <- ifelse(is.na(out$itemName), as.character(out$itemId), out$itemName)
  out
}

topk_from_scores <- function(scores, k = TOPK){
  k <- max(1L, min(as.integer(k), length(scores)))
  idx <- head(order(scores, decreasing = TRUE), k)
  out <- tibble(i_idx = idx, score = as.numeric(scores[idx])) %>%
    inner_join(ART$item_index, by = "i_idx") %>%
    transmute(itemId = as.character(itemId), score) %>%
    mutate(rank = row_number())
  enrich_with_names(out)
}

recommend_known_user <- function(user_id, k = TOPK){
  uid <- str_trim(as.character(user_id))

  # 1) Se houver topN pré-calculado
  if (!is.null(ART$topn_df) && nrow(ART$topn_df) > 0){
    sub <- ART$topn_df %>% filter(as.character(userId) == uid)
    if (nrow(sub) > 0){
      cols <- c("itemId","rank", if ("score" %in% names(sub)) "score")
      out <- sub %>% select(all_of(cols)) %>% arrange(rank) %>% head(k)
      if (!"score" %in% names(out)) out$score <- NA_real_
      return(enrich_with_names(out))
    }
  }

  # 2) Se houver fator de usuário
  if (uid %in% names(USERID_TO_UIDX) && !is.null(ART$user_factors)){
    u_idx <- USERID_TO_UIDX[[uid]]
    u_vec <- ART$user_factors[u_idx + 1, , drop = TRUE]  # +1 se i_idx/u_idx base 0
    denom <- sqrt(sum(u_vec^2)); if (denom == 0) denom <- 1
    scores <- ART$item_factors %*% (u_vec / denom)
    scores <- as.numeric(scores)
    return(topk_from_scores(scores, k = k))
  }

  # 3) Fallback: popularidade
  pop <- ART$popularity %>% head(k) %>% transmute(itemId, score = as.numeric(freq)) %>%
    mutate(rank = row_number())
  enrich_with_names(pop)
}

recommend_user_batch <- function(user_ids, k = TOPK){
  uids <- unique(str_trim(as.character(user_ids)))
  rows <- lapply(uids, function(uid){
    recs <- recommend_known_user(uid, k = k)
    if (is.null(recs) || nrow(recs) == 0)
      return(tibble(userId = uid, rank = integer(), itemId = character(), itemName = character(), score = numeric()))
    recs %>% mutate(userId = uid, .before = 1) %>% select(userId, rank, itemId, itemName, score, dplyr::any_of(c("disciplina")))
  })
  bind_rows(rows)
}

# =======================
# Formatação p/ UI (exibe 'lesson_title — disciplina')
# =======================
format_table <- function(df){
  if (is.null(df) || nrow(df) == 0) return(tibble())
  out <- df %>% mutate(rank = as.integer(rank))

  # Símbolos circundados (1..10)
  rank_chr <- as.character(out$rank)
  out$Rank <- ifelse(rank_chr %in% names(CIRCLED), CIRCLED[rank_chr], rank_chr)

  # Nome legível + disciplina
  base_name <- ifelse(is.na(out$itemName) | out$itemName == "", as.character(out$itemId), out$itemName)
  if ("disciplina" %in% names(out)){
    disc_suf <- ifelse(is.na(out$disciplina) | out$disciplina == "", "", paste0(" — ", out$disciplina))
  } else {
    disc_suf <- ""
  }
  txt <- paste0(base_name, disc_suf)

  # Trunca texto para caber no card
  too_long <- nchar(txt, type = "width") > MAX_ITEM_CHARS
  txt[too_long] <- paste0(str_sub(txt[too_long], 1, MAX_ITEM_CHARS - 1), "…")
  out$Item <- txt

  # Score (se houver)
  if ("score" %in% names(out)) out$Score <- round(as.numeric(out$score), 4)

  cols <- c("Rank","Item","Score")
  out[, intersect(cols, names(out)), drop = FALSE]
}

# =======================
# UI
# =======================
ui <- fluidPage(
  tags$head(tags$style(HTML(THEME_CSS))),

  div(
    div("Fundação 1Bi", class = "brand"),
    div("Recomendações Top-5 • ALS Implícito (BM25)", class = "subtitle"),
    class = "header"
  ),

  div(
    # grid
    div(
      # Coluna esquerda
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
      # Coluna direita
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
)

# =======================
# Server
# =======================
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
    df <- tryCatch(suppressMessages(read_csv(path, show_col_types = FALSE, progress = FALSE)),
                   error = function(e) NULL)
    if (is.null(df)) return(tibble(`Erro ao ler CSV` = "Arquivo inválido."))
    if (!"userId" %in% names(df)) return(tibble(erro = "CSV deve conter coluna 'userId'"))
    uids <- df$userId %>% as.character() %>% str_trim()
    uids <- uids[uids != ""]
    uids <- unique(uids)
    if (length(uids) == 0) return(tibble(erro = "Nenhum userId válido encontrado."))
    out <- recommend_user_batch(uids, k = TOPK)
    cache(out)

    # Preview formatado (mesma lógica do format_table, mas preserva userId)
    prev <- out %>% group_by(userId) %>% slice_head(n = TOPK) %>% ungroup()

    prev$Rank <- ifelse(as.character(prev$rank) %in% names(CIRCLED),
                        CIRCLED[as.character(prev$rank)], as.character(prev$rank))

    base_name <- ifelse(is.na(prev$itemName) | prev$itemName == "",
                        as.character(prev$itemId), prev$itemName)
    if ("disciplina" %in% names(prev)){
      disc_suf <- ifelse(is.na(prev$disciplina) | prev$disciplina == "", "", paste0(" — ", prev$disciplina))
    } else {
      disc_suf <- ""
    }
    txt <- paste0(base_name, disc_suf)

    too_long <- nchar(txt, type = "width") > MAX_ITEM_CHARS
    txt[too_long] <- paste0(str_sub(txt[too_long], 1, MAX_ITEM_CHARS - 1), "…")
    prev$Item <- txt

    prev$Score <- round(as.numeric(prev$score), 4)
    prev %>% select(userId, Rank, Item, Score)
  })

  output$dl_csv <- downloadHandler(
    filename = function(){ "recs_top5.csv" },
    content = function(file){
      df <- cache()
      if (is.null(df) || nrow(df) == 0){
        df <- tibble(userId = character(), rank = integer(), itemId = character(), itemName = character(), score = numeric())
      }
      write_csv(df, file)
    }
  )

  output$catalog_notice <- renderUI({
    if (is.null(ART$catalog) || nrow(ART$catalog) == 0){
      div(
        "Aviso: catálogo de nomes de itens não encontrado. ",
        "Os itens aparecem como IDs. Para nomes legíveis, exporte ",
        tags$code("aulas_aprendizap.csv"),
        " (colunas ",
        tags$code("lesson_id, lesson_title, disciplina"),
        ") ou ",
        tags$code("catalog_items.parquet|csv"),
        " com colunas ",
        tags$code("itemId,itemName[,itemUrl]"),
        " na pasta do campeão (artifacts/champ_*/).",
        class = "notice"
      )
    } else div()
  })
}

shinyApp(ui, server)

app <- shiny::shinyApp(ui = ui, server = server)
app
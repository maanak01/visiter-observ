# =============================================================
# 行動観察アプリのCSVを点検し、分析用の表に組み替えるスクリプト
#
# 使い方：下の「設定」の3行を書き換えて、全体を実行する（RStudioなら Ctrl/Cmd + Shift + Enter）
# 必要なパッケージ：dplyr, tidyr, readr（jsonlite は計画JSONを使うときだけ）
#
# 考え方：CSVは「1行＝1つの出来事」の縦長の形。
#   1. 取り消した行（voided = 1）を除く
#   2. 測り方ごとに必要な行だけを取り出し、横に並べ替えて（pivot_wider）1行＝1グループの表にする
#   3. グループの番号は観察ごとに1から始まるので、「観察ID × 番号」で1つのグループを表す
# =============================================================

# ---- 設定 ----
csv_path  <- "vobs_20261104-101310.csv"   # アプリで書き出したCSV
plan_path <- NULL                          # 計画JSON（例 "plan_mock.json"）。歩く速さを m/s にするときに指定
out_dir   <- "vobs_tables"                 # 組み替えた表を書き出すフォルダ

# 入っていないパッケージだけ入れる（Google Colab の R でも、そのまま動くように）
for (p in c("dplyr", "tidyr", "readr", if (!is.null(plan_path)) "jsonlite")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

# ---- 1. 読み込み ----
# 最初はすべて文字として読み、あとで必要な列だけ数値にする（列の型の推測ミスを防ぐため）
raw <- read_csv(csv_path, col_types = cols(.default = col_character()), progress = FALSE)

# 列は名前で扱う（順番には頼らない）。
# 観察の種類と観察点の5列は、改修前のCSVにはないので、なければ空欄（NA）の列として足す
required <- c("observation_id", "plan_id", "facility", "zone_id", "event_id", "event_type",
              "unit_id", "t_epoch_ms", "t_local", "value_num", "value_text", "voided")
added    <- c("obs_mode", "point_id", "point_name", "point_x", "point_y")
missing  <- setdiff(required, names(raw))
if (length(missing) > 0) stop("必要な列がありません：", paste(missing, collapse = ", "))
for (col in added) if (!col %in% names(raw)) raw[[col]] <- NA_character_
raw <- raw %>% mutate(across(all_of(added), ~ na_if(.x, "")))   # 空の文字は NA にそろえる

raw <- raw %>%
  mutate(event_id   = as.integer(event_id),
         unit_id    = as.integer(unit_id),
         t_epoch_ms = as.numeric(t_epoch_ms),
         value_num  = as.numeric(value_num),
         point_x    = as.numeric(point_x),
         point_y    = as.numeric(point_y),
         voided     = as.integer(voided))

ev <- raw %>% filter(voided == 0)   # 取り消した行を除いたもの。以降はこれを使う

# ---- 2. 点検 ----
# 問題を見つけたら、ここに1行ずつためておき、最後にまとめて表示する
issues <- tibble(observation_id = character(), problem = character())
note <- function(df, msg) {
  if (nrow(df) > 0) issues <<- bind_rows(issues, tibble(observation_id = df$observation_id, problem = msg))
}

# 同じ観察の中で、同じ event_id が2回出てこないか
raw %>% count(observation_id, event_id) %>% filter(n > 1) %>%
  note("event_id が重なっている")

# 観察の始まりと終わりが1回ずつあるか
ev %>% group_by(observation_id) %>%
  summarise(n_start = sum(event_type == "session_start"), n_end = sum(event_type == "session_end"), .groups = "drop") %>%
  filter(n_start != 1 | n_end != 1) %>%
  note("session_start か session_end が1回ではない（途中で終わった観察かもしれない）")

# 時刻合わせが始まりと終わりの両方にあるか
ev %>% group_by(observation_id) %>%
  summarise(s = sum(event_type == "sync_mark" & value_text == "start"),
            e = sum(event_type == "sync_mark" & value_text == "end"), .groups = "drop") %>%
  filter(s == 0 | e == 0) %>%
  note("時刻合わせ（start か end）がない。CoreS3との補正が片側しかできない")

# 押した順に、時刻が進んでいるか（端末の時計が途中で変わっていないか）
ev %>% arrange(observation_id, event_id) %>% group_by(observation_id) %>%
  filter(t_epoch_ms < lag(t_epoch_ms)) %>% ungroup() %>%
  note("時刻が前の記録より戻っている行がある")

# 観察の種類と観察点は、1つの観察の中で変わらないはず
raw %>% group_by(observation_id) %>%
  summarise(n = n_distinct(paste(obs_mode, point_id, point_name, point_x, point_y)), .groups = "drop") %>%
  filter(n > 1) %>%
  note("1つの観察の中で、観察の種類か観察点が変わっている")

# 定点観察なのに観察点がない
ev %>% filter(obs_mode == "fixed" & is.na(point_name)) %>% distinct(observation_id) %>%
  note("定点観察なのに、観察点の名前がない")

# 体験の観察に、他者の行動の記録が混ざっていないか（2種類を混ぜない約束）
others <- c("dwell_enter", "dwell_exit", "passby", "speed_line1", "speed_line2",
            "group_size", "group_rel", "group_member", "count_prompt", "count_present", "count_phone")
ev %>% filter(obs_mode == "walk" & event_type %in% others) %>% distinct(observation_id) %>%
  note("体験の観察に、他者の行動の記録が入っている")

# 体験の観察で、自分の出入りがそろっているか
ev %>% filter(obs_mode == "walk") %>% group_by(observation_id) %>%
  summarise(i = sum(event_type == "self_enter"), o = sum(event_type == "self_exit"), .groups = "drop") %>%
  filter(i != 1 | o != 1) %>%
  note("体験の観察で、「区画に入った」か「区画を出た」が1回ではない")

# ---- 3. グループの構成（1行＝1グループ） ----
# 構成は付け直すことがあるので、同じ項目は最後に付けた値を使う
group_attr <- ev %>%
  filter(event_type %in% c("group_size", "group_rel", "group_member")) %>%
  mutate(key = case_when(event_type == "group_size" ~ "group_size",
                         event_type == "group_rel"  ~ "group_rel",
                         TRUE ~ paste0("has_", value_text)),
         val = case_when(event_type == "group_size" ~ as.character(value_num),
                         event_type == "group_rel"  ~ value_text,
                         TRUE ~ "1")) %>%
  group_by(observation_id, unit_id, key) %>%
  slice_max(t_epoch_ms, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(observation_id, unit_id, key, val) %>%
  pivot_wider(names_from = key, values_from = val)
# まだ一度も付けていない項目の列も作っておく（あとで列がないと困らないように）
for (col in c("group_size", "group_rel", "has_child", "has_elder")) {
  if (!col %in% names(group_attr)) group_attr[[col]] <- NA_character_
}
group_attr <- group_attr %>%
  mutate(group_size = as.numeric(group_size),          # 5 は「5人以上」
         has_child  = as.integer(!is.na(has_child)),
         has_elder  = as.integer(!is.na(has_elder)))
add_groups <- function(df) {
  df %>% left_join(group_attr, by = c("observation_id", "unit_id")) %>%
    mutate(has_child = coalesce(has_child, 0L), has_elder = coalesce(has_elder, 0L))
}

# ---- 4. 観察ごとの表（1行＝1回の観察） ----
first_or_na <- function(x) if (length(x) == 0) NA else x[1]
sessions <- ev %>%
  group_by(observation_id, plan_id, facility, zone_id, obs_mode, point_id, point_name, point_x, point_y) %>%
  summarise(
    t_start       = first_or_na(t_epoch_ms[event_type == "session_start"]),
    self_enter    = first_or_na(t_epoch_ms[event_type == "self_enter"]),   # 体験：自分が区画に入った時刻
    self_exit     = first_or_na(t_epoch_ms[event_type == "self_exit"]),    # 体験：自分が区画を出た時刻
    t_end         = first_or_na(t_epoch_ms[event_type == "session_end"]),
    sync_start_ms = first_or_na(t_epoch_ms[event_type == "sync_mark" & value_text == "start"]),
    sync_end_ms   = first_or_na(t_epoch_ms[event_type == "sync_mark" & value_text == "end"]),
    self_est_min  = first_or_na(value_num[event_type == "self_est_min"]),
    self_desire   = first_or_na(value_num[event_type == "self_desire"]),
    self_comment  = first_or_na(value_text[event_type == "self_comment"]),
    .groups = "drop") %>%
  mutate(duration_min   = (t_end - t_start) / 60000,
         self_dwell_min = (self_exit - self_enter) / 60000,
         # 時間の見積もりと比べる「実際の時間」：
         #   体験 → 自分が区画にいた時間（入った〜出た）。定点観察 → 観察していた時間（始めた〜終えた）
         est_base_min = if_else(obs_mode %in% "walk", self_dwell_min, duration_min),
         est_ratio    = self_est_min / est_base_min,   # 1より大きい＝実際より長く感じた
         start_local  = format(as.POSIXct(t_start / 1000, origin = "1970-01-01", tz = "Asia/Tokyo"), "%Y-%m-%d %H:%M:%S"))

sessions %>% filter(!is.na(self_desire) & (self_desire < 0 | self_desire > 10)) %>%
  note("ずっといたい度が0〜10の外にある")
sessions %>% filter(!is.na(self_dwell_min) & self_dwell_min < 0) %>%
  note("区画を出た時刻が、入った時刻より前になっている")

# ---- 5. 滞在（1行＝1グループ） ----
# 入った時刻と出た時刻を横に並べ、差を滞在時間にする。
# 観察を終えた時点でまだ中にいたグループは「打ち切り」（censored = 1）。
# その滞在時間は「少なくともこれだけはいた」という下限になる（生存時間分析で扱える形）
dwell <- ev %>%
  filter(event_type %in% c("dwell_enter", "dwell_exit")) %>%
  select(observation_id, zone_id, unit_id, event_type, t_epoch_ms) %>%
  pivot_wider(names_from = event_type, values_from = t_epoch_ms, values_fn = min)
for (col in c("dwell_enter", "dwell_exit")) if (!col %in% names(dwell)) dwell[[col]] <- NA_real_
dwell <- dwell %>%
  left_join(sessions %>% select(observation_id, t_end), by = "observation_id") %>%
  mutate(censored = as.integer(is.na(dwell_exit)),
         dwell_s  = (coalesce(dwell_exit, t_end) - dwell_enter) / 1000) %>%
  select(-t_end) %>%
  add_groups()

dwell %>% filter(is.na(dwell_enter)) %>% note("「入った」がないのに「出た」がある")
dwell %>% filter(!is.na(dwell_s) & dwell_s < 0) %>% note("出た時刻が入った時刻より前になっている")

# ---- 6. 立ち止まり（1行＝1グループ） ----
passby <- ev %>%
  filter(event_type == "passby") %>%
  transmute(observation_id, zone_id, unit_id, t_epoch_ms, stopped = as.integer(value_text == "stop")) %>%
  add_groups()

# ---- 7. 歩く速さ（1行＝1グループ） ----
speed <- ev %>%
  filter(event_type %in% c("speed_line1", "speed_line2")) %>%
  select(observation_id, zone_id, unit_id, line_pair_id = value_text, event_type, t_epoch_ms) %>%
  pivot_wider(names_from = event_type, values_from = t_epoch_ms, values_fn = min)
for (col in c("speed_line1", "speed_line2")) if (!col %in% names(speed)) speed[[col]] <- NA_real_
speed <- speed %>% mutate(dt_s = (speed_line2 - speed_line1) / 1000)

speed %>% filter(is.na(speed_line2)) %>% note("1本目だけで2本目がない（歩く速さは計算できない）")
speed %>% filter(!is.na(dt_s) & dt_s <= 0) %>% note("2本目が1本目と同時か前になっている")

if (!is.null(plan_path)) {
  # 計画JSONから、線の組ごとの実際の距離を持ってきて、秒速（m/s）にする
  plan <- jsonlite::fromJSON(plan_path)
  dist <- as_tibble(plan$speed_lines) %>% select(line_pair_id, distance_m)
  speed <- speed %>% left_join(dist, by = "line_pair_id") %>% mutate(speed_mps = distance_m / dt_s)
}
speed <- speed %>% add_groups()

# ---- 8. 一斉の数え上げ（1行＝1回の数え上げ） ----
# 区画の人数とスマホの人数は、続けて2行で記録されている。
# 通知（count_prompt）のあとの最初の数え上げを「通知への答え」とみなし、通知からの遅れを出す
counts <- ev %>%
  filter(event_type %in% c("count_prompt", "count_present", "count_phone")) %>%
  arrange(observation_id, event_id) %>%
  group_by(observation_id) %>%
  mutate(prompt_no = cumsum(event_type == "count_prompt"),
         phone     = if_else(lead(event_type) == "count_phone", lead(value_num), NA_real_)) %>%
  group_by(observation_id, prompt_no) %>%
  mutate(prompt_t  = if_else(prompt_no > 0, first(t_epoch_ms[event_type == "count_prompt"]), NA_real_),
         prompt_gap_min = if_else(prompt_no > 0, first(value_num[event_type == "count_prompt"]), NA_real_), # 前の知らせからの間隔（分）
         first_ans = row_number() == which(event_type == "count_present")[1]) %>%
  ungroup() %>%
  filter(event_type == "count_present") %>%
  transmute(observation_id, zone_id, t_epoch_ms,
            present = value_num, phone,
            phone_rate = phone / present,
            by_prompt  = as.integer(prompt_no > 0 & first_ans),
            delay_s    = if_else(by_prompt == 1, (t_epoch_ms - prompt_t) / 1000, NA_real_),
            prompt_gap_min = if_else(by_prompt == 1, prompt_gap_min, NA_real_))

counts %>% filter(!is.na(phone) & phone > present) %>% note("スマホを見ている人数が区画の人数より多い")
counts %>% filter(is.na(phone)) %>% note("区画の人数はあるが、スマホの人数がない")

# ---- 9. 環境の値と写真（縦長のまま） ----
env <- ev %>%
  filter(event_type %in% c("env_noise", "env_light", "photo_mark")) %>%
  transmute(observation_id, zone_id, t_epoch_ms, t_local,
            kind = recode(event_type, env_noise = "noise_db", env_light = "light_lux", photo_mark = "photo"),
            value_num, value_text)

# ---- 10. CoreS3の時刻を、このアプリの時刻に直す関数 ----
# 開始と終了の2回の時刻合わせから、2つの時計の対応を直線で表す。
#   app_start, app_end：アプリの sync_mark の時刻（sessions の sync_start_ms, sync_end_ms）
#   dev_start, dev_end：同じ瞬間のCoreS3の時刻（ミリ秒にそろえる）
# 終了の時刻合わせがないときは、ずれの量だけを直す（傾きは1）
make_sync <- function(app_start, dev_start, app_end = NA, dev_end = NA) {
  slope <- if (is.na(app_end) || is.na(dev_end)) 1 else (app_end - app_start) / (dev_end - dev_start)
  function(dev_t) app_start + (dev_t - dev_start) * slope
}
# 例：to_app <- make_sync(1793754002000, 50123, 1793754603000, 651200)
#     to_app(c(60000, 120000))   # CoreS3の時刻 → アプリの時刻（ミリ秒）

# ---- 11. 書き出しと結果の表示 ----
dir.create(out_dir, showWarnings = FALSE)
tables <- list(sessions = sessions, dwell = dwell, passby = passby, speed = speed,
               counts = counts, env = env, group_attr = group_attr, issues = issues)
for (nm in names(tables)) write_excel_csv(tables[[nm]], file.path(out_dir, paste0(nm, ".csv")))  # Excelでも文字化けしない形

cat("\n==== 読み込み ====\n")
cat(sprintf("全 %d 行（取り消し %d 行を除くと %d 行）、観察 %d 回\n",
            nrow(raw), sum(raw$voided == 1), nrow(ev), n_distinct(ev$observation_id)))
cat("\n==== 作った表（行数） ====\n")
for (nm in names(tables)) cat(sprintf("  %-10s %4d 行\n", nm, nrow(tables[[nm]])))
cat("\n==== 点検の結果 ====\n")
if (nrow(issues) == 0) {
  cat("  問題は見つかりませんでした。\n")
} else {
  cat(sprintf("  %s：%s\n", issues$observation_id, issues$problem), sep = "")
}
cat(sprintf("\n表は %s フォルダに書き出しました。\n", out_dir))

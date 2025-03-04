#' @title make_univdata
#' @description \code{make_univdata} は文科省の大学一覧のネ申エクセルから学部ごと
#' の定員情報を作成するRコードです

library(tidyverse)
library(readxl)

excel_sheets("public_2024.xlsx") |>
  set_names() |>
  map(read_excel, path =  "public_2024.xlsx")->public_list


map(public_list,~setNames(.x,str_c("v",seq_len(ncol(.))))) %>%
  bind_rows(.,.id = "group") %>%
  group_nest(group) %>%
  mutate(start_row=map_dbl(data,~which(.x$v1=="学部")[1])) %>%
  mutate(end_row=map2_dbl(data,start_row,~{
    na_rows<-which(is.na(.x$v1))
    na_rows_filter<-na_rows[na_rows>.y+2]
    if(length(na_rows_filter>0)){
      na_rows_filter[1]-1
    }else{
      nrow(.)
    }
  })) %>%
  mutate(data2 = pmap(list(data, start_row, end_row), function(data, start_row, end_row) {
    data %>%
      slice(start_row:end_row) %>%    # 指定範囲でスライス
      select(where(~ !all(is.na(.))))
  })) %>%
  mutate(data3=map(data2,
                   ~.x %>%
                     fill(v1) %>%
                     filter(v1=="学部") %>%
                     summarise(across(everything(), ~ paste0(.x, collapse = ""))) %>%
                     bind_rows(.x %>%
                                 select(where(~ !all(is.na(.)))) %>%
                                 fill(v1) %>%
                                 filter(v1!="学部"))
  )) %>%
  mutate(data3=map2(data3,group,~mutate(.x,group=.y))) ->public_list2

##変数名の設定
bind_rows(public_list2$data3) %>%
  setNames(c("学部","学科","都道府県","市区町村","修業年限","入学定員昼間",
             "入学定員夜間","入学定員計","編入定員2年次","編入定員3年次","編入定員4年次",
             "編入(夜間)2年次","編入(夜間)3年次","大学名")) %>%
  filter(学部!="学部学部学部")->public_df

#読み込んだデータと作成したデータがあっているか確認
public_list2$group %>%
  as_tibble() %>%
  anti_join(.,public_df,by=c(value="大学名"))


















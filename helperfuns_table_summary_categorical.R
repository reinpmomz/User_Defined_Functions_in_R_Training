library(dplyr)
library(gtsummary)
library(flextable)
library(labelled)
library(sjlabelled)

## descriptive summary tables
descriptive_table <- 
  function(df, foot_note = NULL, caption = "", ci=FALSE, include_vars, mean_vars = NULL, sum_vars = NULL,
           flex_table = TRUE, categorical_proportion_digits = 1, continous_digits = 1,
           statistic_categorical = "{n} ({p}%)") {
    df <- (df %>%
             dplyr::mutate(across(where(is.character), sjlabelled::as_factor))
           )
    summ <- if (is.null(mean_vars) & is.null(sum_vars)) {
      gtsummary::tbl_summary(df
                             , include = any_of(include_vars)
                             , type = list(
                               all_dichotomous() ~ "categorical"
                               ,all_continuous() ~ "continuous2"
                             )
                             , statistic = list(
                               all_continuous(continuous2 = TRUE) ~ c(
                               "{mean} ({sd})",
                               "{median} ({p25}, {p75})",
                               "{min}, {max}" ),
                               all_categorical() ~ statistic_categorical
                               )
                             , digits = list(all_continuous(continuous2 = TRUE) ~ continous_digits, 
                                             all_categorical() ~ c(0, categorical_proportion_digits)
                             )
                             , percent = "column" #"column", "row", or "cell"
                             , missing = "ifany" #list missing data separately #ifany #no #always
                             , missing_text = "Missing"
      ) 
    } else {
      gtsummary::tbl_summary(df
                             , include = any_of(include_vars)
                             , type = list(
                               any_of(mean_vars) ~ "continuous2"
                               ,any_of(sum_vars) ~ "continuous"
                               ,all_dichotomous() ~ "categorical"
                               ,c(all_continuous(), -any_of(sum_vars)) ~ "continuous2" 
                             )
                             , statistic = list(
                               any_of(sum_vars) ~ "{sum}",
                               all_continuous2() ~ c(
                                 "{mean} ({sd})",
                                 "{median} ({p25}, {p75})",
                                 "{min}, {max}" ),
                               all_categorical() ~ statistic_categorical
                               )
                             , digits = list(any_of(sum_vars) ~ continous_digits, 
                                             all_continuous2() ~ continous_digits, 
                                             all_categorical() ~ c(0, categorical_proportion_digits)
                             )
                             , percent = "column" #"column", "row", or "cell"
                             , missing = "ifany" #list missing data separately #ifany #no #always
                             , missing_text = "Missing"
      )
    }
    
    summ1 <- if (ci == TRUE) {
      summ %>%
        add_ci(conf.level = 0.95, # add columns with confidence interval
               statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                                all_continuous() ~ "{conf.low} - {conf.high}"
               ),
               style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                                all_continuous() ~ style_sigfig
               )
        )
    } else { summ }
    
    summ2 <- summ1 %>% 
      modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                    # update the column header
      ) %>% 
      bold_labels() %>%
      italicize_levels() %>% 
      add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
             # add column with total number of non-missing observations 
      ) %>% 
      modify_caption(caption)
    
    summ3 <- if (is.null(foot_note)) {
      summ2
    } else {
      summ2 %>%
        modify_footnote(all_stat_cols() ~ foot_note)
    }
    
    summ4 <- if (flex_table == TRUE) { 
      summ3 %>%
        gtsummary::as_flex_table() 
      # as_kable_extra() covert gtsummary object to knitrkable object. 
      #as_flex_table() maintains identation, footnotes, spanning headers
    } else {
      summ3
      }
    
    summ4
    
  }

## inferential summary tables
categorical_inferential_table <- 
  function(df, foot_note = NULL, caption = "", by_vars , percent = "column", include_vars , ci=FALSE, 
           mean_vars = NULL, sum_vars = NULL, overall = FALSE, p_value = TRUE, flex_table = TRUE,
           categorical_proportion_digits = 1, continous_digits = 1, statistic_categorical = "{n} ({p}%)") {
    out <- lapply(by_vars, function(x){
      df <- (df %>%
               dplyr::mutate(across(where(is.character), sjlabelled::as_factor))
             )
      df <- df[!is.na(df[[x]]),]
      index_by <- df[[x]]
      levels_by <- if (class(index_by) == "factor") {nlevels(index_by)
      } else {
        length(unique(index_by))
      }
      label_by <- if (is.null(labelled::var_label(index_by))) {x
      } else {
        labelled::var_label(index_by)
      }
      
      summ <- if (is.null(mean_vars) & is.null(sum_vars)) {
        gtsummary::tbl_summary(df
                               , by = any_of(x)
                               , include = any_of(include_vars)
                               , type = list(
                                 all_dichotomous() ~ "categorical"
                                 ,all_continuous() ~ "continuous2"
                                 )
                               , statistic = list(
                                 all_continuous(continuous2 = TRUE) ~ c(
                                 "{mean} ({sd})",
                                 "{median} ({p25}, {p75})",
                                 "{min}, {max}" ),
                                 all_categorical() ~ statistic_categorical
                                 )
                               , digits = list(all_continuous(continuous2 = TRUE) ~ continous_digits,
                                               all_categorical() ~ c(0, categorical_proportion_digits))
                               , percent = percent #"column", "row", or "cell"
                               , missing = "ifany" #list missing data separately #ifany #no #always
                               ,missing_text = "Missing"
        ) 
      } else {
        gtsummary::tbl_summary(df
                               , by = any_of(x)
                               , include = any_of(include_vars)
                               , type = list(
                                 any_of(mean_vars) ~ "continuous2"
                                 ,any_of(sum_vars) ~ "continuous"
                                 ,all_dichotomous() ~ "categorical"
                                 ,c(all_continuous(), -any_of(sum_vars)) ~ "continuous2"
                                 )
                               , statistic = list(any_of(sum_vars) ~ "{sum}",
                                                  all_continuous2() ~ c(
                                                    "{mean} ({sd})",
                                                    "{median} ({p25}, {p75})",
                                                    "{min}, {max}" ),
                                                  all_categorical() ~ statistic_categorical
                                                  )
                               , digits = list(any_of(sum_vars) ~ continous_digits, 
                                               all_continuous2() ~ continous_digits, 
                                               all_categorical() ~ c(0, categorical_proportion_digits)
                               )
                               , percent = percent #"column", "row", or "cell"
                               , missing = "ifany" #list missing data separately #ifany #no #always
                               , missing_text = "Missing"
        )
      }
      
      summ1 <- if (ci == TRUE) {
        summ %>%
          add_ci(method = list(all_categorical() ~ "wilson", all_continuous() ~ "t.test"),
                 conf.level = 0.95, # add columns with confidence interval
                 statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                                  all_continuous() ~ "{conf.low} - {conf.high}"
                 ),
                 style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                                  all_continuous() ~ style_sigfig
                 ),
                 pattern = NULL #"{stat} / ({ci})"
          ) 
      } else { 
        summ 
      }
      
      summ2 <- if (p_value == TRUE) {
        summ1 %>% 
          add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
          ) %>%
          bold_p(t= 0.05)
      } else { summ1 }
      
      summ3 <- if (overall == TRUE) {
        summ2 %>% 
          add_overall()
      } else { summ2 }
      
      summ4 <- summ3 %>% 
        modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                      # update the column header
        ) %>% 
        bold_labels() %>%
        italicize_levels() %>%
        add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
               # add column with total number of non-missing observations
        ) %>% 
        modify_spanning_header(paste0("stat_", seq(levels_by)) ~ paste0("**", label_by, "**")
                               #all_stat_cols() ~ paste0("**", label_by, "**")
        ) %>% 
        modify_caption(caption)
      
      summ5 <- if (is.null(foot_note)) {
        summ4
      } else {
        summ4 %>%
          modify_footnote(all_stat_cols() ~ foot_note)
      }
      
      summ6 <- if (flex_table == TRUE) { 
        summ5 %>%
          gtsummary::as_flex_table() 
        # as_kable_extra() covert gtsummary object to knitrkable object. 
        #as_flex_table() maintains identation, footnotes, spanning headers
      } else {
        summ5
      }
      
      summ6
      
    })
    out
  }


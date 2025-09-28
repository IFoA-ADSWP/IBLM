testthat::test_that("test against Karol original script", {

  # ============================ Input data =====================

  data <- freMTPL2freq |> split_into_train_validate_test()

  # changing factors to characters... this is necessary as bug in original script handles factors incorrectly
  # changing "ClaimRate" to use "ClaimNb"... this is necessary as "ClaimNb" hardcoded in KG script and easier to modify in package script
  # changing "ClaimNb" to round to integer values. This is to avoid warnings in the test environment.
  splits <- data |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(tidyselect::where(is.factor), function(field) as.character(field)))) |>
    purrr::modify(.f = function(x) dplyr::rename(x, "ClaimNb" = "ClaimRate")) |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimNb = round(ClaimNb)))

  # ============================ IBLM package process =====================

  IBLM <- train_glm_xgb(
    splits,
    response_var = "ClaimNb",
    family= "poisson"
  )

  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  explainer_nu <- IBLMpackage::explain(x = IBLM, data = splits$test, migrate_reference_to_bias = FALSE)


  # ============================ Karol (og) process =====================

  withr::local_package("tidyverse")
  withr::local_package("xgboost")

  explain_og <- function(x,d,as_contribution = F){

    rownames(d) = NULL

    # currently works only if d first row contains VehGas=Regular; Area not A and Region not _?
    # caveat is not only with x=0, it's also for cases where categoricals reference level falls into intercept,
    # but shap is non-zero


    # we'll need some indication of whether a cont. varuable is = 0

    # definitions and global vars
    betas = x$glm_model$coefficients
    coef_names = names(betas)
    vartypes = lapply(x$glm_model$data,typeof) %>% unlist()
    cat_levels = lapply(x$glm_model$data[,!(vartypes %in% c("integer","double"))],FUN = function(x)sort(unique(x)))
    cat_unique_names = lapply(names(cat_levels),FUN = function(x) paste0(x,cat_levels[[x]])) %>% unlist()
    all_names = c("(Intercept)",names(vartypes[vartypes %in% c("integer","double")]),cat_unique_names)
    target = all.vars(x$glm_model$formula)[1]
    reference_levels_raw = sapply(names(cat_levels), function(var) {
      all_levels = cat_levels[[var]]
      present_levels = coef_names[startsWith(coef_names, var)]
      present_levels_clean = gsub(paste0("^", var), "", present_levels)
      setdiff(all_levels, present_levels_clean)
    }, USE.NAMES = TRUE)

    reference_levels = setdiff(all_names, c(names(betas),target))

    cont_vars = names(which(vartypes != "character" & names(vartypes) != target))

    no_cat_toggle = (length(cat_levels)==0)

    custom_colors = c("#113458", "#D9AB16", "#4096C0", "#DCDCD9", "#113458","#2166AC", "#FFFFFF", "#B2182B")

    chart_theme = theme_minimal() +
      theme(
        plot.title = element_text(color = custom_colors[5], face = "bold", size = 14),  # Title in gold
        plot.subtitle = element_text(color = custom_colors[2], size = 12),  # Subtitle in dark blue
        panel.grid.major = element_line(color = custom_colors[4], size = 0.3),  # Faint major grid
        panel.grid.minor = element_line(color = custom_colors[4], size = 0.2)
      )

    # from source data to wide OHE format inclusive of all levels
    data_dim_helper = function(frame,remove_target=T){

      # if there are no categorical columns return self
      if (no_cat_toggle){return(frame)}

      main_frame = data.frame(matrix(0,nrow=nrow(frame),ncol=length(all_names))) %>%
        set_names(all_names)

      df_onehot = frame %>%
        fastDummies::dummy_cols(select_columns = names(cat_levels),
                                remove_first_dummy = F,
                                remove_selected_columns = T) %>%
        rename_with(~ gsub("_", "", .x))
      # pivot_longer(cols =  all_of(names(cat_levels)), names_to = "variable", values_to = "value") %>%
      # mutate(value = paste0(variable, value), present = 1) %>%
      # select(-variable) %>%
      # # pivot_wider(names_from = value, values_from = present, values_fill = list(present = 0))
      # pivot_wider(names_from = value, values_from = present, values_fill = 0)

      output_frame = cbind(df_onehot,main_frame[,setdiff(all_names,colnames(df_onehot))]) %>%
        mutate("(Intercept)"=1) %>%
        select_at(all_names) %>%
        {if(remove_target) select(.,-any_of(target)) else . }

      return(output_frame)

    }

    shap_dim_helper = function(frame,wide_frame = wide_input_frame,remove_bias=F,beta_correction = T){

      if(no_cat_toggle){

        output_frame = frame %>%
          mutate(bias = frame$BIAS[1],.before=everything())

      }else{

        wide_frame = wide_frame %>% select(-any_of(c("(Intercept)",target)))

        cat_frame = lapply(names(cat_levels),FUN = function(x){

          lvl = cat_levels[[x]]
          mask = (wide_input_frame %>% select_at(paste0(x,lvl)) %>% data.matrix())

          return(matrix(rep(frame[,x],length(lvl)),byrow = F,ncol=length(lvl)) * mask)

        }) %>% bind_cols()

        output_frame = cbind(frame[,setdiff(colnames(wide_frame),colnames(cat_frame))],
                             cat_frame) %>%
          select(colnames(wide_frame)) %>%
          mutate(bias = frame$BIAS[1],.before=everything())

      }

      if(beta_correction){

        nums = names(vartypes[vartypes %in% c("integer","double")])
        nums = nums[!(nums %in% target)]

        # identify shap corrections for 0 entries
        shap_for_zeros = rowSums(((wide_frame[,nums]==0)*1)*output_frame[,nums])

        # identify shap corrections for reference levels for categoricals
        if(no_cat_toggle){
          shap_for_cat_ref = rowSums(output_frame[,reference_levels])
        }else{
          shap_for_cat_ref = 0
        }

        output_frame$bias = output_frame$bias + shap_for_zeros + shap_for_cat_ref

        if(TRUE){
          epsilon = 0.05
          denominator <- wide_frame[, nums]
          denominator[abs(denominator) < epsilon] <- epsilon
          calc = output_frame[,nums]/wide_frame[,nums]
        }else{
          calc = output_frame[,nums]/wide_frame[,nums]
        }

        # to decide if that's better than replacing with original shap values
        calc[apply(calc,2,is.infinite)] = 0 #output_frame[apply(calc,2,is.infinite)]
        output_frame[,nums] = calc

      }

      return(output_frame)

    }

    shap_correction_density = function(varname = "VehPower",q = 0.05,type="kde"){

      if(!(type %in% c("kde","hist"))){stop("type must be kde or hist!")}

      if(!(varname %in% c(names(betas),colnames(d)))){stop("varname not in model!")}

      # if it's a categorical then we have to match all varnames
      if(varname %in% names(betas) & vartypes[varname] %in% c("double","integer")){
        vartype = "numerical"
      }else if(varname %in% names(betas) & is.na(vartypes[varname])){
        vartype = "categorical_level"
      }else{
        vartype = "categorical"
      }

      if(vartype %in% c("numerical","categorical_level")){
        stderror = summary(x$glm_model)$coefficients[varname, "Std. Error"]
        x = wide_input_frame[,varname]
        shap_deviations = shap_wide[,varname]
        beta = betas[varname]
      }

      if(vartype=="categorical_level"){shap_deviations = shap_deviations[x==1]}

      from_shap = beta + quantile(shap_deviations,probs = c(q,1-q))

      lower_bound = min(from_shap[1],beta - stderror)
      upper_bound = max(from_shap[2],beta + stderror)

      data.frame(x = beta + shap_deviations) %>%
        ggplot(aes(x = x))+
        {if(type == "kde") geom_density(color = custom_colors[1], fill = custom_colors[4], alpha = 0.3)}+
        {if(type == "hist") geom_histogram(color = custom_colors[1], fill = custom_colors[4], alpha = 0.3,bins = 100)} +
        geom_vline(xintercept = beta, color = custom_colors[2], size = 0.5) +
        geom_vline(xintercept = beta - stderror, linetype = "dashed", color = custom_colors[3], size = 0.5) +
        geom_vline(xintercept = beta + stderror, linetype = "dashed", color = custom_colors[3], size = 0.5) +
        ggtitle(paste("SHAP correction density for", varname)) +
        labs(subtitle = paste0(varname, " beta: ", round(beta, 3), ", SE: ", round(stderror, 3)))+
        xlab("")+
        xlim(lower_bound,upper_bound)+
        chart_theme %>%
        return()

    }

    # browser()
    #
    # interaction = function(xvar,yvar){
    #   # yvar = "BonusMalus"
    #   # xvar = "DrivAge"
    #
    #   data.frame(xvar_shap = shap_wide[[xvar]],
    #              xvar_val = wide_input_frame[[xvar]],
    #              yvar_val = wide_input_frame[[yvar]]) %>%
    #   ggplot(aes(x = yvar_val,y=xvar_shap))+
    #     geom_point()
    #
    #
    # }

    shap_correction_scatter = function(varname = "DrivAge",q = 0.05,color=NULL,marginal=F,excl_outliers=F){

      # TODO: warning or error if reference level passed as varname
      color_vartype = "numerical"

      # if it's a categorical then we have to match all varnames
      if(varname %in% names(betas) & vartypes[varname] %in% c("double","integer")){
        vartype = "numerical"
      }else if(varname %in% names(cat_levels)){
        vartype = "categorical"
        matched_names =  sort(all_names[grep(varname, all_names)])
        reference_level = reference_levels[grep(varname, reference_levels)]
        helper_names =  matched_names[matched_names!=reference_level]

      }else{
        stop("varname not in model!")
      }

      if (!is.null(color)) {
        if(color %in% names(betas) & vartypes[color] %in% c("double","integer")){
          color_vartype = "numerical"
        }else if(color %in% names(cat_levels)){
          color_vartype = "categorical"
        }else{
          stop("color varname not in model, skipping coloring")
          color = NULL
        }
      }

      # is reference level (no glm beta)
      if(vartype=="categorical"){

        x = wide_input_frame[,matched_names]
        shap_deviations = shap_wide[,matched_names]

        # stderror = c(0,summary(x$glm_model)$coefficients[helper_names, "Std. Error"]) %>% set_names(reference_level,helper_names)
        beta = c(0,betas[helper_names]) %>% set_names(reference_level,helper_names)

        after_shap = sweep(shap_deviations, 2, beta[matched_names], FUN = "+")   %>%
          # {if(is.null(color)) . else mutate(.,color = wide_input_frame[,color])} %>%
          pivot_longer(cols = (matched_names),names_to = "x",values_to="shp")

        color = NULL
        message("color for categoricals currently not supported")

        beta_lines_df = data.frame(
          x = names(beta[matched_names]),
          y = as.numeric(beta[matched_names])
        )

        # Add the lines to the plot
        p = ggplot(data = after_shap, aes(x = x, y = shp)) +
          geom_boxplot() +
          geom_point(
            data = beta_lines_df,
            aes(x = x, y = y),
            color = "red",
          ) +
          xlab("") +
          ylab("beta correction") +
          ggtitle(paste("SHAP corrections for", varname)) +
          chart_theme

      }else{

        stderror = summary(x$glm_model)$coefficients[varname, "Std. Error"]
        beta = betas[varname]

        x = wide_input_frame[,varname]
        shap_deviations = shap_wide[,varname]
        after_shap = data.frame(x = x,
                                shp = beta + shap_deviations) %>%
          {if(is.null(color)) . else mutate(.,color = d[,color])} %>%
          {if(excl_outliers) filter(., detect_outliers(shp,method = "quantile",q=0.01)) else . }

        p  = after_shap %>%
          ggplot()+
          geom_point(aes(x = x,y=shp,group = color, color=color),alpha=0.4)+
          geom_smooth(aes(x = x,y=shp))+
          # {if(color_vartype=="categorical") scale_color_discrete(,.name = color) }+
          {if(color_vartype=="numerical") scale_color_gradientn(name = color,colors = custom_colors[c(2,1)])}+
          xlab(varname)+
          ylab("beta correction")+
          ggtitle(paste("SHAP corrections for", varname)) +
          labs(subtitle = paste0(varname, " beta: ", round(beta, 4), ", SE: ", round(stderror, 4)))+
          geom_hline(yintercept = beta, color = custom_colors[2], size = 0.5) +
          geom_hline(yintercept = beta - stderror, linetype = "dashed", color = custom_colors[3], linewidth = 0.5) +
          geom_hline(yintercept = beta + stderror, linetype = "dashed", color = custom_colors[3], linewidth = 0.5) +
          chart_theme

        if(marginal){
          p = ggExtra::ggMarginal(p,type = "density",groupColour = F, groupFill = F)
        }

      }

      return(p)

    }

    shap_intercept = function(shp){

      # if ('(Intercept)' %in% all_names){
      beta_0 = x$glm_model$coefficients['(Intercept)'] %>% as.numeric()
      beta_0_SE = data.frame(summary(x$glm_model)$coefficients)$Std..Error[1]
      # }else{
      # beta_0 = 0
      # }

      baseline = shp$BIAS[1]

      if(no_cat_toggle){

        shap_mask = d %>%
          select(-all_of(target)) %>%
          mutate(
            across(
              all_of(cont_vars),
              ~ as.integer(. == 0)
            )
          )

      }else{

        shap_mask = d %>%
          select(-all_of(target)) %>%
          mutate(
            across(
              all_of(cont_vars),
              ~ as.integer(. == 0)
            ),
            across(
              all_of(names(reference_levels_raw)),
              ~ as.integer(. == reference_levels_raw[cur_column()])
            )
          )
      }

      intercept_shap = (select(shp,-'BIAS')*shap_mask) %>%
        select_at(names(which(colSums(shap_mask)>0))) %>%
        filter(rowSums(.)!=0) %>%
        select(-any_of(cont_vars))

      if (nrow(intercept_shap)==0){
        warning("No intercept corrections due to 0-valued numeric predictors or categorical variables")
        return(NULL)
      }

      # TODO: test if works for v low nr of rows
      overall_density = intercept_shap %>%
        mutate(total_correction = rowSums(across(everything())) + baseline + beta_0) %>%
        ggplot(aes(x = total_correction))+
        geom_density()+
        geom_vline(xintercept = baseline + beta_0, color = custom_colors[2], size = 0.5)+
        geom_vline(xintercept = baseline + beta_0 - beta_0_SE, color = custom_colors[1], size = 0.5)+
        geom_vline(xintercept = baseline + beta_0 + beta_0_SE, color = custom_colors[1], size = 0.5)+
        ggtitle("Overall intercept correction distribution")+
        chart_theme

      grouped_density = intercept_shap %>%
        pivot_longer(cols = everything()) %>%
        filter(value!=0) %>%
        mutate(value = value + baseline + beta_0) %>%
        ggplot(aes(x=value))+
        geom_density()+
        facet_wrap(~name,scales="free")+
        geom_vline(xintercept = baseline + beta_0, color = custom_colors[2], size = 0.5)+
        geom_vline(xintercept = baseline + beta_0 - beta_0_SE, color = custom_colors[1], size = 0.5)+
        geom_vline(xintercept = baseline + beta_0 + beta_0_SE, color = custom_colors[1], size = 0.5)+
        ggtitle("Individual intercept correction distributions")+
        xlab("")+
        ylab("")+
        chart_theme

      jitter = intercept_shap %>%
        pivot_longer(cols = everything()) %>%
        filter(value!=0) %>%
        mutate(name = factor(name, levels = names(sort(-colSums(intercept_shap!=0)))),
               value = value + baseline + beta_0) %>%
        ggplot(aes(x = name,y=value))+
        # geom_jitter(alpha = 0.3)+
        # geom_violin(color = custom_colors[3], linewidth = 0.5)+
        geom_boxplot()+
        # ggbeeswarm::geom_beeswarm()+
        geom_hline(yintercept = baseline + beta_0, color = custom_colors[2], size = 0.5)+
        ggtitle(paste0("Jitter chart of beta corrections for intercept"),
                subtitle = paste0("Intercept: ", round(beta_0,2)," with shap baseline: ",round(baseline,2)))+
        xlab("")+
        ylab("")+
        chart_theme

      return(list(overall_density = overall_density,
                  grouped_density = grouped_density,
                  jitter = jitter))

    }

    global_c = function(shp = shap){
      dt = shp %>%
        mutate(total = rowSums(across(everything())),
               total_exp = exp(total))
      dt %>%
        ggplot(aes(x = total_exp))+
        geom_density()+
        geom_vline(xintercept = 1)+
        ggtitle(paste0("Distribution of corrections to GLM prediction"),
                subtitle = paste0("mean correction: ", round(mean(dt$total_exp),3)))+
        xlab("")+
        ylab("")+
        chart_theme
    }


    shap = predict(x$xgb_model,newdata = xgb.DMatrix(data.matrix(select(d,-target))),predcontrib = T) %>% data.frame()
    wide_input_frame = data_dim_helper(frame = d)
    shap_wide = shap_dim_helper(frame = shap)

    return(list(shap_correction_scatter = shap_correction_scatter,
                shap_correction_density = shap_correction_density,
                shap_intercept = shap_intercept(shp = shap),
                overall_correction = global_c,
                input_frame = d,
                shap_wide = shap_wide, # beta_corrections
                raw_shap = shap,
                betas = betas,
                allnames = names(betas)[-1]))


    #
    #
    #
    #
    # #check - ok
    # exp(betas  %*% t(wide_input_frame[10,names(betas)]) %>% c())==predict(x$glm_model,d[10,],type="response")
    # (predict(x$xgb_model,xgb.DMatrix(data.matrix(d[,-1])),type="response")-exp(rowSums(shap)))/exp(rowSums(shap))
    #
    # predict(x$xgb_model,xgb.DMatrix(data.matrix(d[10,-1])),type="response")
    # predict(x$xgb_model,xgb.DMatrix(data.matrix(d[,-1])),type="response")
    #
    #
    #
    # d = d[,x$xgb_model$feature_names]
    #
    # xgbdata = xgb.DMatrix(data.matrix(d))
    #
    # exp(sum(predict(x$xgb_model,newdata = xgb.DMatrix(data.matrix(d[10,-1])),predcontrib = T) %>% data.frame()))
    # shap =  predict(x$xgb_model,newdata = xgb.DMatrix(data.matrix(d[  ,-1])),predcontrib = T) %>% data.frame()
    # exp(sum(shap[10,]))


  }

  suppressWarnings({

  explainer_og <- explain_og(x = IBLM,d = splits$test)

  })

  # ============================ comparisons =====================

  testthat::expect_equal(
    explainer_nu$allnames,
    explainer_og$allnames
  )

  testthat::expect_equal(
    explainer_nu$beta_corrections |> colSums(),
    explainer_og$shap_wide |> colSums()
  )

  testthat::expect_equal(
    explainer_nu$shap |> colSums(),
    explainer_og$raw_shap |> colSums()
  )

  testthat::expect_equal(
    explainer_nu$glm_beta_coeff,
    explainer_og$betas
  )



})

fit_models <- function(model_func, outcome_vars, rhs_formula, data, ...) {
  models <- lapply(outcome_vars, function(outcome_var) {
    formula <- reformulate(rhs_formula, response = outcome_var)
    model <- do.call(model_func, list(formula = formula, data = data, ...))
    return(model)
  })
  names(models) <- outcome_vars
  return(models)
}




p_sobel <- function(a, s_a, b, s_b){
  z = (a*b)/(sqrt(b^2 * s_a^2 + a^2 * s_b^2))
  return (1- pnorm(z))*2
}

sobel_test <- function(Hypothesis, model_med, vcov_med, model_out, vcov_out, predictor, mediator, Predictor, Mediator, stars=FALSE, method, decimal_places = 3, model_orig, vcov_orig){
  #replace vcov by cluster robust version
  model_med@vcov_beta <- as.matrix(vcov_med) 
  model_out@vcov_beta <- as.matrix(vcov_out)
  model_orig@vcov_beta <- as.matrix(vcov_orig)
  
  parameters_med <- data.frame(summary(model_med)$coefficients, ddf = method) %>% rownames_to_column()
  a <- parameters_med %>% filter(rowname == predictor) %>% .[[1,"Estimate"]]
  s_a <- parameters_med %>% filter(rowname == predictor) %>% .[[1,"Std..Error"]]
  
  parameters_out <- data.frame(summary(model_out)$coefficients, ddf = method) %>% rownames_to_column()
  b <- parameters_out %>% filter(rowname == mediator) %>% .[[1,"Estimate"]]
  s_b <- parameters_out %>% filter(rowname == mediator) %>% .[[1,"Std..Error"]]
  c_dash <- parameters_out %>% filter(rowname == predictor) %>% .[[1,"Estimate"]]
  
  parameters_orig <- data.frame(summary(model_orig)$coefficients, ddf = method) %>% rownames_to_column()
  c <- parameters_orig %>% filter(rowname == predictor) %>% .[[1,"Estimate"]]
  
  if(stars){
    p_a <- parameters_med %>% filter(rowname == predictor) %>% .[[1,"Pr...t.."]]
    p_b <- parameters_out %>% filter(rowname == mediator) %>% .[[1,"Pr...t.."]]
    p_c_dash <- parameters_out %>% filter(rowname == predictor) %>% .[[1,"Pr...t.."]]
    p_c <- parameters_orig %>% filter(rowname == predictor) %>% .[[1,"Pr...t.."]]
    a_star <- paste0(round(a,decimal_places), ifelse(p_a<0.1,
                              ifelse(p_a<0.05,
                                     ifelse(p_a<0.01,
                                            ifelse(p_a<0.001,
                                                       "***",
                                                       "**"
                                                ),
                                            "*"),
                                     "+"),
                              ""))
    b_star <- paste0(round(b,decimal_places), ifelse(p_b<0.1,
                               ifelse(p_b<0.05,
                                      ifelse(p_b<0.01,
                                             ifelse(p_b<0.001,
                                                    "***",
                                                    "**"
                                             ),
                                             "*"),
                                      "+"),
                               ""))
    c_dash_star <- paste0(round(c_dash,decimal_places), ifelse(p_c_dash<0.1,
                               ifelse(p_c_dash<0.05,
                                      ifelse(p_c_dash<0.01,
                                             ifelse(p_c_dash<0.001,
                                                    "***",
                                                    "**"
                                             ),
                                             "*"),
                                      "+"),
                               ""))
    c_star <- paste0(round(c,decimal_places), ifelse(p_c<0.1,
                                                     ifelse(p_c<0.05,
                                                            ifelse(p_c<0.01,
                                                                   ifelse(p_c<0.001,
                                                                          "***",
                                                                          "**"
                                                                   ),
                                                                   "*"),
                                                            "+"),
                                                     ""))
    return(data.frame("Hypothesis" = Hypothesis, "Predictor" = Predictor, "Mediator" = Mediator, "c" = c_star, "a" = a_star, "b" = b_star, "c'" = c_dash_star, "p_Sobel" = p_sobel(a, s_a, b, s_b)))
  }
  
  return(data.frame("Hypothesis" = Hypothesis, "Predictor" = Predictor, "Mediator" = Mediator, "c" = c, "a" = a, "b" = b, "c'" = c_dash, "p_Sobel" = p_sobel(a, s_a, b, s_b)))
}


mediation_table <- function(list_mediation_models, stars = FALSE, method, decimal_places = 3, model_orig, vcov_orig){
  
  
  sobel_results_list <- lapply(list_mediation_models, 
                               FUN = function(x){ 
                                 sobel_test(x$Hypothesis, x$model_med, x$vcov_med, x$model_out, x$vcov_out, x$predictor, x$mediator, x$Predictor, x$Mediator, stars, method, decimal_places, model_orig, vcov_orig)
                                 })
  
  
  
  return(bind_rows(sobel_results_list))
}


# R function to plot conditional effects for linear (and multilevel linear) regression
# Johannes Karreth, customized by Lea Tamberg

# This function was inspired by David A. Armstrong's DAintfun2
#  function (in the DAmisc package).
# It uses ggplot2, adds custom variable names for axis labels (varlabs)
#  and puts the conditional effect of X1/X2 in the subplot title to save space.
# The function also automatically adjusts the plot type for binary vs. continuous
#  moderators.
# obj: lm or lmer object
# varnames: character vector of the constitutive terms, e.g. c("x1", "x2")
# varlabs: character vector of length 2 with the desired variable names for
#  x1 and x2 to show up as labels in the plot.
ggintplot <- function (obj, varnames, vcov = NULL, varlabs, 
          other_interactions = NULL,
          alpha = 0.05,
          title = FALSE,
          subtitle = NULL,
          rug = FALSE, 
          twoways = FALSE, 
          rugsize = 0.1, 
          jitter_factor = 0,
          max_x = NULL,
          unlog_x = FALSE,
          log_scale_x_axis = FALSE,
          font_family = "sans",
          font_size = 12,
          color_rug = "red",
          rug_alpha = 1){
  
  require(ggplot2); require(gridExtra)
  
  if(is.null(vcov)) vcov = vcov(obj)
  
  if(!is.null(other_interactions) & twoways) print("Warning: other_interactions will only be considered in the first marginal effects plot.")
  
  if(class(obj) == "lm"){
    
    if(!is.null(other_interactions)) print("Warning: so far only one interaction term per effect in lm")
    
    if (!("model" %in% names(obj))) {
      obj <- update(obj, model = T)
    }
    
    v1 <- varnames[1]
    v2 <- varnames[2]
    vlab1 <- varlabs[1]
    vlab2 <- varlabs[2]
    if(is.null(max_x)) max_x <- max(model.matrix(obj)[, v2])
    
    if(length(unique(model.matrix(obj)[, paste(v1)])) == 2){
      s1 <- c(min(model.matrix(obj)[, paste(v1)]), max(model.matrix(obj)[, paste(v1)]))
    }
    
    if(length(unique(model.matrix(obj)[, paste(v1)])) > 2){
      s1 <- seq(from = min(model.matrix(obj)[, paste(v1)]), to = max(model.matrix(obj)[, paste(v1)]), length.out = 25)
    }
    
    if(length(unique(model.matrix(obj)[, paste(v2)])) == 2){
      s2 <- c(min(model.matrix(obj)[, paste(v2)]), max(model.matrix(obj)[, paste(v2)]))
    }
    
    if(length(unique(model.matrix(obj)[, paste(v2)])) > 2){
      s2 <- seq(from = min(model.matrix(obj)[, v2]), to = max_x, length.out = 25)
    }
    
    
    
    b1.pos <- grep(pattern = v1, x = names(coef(obj)), fixed = TRUE)[1]
    b3.pos <- c(grep(pattern = paste(v1,":",v2, sep = ""), x = names(coef(obj)), fixed = TRUE), 
                grep(pattern = paste(v2,":",v1, sep = ""), x = names(coef(obj)), fixed = TRUE))[1]
    b1 <- as.numeric(obj$coef[b1.pos])
    b3 <- as.numeric(obj$coef[b3.pos])
    var1 <- vcov[b1.pos, b1.pos]
    var3 <- vcov[b3.pos, b3.pos]
    cov13 <- vcov[b1.pos, b3.pos]
    
    eff1 <- b1 + b3 * s2
    var.eff1 <- var1 + s2^2 * var3 + 2 * s2 * cov13
    se.eff1 <- sqrt(var.eff1)
    low1 <- eff1 - qt(1-alpha/2, df = obj$df.residual) * se.eff1
    up1 <- eff1 + qt(1-alpha/2, df = obj$df.residual) * se.eff1
    
    b2.pos <- grep(pattern = v2, x = names(coef(obj)), fixed = TRUE)[1]
    b2 <- as.numeric(obj$coef[b2.pos])

    var2 <- vcov[b2.pos, b2.pos]

    cov23 <- vcov[b2.pos, b3.pos]
    
    eff2 <- b2 + b3 * s1
    var.eff2 <- var2 + s1^2 * var3 + 2 * s1 * cov23
    se.eff2 <- sqrt(var.eff2)
    low2 <- eff2 - qt(1-alpha/2, df = obj$df.residual) * se.eff2
    up2 <- eff2 + qt(1-alpha/2, df = obj$df.residual) * se.eff2
    
    rug2.dat <- data.frame(model.matrix(obj)[, paste(v2)], model.matrix(obj)[, paste(v1)]) %>% filter(model.matrix.obj....paste.v2.. < max_x)
    rug1.dat <- data.frame(model.matrix(obj)[, paste(v1)], model.matrix(obj)[, paste(v2)])
    
  }
  
  if(class(obj) == "lmerMod"){
    
    
    
    # Model matrix always included as obj@frame
    
    v1 <- varnames[1]
    v2 <- varnames[2]
    vlab1 <- varlabs[1]
    vlab2 <- varlabs[2]
    if(is.null(max_x)) max_x <- max(model.matrix(obj)[, v2])
    
    
    b1.pos <- grep(v1, names(fixef(obj)))[1]
    b3.pos <- c(grep(pattern = paste(v1,":",v2, sep = ""), x = names(fixef(obj)), fixed = TRUE), 
                grep(pattern = paste(v2,":",v1, sep = ""), x = names(fixef(obj)), fixed = TRUE))[1]
    b1 <- as.numeric(fixef(obj)[b1.pos])
    b3 <- as.numeric(fixef(obj)[b3.pos])
    var1 <- vcov[b1.pos, b1.pos]
    var3 <- vcov[b3.pos, b3.pos]
    cov13 <- vcov[b1.pos, b3.pos]
    
    if(length(unique(model.matrix(obj)[, paste(v1)])) == 2){
      s1 <- c(min(model.matrix(obj)[, paste(v1)]), max(model.matrix(obj)[, paste(v1)]))
    }
    
    if(length(unique(model.matrix(obj)[, paste(v1)])) > 2){
      s1 <- seq(from = min(model.matrix(obj)[, paste(v1)]), to = max(model.matrix(obj)[, paste(v1)]), length.out = 25)
    }
    
    if(length(unique(model.matrix(obj)[, paste(v2)])) == 2){
      s2 <- c(min(model.matrix(obj)[, paste(v2)]), max(model.matrix(obj)[, paste(v2)]))
    }
    
    if(length(unique(model.matrix(obj)[, paste(v2)])) > 2){
      s2 <- seq(from = min(model.matrix(obj)[, v2]), to = max_x, length.out = 25)
    }
    
    eff1 <- b1 + b3 * s2
    var.eff1 <- var1 + s2^2 * var3 + 2 * s2 * cov13
    
    if(!is.null(other_interactions)){
      positions_values <- lapply(other_interactions, 
                    FUN = function(v){
                      pos = c(grep(pattern = paste(v1,":",v, sep = ""), x = names(fixef(obj)), fixed = TRUE), 
                        grep(pattern = paste(v,":",v1, sep = ""), x = names(fixef(obj)), fixed = TRUE))[1]
                      if(length(unique(model.matrix(obj)[, paste(v)])) == 2){
                        value = 1
                      } else{
                        value = mean(model.matrix(obj)[, v])
                      }
                      return(c(pos,value))
                    }
                    )
      
      for(pos_value in positions_values){
        pos = pos_value[1]
        value = pos_value[2]
        eff1 <- eff1 + value * as.numeric(fixef(obj)[pos])
        var.eff1 <- var.eff1 + value^2 * vcov[pos,pos] + 2 * value * vcov[b1.pos, pos]  + 2 * s2 * value * vcov[b3.pos, pos]
        for(pos_value2 in positions_values){
          pos2 = pos_value2[1]
          value2 = pos_value2[2]
          if(pos < pos2){
            var.eff1 <- var.eff1 + 2 * value * value2 * vcov[pos,pos2]
          }
        }
      }
      
    }
    
    se.eff1 <- sqrt(var.eff1)
    low1 <- eff1 - qnorm(1-alpha/2) * se.eff1
    up1 <- eff1 + qnorm(1-alpha/2) * se.eff1
    
    b2.pos <- grep(v2, names(fixef(obj)))[1]
    b2 <- as.numeric(fixef(obj)[b2.pos])
    var2 <- vcov[b2.pos, b2.pos]

    cov23 <- vcov[b2.pos, b3.pos]
    
    eff2 <- b2 + b3 * s1
    var.eff2 <- var2 + s1^2 * var3 + 2 * s1 * cov23
    se.eff2 <- sqrt(var.eff2)
    low2 <- eff2 - qnorm(1-alpha/2) * se.eff2
    up2 <- eff2 + qnorm(1-alpha/2) * se.eff2
    
    rug2.dat <- data.frame(model.matrix(obj)[, paste(v2)], model.matrix(obj)[, paste(v1)]) %>% filter(model.matrix.obj....paste.v2.. < max_x)
    rug1.dat <- data.frame(model.matrix(obj)[, paste(v1)], model.matrix(obj)[, paste(v2)])
    
  }
  
  
  if(unlog_x){
    s2 = exp(s2)
    rug2.dat <- rug2.dat %>% mutate(model.matrix.obj....paste.v2.. = exp(model.matrix.obj....paste.v2..))
  }
  
  plot1.dat <- data.frame(s2, eff1, se.eff1, low1, up1)
  
  plot2.dat <- data.frame(s1, eff2, se.eff2, low2, up2)  
  
  if (twoways == FALSE) {
    
    if(length(unique(model.matrix(obj)[, paste(v2)])) == 2){
      
      p1 <- ggplot(data = plot1.dat, aes(x = factor(s2), y = eff1), environment = environment()) +     # env. important for rug
        geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
        geom_segment(aes(x = factor(s2), xend = factor(s2), y = low1, yend = up1), color = "black") +
        geom_point() + 
        xlab(paste(vlab2)) + ylab(paste("Effect of ", vlab1, sep = ""))
    }
    
    if(length(unique(model.matrix(obj)[, paste(v2)])) > 2){
      
      p1 <- ggplot(data = plot1.dat, aes(x = s2, y = eff1), environment = environment()) +     # env. important for rug
        geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
        geom_ribbon(aes(x = s2, ymin = low1, ymax = up1), alpha = 0.25, color = NA) +
        geom_line() + 
        xlab(paste(vlab2)) + ylab(paste("Effect of ", vlab1, sep = ""))
    }
    
    if(title == TRUE){
      p1 <- p1 + labs(title = paste("Conditional effect of \n", vlab1, sep = ""),
                      subtitle = subtitle)
    }  
    
    if(rug == TRUE & length(unique(model.matrix(obj)[, paste(v2)])) > 2){
      if(length(unique(model.matrix(obj)[, paste(v1)])) == 2 & !is.null(color_rug)){
        p1 <- p1 + 
          geom_rug(data = rug2.dat, 
                   aes(x = jitter(rug2.dat[, 1], factor = jitter_factor), y = 0, color = factor(rug2.dat[, 2])), sides = "b", size = rugsize, alpha = rug_alpha, show.legend = FALSE) +
          scale_color_manual(values = c("black", color_rug))
      }else{
        p1 <- p1 + 
          geom_rug(data = rug2.dat, 
                   aes(x = jitter(rug2.dat[, 1], factor = jitter_factor), y = 0), sides = "b", size = rugsize, alpha = rug_alpha, show.legend = FALSE)
      }
    }     
    
    p1 <- p1 + theme_light() +
      theme(text = element_text(size=font_size, family=font_family))
    
    if(log_scale_x_axis){
      p1 <- p1 + scale_x_continuous(trans='log10')
    }
    
    return(p1)
  }
  
  
  
  
  if (twoways == TRUE) {
    if(length(unique(model.matrix(obj)[, paste(v2)])) == 2){
      p1 <- ggplot(data = plot1.dat, aes(x = factor(s2), y = eff1), environment = environment()) +     # env. important for rug
        geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
        geom_segment(aes(x = factor(s2), xend = factor(s2), y = low1, yend = up1), color = "black") +
        geom_point() + 
        xlab(paste(vlab2)) + ylab(paste("Effect of ", vlab1, sep = ""))
    }
    
    if(length(unique(model.matrix(obj)[, paste(v2)])) > 2){
      p1 <- ggplot(data = plot1.dat, aes(x = s2, y = eff1), environment = environment()) +     # env. important for rug
        geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
        geom_ribbon(aes(x = s2, ymin = low1, ymax = up1), alpha = 0.25, color = NA) +
        geom_line() + 
        # scale_x_continuous(trans = exp_trans()) +
        xlab(paste(vlab2)) + ylab(paste("Effect of ", vlab1, sep = ""))
    }   
    
    # if(length(unique(model.matrix(obj)[, paste(v2)])) > 2){
    #   p1 <- ggplot(data = plot1.dat, aes(x = exp(s2), y = eff1), environment = environment()) +     # env. important for rug
    #     geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
    #     geom_ribbon(aes(x = exp(s2), ymin = low1, ymax = up1), alpha = 0.25, color = NA) +
    #     geom_line() + 
    #     # scale_x_continuous(trans = exp_trans()) +
    #     xlab(paste(vlab2)) + ylab(paste("Effect of ", vlab1, sep = ""))
    # }
    
    p1 <- p1 + labs(title = paste("Conditional effect of \n", vlab1, sep = ""),
                    subtitle = subtitle) + theme_light() +
      theme(text = element_text(size=font_size, family=font_family))
    
    if(rug == TRUE & length(unique(model.matrix(obj)[, paste(v2)])) > 2){
      if(length(unique(model.matrix(obj)[, paste(v1)])) == 2 & !is.null(color_rug)){
        p1 <- p1 + 
          geom_rug(data = rug2.dat, 
                   aes(x = jitter(rug2.dat[, 1], factor = jitter_factor), y = 0, color = factor(rug2.dat[, 2])), sides = "b", size = rugsize, alpha = rug_alpha, show.legend = FALSE) +
          scale_color_manual(values = c("black", color_rug))
      }else{
        p1 <- p1 + 
          geom_rug(data = rug2.dat, 
                   aes(x = jitter(rug2.dat[, 1], factor = jitter_factor), y = 0), sides = "b", size = rugsize, alpha = rug_alpha, show.legend = FALSE)
      }
    }     
    
    if(length(unique(model.matrix(obj)[, paste(v1)])) == 2){
      p2 <- ggplot(data = plot2.dat, aes(x = factor(s1), y = eff2), environment = environment()) +     # env. important for rug
        geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
        geom_segment(aes(x = factor(s1), xend = factor(s1), y = low2, yend = up2), color = "black") +
        geom_point() + 
        xlab(paste(vlab1)) + ylab(paste("Effect of ", vlab2, sep = ""))
    }
    
    if(length(unique(model.matrix(obj)[, paste(v1)])) > 2){
      p2 <- ggplot(data = plot2.dat, aes(x = s1, y = eff2), environment = environment()) +     # env. important for rug
        geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
        geom_ribbon(aes(x = s1, ymin = low2, ymax = up2), alpha = 0.25, color = NA) +
        geom_line() + 
        xlab(paste(vlab1)) + ylab(paste("Effect of ", vlab2, sep = ""))
      
    }  
    
    p2 <- p2 + labs(title = paste("Conditional effect of \n", vlab2, sep = ""),
                    subtitle = subtitle) + theme_light() +
      theme(text = element_text(size=font_size, family=font_family))
    
    if(rug == TRUE & length(unique(model.matrix(obj)[, paste(v1)])) > 2){
      if(length(unique(model.matrix(obj)[, paste(v2)])) == 2 & !is.null(color_rug)){
        p2 <- p2 + 
          geom_rug(data = rug1.dat, 
                   aes(x = jitter(rug1.dat[, 1], factor = jitter_factor), y = 0, color = factor(rug1.dat[, 2])), sides = "b", size = rugsize, alpha = rug_alpha, show.legend = FALSE) +
          scale_color_manual(values = c("black", color_rug))
      }else{
        p2 <- p2 + 
          geom_rug(data = rug1.dat, 
                   aes(x = jitter(rug1.dat[, 1], factor = jitter_factor), y = 0), sides = "b", size = rugsize, alpha = rug_alpha, show.legend = FALSE)
      }
    }     
    
    
    if(log_scale_x_axis){
      p1 <- p1 + scale_x_continuous(trans='log')
    }
    
    grid.arrange(p1, p2, ncol = 2)
  }
}





ggintplot_categorical_moderator <- function (obj, pred, categorical_moderator, levels, vcov = NULL, varlabs, other_interactions = NULL,
                       alpha = 0.05,
                       title = FALSE,
                       subtitle = NULL, 
                       font_family = "sans",
                       font_size = 7){
  
  require(ggplot2); require(gridExtra)
  
  if(is.null(vcov)) vcov = vcov(obj)
  
  if(class(obj) == "lmerMod"){
    
    if(!is.null(other_interactions)) print("Warning: so far only one interaction term per effect")
    
    

    # build the names of the dummy variables (first one needs none because it is the reference level)
    modnames <- lapply(levels[-1], function(level) paste(categorical_moderator, level, sep=""))
    
    
    
    # the effect of the reference level is simply the one reported for the predictor 
    eff_reference <- as.numeric(fixef(obj)[pred])
    var_reference <- vcov[pred,pred]
    se_reference <- sqrt(var_reference)
    
    # for each other level, we have to add the interaction coefficient
    effects <- 
      lapply(modnames,
             function(mod){
               pos = c(grep(pattern = paste(mod,":",pred, sep = ""), x = names(fixef(obj)), fixed = TRUE), 
                       grep(pattern = paste(pred,":",mod, sep = ""), x = names(fixef(obj)), fixed = TRUE))[1]
               eff_reference + as.numeric(fixef(obj)[pos])
             })
    
    # same for standard errors
    standard_errors <- 
      lapply(modnames,
             function(mod){
               pos = c(grep(pattern = paste(mod,":",pred, sep = ""), x = names(fixef(obj)), fixed = TRUE), 
                       grep(pattern = paste(pred,":",mod, sep = ""), x = names(fixef(obj)), fixed = TRUE))[1]
               sqrt(var_reference + vcov[pos, pos] + 2 * vcov[pred, pos])
             })
    
    
    
    effects <- append(effects, eff_reference, after = 0)
    standard_errors <- append(standard_errors, se_reference, after = 0)
    
    plot.dat <- 
      tibble(moderator = unlist(levels), effect = unlist(effects), standard_error = unlist(standard_errors)) %>% 
      mutate(low = effect - qnorm(1-alpha/2) * standard_error,
             up = effect + qnorm(1-alpha/2) * standard_error
      )
    
    
    
    
  }
  
  
  if(class(obj) == "lm"){
    
    if(!is.null(other_interactions)) print("Warning: so far only one interaction term per effect")
    
    if (!("model" %in% names(obj))) {
      obj <- update(obj, model = T)
    }
    

    # build the names of the dummy variables (first one needs none because it is the reference level)
    modnames <- lapply(levels[-1], function(level) paste(categorical_moderator, level, sep=""))
    
    # the effect of the reference level is simply the one reported for the predictor 
    eff_reference <- as.numeric(obj$coef[pred])
    var_reference <- vcov[pred,pred]
    se_reference <- sqrt(var_reference)
    
    # for each other level, we have to add the interaction coefficient
    effects <- 
      lapply(modnames,
             function(mod){
               pos = c(grep(pattern = paste(mod,":",pred, sep = ""), x = names(coef(obj)), fixed = TRUE), 
                       grep(pattern = paste(pred,":",mod, sep = ""), x = names(coef(obj)), fixed = TRUE))[1]
               eff_reference + as.numeric(obj$coef[pos])
    })
    
    # same for standard errors
    standard_errors <- 
      lapply(modnames,
             function(mod){
               pos = c(grep(pattern = paste(mod,":",pred, sep = ""), x = names(coef(obj)), fixed = TRUE), 
                       grep(pattern = paste(pred,":",mod, sep = ""), x = names(coef(obj)), fixed = TRUE))[1]
               sqrt(var_reference + vcov[pos, pos] + 2 * vcov[pred, pos])
             })
    
    
    
    effects <- append(effects, eff_reference, after = 0)
    standard_errors <- append(standard_errors, se_reference, after = 0)
    
    plot.dat <- 
      tibble(moderator = unlist(levels), effect = unlist(effects), standard_error = unlist(standard_errors)) %>% 
      mutate(low = effect - qt(1-alpha/2, df = obj$df.residual) * standard_error,
             up = effect + qt(1-alpha/2, df = obj$df.residual) * standard_error
      )
      
  
    
    
  }
  
    p1 <- ggplot(data = plot.dat, aes(x = forcats::fct_inorder(moderator), y = effect)) +   
      geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) + 
      geom_segment(aes(x = forcats::fct_inorder(moderator), xend = moderator, y = low, yend = up), color = "black") +
      geom_point() +
      xlab(sub("_", " ", categorical_moderator)) + 
      ylab(paste("Effect of ", sub("_", " ", pred), sep = "")) +
      scale_x_discrete(labels = sub("_", " ", levels))
 
    

    
    if(title == TRUE){
      p1 <- p1 + labs(title = paste("Conditional effect of \n", sub("_", " ", pred), sep = ""),
                      subtitle = subtitle)
    }  
    
    p1 <- p1 + theme_light() +
      theme(text = element_text(size=font_size, family=font_family))
    

    
    return(p1)
  
  
  
  
  
  
}

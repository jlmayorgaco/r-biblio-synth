# stats/models/WeibullSModel.R
WeibullSModel <- R6::R6Class(
  "WeibullSModel",
  inherit = BaseModel,
  public = list(
    fit = function(df) {
      self$df <- df
      cat("[WeibullSModel] Starting fit…\n")

      a_start <- max(df$Value, na.rm=TRUE)
      k_start <- 3      # shape >0
      l_start <- 10     # scale >0 (years)
      df$Year_scaled <- df$Year - min(df$Year, na.rm=TRUE)

      start_list <- list(a=a_start, k=k_start, l=l_start)
      ctrl_lm <- minpack.lm::nls.lm.control(maxiter=6000, ftol=1e-10, ptol=1e-10)

      # F(x) = a*(1 - exp(-(x/l)^k))
      self$model <- tryCatch({
        minpack.lm::nlsLM(
          Value ~ a * (1 - exp(- ( (Year_scaled / l)^k ))),
          data=df, start=start_list, control=ctrl_lm, trace=TRUE,
          lower=c(a=0, k=1e-3, l=1e-3)
        )
      }, error=function(e) { cat("[WeibullSModel] nlsLM failed:", e$message, "\n"); NULL })

      if (is.null(self$model)) {
        cat("[WeibullSModel] Falling back to nls2 grid…\n")
        start_grid <- expand.grid(
          a = seq(0.7,1.3,length.out=5)*a_start,
          k = c(0.8,1.2,2,3,5,8),
          l = seq(5, 30, length.out=8)
        )
        self$model <- tryCatch({
          suppressWarnings(
            nls2::nls2(
              Value ~ a * (1 - exp(- ( (Year_scaled / l)^k ))),
              data=df, start=start_grid, algorithm="brute-force",
              control=nls.control(maxiter=2500, tol=1e-6, minFactor=1e-10), trace=TRUE
            )
          )
        }, error=function(e){ cat("[WeibullSModel] nls2 failed:", e$message, "\n"); NULL })
      }

      if (!is.null(self$model)) { cat("[WeibullSModel] Fit OK.\n"); print(coef(self$model)) }
      else cat("[WeibullSModel] Fit failed.\n")
      invisible(self)
    },

    r2 = function() {
      rss <- tryCatch(deviance(self$model), error=function(e) NA_real_)
      y   <- self$df[[self$response_col]]
      tss <- sum((y - mean(y,na.rm=TRUE))^2, na.rm=TRUE)
      ifelse(is.na(rss) || tss<=0, NA_real_, 1 - rss/tss)
    },

    summary = function() {
      list(
        type="weibull_s",
        formula = if(!is.null(self$model)) deparse(formula(self$model)) else NA_character_,
        coef = tryCatch(as.list(coef(self$model)), error=function(e) NULL),
        Robustness = robust_for_json(self$robust),
        stats=list(
          rss=tryCatch(deviance(self$model), error=function(e) NA_real_),
          R2 = self$r2(),
          AIC=tryCatch(AIC(self$model), error=function(e) NA_real_),
          BIC=tryCatch(BIC(self$model), error=function(e) NA_real_)
        )
      )
    },

    graphs = function() {
      cf <- as.list(coef(self$model)); a <- cf$a; k <- cf$k; l <- cf$l
      y0 <- min(self$df$Year, na.rm=TRUE)

      # Solve a*(1 - exp(-(t/l)^k)) = p*a  ->  (t/l)^k = -ln(1-p)
      inv_weib_t <- function(p) ( l * (-log(1-p))^(1/k) )
      t5  <- inv_weib_t(0.05) + y0
      t50 <- inv_weib_t(0.50) + y0
      t95 <- inv_weib_t(0.95) + y0

      list(
        list(param_name="a", param_latex="$a$", param_value=a, param_axis="y"),
        list(param_name="k", param_latex="$k$", param_value=k, param_axis=NULL),
        list(param_name="l", param_latex="$\\lambda$", param_value=l, param_axis=NULL),
        list(param_name="t5",  param_latex="$t_{5\\%}$",  param_value=t5,  param_axis="x"),
        list(param_name="t50", param_latex="$t_{50\\%}$", param_value=t50, param_axis="x"),
        list(param_name="t95", param_latex="$t_{95\\%}$", param_value=t95, param_axis="x"),
        list(param_name="Delta_t", param_latex="$\\Delta t$", param_value=t95-t5, param_axis=NULL)
      )
    }
  )
)
MODEL_REGISTRY$weibull_s <- WeibullSModel

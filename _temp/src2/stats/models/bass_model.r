# stats/models/BassModel.R
# Cumulative adoption curve: F(t) = a * (1 - exp(-(p+q)t)) / (1 + (q/p)*exp(-(p+q)t))
# We parameterize in years since first year.
BassModel <- R6::R6Class(
  "BassModel",
  inherit = BaseModel,
  public = list(
    fit = function(df) {
      self$df <- df
      cat("[BassModel] Starting fit…\n")

      a_start <- max(df$Value, na.rm=TRUE)
      p_start <- 0.01
      q_start <- 0.3

      y0 <- min(df$Year, na.rm=TRUE)
      df$Year_scaled <- df$Year - y0

      start_list <- list(a=a_start, p=p_start, q=q_start)
      ctrl_lm <- minpack.lm::nls.lm.control(maxiter=6000, ftol=1e-10, ptol=1e-10)

      F_bass <- function(a,p,q,t) a * (1 - exp(-(p+q)*t)) / (1 + (q/p)*exp(-(p+q)*t))

      self$model <- tryCatch({
        minpack.lm::nlsLM(
          Value ~ F_bass(a,p,q,Year_scaled),
          data=df, start=start_list, control=ctrl_lm, trace=TRUE,
          lower=c(a=0, p=1e-5, q=1e-5)
        )
      }, error=function(e){ cat("[BassModel] nlsLM failed:", e$message, "\n"); NULL })

      if (is.null(self$model)) {
        cat("[BassModel] Falling back to nls2 grid…\n")
        start_grid <- expand.grid(
          a = seq(0.7,1.3,length.out=5)*a_start,
          p = c(0.001,0.005,0.01,0.02,0.05),
          q = c(0.05,0.1,0.2,0.3,0.5,0.8)
        )
        self$model <- tryCatch({
          suppressWarnings(
            nls2::nls2(
              Value ~ F_bass(a,p,q,Year_scaled),
              data=df, start=start_grid, algorithm="brute-force",
              control=nls.control(maxiter=2500, tol=1e-6, minFactor=1e-10), trace=TRUE
            )
          )
        }, error=function(e){ cat("[BassModel] nls2 failed:", e$message, "\n"); NULL })
      }

      if (!is.null(self$model)) { cat("[BassModel] Fit OK.\n"); print(coef(self$model)) }
      else cat("[BassModel] Fit failed.\n")
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
        type="bass",
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
      cf <- as.list(coef(self$model)); a <- cf$a; p <- cf$p; q <- cf$q
      y0 <- min(self$df$Year, na.rm=TRUE)
      F_inv <- function(pct) {
        # invert numerically (safe + simple secant)
        f <- function(t) a * (1 - exp(-(p+q)*t)) / (1 + (q/p)*exp(-(p+q)*t)) - pct*a
        t <- uniroot(f, interval=c(0, 2*max(self$df$Year - y0, na.rm=TRUE) + 50))$result
      }
      t5  <- tryCatch(F_inv(0.05) + y0, error=function(e) NA_real_)
      t50 <- tryCatch(F_inv(0.50) + y0, error=function(e) NA_real_)
      t95 <- tryCatch(F_inv(0.95) + y0, error=function(e) NA_real_)

      list(
        list(param_name="a", param_latex="$a$", param_value=a, param_axis="y"),
        list(param_name="p", param_latex="$p$", param_value=p, param_axis=NULL),
        list(param_name="q", param_latex="$q$", param_value=q, param_axis=NULL),
        list(param_name="t5",  param_latex="$t_{5\\%}$",  param_value=t5,  param_axis="x"),
        list(param_name="t50", param_latex="$t_{50\\%}$", param_value=t50, param_axis="x"),
        list(param_name="t95", param_latex="$t_{95\\%}$", param_value=t95, param_axis="x"),
        list(param_name="Delta_t", param_latex="$\\Delta t$", param_value=t95-t5, param_axis=NULL)
      )
    }
  )
)
MODEL_REGISTRY$bass <- BassModel

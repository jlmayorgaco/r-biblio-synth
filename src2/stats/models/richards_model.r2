# stats/models/RichardsModel.R
RichardsModel <- R6::R6Class(
  "RichardsModel",
  inherit = BaseModel,
  public = list(
    fit = function(df) {
      self$df <- df
      cat("[RichardsModel] Starting fit…\n")

      a_start  <- max(df$Value, na.rm=TRUE)
      k_start  <- 0.1
      x0_start <- median(df$Year, na.rm=TRUE)
      v_start  <- 1.2  # shape (>0). v=1 gives logistic; v<1/Gompertz-like skew

      df$Year_scaled <- df$Year - min(df$Year, na.rm=TRUE)
      x0_s <- x0_start - min(df$Year, na.rm=TRUE)

      start_list <- list(a=a_start, k=k_start, x0=x0_s, v=v_start)
      ctrl_lm <- minpack.lm::nls.lm.control(maxiter=7000, ftol=1e-10, ptol=1e-10, factor=100)

      # Formula: a / (1 + v * exp(-k*(x-x0)))^(1/v)
      self$model <- tryCatch({
        minpack.lm::nlsLM(
          Value ~ a / (1 + v * exp(-k * (Year_scaled - x0)))^(1/v),
          data=df, start=start_list, control=ctrl_lm, trace=TRUE,
          lower=c(a=0, k=1e-6, x0=-Inf, v=1e-3)
        )
      }, error=function(e) { cat("[RichardsModel] nlsLM failed:", e$message, "\n"); NULL })

      if (is.null(self$model)) {
        cat("[RichardsModel] Falling back to nls2 grid…\n")
        start_grid <- expand.grid(
          a  = seq(0.7,1.3,length.out=5)*a_start,
          k  = seq(0.02,0.5,length.out=6),
          x0 = seq(x0_s-5,x0_s+5,length.out=6),
          v  = c(0.6,0.8,1,1.2,1.6,2.0)
        )
        self$model <- tryCatch({
          suppressWarnings(
            nls2::nls2(
              Value ~ a / (1 + v * exp(-k * (Year_scaled - x0)))^(1/v),
              data=df, start=start_grid, algorithm="brute-force",
              control=nls.control(maxiter=2500, tol=1e-6, minFactor=1e-10), trace=TRUE
            )
          )
        }, error=function(e) { cat("[RichardsModel] nls2 failed:", e$message, "\n"); NULL })
      }

      if (!is.null(self$model)) { cat("[RichardsModel] Fit OK.\n"); print(coef(self$model)) }
      else cat("[RichardsModel] Fit failed.\n")
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
        type="richards",
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
      cf <- as.list(coef(self$model))
      a <- cf$a; k <- cf$k; x0s <- cf$x0; v <- cf$v
      y0 <- min(self$df$Year, na.rm=TRUE)

      # Solve a/(1+v*exp(-k(t-x0)))^(1/v) = p*a  ->  (1+v*e^{-k(t-x0)}) = p^{-v}
      inv_richards_t <- function(p) {
        log( ( (p^(-v)) - 1 ) / v ) * (-1/k) + x0s
      }
      t5  <- inv_richards_t(0.05) + y0
      t50 <- inv_richards_t(0.50) + y0
      t95 <- inv_richards_t(0.95) + y0

      list(
        list(param_name="a", param_latex="$a$", param_value=a, param_axis="y"),
        list(param_name="k", param_latex="$k$", param_value=k, param_axis=NULL),
        list(param_name="v", param_latex="$v$", param_value=v, param_axis=NULL),
        list(param_name="t5",  param_latex="$t_{5\\%}$",  param_value=t5,  param_axis="x"),
        list(param_name="t50", param_latex="$t_{50\\%}$", param_value=t50, param_axis="x"),
        list(param_name="t95", param_latex="$t_{95\\%}$", param_value=t95, param_axis="x"),
        list(param_name="Delta_t", param_latex="$\\Delta t$", param_value=t95-t5, param_axis=NULL)
      )
    }
  )
)
MODEL_REGISTRY$richards <- RichardsModel

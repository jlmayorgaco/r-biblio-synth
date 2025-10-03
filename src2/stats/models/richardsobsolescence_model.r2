RichardsObsolescenceModel <- R6::R6Class(
  "RichardsObsolescenceModel",
  inherit = BaseModel,
  public = list(
    fit = function(df) {
      self$df <- df
      cat("[RichardsObsolescenceModel] Starting fit…\n")

      y0 <- min(df$Year, na.rm = TRUE)
      df$Year_scaled <- df$Year - y0

      a_start  <- max(df$Value, na.rm=TRUE)
      k_start  <- 0.1
      x0_start <- median(df$Year, na.rm=TRUE) - y0
      v_start  <- 1.2
      td_start <- max(df$Year_scaled) - 3
      d_start  <- 0.05

      start_list <- list(a=a_start, k=k_start, x0=x0_start, v=v_start,
                         td=max(0, td_start), d=d_start)

      F_rich <- function(a,k,x0,v,t) a / (1 + v*exp(-k*(t-x0)))^(1/v)
      decay  <- function(d,td,t)     exp(-d * pmax(0, t-td))

      ctrl <- minpack.lm::nls.lm.control(maxiter=6000, ftol=1e-10, ptol=1e-10, factor=100)

      self$model <- tryCatch({
        minpack.lm::nlsLM(
          Value ~ F_rich(a,k,x0,v,Year_scaled) * decay(d,td,Year_scaled),
          data=df, start=start_list, control=ctrl, trace=TRUE,
          lower=c(a=0, k=1e-6, x0=-Inf, v=1e-3, td=0, d=0)
        )
      }, error=function(e){ cat("[RichardsObsolescenceModel] nlsLM failed:", e$message, "\n"); NULL })

      if (is.null(self$model)) {
        cat("[RichardsObsolescenceModel] Falling back to nls2 grid…\n")
        start_grid <- expand.grid(
          a  = seq(0.7,1.3,length.out=5)*a_start,
          k  = seq(0.02,0.4,length.out=6),
          x0 = seq(x0_start-5, x0_start+5, length.out=6),
          v  = c(0.6,0.8,1,1.2,1.6,2.0),
          td = seq(max(0, td_start-4), max(0, td_start+4), length.out=6),
          d  = c(0.0, 0.02, 0.05, 0.08, 0.12)
        )
        self$model <- tryCatch({
          suppressWarnings(
            nls2::nls2(
              Value ~ F_rich(a,k,x0,v,Year_scaled) * decay(d,td,Year_scaled),
              data=df, start=start_grid, algorithm="brute-force",
              control=nls.control(maxiter=2500, tol=1e-6, minFactor=1e-10), trace=TRUE
            )
          )
        }, error=function(e){ cat("[RichardsObsolescenceModel] nls2 failed:", e$message, "\n"); NULL })
      }

      if (!is.null(self$model)) { cat("[RichardsObsolescenceModel] Fit OK.\n"); print(coef(self$model)) }
      else cat("[RichardsObsolescenceModel] Fit failed.\n")
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
        type="richards_obsolescence",
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
      list(
        list(param_name="a",  param_latex="$a$",  param_value=cf$a,  param_axis="y"),
        list(param_name="k",  param_latex="$k$",  param_value=cf$k,  param_axis=NULL),
        list(param_name="v",  param_latex="$v$",  param_value=cf$v,  param_axis=NULL),
        list(param_name="x0", param_latex="$t_{50\\%}$", param_value=min(self$df$Year,na.rm=TRUE)+cf$x0, param_axis="x"),
        list(param_name="td", param_latex="$t_d$", param_value=min(self$df$Year,na.rm=TRUE)+cf$td, param_axis="x"),
        list(param_name="d",  param_latex="$d$",  param_value=cf$d,  param_axis=NULL)
      )
    }
  )
)
MODEL_REGISTRY$richards_obsolescence <- RichardsObsolescenceModel

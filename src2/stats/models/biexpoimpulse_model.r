# stats/models/BiExpoImpulseModel.R
BiExpoImpulseModel <- R6::R6Class(
  "BiExpoImpulseModel",
  inherit = BaseModel,
  public = list(
    fit = function(df) {
      self$df <- df
      cat("[BiExpoImpulseModel] Starting fit…\n")
      y0 <- min(df$Year, na.rm=TRUE)
      df$Year_scaled <- df$Year - y0

      A0  <- max(df$Value, na.rm=TRUE)
      # crude slopes: use first/last segments to seed lambdas
      r_up  <- max(1e-3, (max(diff(df$Value), na.rm=TRUE) / max(diff(df$Year), na.rm=TRUE)))
      lam2  <- min(1, 0.5 + abs(r_up)/max(A0,1))      # fast rise
      lam1  <- max(1e-3, lam2/8)                      # slower decay
      t00   <- df$Year_scaled[which.max(df$Value)] - 2
      start <- list(A=A0, l1=lam1, l2=lam2, t0=max(0, t00))

      f_imp <- function(A,l1,l2,t0,t) A * pmax(exp(-l1*(t-t0)) - exp(-l2*(t-t0)), 0)

      ctrl <- minpack.lm::nls.lm.control(maxiter=6000, ftol=1e-10, ptol=1e-10)
      self$model <- tryCatch({
        minpack.lm::nlsLM(Value ~ f_imp(A,l1,l2,t0,Year_scaled),
          data=df, start=start, control=ctrl,
          lower=c(A=0, l1=1e-5, l2=1e-5, t0=0), trace=TRUE)
      }, error=function(e){ cat("[BiExpoImpulseModel] nlsLM failed:", e$message, "\n"); NULL })

      if (is.null(self$model)) {
        cat("[BiExpoImpulseModel] Falling back to nls2 grid…\n")
        sg <- expand.grid(
          A = seq(0.5,1.5,length.out=5)*A0,
          l1= c(0.01,0.03,0.06,0.1,0.2),
          l2= c(0.2,0.5,0.8,1.2,1.6),
          t0= seq(max(0, t00-3), max(0, t00+3), length.out=5)
        )
        self$model <- tryCatch({
          suppressWarnings(nls2::nls2(
            Value ~ f_imp(A,l1,l2,t0,Year_scaled),
            data=df, start=sg, algorithm="brute-force",
            control=nls.control(maxiter=2500, tol=1e-6, minFactor=1e-10), trace=TRUE))
        }, error=function(e){ cat("[BiExpoImpulseModel] nls2 failed:", e$message, "\n"); NULL })
      }

      if (!is.null(self$model)) { cat("[BiExpoImpulseModel] Fit OK.\n"); print(coef(self$model)) }
      invisible(self)
    },

    r2 = function() {
      rss <- tryCatch(deviance(self$model), error=function(e) NA_real_)
      y <- self$df[[self$response_col]]
      tss <- sum((y - mean(y,na.rm=TRUE))^2, na.rm=TRUE)
      ifelse(is.na(rss)||tss<=0, NA_real_, 1 - rss/tss)
    },

    summary = function() {
      list(
        type="bi_expo_impulse",
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
      cf <- as.list(coef(self$model)); A<-cf$A; l1<-cf$l1; l2<-cf$l2; t0s<-cf$t0
      y0 <- min(self$df$Year, na.rm=TRUE)
      tpeak_s <- (log(l2/l1)/(l2-l1))
      t5_s <- max(0, tpeak_s - 2/l2); t95_s <- tpeak_s + 2/l1  # rough width markers
      list(
        list(param_name="A",  param_latex="$A$",   param_value=A,  param_axis="y"),
        list(param_name="l1", param_latex="$\\lambda_1$", param_value=l1, param_axis=NULL),
        list(param_name="l2", param_latex="$\\lambda_2$", param_value=l2, param_axis=NULL),
        list(param_name="t0", param_latex="$t_0$", param_value=t0s + y0, param_axis="x"),
        list(param_name="t_peak", param_latex="$t_{\\text{peak}}$", param_value=t0s + tpeak_s + y0, param_axis="x"),
        list(param_name="Delta_t", param_latex="$\\Delta t$", param_value=(t95_s - t5_s), param_axis=NULL)
      )
    }
  )
)
MODEL_REGISTRY$bi_expo_impulse <- BiExpoImpulseModel

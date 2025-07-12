generate_data <- function(beta, n){
  # Set random seed
  # set.seed(442008606)
  # Set simulation parameters
  # n <- 100
  # Simulate exogenous nodes
  needs <- rnorm(n = n)
  # Simulate endogenous nodes
  intrinsic_motivation <- -(0.19 * needs) + rnorm(n = n)
  wellbeing <- beta * intrinsic_motivation - 0.42 * needs + rnorm(n = n)
  df <- data.frame(
    intrinsic_motivation = intrinsic_motivation,
    needs = needs,
    wellbeing = wellbeing
  )
  return(df)
}

analyze_data <- function(df){
  # Conduct linear regression
  res <- lm(wellbeing ~ intrinsic_motivation + needs, data = df)
  # Obtain a model summary
  sum_res <- summary(res)
  # Compare p-value of our coefficient of interest to the significance level, .05
  sum_res$coefficients["intrinsic_motivation", "Pr(>|t|)"] < .05
}

perform_study <- function(study_design, reps = 100){
  library(future)
  # Sets up clusters from number of cores
  plan(multisession, workers = parallelly::availableCores()-2L)
  pwr <- apply(study_design, 1, function(thisrow){
    # Replicate the row of the study design reps times
    out <- future.apply::future_replicate(n = reps, expr = {
      # Simulate data with the beta and n from thisrow
      df <- with(as.list(thisrow), generate_data(beta = beta, n = n))
      # Analyze those data
      analyze_data(df)
    },
    future.seed = TRUE)
    # Calculate the proportion of significant results using mean()
    mean(out)
  })
  data.frame(study_design, power = pwr)
}

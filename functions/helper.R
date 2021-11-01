generate_n_samples <- function(n){
  tibble(
    normal = rnorm(n = n, mean = 50, sd = 10),
    exponential = rexp(n = n, rate = 0.2),
    binomial = rbinom(n = n, size = 100, prob = 0.25),
    uniform = runif(n = n, min = 10, max = 20)
  )
}


distribution_exploration <- function(n, rep, mean, sd, rate, df, min, max){
  
  .samps <- map(1:rep, generate_n_samples)
  
  .samp_means <- map_df(.samps, sample_mean)
  
  .restructured_means <- .samp_means %>% 
    pivot_longer(
      cols = everything(),
      names_to = "distribution",
      values_to = "means"
    )
  
  .plot <- .restructured_means %>% 
    ggplot(aes(x = means)) +
    geom_histogram(aes(y = ..count.. / sum(..count..))) +
    facet_wrap(~ distribution, ncol = 4, scales = "free") +
    labs(y = "proportion")
  
  .tab <- .restructured_means %>% 
    group_by(distribution) %>% 
    summarise(mean = mean(means),
              sd = sd(means))
  
  list(.plot, .tab)
  
}
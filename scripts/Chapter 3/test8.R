library("tidyverse")
library("here")
library("patchwork")
library("krulRutils")
library("ISLR2")
library("magrittr")

options(scipen = 999)

alpha <- 2
beta <- 1

# Create sequence of x values
x_data <- seq(0, 10, length.out = 200)

# Calculate density values
gamma_distribution_tbl <- tibble(
  x_data = x_data,
  density = dgamma(x_data, shape = alpha, rate = beta)
)

# Calculate mean and median
mean_val <- alpha / beta
median_val <- qgamma(0.5, shape = alpha, rate = beta)

# Plot
gamma_distribution_tbl %>%
  ggplot(aes(x = x_data, y = density)) +
  geom_line(
    color = c_palette["C blue"],
    linewidth = 1
  ) +
  geom_vline(
    xintercept = mean_val,
    color = c_palette["C red"],
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = median_val,
    color = c_palette["C green"],
    linetype = "dotted",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = mean_val,
    y = 0.35,
    label = "Mean",
    color = c_palette["C red"],
    hjust = -0.1
  ) +
  annotate(
    "text",
    x = median_val,
    y = 0.35,
    label = "Median",
    color = c_palette["C green"],
    hjust = 1.1
  ) +
  labs(
    title = paste(
      "Gamma Distribution PDF (shape =", alpha, ", rate =", beta, ")"
    ),
    x = "x",
    y = "Density"
  ) +
  theme_krul()

mu <- 0 # mean (location)
b <- 1 / sqrt(2) # scale

# Sequence of x values covering enough range to see tails
x_data <- seq(-5, 5, length.out = 300)

# Laplace PDF function
dlaplace <- function(x, mu, b) {
  1 / (2 * b) * exp(-abs(x - mu) / b)
}

# Create data frame with PDF values
laplace_distribution_tbl <- data.frame(
  x_data = x_data,
  density = dlaplace(x_data, mu, b)
)

# Plot PDF
laplace_distribution_tbl %>%
  ggplot(aes(x = x_data, y = density)) +
  geom_line(color = c_palette["C blue"], linewidth = 1) +
  labs(
    title = "Laplace Distribution PDF (mean = 0, sd = 1)",
    x = "x",
    y = "Density"
  ) +
  theme_krul()

set.seed(123) # for reproducibility

# Sample size
n <- 300
# Generate random samples from the Gamma distribution
gamma_samples <- rgamma(n, shape = alpha, rate = beta)

gamma_samples_tbl <- tibble(
  sample = gamma_samples
)

# Generate uniform random numbers in (0,1)
u <- runif(100)

# Apply inverse CDF of Laplace
laplace_sample <- ifelse(u < 0.5,
  mu + b * log(2 * u),
  mu - b * log(2 * (1 - u))
)

laplace_samples_tbl <- tibble(
  sample = laplace_sample
)

gamma_qq_plot <- gamma_samples_tbl %>%
  ggplot(aes(sample = sample)) +
  geom_qq(
    color = c_palette["C red"],
    size = 0.3
  ) +
  geom_qq_line(
    color = c_palette["C blue"],
    linewidth = 0.5
  ) +
  labs(
    title = "QQ Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_krul()

gamma_histogram <- gamma_samples_tbl %>%
  ggplot(aes(x = sample)) +
  geom_histogram(
    bins = 30,
    fill = c_palette["C blue"],
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "Histogram",
    x = "Sample Values",
    y = "Density"
  ) +
  theme_krul()

gamma_plot <- gamma_qq_plot + gamma_histogram +
  plot_layout(ncol = 2) +

  plot_annotation(
    title = "Gamma Distribution: QQ Plot and Histogram",
    theme = theme(
      plot.title = element_text(
        size = 24,
        face = "bold"
      )
    )
  )

gamma_qq_plot <- gamma_samples_tbl %>%
  ggplot(aes(sample = sample)) +
  geom_qq(
    color = c_palette["C red"],
    size = 0.3
  ) +
  geom_qq_line(
    color = c_palette["C blue"],
    linewidth = 0.5
  ) +
  labs(
    title = "QQ Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_krul()

gamma_histogram <- gamma_samples_tbl %>%
  ggplot(aes(x = sample)) +
  geom_histogram(
    bins = 30,
    fill = c_palette["C blue"],
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "Histogram",
    x = "Sample Values",
    y = "Density"
  ) +
  theme_krul()

gamma_plot <- gamma_qq_plot + gamma_histogram +
  plot_layout(ncol = 2) +

  plot_annotation(
    title = "Gamma Distribution: QQ Plot and Histogram",
    theme = theme(
      plot.title = element_text(
        size = 24,
        face = "bold"
      )
    )
  )


laplace_qq_plot <- laplace_samples_tbl %>%
  ggplot(aes(sample = sample)) +
  geom_qq(
    color = c_palette["C red"],
    size = 0.3
  ) +
  geom_qq_line(
    color = c_palette["C blue"],
    linewidth = 0.5
  ) +
  labs(
    title = "QQ Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_krul()

laplace_histogram <- laplace_samples_tbl %>%
  ggplot(aes(x = sample)) +
  geom_histogram(
    bins = 30,
    fill = c_palette["C blue"],
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "Histogram",
    x = "Sample Values",
    y = "Density"
  ) +
  theme_krul()
laplace_plot <- laplace_qq_plot + laplace_histogram +
  plot_layout(ncol = 2) +

  plot_annotation(
    title = "Laplace Distribution: QQ Plot and Histogram",
    theme = theme(
      plot.title = element_text(
        size = 24,
        face = "bold"
      )
    )
  )

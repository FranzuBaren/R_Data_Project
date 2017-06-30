<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=default"></script>

(E=mc^2)，$$x_{1,2} = \frac{-b \pm \sqrt{b^2-4ac}}{2b}.$$



# Relating a Gaussian Distribution with (mean,variance) to a Beta Distribution with parameters (alpha,beta)

Beta_Parameters <- function(mean, variance) {
  alpha <- ((1 - mean) / variance - 1 / mean) * mean ^ 2
  beta <- alpha * (1 / mean - 1)
  return(Beta_Parameters = list(alpha = alpha, beta = beta))
}

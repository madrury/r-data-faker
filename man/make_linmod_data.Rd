\name{make_linmod_data}
\alias{make_linmod_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_linmod_data(n_rows, n_columns, n_columns_with_large_effects = 0, n_nuisance_columns = 0, n_hidden_columns = 0, beta_sd = 1, beta_inflate_factor = 2.5, noise_sd = 2.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n_rows}{
%%     ~~Describe \code{n_rows} here~~
}
  \item{n_columns}{
%%     ~~Describe \code{n_columns} here~~
}
  \item{n_columns_with_large_effects}{
%%     ~~Describe \code{n_columns_with_large_effects} here~~
}
  \item{n_nuisance_columns}{
%%     ~~Describe \code{n_nuisance_columns} here~~
}
  \item{n_hidden_columns}{
%%     ~~Describe \code{n_hidden_columns} here~~
}
  \item{beta_sd}{
%%     ~~Describe \code{beta_sd} here~~
}
  \item{beta_inflate_factor}{
%%     ~~Describe \code{beta_inflate_factor} here~~
}
  \item{noise_sd}{
%%     ~~Describe \code{noise_sd} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (n_rows, n_columns, n_columns_with_large_effects = 0, 
    n_nuisance_columns = 0, n_hidden_columns = 0, beta_sd = 1, 
    beta_inflate_factor = 2.5, noise_sd = 2.5) 
{
    n_common_columns <- n_columns - n_nuisance_columns
    if (n_common_columns < 0) {
        stop("Illogical arguments: n_nuisance_columns > n_columns.")
    }
    if (n_columns_with_large_effects > n_common_columns) {
        stop("Illogical arguments: n_columns_with_large_effects > number of effects.")
    }
    X_common <- make_binary_matrix(n_rows, n_common_columns)
    X_nuisance <- make_binary_matrix(n_rows, n_nuisance_columns)
    X_hidden <- make_binary_matrix(n_rows, n_hidden_columns)
    beta_common <- rnorm(n_common_columns, mean = 0, sd = beta_sd)
    beta_common[1:n_columns_with_large_effects] <- (beta_inflate_factor * 
        beta_common[1:n_columns_with_large_effects])
    beta_nuisance <- rnorm(n_nuisance_columns, mean = 0, sd = beta_sd)
    beta_hidden <- rnorm(n_hidden_columns, mean = 0, sd = beta_sd)
    Y <- (cbind(X_common, X_hidden) \%*\% c(beta_common, beta_hidden) + 
        rnorm(n_rows, mean = 0, sd = noise_sd))
    X <- cbind(X_common, X_nuisance)
    list(X = X, Y = Y, beta_common = beta_common, beta_nuisance = beta_nuisance, 
        beta_hidden = beta_hidden)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

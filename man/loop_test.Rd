% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upload.R
\name{loop_test}
\alias{loop_test}
\title{Tries to run an expression a certain number of times}
\usage{
loop_test(test_expr, test_fcn, thing_expr, n_lim = 15,
  warning_text = "", sleep_before = 0, sleep_after = 0)
}
\arguments{
\item{test_expr}{a parameterless function that will be executed to determine
while loop control flow}

\item{test_fcn}{a function to evaluate the value of test_expr, with one
parameter}

\item{thing_expr}{the main task that must be completed by the end of the loop
(no parameters)}

\item{n_lim}{maximum number of iterations}

\item{warning_text}{warning to print if iteration limit is reached}

\item{sleep_before}{time to sleep before evaluating thing_expr}

\item{sleep_after}{time to sleep after evaluating thing_expr and before
evaluating test_expr at the end of the while loop}
}
\description{
This function executes test_expr to determine whether to enter a while loop.
In the while loop, the control flow is as follows: sleep(sleep_before),
thing_expr(), sleep(sleep_after).
}
\details{
The while loop will exit if test_fcn(test_expr) is false or if the number of
iterations exceeds n_lim.
}

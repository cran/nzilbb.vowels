% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lobanov_2.R
\name{lobanov_2}
\alias{lobanov_2}
\title{Apply Lobanov 2.0 normalisation}
\usage{
lobanov_2(vowel_data)
}
\arguments{
\item{vowel_data}{a dataframe whose first four columns are speaker ids,
vowel ids, F1 values, and F2 values.}
}
\value{
a dataframe matching the input dataframe with additional columns
\code{F1_lob2} and \code{F2_lob2}, containing the lobanov normalised F1 and F2 values
respectively.
}
\description{
\code{lobanov_2()} takes a data frame where the first four columns are:
\enumerate{
\item speaker identifiers,
\item vowel identifiers,
\item first formant values in Hertz,
\item second formant values in Hertz.
}

It returns a dataframe with two additional columns, \code{F1_lob2} and \code{F2_lob2},
containing normalised formant values.
}
\details{
This functions applies Lobanov 2.0 normalisation presented in
Brand et al. (2021). This variant
of Lobanov normalisation is designed to work for datasets whether the vowel
types have different token counts from one
another. The Lobanov 2.0 value for a vowel is given by \deqn{F_{lobanov2.0_i}
= \frac{F_{raw_i} - \mu(\mu_{vowel_1}, \ldots,
\mu_{vowel_n})}{\sigma(\mu_{vowel_1}, \ldots, \mu_{vowel_n})}} where, for
ease of notation, we assume all values are from a single speaker. We signify
the n vowel types as vowel_1, \ldots, vowel_2, while i indicates the formant
number. We implement the function for F1 and F2.
}
\examples{
normed_vowels <- lobanov_2(onze_vowels)
head(normed_vowels)

}
\references{
Brand, James, Jen Hay, Lynn Clark, Kevin Watson & Márton Sóskuthy (2021):
Systematic co-variation of monophthongs across speakers of New Zealand
English. Journal of Phonetics. Elsevier. 88. 101096.
doi:10.1016/j.wocn.2021.101096
}

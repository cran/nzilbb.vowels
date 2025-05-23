% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{qb_vowels}
\alias{qb_vowels}
\title{Formants from QuakeBox 1}
\format{
A data frame with 26331 rows and 14 variables:
\describe{
\item{speaker}{Anonymised speaker code (char).}
\item{vowel}{Wells lexical sets for 10 NZE monophthongs. Levels:  DRESS, FLEECE, GOOSE, KIT, LOT, NURSE, START, STRUT, THOUGHT, TRAP, FOOT (char).}
\item{F1_50}{First formant in Hz, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
\item{F2_50}{Second formant in Hz, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
\item{participant_age_category}{Age category of speaker. Values: 18-25, 26-35, 36-45, ..., 76-85 (char).}
\item{participant_gender}{Gender of participant. Values: M, F (char).}
\item{participant_nz_ethnic}{New Zealand ethnic category of participant. Values: NZ mixed ethnicity, NZ European, Other (char).}
\item{word_freq}{Frequency of word from which vowel token is taken in CELEX.}
\item{word}{Anonymised word id (char).}
\item{time}{Time in seconds at which vowel segment starts.}
\item{vowel_duration}{Length of vowel in seconds.}
\item{articulation_rate}{Articulation rate of utterance from which token is taken.}
\item{following_segment_category}{Category of following segment. NB: liquids have already been removed. Levels: labial, velar, other (factor).}
\item{amplitude}{Maximum amplitude of word from which vowel token is taken, generated by LaBB-CAT interface with Praat.}
}
}
\source{
\url{https://osf.io/m8nkh/}
}
\usage{
qb_vowels
}
\description{
A dataset containing formant values, amplitude, articulation rate, and
following segment data for 10 New Zealand English monophthongs, along with
participant demographics.
}
\details{
Original data was generated for Wilson Black et al. (2023).
}
\references{
Wilson Black, Joshua, Jennifer Hay, Lynn Clark & James Brand (2023): The
overlooked effect of amplitude on within-speaker vowel variation.
Linguistics Vanguard. Walter de Gruyter GmbH. 9(1). 173–189.
doi:10.1515/lingvan-2022-0086
}
\keyword{datasets}

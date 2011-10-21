\name{extract.pv}
\alias{extract.pv}
\title{Extracts Pv capture histories and covariates
: from ACCESS database, it constructs the relevant capture histories from the
tables. It constructs all the queries that used to be done in ACCESS.}
\usage{
  extract.pv(file = "PvObservations.mdb", dir = "", begin =
  615, end = 1015)
}
\arguments{
  \item{file}{ACCESS database filename}

  \item{dir}{Directory containing ACCESS database; if empty
  uses package directory}

  \item{begin}{month-day at beginning of resight period
  (615 = June 15)}

  \item{end}{month-day at end of resight period (1015 = Oct
  15)}
}
\value{
  dataframe containing following fields \item{ch}{capture
  history; character string} \item{Speno}{unnique
  identifier} \item{Brand}{brand identifier}
  \item{BrandYear}{year branded} \item{AgeClass}{Age class
  P,Y,S,A at time of initial marking} \item{Sex}{either M
  or F; factor variable} \item{Weight}{weight (kg) at time
  of branding minus sex-specific mean}
  \item{Cohort}{initial birth year} \item{td}{sequence of
  fields named tdyyyy with values 0/1; it is 1 if resighted
  in the prior year} \item{TotalTimesResighted}{number of
  years resighted} \item{recap}{0 if never resighted and 1
  otherwise} \item{Location}{factor variable; Gertrude or
  Eagle location of animal at initial marking}
  \item{first}{sequence of fields named firstyyyy with
  values 0/1; it is 1 for year it was first branded}
  \item{age}{factor variable of age at first marking}
  \item{digits}{number of brand digits}
}
\description{
  Attaches directly to ACCESS database and extracts initial
  tag, recapture and resightings information.
}
\examples{
pvdata=extract.pv()
}
\author{
  Jeff Laake
}


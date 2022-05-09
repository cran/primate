\name{AWP.driver}
\alias{AWP.driver}
\title{Load the driver to access the All the World's Primate remote SQL database}
\description{
Load the driver to utilize the database sofware
}
\usage{
AWP.driver(drv.name="net.sourceforge.jtds.jdbc.Driver", 
           drv.file=system.file("drivers","jtds-1.2.8.jar", package='primate'))
}
\arguments{
\item{drv.name}{The name of the driver}
\item{drv.file}{The file name for the database driver}
}
\value{
driver argument to AWP.connect
}
\examples{
\donttest{
AWP.driver()
}
}

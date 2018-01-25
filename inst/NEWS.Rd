\name{NEWS}
\title{aquamet News}

\section{CHANGES IN aquamet VERSION 2.5.0 (Released 2018-01-25)}{
  \subsection{NEW FEATURES}{
      \itemize{
        \item Updated assignDistinct function to work more broadly across
        other datasets.
        
        \item Added functions to calculate NRSA physical habitat indicators.
      }
    }
}

\section{CHANGES IN aquamet VERSION 2.4.6 (Released 2017-08-30)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added functions to calculate NLA physical habitat indicators. 
      
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item None.
    }
  }
}
\section{CHANGES IN aquamet VERSION 2.4 (Released 2016-11-01)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Rewrote \code{fishMet} and \code{invertMet} into several functions which each calculate 
      a subset of metrics. Added functions to calculate NRSA multimetric indices (MMIs) and just the 
      metrics included in the MMIs. Added flexibility in input variable names for all functions. Added
      vignettes and improved documentation.
      
      \item Modified and renamed physical habitat functions for NRSA to allow total flexibility in naming
      of input variables and values associated with certain input variables. Updated documentation for
      the new functions. 
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item None.
    }
  }
}

\section{CHANGES IN aquamet VERSION 2.3 (Released 2015-03-10)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Modified function \code{fishMet} to determine whether variable
        ANOM_CT exists among the input counts and, if not, set ANOMPIND equal to missing.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item None.
    }
  }
}


\section{CHANGES IN aquamet VERSION 2.2 (Released 2014-05-28)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added an index.html file to the doc directory so that files in the
      directory can be viewed from the package help page.

      \item Added datasets to the data directory for use by help page examples.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item None.
    }
  }
}


\section{CHANGES IN aquamet VERSION 2.1 (Released 2014-04-15)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added improved help directory documentation files for NRSA metrics.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item None.
    }
  }
}


\section{CHANGES IN aquamet VERSION 2.0 (Released 2014-03-18)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added functions to calculate invertebrate and fish metrics for NRSA.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item None.
    }
  }
}


\section{CHANGES IN aquamet VERSION 1.0 (Released 2013-01-30)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item This is the original version of the package.  It calculates physical
      habitat metrics for the National Rivers and Streams Assessment (NRSA). 

    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item None.
    }
  }
}

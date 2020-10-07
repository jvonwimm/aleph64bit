      COMMON /TRFDCT/ TDZTOL,TZ0TOL,TD0TOL,RHOMIN
     1               ,TDWMAX,TDZMAX,TDWSTR,TDZSTR
     2               ,MAXGAP,DZNEXT,DPNEXT,CURTOL,DIPTOL
     3               ,MAXTRY,SIGMAW,SFACTR,SIGMAZ,CHLIM1,CHLIM2
     4               ,TDCHM1,TDCHM2
      REAL TDZTOL,TZ0TOL,TD0TOL,RHOMIN
      REAL TDWMAX,TDZMAX,TDWSTR,TDZSTR
      INTEGER MAXGAP
      REAL DZNEXT,DPNEXT,CURTOL,DIPTOL
      INTEGER MAXTRY
      REAL SIGMAW,SFACTR,SIGMAZ,CHLIM1,CHLIM2
      REAL TDCHM1,TDCHM2
C
#if defined(DOC)
C
C!      This common contains the cuts for track finding.  It is filled
C      in TRNCON from the direct access bank 'TCCN'
C
C      TDZTOL  = Tolerance for link helix test
C      TZ0TOL  = Max value of Z0 for accepting link
C      TD0TOL  = Max value of D0 for accepting link
C      RHOMIN  = Min radius of curvature for accepting link
C      TDWMAX  = Max r-phi window for finding hits
C      TDZMAX  = Max z window for finding hits
C      TDWSTR  = Nominal r-phi road width for finding hits
C      TDZSTR  = Nominal z road width for finding hits
C      MAXGAP  = Max no of successive padrows with no found hit
C                                             for local search
C      DZNEXT  = Max difference in z between successive padrows for
C                                             accepting hit in link
C      DPNEXT  = Max difference in phi between successive padrows for
C                                             accepting hit in link
C      CURTOL  = Max difference in curvature between successive
C                                          links to accept in chain
C      DIPTOL  = Max difference in tan(lanbda) between successive
C                                            links to accept in chain
C      MAXTRY  = Max number of hits that can be rejected in fitting
C                                              chain
C      SIGMAW  = Parameter for computing external r-phi error
C      SFACTR  = Parameter for computing external r-phi error
C      SIGMAZ  = External z coordinate error of hit
C      CHLIM1  = Max value of chisq per degree of freedom for r-phi
C                                              circle fit
C      CHLIM2  = Max value of chisq per degree of freedom for s-z
C                                              line fit
C      TDCHM1  = Max chisq for connecting chain segments
C      TDCHM2  = Max chisq for connecting chain semi-circles
C
C----------------------------------------------------------------------
#endif

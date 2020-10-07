      COMMON /SSTATC/NERDSS,NRAWSS(2),NCOOSS(2),NETRSS(2,0:2),
     +  NTRKSS(2,0:2)
      INTEGER NERDSS,NRAWSS,NCOOSS,NETRSS,NTRKSS
#if defined(DOC)
C
C!         SATR statistics arrays
C
C NERDSS   : number of events with SATR raw data
C NRAWSSi  : number of SATR raw data on side i
C NCOOSSi  : number of SATR coordinates on side i
C NETRSSij : number of events with SATR tracks on side i, quality flag j
C NTRKSSij : number of SATR tracks on side i, quality flag j
#endif

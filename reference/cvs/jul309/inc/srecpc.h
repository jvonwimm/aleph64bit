      COMMON /SRECPC/TLOWSR,TUPPSR,RUPPSR,SIGMSR,DTHCSR,
     +               DSECSR,DGASSR,NPARSR,CHPRSR,ACTRSR,ACPASR,ZONWSR,
     +               SIG2SR,NPA2SR,NDR2SR,DPHBSR
      INTEGER NPARSR,NPA2SR,NDR2SR
      REAL    TLOWSR,TUPPSR,RUPPSR,SIGMSR,DTHCSR,DSECSR,DGASSR,CHPRSR,
     +        ACTRSR,ACPASR,ZONWSR,SIG2SR,DPHBSR
#if defined(DOC)
C
C!         SATR reconstruction cuts
C
C TLOWSR   : lower time cut in SRDIST
C TUPPSR   : upper time cut in SRDIST
C RUPPSR   : upper cut on distance track - wire in SRDIST
C SIGMSR   : spacial resolution in r of wire tubes
C DTHCSR   : theta window width
C DSECSR   : effectively dead zone between two inner sectors
C DGASSR   : effectively dead zone at gas channel
C NPARSR   : minimum number of wire paralleles to try a track fit
C CHPRSR   : chisquare probability a good fit must have at least
C ACTRSR   : cut on distance measure on tracks in SRSELO
C ACPASR   : cut on distance measure on patches in SRSELO
C ZONWSR   : zone width for cluster and patch finding
C SIG2SR   : assumed spatial resolution for double arm track fit
C NPA2SR   : minimum number of wire parallels per side for 2-arm fit
C NDR2SR   : maximum number of wire parallels to drop for 2-arm fit
C DPHBSR   : phi correction due to magnetic field
#endif

C!  BOM analysis flags
      LOGICAL XINIFL, XCURFL
      INTEGER IMCURR
      COMMON /BOMFLG/ XINIFL, XCURFL, IMCURR
#if defined(DOC)
C
C   XINIFL  Initialisation successful
C   XCURFL  Magnet currents read from Slow Control record
C   IMCURR  Sum of magnet currents
C
#endif

        SUBROUTINE YPSIVN(PR,ELIP1,PSIN,XD0)
C-------------------------------------------------
C! Find the psi angle respect to the pr point
CKEY YV0 ANGLE /USER
C      AUTHOR  : M.A.CIOCCI 3/4/91
C
C      INPUT:
C           PR(3)/R   :Generally the mean value of primary vertex
C           ELIP1(5)/R: FIVE PARAMETERS OF THE TRACK AS IN FRFT
C
C      OUTPUT:
C           PSIN/R    :Angle psi for track at minum distance from pr
C                      on the space
C           xd0(3)/r  :coordinates of point at minimum distance from
C                      from pr
C
C
C
C                  DESCRIPTION
C                  ===========
C       Finds the psi angle of a charged track minimizing the distance
C       between helix and point pr.
C       Searchs the points coordinate at minimum distance from pr(3)
C       in the x-y plane (yd0nfn), calculates psi angle mod twopi
C       (yfixyn),  minimizing also the distance in z calculate
C       psi angle defining phase(yzphan)
C
C
C       CALLED: user
C
C       CALLS:  YD0NFN,YZPHAN,YFIXYN
C
C-------------------------------------------------------------
      SAVE
      EXTERNAL YFIXYN
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
C
        REAL PR(*),XD0(*),CF
        REAL ELIP1(*)
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
C+      ELIP1: THE FIVE PARAMETERS OF THE HELIX
C       FROM THE FRFT BANK
C
C
C+       HERE DEFINE THE SIX NEW PARAMETERS OF
C+       OF THE TWO HELICES
C
       CF=-ELIP1(1)/ABS(ELIP1(1))
       RHOF=ABS(1./ELIP1(1))
       D0F=-CF*ELIP1(4)
       XKF=-CF/(RHOF-D0F)
       XLAMF=ELIP1(2)
       FI0F=ELIP1(3)
       Z0F=ELIP1(5)
C
C+        SEARCHES THE COORDINATES OF POINT AT MINIMUM DISTANCE
C+        FROM PR(3) IN THE X-Y PLANE  (XD0(3))
C
         CALL YD0NFN(PR,RHOF,D0F,FI0F,CF,XD0,SIGND)
C
C
C+      CALCULATE PSI ANGLE AT XD0
C+
C
C
        PSIN=YFIXYN(CF,XD0(1),XD0(2),RHOF,FI0F,D0F)
C
C+   PSI MINIMIZING ALSO THE Z DISTANCE (PSI= PSI+N*TWOPI)
C
        CALL YZPHAN(PR,RHOF*XLAMF,Z0F,PSIN,XD0)
         RETURN
         END

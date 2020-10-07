      INTEGER FUNCTION VDMJLS (CLOC, ILAY, IWFF, IFAC, IVIEW)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Calculates line index for VDET91 (temporary)
C - John Drinkard, 16 Sep 1991 (original version)
C - Steve Wasserbaech, 20 March 1995
C
C   This is a crude version of VDMJLS that works with the new
C   VDET geometry package, but only for VDET91!  It is designed to
C   reproduce the results of the old version.
C
C   Note: the input coordinate CLOC is a floating number and the
C   output VDMJLS is an integer.  VDMJLS steps by one when CLOC
C   crosses each readout strip.  If CLOC lies between two readout
C   strips, it is not obvious which of the two nearest strips will
C   correspond to VDMJLS.
C
C - Input:
C   CLOC    / R  Local coordinate (w or u) in cm
C   ILAY    / I  Layer index
C   IWFF    / I  Wafer-in-face index
C   IFAC    / I  Local face index
C   IVIEW   / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VDMJLS  / I  What is this?  Today we call this the electronics
C                  channel number + 1.
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
      SAVE
C
C     Arguments:
C
      REAL CLOC
      INTEGER ILAY, IWFF, IFAC, IVIEW
C
C     Local variables
C
      INTEGER IRET, IV, NROSTR(2), IRFREQ, MODSN, IORSN
      INTEGER IMOD, IWAF, JFAC, IORI, IROSI
      INTEGER VROSTM, VFWAFF, VJFACI, VFAORI, VNRDIR
      REAL CENT(2), FACT(2), RPITCH
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C
C ----------------------------------------------------------------------
C
      IF (FIRST) THEN
C       We should not assume that CENT and FACT will be constant
C       throughout the job.  But that's what's being assumed here...
        FIRST = .FALSE.
        DO IV=1,2
          IRET = VROSTM(IV,NROSTR(IV),RPITCH,IRFREQ)
          CENT(IV) = FLOAT(NROSTR(IV))/2. + 0.5
          FACT(IV) = 1./RPITCH
        ENDDO
      ENDIF
C
      VDMJLS = CLOC*FACT(IVIEW) + CENT(IVIEW)
C
C     Readout sign.  What used to be called the readout sign
C     (bank VDSI) is given by VNRDIR, times the relative sign
C     between the w and a or u and b directions.
C
      IRET = VFWAFF(IWFF,IMOD,IWAF)
      MODSN = 2*IMOD - 3
      IRET = VJFACI(ILAY,IFAC,JFAC)
      IRET = VFAORI(JFAC,IORI)
      IORSN = 2*IORI - 3
      IF (IVIEW .EQ. 1) THEN
        IROSI = MODSN
      ELSE
        IROSI = -MODSN*IORSN
      ENDIF
      IROSI = IROSI * VNRDIR(IVIEW)
C
C     If the readout sign is -1, flip the numbering:
C
      IF (IROSI .EQ. -1) THEN
        VDMJLS = NROSTR(IVIEW) + 1 - VDMJLS

      ENDIF
C
      RETURN
      END
#endif

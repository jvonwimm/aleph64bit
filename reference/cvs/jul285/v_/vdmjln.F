      INTEGER FUNCTION VDMJLN(UW,NRWAF,IVIEW)
C----------------------------------------------------------------------
C!  - Function to define a VDET LINE number index
C!  Written by John Carr, 16-9-91
C!
C!   Input
C!        UW local position
C!        IVIEW: =1 W  , =2 U
C!        NRWAF: Bank number from same
C!
C!   Output
C!        VDMJLN = 1,288 (JJD CONVENTION)
C?
C!======================================================================
#ifndef DOC
      SAVE
      REAL UW
      INTEGER NRWAF,IVIEW
C Local variable
      INTEGER ILINP(2,15,2,2)
      INTEGER ILINZ(4,15,2,2)
      INTEGER JZOFF
      INTEGER IUWS,IP,IZ,ILAY,ISID,IFAC1,ILINE
      INTEGER IL,IWF,IPH,IVX
      LOGICAL FIRST
      INTEGER NFAC(2),NLINE(2)
      INTEGER VDMJLS
C
      DATA JZOFF/96/
      DATA FIRST/.TRUE./
      DATA NFAC/9,15/,NLINE/4,2/
C --------------------------------------------------------------
      IF(FIRST) THEN
         FIRST=.FALSE.
         IP=0
         IZ=0
C        Indexing scheme for phi,
         DO 1 ILAY=1,2
         DO 1 ISID=1,2
         DO 1 IFAC1=1,NFAC(ILAY)
         DO 1 ILINE=1,NLINE(2)
            IP=IP+1
            ILINP(ILINE,IFAC1,ISID,ILAY)=IP
 1       CONTINUE
C         Indexing scheme for Z
         DO 2 ILAY=1,2
         DO 2 ISID=1,2
         DO 2 IFAC1=1,NFAC(ILAY)
         DO 2 ILINE=1,NLINE(1)
            IZ=IZ+1
            ILINZ(ILINE,IFAC1,ISID,ILAY)=IZ
 2       CONTINUE
      ENDIF
C
C     normal operation
      VDMJLN=0
C     Decode wafer ID
      CALL VADEWA(NRWAF,IL,IWF,IPH,IVX)
C      Convert to strip coordinates
      IUWS=VDMJLS(UW,IL,IWF,IPH,IVIEW)
C     Line driver within module
      ILINE=1+IUWS/256
C     Set Z
      IF(IVIEW.EQ.1) THEN
         IF(IWF.LE.2) THEN
            VDMJLN=ILINZ(ILINE+(IWF-1)*2,IPH,1,IL)+JZOFF
         ELSE
            VDMJLN=ILINZ(ILINE+(4-IWF)*2,IPH,2,IL)+JZOFF
         ENDIF
      ELSE
C     Set Phi
         IF(IWF.LE.2) THEN
            VDMJLN=ILINP(ILINE,IPH,1,IL)
         ELSE
            VDMJLN=ILINP(ILINE,IPH,2,IL)
         ENDIF
      ENDIF
C
      RETURN
      END
#endif

      SUBROUTINE PRKINE
C ----------------------------------------------------------------
C  J. Boucrot , Y. Karyotakis , F.Ranjard  - 860929 -
C! Print Kinematics Track and Vertex banks in Readable Format
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JKVOVN=1,JKVOVM=2,LKVOLA=2)
      CHARACTER*4 TVOL,TMEC,TBLK,CHAINT,NAME(3)
      DATA NAMK /0/
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
      IF (NAMK .EQ. 0) THEN
         LOUT = IW(6)
         NAMK = NAMIND('KINE')
         NAMV = NAMIND('VERT')
         CALL KIAVER (AVER,IPROG)
      ENDIF
      IF (IW(NAMK).EQ.0 .OR. IW(NAMV).EQ.0) THEN
        WRITE (LOUT,'(/1X,''+++PRKINE+++ NO KINE/VERT bank - RETURN'')')
        RETURN
      ENDIF
C
C - get 'PART' bank index , The name of the part# ITYP is in
C                           ITABL(JPART,ITYP,2):ITABL(JPART,ITYP,4)
      JPART = NLINK ('PART',0)
      IF (JPART .EQ. 0) THEN
         WRITE (LOUT,'(/1X,''+++PRKINE+++ NO PART bank so no name '')')
      ENDIF
C
C - get 'KVOL' bank index if it exists
      JKVOL = IW(NAMIND('KVOL'))
      TBLK  = '    '
C
      WRITE(LOUT,1000)
 1000 FORMAT(/1X,'+++ PRKINE +++ Vertex banks' /
     &  T2,'Number',14X,'Vx',11X,'Vy',9X,'Vz',3X,'TOF nsec',
     & 3X,'Mother track #',T80,'generated track #s'/)
C
      JVER = NAMV + 1
 1    JVER = IW(JVER-1)
      IF (JVER .NE. 0) THEN
         NVX = IW(JVER-2)
         LHDR = IW(JVER+1)
         LPVER= IW(JVER+2)
         MXTRK= IW(JVER+3)
         KVER = JVER + LHDR
         TVOL = TBLK
         TMEC = TBLK
         IF (JKVOL .GT. 0) THEN
            TVOL = CHAINT(ITABL(JKVOL,NVX,JKVOVN))
            IF (LCOLS(JKVOL).GE.JKVOVM)THEN
               TMEC = CHAINT(ITABL(JKVOL,NVX,JKVOVM))
            ENDIF
         ENDIF
C
         WRITE (LOUT,1001) NVX,TVOL,TMEC,(RW(KVER+J),J=1,3)
     &                   , RW(KVER+4)*10.**9
     &                   ,(IW(KVER+LPVER+I),I=0,MXTRK)
 1001    FORMAT (T2,I4,1X,A4,1X,A4,4F11.4,4X,I4,(T76,11I4))
         GO TO 1
      ENDIF
C
      IF (AVER .GE. 9.0) THEN
        WRITE ( LOUT,1010 )
 1010   FORMAT(/1X,'+++PRKINE+++ kinematics banks '/T3,
     & 'Number',2X,'Particle',12X,'Px',10X,'Py',10X,'Pz',10X,'Mass',
     &  6X,'Origin Vertex #',T95,'Generated Vertex #s'/)
      ELSE
        WRITE ( LOUT,1009 )
 1009   FORMAT(/1X,'+++PRKINE+++ kinematics banks '/T3,
     & 'Number',2X,'Particle',12X,'Px',10X,'Py',10X,'Pz',8X,'Energy',
     &  6X,'Origin Vertex #',T95,'Generated Vertex #s'/)
      ENDIF
C
      JKIN = NAMK + 1
 2    JKIN = IW(JKIN-1)
      IF (JKIN .NE. 0) THEN
         NTRK = IW(JKIN-2)
         LHKIN = IW(JKIN+1)
         LPKIN = IW(JKIN+2)
         MXVX = IW(JKIN+3)
         KIN  = JKIN + LHKIN
         ITYP = IW(KIN+5)
         IF (JPART .EQ. 0) THEN
            WRITE (LOUT,1012) NTRK,ITYP,(RW(KIN+J),J=1,LPKIN-1)
     &                       ,(IW(KIN+LPKIN+I),I=1,MXVX)
         ELSE
            DO 21 J=1,3
 21         NAME(J) = CHAINT(ITABL(JPART,ITYP,J+1))
            WRITE ( LOUT,1011 ) NTRK,NAME
     &                         ,(RW(KIN+K),K=1,LPKIN-1)
     &                         ,(IW(KIN+LPKIN+I),I=1,MXVX)
         ENDIF
         GO TO 2
      ENDIF
C
      RETURN
1011  FORMAT(T3,I4,4X,3A4,4(F12.3),9X,I4,(T95,8I4))
1012  FORMAT(T3,I4,4X,I8,4X,4(F12.3),9X,I4,(T95,8I4))
      END

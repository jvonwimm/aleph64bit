      SUBROUTINE ECTRIG
C-----------------------------------------------------------------
C    G.DEBOUARD     20-JAN-86
C! EC trigger anal. signals
C
C  -Called from ECDIGI                                  this .HLB
C. - calls  ECRWRG                                      this .HLB
C.          RANNOR                                      CERNLIB
C-----------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER (LPS1=6, LPS2=300)
      COMMON /ECNAMC/   NAETHT, NAEWHT, NAETTD, NAEWTD, NAETDI, NAEWDI
     &                , NAEWHE
     &                , NAETTR, NAEWTR, NAENDI
     &                , IDPSIG, IDEWTM, IDETTM
     &                , NAESHI, NAEWHI
C
      PARAMETER (LSTCK=3,LPHI=384,LTHET=228)
      PARAMETER (LWPLA=45,LMODU=12,LCOMP=3)
      PARAMETER (NTHSG=12,NPHSG=24)
      COMMON/EWTMCO/IWIRES(LWPLA+9,LMODU*LCOMP)
      PARAMETER(JEWTSO=1,JEWTSE=2,LEWTRA=2)
      PARAMETER(JETTTS=1,LETTRA=72)
      COMMON /EDPARA/ EDTMCO,EDTSCO,EDTNOI(3),EDWMCO,EDWSCO,EDWNOI,
     +                EDTGAI(3),EDTGER(3),KALEDT(3),EDWGAI,EDWGER,
     +                KALEDW,EDCUT1,EDCUT2,EDWCUT,ETTNOI(12,3),
     +                ETSTWE(3), EDZTHR(3,3)
      DIMENSION KTHEB(NTHSG)
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
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
C ----------------------------------------------------------------------
C  ===  initialize theta boundaries in array KTHEB
C
      IF(FIRST) THEN
        CALL EDTRGI(KTHEB)
        FIRST = .FALSE.
      ENDIF
C ----------------------------------------------------------
C
C === first , compute tower trigger signals
C
      CALL ALBOS( 'ETTR', 0, LMHLEN+3*NTHSG*NPHSG, KETTR, IGARB)
      IW( KETTR+LMHCOL) = 3
      IW( KETTR+LMHROW) = NTHSG*NPHSG
      CALL BLIST (IW,'E+','ETTR')
      JETTR = KETTR + LMHLEN + 1
C
      DO 20 K=1,NTHSG
        DO 10 J=1,NPHSG
          DO 5 I=1,3
            CALL RANNOR(SIG,SIH)
            IW(JETTR) = NINT(ETTNOI(K,I) * SIG)
            JETTR     = JETTR + 1
    5     CONTINUE
   10   CONTINUE
   20 CONTINUE
C
      KETHT = IW(NAETHT)
      IF( KETHT .EQ. 0 ) GOTO 55
      NETHT = LROWS( KETHT )
      LETHT = LCOLS (KETHT)
      JETHT = KETHT + LMHLEN + 1
C
C
      DO 50 N=1,NETHT
        IETIQ = IW(JETHT)
        JPHI  = IBITS(IETIQ,2,9)
        KTHE  = IBITS(IETIQ,16,8)
C
C === computation of trigger region number NUMTR
C
        NUMTR = 0
        NUTHE = 1
   35   IF(KTHE.GT.KTHEB(NUTHE)) THEN
          NUTHE = NUTHE + 1
          GO TO 35
        ENDIF
        CALL ECRWRG(KTHE,NUREG,MAXPH)
        NUPHI = INT((NPHSG*(JPHI-1))/MAXPH) + 1
        NUMTR = (NUTHE-1)*NPHSG + NUPHI
        IF(NUMTR.LE.0.OR.NUMTR.GT.NPHSG*NTHSG) GO TO 50
        KPTR = KROW (KETTR,NUMTR)
        DO 40 I=1,3
          SIGNA = ETSTWE(I) * FLOAT(IW(JETHT+I))
          IW(KPTR+I) = IW(KPTR+I) + NINT(SIGNA)
   40   CONTINUE
        JETHT = JETHT + LETHT
   50 CONTINUE
C
C === then , compute wire trigger signals
C
   55 CONTINUE
      CALL ALBOS( 'EWTR', 0, LMHLEN+2*LMODU*LCOMP, JEWTR, IGARB)
      IW( JEWTR+LMHCOL) = 2
      IW( JEWTR+LMHROW) = LMODU*LCOMP
      CALL BLIST (IW,'E+','EWTR')
      KEWTR = JEWTR + LMHLEN
      DO 70 I=1,LMODU*LCOMP
         DO 71 J=2,LWPLA+1
            KCONS=1
C  KCONS simulates the electronic gain in stack 3
            IF(J.GE.35)         KCONS=2
            IF(MOD(J,2).EQ.0)   THEN
               IW(KEWTR+1)=IW(KEWTR+1)+IWIRES(J,I)*KCONS
            ELSE
               IW(KEWTR+2)=IW(KEWTR+2)+IWIRES(J,I)*KCONS
            ENDIF
  71     CONTINUE
  70  KEWTR=KEWTR+LEWTRA
C
 980  RETURN
      END

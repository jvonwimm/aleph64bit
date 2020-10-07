*DK DPH
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DPH
CH
      SUBROUTINE DPH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      INCLUDE 'DALI_CF.INC'

      CHARACTER *1 TCOL(0:15)
      CHARACTER *2 TL
      DATA TCOL/8*'*','W','G','Y','O','R','M','C','B'/
      CHARACTER *3 TPA(999),TCH
      DATA TPA(2)/'IN0'/
      DATA IROT/1/
      CHARACTER *8 TXPA
      CHARACTER *4 T4
      CHARACTER *70 T,TN,TNAM
      CHARACTER *78 TCONT
      CHARACTER *39 TH(2,9)

C
C  _____ color White, Green, Yellow, Orange, Red, Magenta, Cyan, Blue
C  |
C  | _____ command line            go to page: number __  __ name
C  | |                                                 |  |
C  | |         ATLANTIS help                           |  |   this page _____
C  | V               |                                 V  V              |  |
C  V                 V                                                   V  V
C
C
C
C
C +--- user level
C |
C |+____ color White, Green, Yellow, Orange, Red, Magenta, Cyan, Blue
C ||
C || +____ command line            go to page: number __  __ name
C || |                                                 |  |
C || |         ATLANTIS help                           |  |   this page _____
C || V               |                                 V  V              |  |
C |V                 V                                                   V  V
C V

C      123456789 123456789 123456789 123456789
      DATA TH/
     &'+-- user level                         ',
     &'                                       ',
     &'|                                      ',
     &'                                       ',
     &'|+---- color White, Green, Yellow, Oran',
     &'ge, Red, Magenta, Cyan, Blue           ',
     &'||                                     ',
     &'                                       ',
     &'|| +---- * = command line        go to ',
     &'page: number -+  +- name               ',
     &'|| |                                   ',
     &'              |  |                     ',
     &'|| |          Help                     ',
     &'              |  |   this page _____   ',
     &'V| V               |                   ',
     &'              V  V              |  |   ',
     &' V                 V                   ',
     &'                                V  V   '/

C    123456789 123456789 123456789 123456789 123456789 1234567
C-----------------------------------------------------------------
C    0          |                                    -// 12345
C    0   . TI   | Type time
C    0   +      | Suggestions and complaints     =>?U
C    0   C 12     SUGGESTIONS                         // X
C    0          | (watch the Terminal Output)         // PP
C
C-----------------------------------------------------------------
C         _____________________________________________    IN\0
C     0 > |TI   | Type time                           |
C     0R> |     | Suggestions and complaints     =>?U |
C     0   |     | (watch the Terminal Output)         |
C         |___________________________________________|
C-----------------------------------------------------------------
C     123456789 123456789 123456789 123456789 123456789 1234567
      CHARACTER *45 T1,T2
      DATA T1/'_____________________________________________'/
      DATA T2/'|___________________________________________|'/

      CHARACTER *1 TUSL
      CHARACTER *49 TYA
C               123456789 123456789 123456789 123456789 123456789
      DATA TYA/'Store ATLANTIS_??.HELP_USL_1     Y=Yes   <cr>=no '/
      
      CALL DPARGI(81,'USL',LEVUS)
    1 CALL DTINT(LEVUS,1,1,TUSL)
      TYA(7:28)=TFILDC//'HELP_USL_'//TUSL

      CALL DTYANS(TYA,'1234567Y',NANSW)
      IF(NANSW.LE.0) RETURN
      IF(NANSW.LE.7) THEN
        LEVUS=NANSW
        GO TO 1
      END IF

      TNAM=TFILDC//'HELP'
      LNAM=LENOCC(TNAM)

      CALL DGOPEN(NUNIDU,TFILDC//'HELP',2,*999,ISTAT)
      CALL DWRT('Wait')
      
      CALL DWF_OPEN(29,TYA(7:28),IER,IROT)
      IF(IER.NE.0) GO TO 999
C     ................................................ READ MANUAL
 4000 FORMAT(A,4X,A,20X,I3,1X,A)
 4001 FORMAT(' ',A,'|',A,' | ',A)
      NP=2
      LP=2

      LEVPA=0
      CALL D_READ_INC(TN,*5)

  804 T=TN

      CALL D_READ_INC(TN,*5)
      IF(T(2:10).EQ.'=========') THEN
        READ(TN,1804) LEVPA
 1804   FORMAT(I1)
        IF(LEVPA.LE.LEVUS) THEN
C         .... this starts with NP=3. TPA(2)='IN0' is set by a data statement.
          NP=NP+1
          TPA(NP)=TN(2:4)
          IF(TPA(NP).EQ.'GT0') N0=NP
        END IF
      END IF
      GO TO 804
    5 REWIND NUNIDU

      WRITE(TXTADW,1002) TYA(7:27),LEVUS
 1002 FORMAT('File = ',A,'     User level =',I2)
      CALL DWF
      CALL DWRC

      CALL DWF_PLATFORM_TEXT('I1',29)
      WRITE(29,4000) '1'
      DO K=1,9
        WRITE(TXTADW,1234) TH(1,K),TH(2,K)
 1234   FORMAT(1X,2A)
        CALL DWF
      END DO
C
      WRITE(TXTADW,4000) ' ',T1,2,'IN0'
      CALL DWF

      CALL D_READ_INC(TN,*900)

      LEVPA=0

  904 T=TN

      CALL D_READ_INC(TN,*900)
      IF(T(2:10).EQ.'=========') READ(TN,1804) LEVPA
      IF(LEVPA.GT.LEVUS) GO TO 904
      READ(T,1804) LEVL
      IF(LEVL.GT.LEVUS) GO TO 904
      IF(T(5:9).EQ.'NAVIG') GO TO 904
      LNGTH=LENOCC(T)
      IF(LNGTH.GT.60) THEN
        WRITE(TXTADW,3990) LNGTH,T
        CALL DWF
        CALL DWRC
        WRITE(TXTADW,3990) LNGTH,TN
        CALL DWF
        CALL DWRC
 3990   FORMAT(' Length=',I3,A)
      END IF
C     ........................................... high ligthing
      T4=' '
      TXPA=' '
      IF(T(1:1).NE.'0') T4(1:1)=T(1:1)
      IF(     T(5:5).EQ.'.') THEN
        T4(4:4)='*'
      ELSE IF(T(5:5).EQ.'+') THEN
        T4(4:4)='*'
        T(7:8)='=>'
        TCH=TPA(LP)(1:2)//T(48:48)
        CALL DPH_FINDC(TPA,TCH,NP,TXPA)
        T(47:48)='  '
      END IF
      IF(T(46:47).EQ.' \') THEN
        TCH=TPA(LP)(1:2)//T(48:48)
        CALL DPH_FINDC(TPA,TCH,NP,TXPA)
      END IF
      IF(T(53:53).EQ.'+') THEN
        TCH=T(54:56)
        CALL DPH_FINDC(TPA,TCH,NP,TXPA)
      END IF
      IF(T(7:22).EQ.'GT   0 Top Level') WRITE(TXPA,3400) N0,'GT0'
 3400 FORMAT(I4,1X,A)
      IF(T(10:10).EQ.'\') THEN
        TCH='GT'//T(11:12)
        CALL DPH_FINDC(TPA,TCH,NP,TXPA)
      END IF
      IF(TN(5:5).EQ.'c'.OR.TN(5:5).EQ.'C') THEN
        DO L=12,48
          IF(TN(L:L).NE.' ') CALL CLTOU(T(L:L))
          READ(TN(7:8),1000) NCOL
 1000     FORMAT(I2)
          T4(2:2)=TCOL(NCOL)
        END DO
      END IF
      IF(T(2:10).EQ.'=========') THEN
        LP=LP+1
        WRITE(TXTADW,4000) ' ',T2
        CALL DWF
        WRITE(29,4000) '1 ',T1,LP,TPA(LP)
C        CALL DWF
      ELSE
        IF(T(5:5).NE.'c'.AND.T(5:5).NE.'C') THEN
          WRITE(TXTADW,4001) T4,T(7:48),TXPA
 2000     FORMAT(5A)
          CALL DWF
        END IF
      END IF
      GO TO 904
  900 CLOSE(UNIT=NUNIDU)
      WRITE(TXTADW,4000) ' ',T2
      CALL DWF
      WRITE(29,4000) '1'
      CALL DWF
      L=1
      TL='IN'
      TCONT=' '
      DO K=2,NP-1
        IF(TPA(K)(1:2).NE.TL.OR.L.GT.70) THEN
          WRITE(TXTADW,3001) TCONT
 3001     FORMAT(1X,A)
          CALL DWF
          TL=TPA(K)(1:2)
          TCONT=' '
          L=1
        END IF
        WRITE(TCONT(L:L+10),3000) K,TPA(K)
 3000   FORMAT(I5,' = ',A)
        L=L+11
      END DO
      IF(L.GT.1) WRITE(TXTADW,3001) TCONT
      CALL DWF
      CALL DWF_CLOSE
      WRITE(TXTADW,2003) NP
      CALL DWRC
 2003 FORMAT(I5,' pages')
      TXTADW=TYA(7:27)//' stored for printing.'
      CALL DWRC
  999 END

*DK DPH_FINDC
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++ DPH_FINDC
CH
      SUBROUTINE DPH_FINDC(TPA,TCH,NP,TXPA)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
      CHARACTER *(*) TPA(*),TCH,TXPA
      DO N=2,NP
        IF(TPA(N).EQ.TCH) THEN
          WRITE(TXPA,3400) N,TCH
 3400     FORMAT(I4,1X,A)
          RETURN
        END IF
        TXPA='???'
      END DO
      END

************************************************************************
      SUBROUTINE SMINIT(NV, NS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (LVER=1, LSUBV=1)
*
*   INITIALIZE RUN-TIME RUOTINES
*
*   INPUT
*     NV, NS : VERSION NUMBER
*
*-----------------------------------------------------------------------
      IF(NV.NE.LVER .OR. NS.NE.LSUBV) THEN
        WRITE(6,100) NV, NS
  100   FORMAT(' ****** VERSION ',I4,'.',I2.2,' IS NOT SUPPORTED *****')
        STOP
      ENDIF
      RETURN
      END
************************************************************************
      SUBROUTINE SMCONF(LT1,LT2,LP1,LP2,EW,AV1,AV2,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*   * DUMMY ARRAY SIZE.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      DOUBLE COMPLEX AV1(0:LASIZE), AV2(0:LASIZE), AV(0:LASIZE)
      INTEGER    LT1(0:LTSIZE), LT2(0:LTSIZE), LT(0:LTSIZE)
      INTEGER    LP1, LP2
      DIMENSION  EW(2)
*
*   CONNECT TWO PARTIAL AMPLITUDES BY A FERMIONIC PROPAGATOR.
*   DENOMINATOR OF THE PROPAGATOR SHOULD BE MULTIPLIED ELSEWHERE.
*
*      EW       : INPUT  : WEIGHT FOR PROPAGATOR
*      AV1, AV2 : INPUT  : TABLES OF AMPLITUDES
*      LT1, LT2 : INPUT  : TABLES OF SIZES.
*      AV       : OUTPUT : TABLES OF AMPLITUDES
*      LT       : OUTPUT : TABLES OF SIZES.
*
*      DECLARATION OF TABLE OF AMPLITUDES
*
*         DIMENTION AV(LAV)
*         LAV = (FOR I = 1 TO N PRODUCT LT(I)),  N = LT(0)
*
*      IS EQUIVALENT TO
*
*         DIMENSION AV(0:LT(1)-1, 0:LT(2)-1, ... , 0:LT(N)-1)
*
*      AN ELEMENT OF THE TABLE
*
*         AV(I_1, I_2, ... , I_N),    0 <= I_J < LT(J)
*
*      IS ACCESSIBLE BY
*
*         AV(I)
*
*      WHERE
*
*         I = (FOR J = 1 TO N  SUM I_J*BASE_J)
*         BASE_J = (FOR K = 1 TO J - 1 PRODUCT LT(K))
*
*      THIS SUBROUTINE CALCULATES.
*
*        AV( I_1, ..., I_(LP1-1), I_(LP1+1), ..., I_(LT1(0)),
*            J_1, ..., J_(LP2-1), J_(LP2+1), ..., J_(LT2(0)) )
*
*        = (FOR K1 = 0 TO 1, K2 = 0 TO 1  SUM
*             AV1(I_1,...,I_(LP1-1),K1,K2,I_(LP1+1),...,I_(LT1(0)))
*            *AV2(J_1,...,J_(LP2-1),K1,K2,J_(LP2+1),...,J_(LT2(0)))
*            *EW(K2) )
*
*       WHERE, K1 IS THE SPIN INDEX AND K2 IS THE INDEX CORRESPONDING
*       TO DECOMPOSED MOMENTA L1 AND L2.
*
      DIMENSION EM(0:3)

*      COMPLEX*32 AVK
      DOUBLE COMPLEX AVK

*-----------------------------------------------------------------------
      EM(0) = EW(1)
      EM(1) = EW(1)
      EM(2) = EW(2)
      EM(3) = EW(2)
      LP = LT1(LP1)
      IF(LP.NE.LT2(LP2)) THEN
        WRITE(6,*) '*** SMCONF:INCONSISTENT TABLE SIZE OF AMPLITUDE.'
        WRITE(6,*) 'LT1(',LP1,') = ', LT1(LP1)
        WRITE(6,*) 'LT2(',LP2,') = ', LT2(LP2)
        WRITE(6,*) 'LT1 =', LT1
        WRITE(6,*) 'LT2 =', LT2
        STOP
      ENDIF
      J = 0
      ILOW1 = 1
      DO 10 I = 1, LP1 - 1
        J = J + 1
        LT(J) = LT1(I)
        ILOW1  = ILOW1 * LT1(I)
   10 CONTINUE
      IHIGH1 = 1
      DO 20 I = LP1 + 1, LT1(0)
        J = J + 1
        LT(J) = LT1(I)
        IHIGH1 = IHIGH1 * LT1(I)
   20 CONTINUE
      ILOW2 = 1
      DO 30 I = 1, LP2 - 1
        J = J + 1
        LT(J) = LT2(I)
        ILOW2  = ILOW2 * LT2(I)
   30 CONTINUE
      IHIGH2 = 1
      DO 40 I = LP2 + 1, LT2(0)
        J = J + 1
        LT(J) = LT2(I)
        IHIGH2 = IHIGH2 * LT2(I)
   40 CONTINUE
      LT(0) = J
      ILOW   = ILOW1 * IHIGH1
      IHIGH  = ILOW2 * IHIGH2
*
      DO 100 IL1 = 0, ILOW1  - 1
      DO 100 IH1 = 0, IHIGH1 - 1
        IL  = IH1 * ILOW1 + IL1
        KK1 = IH1 * ILOW1 * LP + IL1
        DO 100 IL2 = 0, ILOW2  - 1
        DO 100 IH2 = 0, IHIGH2 - 1
          IH  = IH2 * ILOW2 + IL2
          KK2 = IH2 * ILOW2 * LP + IL2
          K   = IH  * ILOW  + IL
          AVK = 0.0D0
          DO 200 IPOL = 0, LP - 1
            K1 = KK1 + IPOL * ILOW1
            K2 = KK2 + IPOL * ILOW2
            AVK = AVK + AV1(K1)*AV2(K2)*EM(IPOL)
  200     CONTINUE
          AV(K) = AVK
  100 CONTINUE
*     CALL CTIME('SMCONF')
      RETURN
      END
************************************************************************
      SUBROUTINE SMCONV(LT1,LT2,LP1,LP2,EW,AV1,AV2,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      DOUBLE COMPLEX AV1(0:LASIZE), AV2(0:LASIZE), AV(0:LASIZE)
      INTEGER    LT1(0:LTSIZE), LT2(0:LTSIZE), LT(0:LTSIZE)
      INTEGER    LP1, LP2
      DIMENSION  EW(0:3)
*
*   Connect two partial amplitudes by a propagator of vector boson.
*   Denominator of the propagator should be multiplied elsewhere.
*
*      EW       : input  : Weight for propagator
*      AV1, AV2 : input  : Tables of amplitudes
*      LT1, LT2 : input  : Tables of sizes.
*      AV       : output : resulting table of amplitudes
*      LT       : output : resulting table of sizes
*
*      Declaration of table of amplitudes
*
*         DIMENTION AV(lav)
*         lav = (for i = 1 to n product LT(i)),  n = LT(0)
*
*      is equivalent to
*
*         DIMENSION AV(0:LT(1)-1, 0:LT(2)-1, ... , 0:LT(n)-1)
*
*      An element of the table
*
*         AV(i_1, i_2, ... , i_n),    0 <= i_j < LT(j)
*
*      is accessible by
*
*         AV(i)
*
*      where
*
*         i = (for j = 1 to n  sum i_j*base_j)
*         base_j = (for k = 1 to j - 1 product LT(k))
*
*      This subroutine calculates.
*
*        AV( i_1, ..., i_(LP1-1), i_(LP1+1), ..., i_(LT1(0)),
*            j_1, ..., j_(LP2-1), j_(LP2+1), ..., j_(LT2(0)) )
*
*        = (for k = 0 to LP-1  sum
*             AV1( i_1, ..., i_(LP1-1), k, i_(LP1+1), ..., i_(LT1(0)) )
*            *AV2( j_1, ..., j_(LP2-1), k, j_(LP2+1), ..., j_(LT2(0)) )
*            *EW(k) )
*
*       where, LP = LT1(LP1) = LT2(LP2) is a degree of freedom for
*       vector boson poralization.
*

*      COMPLEX*32 AVK
      DOUBLE COMPLEX AVK

*-----------------------------------------------------------------------
      LP = LT1(LP1)
      IF(LP.NE.LT2(LP2)) THEN
        WRITE(6,*) '*** SMCONV:INCONSISTENT TABLE SIZE OF AMPLITUDE.'
        WRITE(6,*) 'LT1(',LP1,') = ', LT1(LP1)
        WRITE(6,*) 'LT2(',LP2,') = ', LT2(LP2)
        WRITE(6,*) 'LT1 =', LT1
        WRITE(6,*) 'LT2 =', LT2
        STOP
      ENDIF
      J = 0
      ILOW1 = 1
      DO 10 I = 1, LP1 - 1
        J = J + 1
        LT(J) = LT1(I)
        ILOW1  = ILOW1 * LT1(I)
   10 CONTINUE
      IHIGH1 = 1
      DO 20 I = LP1 + 1, LT1(0)
        J = J + 1
        LT(J) = LT1(I)
        IHIGH1 = IHIGH1 * LT1(I)
   20 CONTINUE
      ILOW2 = 1
      DO 30 I = 1, LP2 - 1
        J = J + 1
        LT(J) = LT2(I)
        ILOW2  = ILOW2 * LT2(I)
   30 CONTINUE
      IHIGH2 = 1
      DO 40 I = LP2 + 1, LT2(0)
        J = J + 1
        LT(J) = LT2(I)
        IHIGH2 = IHIGH2 * LT2(I)
   40 CONTINUE
      LT(0) = J
      ILOW   = ILOW1 * IHIGH1
CC    IHIGH  = ILOW2 * IHIGH2
*
      DO 100 IL1 = 0, ILOW1  - 1
      DO 100 IH1 = 0, IHIGH1 - 1
        IL  = IH1 * ILOW1 + IL1
        KK1 = IH1 * ILOW1 * LP + IL1
        DO 100 IL2 = 0, ILOW2  - 1
        DO 100 IH2 = 0, IHIGH2 - 1
          IH  = IH2 * ILOW2 + IL2
          KK2 = IH2 * ILOW2 * LP + IL2
          K   = IH  * ILOW  + IL
          AVK = 0.0D0
          DO 200 IPOL = 0, LP - 1
            K1 = KK1 + IPOL * ILOW1
            K2 = KK2 + IPOL * ILOW2
            AVK = AVK + AV1(K1)*AV2(K2)*EW(IPOL)
  200     CONTINUE
          AV(K) = AVK
  100 CONTINUE
*     CALL CTIME('SMCONV')
      RETURN
      END
************************************************************************
      SUBROUTINE SMCONS(LT1,LT2,LP1,LP2,AV1,AV2,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      DOUBLE COMPLEX AV1(0:LASIZE), AV2(0:LASIZE), AV(0:LASIZE)
      INTEGER    LP1, LP2
      INTEGER    LT1(0:LTSIZE), LT2(0:LTSIZE), LT(0:LTSIZE)
*
*   Connect two partial amplitudes by a propagator of vector boson.
*   Denominator of the propagator should be multiplied elsewhere.
*
*      AV1, AV2 : input  : Tables of amplitudes
*      LT1, LT2 : input  : Tables of sizes.
*      AV       : output : resulting table of amplitudes
*      LT       : output : resulting table of sizes
*
*      Declaration of table of amplitudes
*
*         DIMENTION AV(lav)
*         lav = (for i = 1 to n product LT(i)),  n = LT(0)
*
*      is equivalent to
*
*         DIMENSION AV(0:LT(1)-1, 0:LT(2)-1, ... , 0:LT(n)-1)
*
*      An element of the table
*
*         AV(i_1, i_2, ... , i_n),    0 <= i_j < LT(j)
*
*      is accessible by
*
*         AV(i)
*
*      where
*
*         i = (for j = 1 to n  sum i_j*base_j)
*         base_j = (for k = 1 to j - 1 product LT(k))
*
*      This subroutine calculates.
*
*        AV( i_1, ..., i_(LT1(0)), j_1, ..., j_(LT2(0)) )
*
*        =    AV1( i_1, ..., i_(LT1(0)) ) * AV2( j_1, ..., j_(LT2(0)) )
*
*-----------------------------------------------------------------------
      IF(LT1(LP1).NE.1 .OR. LT2(LP2).NE.1) THEN
        WRITE(6,*) '*** SMCONS:INCONSISTENT TABLE SIZE OF AMPLITUDE.'
        WRITE(6,*) 'LT1(',LP1,') = ', LT1(LP1)
        WRITE(6,*) 'LT2(',LP2,') = ', LT2(LP2)
        WRITE(6,*) 'LT1 =', LT1
        WRITE(6,*) 'LT2 =', LT2
        STOP
      ENDIF
      LT(0) = LT1(0) + LT2(0) - 2
      IT = 1
      IL1 = 1
      DO 10 I = 1, LT1(0)
        IF(LP1.NE.I) THEN
          IL1    = IL1*LT1(I)
          LT(IT) = LT1(I)
          IT     = IT + 1
        ENDIF
   10 CONTINUE
      IL2 = 1
      DO 20 I = 1, LT2(0)
        IF(LP2.NE.I) THEN
          IL2 = IL2*LT2(I)
          LT(IT) = LT2(I)
          IT     = IT + 1
        ENDIF
   20 CONTINUE
      K = 0
      DO 100 K2 = 0, IL2 - 1
      DO 100 K1 = 0, IL1 - 1
        AV(K) = AV1(K1)*AV2(K2)
        K = K + 1
  100 CONTINUE
*     CALL CTIME('SMCONS')
      RETURN
      END
*-----------------------------------------------------------------------
*     SMFFV revised 94/04/08 by T.Ishikawa
*-----------------------------------------------------------------------
      SUBROUTINE SMFFV(L2,L1,LV,EW2,EW1,AM2,AM1,CPL,CE2,CE1,
     &                 PS2,PS1,EP,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*   * dummy array size.
      PARAMETER (LTSIZE=20,LASIZE = 1024)
      INTEGER    L1, L2, LV
      DIMENSION  EW1(1), EW2(1)

      DOUBLE COMPLEX CPL(2)
      DOUBLE COMPLEX CE1(2,L1), CE2(2,L2)
      DIMENSION  PS1(4,3), PS2(4,3)

      DIMENSION  EP(4,LV)
      DOUBLE COMPLEX AV(0:LASIZE)

      INTEGER    LT(0:LTSIZE)

      DOUBLE COMPLEX AVT(4,2,2,2,2), CPN
      DIMENSION     CPR(2)

*--------------------------- Entry point -------------------------------

      IF( CPL(1) .NE. ZERO) THEN
          CPN      = CPL(1)
          CPR(1) = ONE
          CPR(2) = CPL(2)/CPL(1)
      ELSEIF( CPL(2).NE.ZERO) THEN
          CPN      = CPL(2)
          CPR(2) = ONE
          CPR(1) = CPL(1)/CPL(2)
      ELSE
          CPN      = ZERO
          CPR(1) = ONE
          CPR(2) = ONE
      ENDIF

      IF(EW1(1).GE.ZERO) THEN
         K1 = 3
      ELSE
         K1 = 1
      ENDIF
      IF(EW2(1).GE.ZERO) THEN
         K2 = 3
      ELSE
         K2 = 1
      ENDIF

      LT(0) =  3
      LT(1) =  L2
      LT(2) =  L1
      LT(3) =  LV

      IF( AM1.GT.0.0D0 )THEN
          IF( AM2.GT.0.0D0 )THEN

              DO 110 LP = 1, LV
                 CALL FFVMM1(LP,K1,K2,CPR(1),CPR(2),
     .                      CE1(1,2),CE2(1,1),PS1(1,1),PS1(1,2),
     .                      PS2(1,1),PS2(1,2),EP(1,LP),AVT(1,1,1,1,1))
  110         CONTINUE

              IF(L1.EQ.4) THEN
                 DO 120 LP = 1, LV
                    CALL FFVMM2(LP,CPR(1),CPR(2),CE2(1,1),PS1(1,3),
     &                        EP(1,LP),AVT(1,1,1,2,1))
  120            CONTINUE
              ENDIF

              IF(L2.EQ.4) THEN
                 DO 130 LP = 1, LV
                    CALL FFVMM3(LP,CPR(1),CPR(2),CE1(1,2),PS2(1,3),
     &                          EP(1,LP),AVT(1,1,1,1,2))
  130            CONTINUE
              ENDIF

              IF(L1.EQ.4 .AND. L2.EQ.4) THEN
                 DO 140 LP = 1, LV
                    CALL FFVMM4(LP,CPR(1),CPR(2),
     .                          EP(1,LP),AVT(1,1,1,2,2))
  140            CONTINUE
              ENDIF

          ELSE

              DO 210 LP = 1, LV
                 CALL FFVMM5(LP,K1,K2,CPR(1),CPR(2),
     .                      CE1(1,2),CE2(1,1),PS1(1,1),PS1(1,2),
     .                      PS2(1,1),PS2(1,2),EP(1,LP),AVT(1,1,1,1,1))
  210         CONTINUE

              IF(L1.EQ.4) THEN
                 DO 220 LP = 1, LV
                    CALL FFVMM6(LP,CPR(1),CPR(2),CE2(1,1),PS1(1,3),
     &                        EP(1,LP),AVT(1,1,1,2,1))
  220            CONTINUE
              ENDIF

              IF(L2.EQ.4) THEN
                 DO 230 LP = 1, LV
                    CALL FFVMM3(LP,CPR(1),CPR(2),CE1(1,2),PS2(1,3),
     &                          EP(1,LP),AVT(1,1,1,1,2))
  230            CONTINUE
              ENDIF

              IF(L1.EQ.4 .AND. L2.EQ.4) THEN
                 DO 240 LP = 1, LV
                    CALL FFVMM4(LP,CPR(1),CPR(2),
     .                          EP(1,LP),AVT(1,1,1,2,2))
  240            CONTINUE
              ENDIF
          ENDIF
      ELSE
          IF( AM2.GT.0.0D0 )THEN
              DO 310 LP = 1, LV
                 CALL FFVMM7(LP,K1,K2,CPR(1),CPR(2),
     .                      CE1(1,2),CE2(1,1),PS1(1,1),PS1(1,2),
     .                      PS2(1,1),PS2(1,2),EP(1,LP),AVT(1,1,1,1,1))
  310         CONTINUE

              IF(L1.EQ.4) THEN
                 DO 320 LP = 1, LV
                    CALL FFVMM2(LP,CPR(1),CPR(2),CE2(1,1),PS1(1,3),
     &                        EP(1,LP),AVT(1,1,1,2,1))
  320            CONTINUE
              ENDIF

              IF(L2.EQ.4) THEN
                 DO 330 LP = 1, LV
                    CALL FFVMM8(LP,CPR(1),CPR(2),CE1(1,2),PS2(1,3),
     &                          EP(1,LP),AVT(1,1,1,1,2))
  330            CONTINUE
              ENDIF

              IF(L1.EQ.4 .AND. L2.EQ.4) THEN
                 DO 340 LP = 1, LV
                    CALL FFVMM4(LP,CPR(1),CPR(2),
     .                          EP(1,LP),AVT(1,1,1,2,2))
  340            CONTINUE
              ENDIF

          ELSE

              DO 410 LP = 1, LV
                 CALL FFVMM9(LP,K1,K2,CPR(1),CPR(2),
     .                      CE1(1,2),CE2(1,1),PS1(1,1),PS1(1,2),
     .                      PS2(1,1),PS2(1,2),EP(1,LP),AVT(1,1,1,1,1))
  410         CONTINUE

              IF(L1.EQ.4) THEN
                 DO 420 LP = 1, LV
                    CALL FFVMM6(LP,CPR(1),CPR(2),CE2(1,1),PS1(1,3),
     &                        EP(1,LP),AVT(1,1,1,2,1))
  420            CONTINUE
              ENDIF

              IF(L2.EQ.4) THEN
                 DO 430 LP = 1, LV
                    CALL FFVMM8(LP,CPR(1),CPR(2),CE1(1,2),PS2(1,3),
     &                          EP(1,LP),AVT(1,1,1,1,2))
  430            CONTINUE
              ENDIF

              IF(L1.EQ.4 .AND. L2.EQ.4) THEN
                 DO 440 LP = 1, LV
                    CALL FFVMM4(LP,CPR(1),CPR(2),
     .                          EP(1,LP),AVT(1,1,1,2,2))
  440            CONTINUE
              ENDIF

          ENDIF
      ENDIF

      IA = 0
      DO 500 IL  = 1, LV
      DO 500 IP1 = 1, L1/2
      DO 500 IL1 = 1, 2
      DO 500 IP2 = 1, L2/2
      DO 500 IL2 = 1, 2
         AV(IA) = CPN*AVT(IL, IL1, IL2, IP1, IP2)
         IA = IA + 1
*        print *,'smffv i,l,p1,l1,p2,l2',ia,il,ip1,il1,ip2,il2,
*    *    AVT(IL, IL1, IL2, IP1, IP2)
  500 CONTINUE

      RETURN
      END
************************************************************************
      SUBROUTINE SMFFS(L2,L1,EW2,EW1,AM2,AM1,CPL,CE2,CE1,
     &                 PS2,PS1,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      INTEGER    L1, L2
      DIMENSION  EW1(1), EW2(1)
*     DOUBLE PRECISION     AM1, AM2
      DOUBLE COMPLEX CPL(2)
      DOUBLE COMPLEX CE1(2,L1), CE2(2,L2)
      DIMENSION  PS1(4,3), PS2(4,3)
*     DIMENSION  PS1(4,L1/2+1), PS2(4,L2/2+1)
      DOUBLE COMPLEX AV(0:LASIZE)
*     DOUBLE COMPLEX AV(0:L2*L1-1)
      INTEGER    LT(0:LTSIZE)
*
*    Calculate fermion-fermion-scalar vertex.
*
*           ! 3                 _      _
*           !                1  u  or  v
*      -->--+-->---          2  u  or  v
*        2     1             3  scalar boson
*
*     L1, L2   : input  : If external then 2 else 4, for fermion
*     EW1, EW2 : input  : If >=0 then particle else anti-particle
*     AM1, AM2 : input  : masses of the fermions
*     CPL      : input  : coupling constants (L, R)
*     CE1, CE2 : input  : phase factor, calculated by SMINTF or SMEXTF
*     PS1, PS2 : input  : decomposed momenta (L21, L22 , L1) or
*                         (L21, L22) calculated by SMINTF or SMEXTF
*     AV       : output : table of amplitudes
*     LT       : output : table of sizes in AV
*
      DOUBLE COMPLEX AVT(2,2,2,2), CPN
      DIMENSION  P0(4), CPR(2)
      DATA P0/4*ZERO/, AM0/ZERO/
*-----------------------------------------------------------------------
      IF(CPL(1).NE.ZERO) THEN
        CPN = CPL(1)
        CPR(1) = ONE
        CPR(2) = CPL(2)/CPL(1)
      ELSE IF(CPL(2).NE.ZERO) THEN
        CPN = CPL(2)
        CPR(2) = ONE
        CPR(1) = CPL(1)/CPL(2)
      ELSE
CC      WRITE(6,*) '*** SMFFS:COUPLING CONSTANT FOR FFS VERTEX IS 0.'
CC      WRITE(6,*) 'L2=',L2,' L1=',L1,' EW2=',EW2,' EW1=',EW1
CC      WRITE(6,*) 'AM2=',AM2,' AM1=',AM1,' CPL=',CPL
CC      FOR FACOM
CC      CALL SDFDMP(1, 0)
CC      STOP
        CPN = ZERO
        CPR(1) = ONE
        CPR(2) = ONE
      ENDIF
      IF(EW1(1).GE.ZERO) THEN
        K1 = 3
      ELSE
        K1 = 1
      ENDIF
      IF(EW2(1).GE.ZERO) THEN
        K2 = 3
      ELSE
        K2 = 1
      ENDIF
      LT(0) = 3
      LT(1) = L2
      LT(2) = L1
      LT(3) = 1
      CALL FFS(K1,K2,AM1,AM2,CPR(1),CPR(2),CE1(1,2),CE2(1,1),
     &         PS1(1,1),PS1(1,2),PS2(1,1),PS2(1,2), AVT(1,1,1,1))
      IF(L1.EQ.4) THEN
          CALL FFS( 3,K2,AM0,AM2,CPR(1),CPR(2),CE1(1,4),CE2(1,1),
     &             PS1(1,3),P0,      PS2(1,1),PS2(1,2), AVT(1,1,2,1))
      ENDIF
      IF(L2.EQ.4) THEN
          CALL FFS(K1, 3,AM1,AM0,CPR(1),CPR(2),CE1(1,2),CE2(1,3),
     &             PS1(1,1),PS1(1,2),PS2(1,3),P0,       AVT(1,1,1,2))
  300   CONTINUE
      ENDIF
      IF(L1.EQ.4 .AND. L2.EQ.4) THEN
          CALL FFS( 3, 3,AM0,AM0,CPR(1),CPR(2),CE1(1,4),CE2(1,3),
     &             PS1(1,3),P0,      PS2(1,3),P0,       AVT(1,1,2,2))
  400   CONTINUE
      ENDIF
      IA = 0
      DO 500 IP1 = 1, L1/2
      DO 500 IL1 = 1, 2
      DO 500 IP2 = 1, L2/2
      DO 500 IL2 = 1, 2
        AV(IA) = CPN*AVT(IL1, IL2, IP1, IP2)
        IA = IA + 1
  500 CONTINUE
*     CALL CTIME('SMFFS ')
      RETURN
      END
************************************************************************
      SUBROUTINE SMVVV(L1,L2,L3,K1,K2,K3,CPL,P1,P2,P3,E1,E2,E3,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      INTEGER    L1, L2, L3
      INTEGER    K1, K2, K3
      DOUBLE COMPLEX CPL
      DIMENSION  P1(4),    P2(4),    P3(4)
      DIMENSION  E1(4,L1), E2(4,L2), E3(4,L3)
      DOUBLE COMPLEX AV(0:LASIZE)
*     DOUBLE COMPLEX AV(0:L3*L2*L1-1)
      INTEGER    LT(0:LTSIZE)
*
*    Calculate vector-vector-vector vertex.
*
*           ! 3
*           V
*      -->--+--<---
*        2     1
*
*     L1,L2,L3 : input  : number of polarization vectors (2, 3 OR 4)
*     K1,K2,K3 : input  : if incoming momentum then 1 else -1
*     CPL      : input  : coupling constant.
*     P1,P2,P3 : input  : momentum vectors
*     E1,E2,E3 : input  : set of polarization vectors
*     AV       : output : table of amplitudes
*     LT       : output : table of sizes in AV
*
      DIMENSION  PP12(4), PP23(4), PP31(4)
*-----------------------------------------------------------------------
      LT(0) = 3
      LT(1) = L1
      LT(2) = L2
      LT(3) = L3
      DO 10 J = 1, 4
        PP12(J) = K1*P1(J) - K2*P2(J)
        PP23(J) = K2*P2(J) - K3*P3(J)
        PP31(J) = K3*P3(J) - K1*P1(J)
   10 CONTINUE
      IA = 0
      DO 500 IL3 = 1, L3
        E3P1P2=E3(4,IL3)*PP12(4)-E3(1,IL3)*PP12(1)
     &        -E3(2,IL3)*PP12(2)-E3(3,IL3)*PP12(3)
      DO 500 IL2 = 1, L2
        E2P3P1=E2(4,IL2)*PP31(4)-E2(1,IL2)*PP31(1)
     &        -E2(2,IL2)*PP31(2)-E2(3,IL2)*PP31(3)
        E2E3=E2(4,IL2)*E3(4,IL3)-E2(1,IL2)*E3(1,IL3)
     &      -E2(2,IL2)*E3(2,IL3)-E2(3,IL2)*E3(3,IL3)
      DO 500 IL1 = 1, L1
CX      CALL VVV(1.0D0,PP1,PP2,PP3,E1(1,IL1),E2(1,IL2),E3(1,IL3),AV0)
        E1P2P3=E1(4,IL1)*PP23(4)-E1(1,IL1)*PP23(1)
     &        -E1(2,IL1)*PP23(2)-E1(3,IL1)*PP23(3)
        E1E2=E1(4,IL1)*E2(4,IL2)-E1(1,IL1)*E2(1,IL2)
     &      -E1(2,IL1)*E2(2,IL2)-E1(3,IL1)*E2(3,IL2)
        E1E3=E1(4,IL1)*E3(4,IL3)-E1(1,IL1)*E3(1,IL3)
     &      -E1(2,IL1)*E3(2,IL3)-E1(3,IL1)*E3(3,IL3)
        AV(IA) = CPL*(E3P1P2*E1E2+E1P2P3*E2E3+E2P3P1*E1E3)
        IA = IA + 1
  500 CONTINUE
*     CALL CTIME('SMVVV ')
      RETURN
      END
************************************************************************
      SUBROUTINE SMSVV (L1,L2,CPL,E1,E2,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      INTEGER    L1, L2
      DOUBLE COMPLEX CPL
      DIMENSION  E1(4,L1), E2(4,L2)
      DOUBLE COMPLEX AV(0:LASIZE)
*     DOUBLE COMPLEX AV(0:L2*L1-1)
      INTEGER    LT(0:LTSIZE)
*
*    Calculate vector-vector-scalar vertex.
*
*           ! 3(S)
*           V
*      -->--+--<---
*      2(V)     1(V)
*
*     L1,L2    : input  : number of polarization vectors (2, 3 OR 4)
*     CPL      : input  : coupling constant.
*     E1,E2    : input  : set of polarization vectors
*     AV       : output : table of amplitudes
*     LT       : output : table of sizes in AV
*
*-----------------------------------------------------------------------
      LT(0) = 3
      LT(1) = 1
      LT(2) = L1
      LT(3) = L2
      IA = 0
      DO 500 IL2 = 1, L2
      DO 500 IL1 = 1, L1
        CALL VVS(1.0D0,E1(1,IL1),E2(1,IL2),AV0)
        AV(IA) = CPL*AV0
        IA = IA + 1
  500 CONTINUE
*     CALL CTIME('SMSVV ')
      RETURN
      END
************************************************************************
      SUBROUTINE SMSSV(L1,K2,K3,CPL,P2,P3,E1,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      INTEGER    L1
      INTEGER    K2, K3
      DOUBLE COMPLEX CPL
      DIMENSION  P2(4),    P3(4)
      DIMENSION  E1(4,L1)
      DOUBLE COMPLEX AV(0:LASIZE)
*     DOUBLE COMPLEX AV(0:L1-1)
      INTEGER    LT(0:LTSIZE)
*
*    Calculate vector-scalar-scalar vertex.
*
*           ! 3(S)
*           V
*      -->--+--<---
*      2(S)     1(V)
*
*     L1       : input  : number of polarization vectors (2, 3 OR 4)
*     K2,K3    : input  : if incoming momentum then 1 else -1
*     CPL      : input  : coupling constant.
*     P2,P3    : input  : momentum vectors
*     E1       : input  : set of polarization vectors
*     AV       : output : table of amplitudes
*     LT       : output : table of sizes in AV
*
      DIMENSION  PP2(4), PP3(4)
*-----------------------------------------------------------------------
      LT(0) = 3
      LT(1) = 1
      LT(2) = 1
      LT(3) = L1
      DO 10 J = 1, 4
        PP2(J) = K2*P2(J)
        PP3(J) = K3*P3(J)
   10 CONTINUE
      IA = 0
      DO 500 IL1 = 1, L1
        CALL SSV(1.0D0,PP2,PP3,E1(1,IL1),AV0)
        AV(IA) = CPL*AV0
        IA = IA + 1
  500 CONTINUE
*     CALL CTIME('SMSSV ')
      RETURN
      END
************************************************************************
      SUBROUTINE SMSSS(CPL, LT, AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      DOUBLE COMPLEX CPL
      DOUBLE COMPLEX AV(0:LASIZE)
*     DOUBLE COMPLEX AV(0:L2*L1-1)
      INTEGER    LT(0:LTSIZE)
*
*    Calculate scalar-scalar-scalar-scalar vertex.
*
*           ! 3
*           V
*      -->--+--<---
*        1     2
*
*     CPL      : input  : coupling constant.
*     AV       : output : table of amplitudes
*     LT       : output : table of sizes in AV
*-----------------------------------------------------------------------
      LT(0) = 3
      LT(1) = 1
      LT(2) = 1
      LT(3) = 1
      AV(0) = CPL
*     CALL CTIME('SMSSS ')
      RETURN
      END
************************************************************************
      SUBROUTINE SMVVVV(L1,L2,L3,L4,CPL,E1,E2,E3,E4,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      INTEGER    L1, L2, L3, L4
      DOUBLE COMPLEX CPL
      DIMENSION  E1(4,L1), E2(4,L2), E3(4,L3), E4(4,L4)
      DOUBLE COMPLEX AV(0:LASIZE)
*     DOUBLE COMPLEX AV(0:L2*L1-1)
      INTEGER    LT(0:LTSIZE)
*
*    Calculate vector-vector-vector-vector vertex.
*
*           ! 4
*           V
*      -->--+--<---
*        1  ^  3
*           !2
*
*     L1,L2,L3,L4 : input  : number of polarization vectors (2, 3 OR 4)
*     CPL         : input  : coupling constant.
*     E1,E2,E3,E4 : input  : set of polarization vectors
*     AV          : output : table of amplitudes
*     LT          : output : table of sizes in AV
*
*-----------------------------------------------------------------------
      LT(0) = 4
      LT(1) = L1
      LT(2) = L2
      LT(3) = L3
      LT(4) = L4
*TI
*      write(6,*) ' <<SMVVVV>> LT = ',LT
*      write(6,*) ' <<SMVVVV>> L4 = ',L4
      IA = 0
      DO 500 IL4 = 1, L4
      DO 500 IL3 = 1, L3
      DO 500 IL2 = 1, L2
      DO 500 IL1 = 1, L1
        CALL VVVV(1.0D0,E1(1,IL1),E2(1,IL2),E3(1,IL3),E4(1,IL4),AV0)
        AV(IA) = CPL*AV0
        IA = IA + 1
  500 CONTINUE
*TI 25th Nov. 1991 for INTEL
*      LT(0) = 4
*      LT(1) = L1
*      LT(2) = L2
*      LT(3) = L3
*      LT(4) = L4
*      write(6,*) ' <<SMVVVV>> LT = ',LT
*      write(6,*) ' <<SMVVVV>> L4 = ',L4
*     CALL CTIME('SMVVVV')
      RETURN
      END
************************************************************************
      SUBROUTINE SMSSVV(L1,L2,CPL,E1,E2,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      INTEGER    L1, L2
      DOUBLE COMPLEX CPL
      DIMENSION  E1(4,L1), E2(4,L2)
      DOUBLE COMPLEX AV(0:LASIZE)
*     DOUBLE COMPLEX AV(0:L2*L1-1)
      INTEGER    LT(0:LTSIZE)
*
*    Calculate vector-vector-scalar-scalar vertex.
*
*           ! 4(S)
*           V
*      -->--+--<---
*     1(V)  ^  3(S)
*           !2(V)
*
*     L1,L2    : input  : number of polarization vectors (2, 3 OR 4)
*     CPL      : input  : coupling constant.
*     E1,E2    : input  : set of polarization vectors
*     AV       : output : table of amplitudes
*     LT       : output : table of sizes in AV
*
*-----------------------------------------------------------------------
      LT(0) = 4
      LT(1) = 1
      LT(2) = 1
      LT(3) = L1
      LT(4) = L2
      IA = 0
      DO 500 IL2 = 1, L2
      DO 500 IL1 = 1, L1
        CALL VVS(1.0D0,E1(1,IL1),E2(1,IL2),AV0)
        AV(IA) = CPL*AV0
        IA = IA + 1
  500 CONTINUE
*     CALL CTIME('SMSSVV')
      RETURN
      END
************************************************************************
      SUBROUTINE SMSSSS(CPL, LT, AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      DOUBLE COMPLEX CPL
      DOUBLE COMPLEX AV(0:LASIZE)
*     DOUBLE COMPLEX AV(0:L2*L1-1)
      INTEGER    LT(0:LTSIZE)
*
*    Calculate scalar-scalar-scalar-scalar vertex.
*
*           ! 4
*           V
*      -->--+--<---
*        1  ^  3
*           !2
*
*     CPL      : input  : coupling constant.
*     AV       : output : table of amplitudes
*     LT       : output : table of sizes in AV
*-----------------------------------------------------------------------
      LT(0) = 4
      LT(1) = 1
      LT(2) = 1
      LT(3) = 1
      LT(4) = 1
      AV(0) = CPL
*     CALL CTIME('SMSSSS')
      RETURN
      END
************************************************************************
      SUBROUTINE SMEXTV(LP, AM, PI, EP, EW, IGAUG)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0, ANOGAU=200.0D0)
      INTEGER LP
*     DOUBLE PRECISION    AM
      DIMENSION PI(4), EP(4,LP), EW(LP)
*
*   Caluculate polarization vectors for vector boson.
*
*     LP    : input  : degree of freedom
*                      2: ext. photon
*                      3: int. photon or ext. weak-boson
*                      4: int. weak-boson
*     AM    : input  : mass of the vector boson
*     PI    : input  : momentum of the vector boson
*     EP    : output : set of polarization vectors
*     EW    : output : weights for the polariztion vectors
*     IGAUG : input  : selctor of gauge parameter
*
* Common for Gauge Parameters 1:A, 2:W, 3:Z, 4:QCD
      COMMON /SMGAUG/AGAUGE(0:4)
*-----------------------------------------------------------------------
*TI
*      write(6,*) '<<SMEXTV>> LP ',LP
*TI
      DO 10 I = 1, LP
        EW(I) = 0.0
   10 CONTINUE
 
      IF(LP.EQ.1) THEN
        DO 20 I = 1, 4
          EP(I,1) = PI(I)
   20   CONTINUE
        EW(1) = 1.0D0
 
*     AXIAL GAUGE
      ELSE IF(IGAUG.EQ.-1) THEN
        VNK = PI(4) - PI(1)
        IF (VNK.NE.0.D0) THEN
          I   = 1
          J   = 3
        ELSE
          VNK = PI(4) - PI(3)
          I   = 3
          J   = 1
        ENDIF
        EP(4,1) = PI(2)/VNK
        EP(I,1) = PI(2)/VNK
        EP(2,1) = 1
        EP(J,1) = 0
        EP(4,2) = PI(J)/VNK
        EP(I,2) = PI(J)/VNK
        EP(2,2) = 0
        EP(J,2) = 1
        EW(1)   = 1
        EW(2)   = 1
        IF(LP.GT.2) THEN
          AKK = PI(4)**2 - PI(1)**2 - PI(2)**2 - PI(3)**2
          IF(AKK.GT.0) THEN
            SIGNK = 1.0
          ELSE
            SIGNK =-1.0
          ENDIF
          AKK   = SQRT(ABS(AKK))
          EP(4,3) = AKK/VNK
          EP(I,3) = AKK/VNK
          EP(2,3) = 0
          EP(J,3) = 0
          EW(3)   = SIGNK
        ENDIF
 
*     UNITARY GAUGE
      ELSE IF(IGAUG.EQ.0) THEN
        DO 30 I = 1, LP
          CALL POLA(I, ANOGAU, AM, PI, EP(1,I), EW)
   30   CONTINUE
 
*     COVARIANT GAUGE
      ELSE
        DO 40 I = 1, LP
          CALL POLA(I, AGAUGE(IGAUG), AM, PI, EP(1,I), EW)
   40   CONTINUE
      ENDIF
 
*     CALL CTIME('SMEXTV')
      RETURN
      END
************************************************************************
      SUBROUTINE SMINTV(LP, AM, PI, EP, EW, VM, IGAUG)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0, ANOGAU=200.0D0)
      INTEGER LP
*     DOUBLE PRECISION    AM, VM
      DIMENSION PI(4), EP(4,LP), EW(LP)
*
*   Caluculate polarization vectors for vector boson.
*
*     LP    : input  : degree of freedom
*                      2: ext. photon
*                      3: int. photon or ext. weak-boson
*                      4: int. weak-boson
*     NP    : OUTPUT : EFFECTIVE VALUE OF LP
*     AM    : input  : mass of the vector boson
*     PI    : input  : momentum of the vector boson
*     EP    : output : set of polarization vectors
*     EW    : output : weights for the polariztion vectors
*     VM    : input  : PI.PI
*     IGAUG : input  : selctor of gauge parameter
*
* Common for Gauge Parameters 1:A, 2:W, 3:Z, 4:QCD
      COMMON /SMGAUG/AGAUGE(0:4)
 
      DATA DD/0.0D0/
*-----------------------------------------------------------------------
      DO 40 I = 1, LP
        EW(I) = 0.0D0
   40 CONTINUE
 
      IF(LP.EQ.1) THEN
        DO 30 I = 1, 4
          EP(I,1) = PI(I)
   30   CONTINUE
        EW(1) = 1.0D0
        NP    = 1
 
*     AXIAL GAUGE
      ELSE IF(IGAUG.EQ.-1) THEN
        VNK = PI(4) - PI(1)
        IF (VNK.NE.0.D0) THEN
          I = 1
          J = 3
        ELSE
          VNK = PI(4) - PI(3)
          I = 3
          J = 1
        ENDIF
        EP(4,1) = PI(2)/VNK
        EP(I,1) = PI(2)/VNK
        EP(2,1) = 1
        EP(J,1) = 0
        EP(4,2) = PI(J)/VNK
        EP(I,2) = PI(J)/VNK
        EP(2,2) = 0
        EP(J,2) = 1
        EW(1)   = 1
        EW(2)   = 1
        NP      = 2
        IF(LP.GT.2) THEN
          IF(AKK.GT.0) THEN
            SIGNK = 1.0
          ELSE
            SIGNK =-1.0
          ENDIF
          AKK   = SQRT(ABS(AKK))
          EP(4,3) = VM/VNK
          EP(I,3) = VM/VNK
          EP(2,3) = 0
          EP(J,3) = 0
          EW(3)   = SIGNK
          NP      = 4
        ENDIF
 
*     COVARIANT GAUGE
      ELSE
        PT2 = PI(1)**2 + PI(2)**2
        IF(PT2.LE.DD) THEN
          PT  = 0.0D0
          RTY = 1.0D0
          RTX = 0.0D0
        ELSE
          PT  = SQRT(PT2)
          PTI = 1.0D0/PT
          RTY = PI(2)*PTI
          RTX = PI(1)*PTI
        ENDIF
        PN2 = PT2 + PI(3)**2
        IF(PN2.LE.DD) THEN
          PN  = 0.0D0
          RNX = 0.0D0
          RNY = 0.0D0
          RNZ = 1.0D0
          RTN = 0.0D0
        ELSE IF(PN2.GT.DD) THEN
          PN  = SQRT(PN2)
          PNI = 1.0D0/PN
          RNX = PI(1)*PNI
          RNY = PI(2)*PNI
          RNZ = PI(3)*PNI
          RTN = PT*PNI
        ENDIF
 
        EP(1,1) =  RTX*RNZ
        EP(2,1) =  RTY*RNZ
        EP(3,1) = -RTN
        EP(4,1) =  0.0D0
        EW(1) =  1.0D0
 
        EP(1,2) = -RTY
        EP(2,2) =  RTX
        EP(3,2) =  0.0D0
        EP(4,2) =  0.0D0
        EW(2) =  1.0D0
        NP    =  2
 
        APP=ABS(VM)
        IF(LP.GE.3) THEN
          IF(APP.GT.DD) THEN
            NP = NP + 1
            RPPI  = 1.0D0/SQRT(APP)
            EP(1,NP) = RNX*PI(4)*RPPI
            EP(2,NP) = RNY*PI(4)*RPPI
            EP(3,NP) = RNZ*PI(4)*RPPI
            EP(4,NP) = PN*RPPI
            EW(NP) = SIGN(1.0D0, VM)
          ENDIF
        ENDIF
 
        IF(LP.GE.4) THEN
          IF (APP.GT.DD) THEN
            NP = NP + 1
            RPPI  = 1.0D0/SQRT(APP)
            EP(1,NP) = PI(1)*RPPI
            EP(2,NP) = PI(2)*RPPI
            EP(3,NP) = PI(3)*RPPI
            EP(4,NP) = PI(4)*RPPI
            AM2 = AM*AM
 
*           COVARIANT GAUGE
            IF(IGAUG.NE.0) THEN
              A = AGAUGE(IGAUG)
              AVM = VM - A*AM2
              IF (ABS(AVM).GT.DD) THEN
                EW(NP) =  SIGN(1.0D0, VM)*(AM2-VM)*A/AVM
              ELSE
                EW(NP) = -SIGN(1.0D0, VM)
                WRITE(6,*) ' Caution : p*p = a*m**2 '
                WRITE(6,*) '  a = 1 was taken. '
              END IF
 
*           UNITARY GAUGE
            ELSE
              IF (AM.GT.0.D0) THEN
                EW(NP) =   SIGN(1.0D0, VM)*(VM/AM**2-1.0D0)
              ELSE
                EW(NP) = -SIGN(1.0D0, VM)*ANOGAU
              END IF
            END IF
          ENDIF
        END IF
      END IF
 
*     CALL CTIME('SMINTV')
      RETURN
      END
************************************************************************
      SUBROUTINE SMINTF(AM, PI, VM, EW, PS, CE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*     DOUBLE PRECISION     AM
      DIMENSION  PI(4), EW(2), PS(4,3)
      DOUBLE COMPLEX CE(2,4)
*
*   Decompose momentum of internal fermion.
*
*            PI = L1 + L2          off-shell momentum
*            L1**2  = 0            massless on-shell
*            L2**2  = AM**2        massive on-shell
*            L2 = L21 + L22
*
*            L21**2 = 0            massless on-shell
*            L22**2 = 0            massless on-shell
*            PS(*, 1) = L21
*            PS(*, 2) = L22
*            PS(*, 3) = L1
*
*     AM : input  : Mass of the fermion
*     PI : input  : Momentum of the fermion (off-shell)
*     EW : output : Weight for L2 and L1
*     PS : output : Decomposed massless momenta
*     CE : output : Phase factor for L2 and L1.
*
*     1994/12/17 : fug fix for pi(j) = 0, j <> 3     T.Kaneko
*     1994/12/20 :                                   Y.Kurihara
*
      DIMENSION PM(4)
*     DATA DD/1.0D-7/
      DATA DD/0.0D0/
*-----------------------------------------------------------------------
      PSN  = SQRT(PI(1)**2 + PI(2)**2 + PI(3)**2)
      IF(ABS(PI(4)).LE.DD) THEN
        IF(PSN.LE.DD) THEN
            P0  = 0.0D0
        ELSE
            A   = 1.0D0/SQRT(3.0D0)
            B   = A
            C   = A
            DEN = PI(4)-A*PI(1)-B*PI(2)-C*PI(3)
            P0  = (VM - AM**2)/(2.0D0*DEN)
        ENDIF
      ELSE 
        IF(PSN.LE.DD) THEN
            A = 1.0D0
            B = 0.0D0
            C = 0.0D0
            DEN = PI(4)-PI(1)
        ELSE IF(PI(4) .LT. 0.0D0) THEN
            PSNI = 1.0D0/PSN
            A = - PI(1)*PSNI
            B = - PI(2)*PSNI
            C = - PI(3)*PSNI
            den=    pi(4)**2
     .-         max(pi(1)**2,pi(2)**2,pi(3)**2)
     .-         max(min(max(pi(1)**2,pi(2)**2),pi(3)**2)
     .,                 min(pi(1)**2,pi(2)**2)          )
     .-         min(pi(1)**2,pi(2)**2,pi(3)**2)
            den=den/(pi(4)-psn)
        ELSE
            PSNI = 1.0D0/PSN
            A = PI(1)*PSNI
            B = PI(2)*PSNI
            C = PI(3)*PSNI
            den=    pi(4)**2
     .-         max(pi(1)**2,pi(2)**2,pi(3)**2)
     .-         max(min(max(pi(1)**2,pi(2)**2),pi(3)**2)
     .,                 min(pi(1)**2,pi(2)**2)          )
     .-         min(pi(1)**2,pi(2)**2,pi(3)**2)
            den=den/(pi(4)+psn)
        ENDIF
c       DEN = PI(4)-A*PI(1)-B*PI(2)-C*PI(3)
        P0  = (VM - AM**2)/(2.0D0*DEN)
      ENDIF

      P10  = ABS(P0)
      IF (P10.LE.0.0D0) THEN
        EW(2) = 0.0D0
      ELSE
        EW(2) = SIGN(1.0D0, P0)
      END IF
      PS(1, 3) = A*P10
      PS(2, 3) = B*P10
      PS(3, 3) = C*P10
      PS(4, 3) =   P10
 
      P00=PI(4)-EW(2)*PS(4, 3)
      P20=ABS(P00)
      IF (P20.LE.0.0D0) THEN
        EW(1) = 0.0D0
      ELSE
        EW(1) = SIGN(1.0D0, P00)
      END IF
 
      PM(1) = EW(1)*(PI(1) - EW(2)*PS(1, 3))
      PM(2) = EW(1)*(PI(2) - EW(2)*PS(2, 3))
      PM(3) = EW(1)*(PI(3) - EW(2)*PS(3, 3))
      PM(4) = EW(1)*(PI(4) - EW(2)*PS(4, 3))
 
      CALL SPLTQ(AM, PM, PS(1,2), PS(1,1))
      CALL PHASEQ(1,      PM, CE(1,1))
      CALL PHASEQ(2,      PM, CE(1,2))
      CALL PHASEQ(1, PS(1,3), CE(1,3))
      CALL PHASEQ(2, PS(1,3), CE(1,4))
 
*     write(*,*) 'pi = ', pi(1),pi(2),pi(3),pi(4),' mass=',am
*     write(*,*) 'p1 = ', ps(1,1),ps(2,1),ps(3,1),ps(4,1),
*    &           ps(4,1)**2-ps(1,1)**2-ps(2,1)**2-ps(3,1)**2
*     write(*,*) 'p2 = ', ps(1,2),ps(2,2),ps(3,2),ps(4,2),
*    &           ps(4,2)**2-ps(1,2)**2-ps(2,2)**2-ps(3,2)**2
*     write(*,*) 'p3 = ', ps(1,3),ps(2,3),ps(3,3),ps(4,3),
*    &           ps(4,3)**2-ps(1,3)**2-ps(2,3)**2-ps(3,3)**2
*     write(*,*) 'pm = ', pm(1),pm(2),pm(3),pm(4),
*    &           pm(4)**2-pm(1)**2-pm(2)**2-pm(3)**2,am**2

*     CALL CTIME('SMINTF')
      RETURN
      END
************************************************************************
      SUBROUTINE SMINTP(LP, AM, PI, EP, EW, VM, IGAUG)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0, ANOGAU=200.0D0)
      INTEGER LP
*     DOUBLE PRECISION    AM, VM
      DIMENSION PI(4), EP(4,LP), EW(LP)
*
*   Caluculate polarization vectors for vector boson.
*   QED vertex with on-shell fermions.
* Gauge invarience can not be checked by using this subroutine.
*
*
*     LP    : input  : degree of freedom
*                      2: ext. photon
*                      3: int. photon or ext. weak-boson
*                      4: int. weak-boson
*     NP    : OUTPUT : EFFECTIVE VALUE OF LP
*     AM    : input  : mass of the vector boson
*     PI    : input  : momentum of the vector boson
*     EP    : output : set of polarization vectors
*     EW    : output : weights for the polariztion vectors
*     VM    : input  : PI.PI
*     IGAUG : input  : selctor of gauge parameter
*
* Common for Gauge Parameters 1:A, 2:W, 3:Z, 4:QCD
      COMMON /SMGAUG/AGAUGE(0:4)

      DATA DD/0.0D0/
*-----------------------------------------------------------------------
      DO 40 I = 1, LP
        EW(I) = 0.0D0
   40 CONTINUE

CX    WRITE(6,*) ' ------  SMINTP --------- '
      IF(LP.EQ.1) THEN
        DO 30 I = 1, 4
          EP(I,1) = PI(I)
   30   CONTINUE
        EW(1) = 1.0D0
        NP    = 1
      END IF

        APP=ABS(VM)
        RAPP = SQRT(APP)

*     COVARIANT GAUGE
        PT2 = PI(1)**2 + PI(2)**2
        IF(PT2.LE.DD) THEN
          PT  = 0.0D0
          RTY = 1.0D0
          RTX = 0.0D0
        ELSE
          PT  = SQRT(PT2)
          PTI = 1.0D0/PT
          RTY = PI(2)*PTI
          RTX = PI(1)*PTI
        ENDIF
        PN2 = PT2 + PI(3)**2
CX      PP = (PI(4)-PI(3))*(PI(4)+PI(3))-PT2
        IF(PN2.LE.DD) THEN
          PN  = 0.0D0
          RNX = 0.0D0
          RNY = 0.0D0
          RNZ = 1.0D0
          RTN = 0.0D0
        ELSE IF(PN2.GT.DD) THEN
          PN  = SQRT(PN2)
          PNI = 1.0D0/PN
          RNX = PI(1)*PNI
          RNY = PI(2)*PNI
          RNZ = PI(3)*PNI
          RTN = PT*PNI
        ENDIF

        EP(1,1) =  RTX*RNZ
        EP(2,1) =  RTY*RNZ
        EP(3,1) = -RTN
        EP(4,1) =  0.0D0
        EW(1) =  1.0D0

        EP(1,2) = -RTY
        EP(2,2) =  RTX
        EP(3,2) =  0.0D0
        EP(4,2) =  0.0D0
        EW(2) =  1.0D0
        NP = 2

c       WRITE(6,*) 'PI(1) PI(2) ',PI(1),PI(2)
c       WRITE(6,*) 'PT PTI RTY  ',PT,PTI,RTY
c       WRITE(6,*) 'EP          ',EP(1,2)

        IF(LP.GE.3) THEN
          IF (APP.GT.DD) THEN
            NP = NP + 1
            RPPI  = 1.0D0/SQRT(APP)
            EP(1,NP) = 0.D0
            EP(2,NP) = 0.D0
            EP(3,NP) = 0.D0
            EP(4,NP) = -RAPP/PN
CX          EP(4,NP) = -1.D0/PN
            EW(NP) = SIGN(1.0D0, VM)
c           WRITE(6,*) ' NP APP EP ',NP,APP,EP(4,NP)
          ENDIF
        ENDIF

        IF(LP.GE.4) THEN
          IF (APP.GT.DD) THEN
            NP = NP + 1
            RPPI  = 1.0D0/SQRT(APP)
            EP(1,NP) = 0.D0
            EP(2,NP) = 0.D0
            EP(3,NP) = 0.D0
            EP(4,NP) = 0.D0
            EW(NP) = 0.D0
            AM2 = AM*AM
         END IF
       END IF

c       WRITE(6,*) 'NP EP  ',NP,EP(1,2)
*     CALL CTIME('SMINTV')
      RETURN
      END
************************************************************************
      SUBROUTINE SMINTT(LP, AM, PI, EP, EW, VM, IGAUG)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0, ANOGAU=200.0D0)
      INTEGER LP
*     DOUBLE PRECISION    AM, VM
      DIMENSION PI(4), EP(4,LP), EW(LP)
*
*   Caluculate polarization vectors for vector boson.
*   QED vertex with on-shell fermions.
* Gauge invarience can not be checked by using this subroutine.
*
*
*     LP    : input  : degree of freedom
*                      2: ext. photon
*                      3: int. photon or ext. weak-boson
*                      4: int. weak-boson
*     NP    : OUTPUT : EFFECTIVE VALUE OF LP
*     AM    : input  : mass of the vector boson
*     PI    : input  : momentum of the vector boson
*     EP    : output : set of polarization vectors
*     EW    : output : weights for the polariztion vectors
*     VM    : input  : PI.PI
*     IGAUG : input  : selctor of gauge parameter
*
* Common for Gauge Parameters 1:A, 2:W, 3:Z, 4:QCD
      COMMON /SMGAUG/AGAUGE(0:4)

      DATA DD/0.0D0/
*-----------------------------------------------------------------------
      DO 40 I = 1, LP
        EW(I) = 0.0D0
   40 CONTINUE

CX    WRITE(6,*) ' ------  SMINTP --------- '
      IF(LP.EQ.1) THEN
        DO 30 I = 1, 4
          EP(I,1) = PI(I)
   30   CONTINUE
        EW(1) = 1.0D0
        NP    = 1
        END IF

        APP=ABS(VM)
        RAPP = SQRT(APP)

*     COVARIANT GAUGE
        PT2 = PI(1)**2 + PI(2)**2
        IF(PT2.LE.DD) THEN
          PT  = 0.0D0
          RTY = 1.0D0
          RTX = 0.0D0
        ELSE
          PT  = SQRT(PT2)
          PTI = 1.0D0/PT
          RTY = PI(2)*PTI
          RTX = PI(1)*PTI
        ENDIF
        PN2 = PT2 + PI(3)**2
CX      PP = (PI(4)-PI(3))*(PI(4)+PI(3))-PT2
        IF(PN2.LE.DD) THEN
          PN  = 0.0D0
          RNX = 0.0D0
          RNY = 0.0D0
          RNZ = 1.0D0
          RTN = 0.0D0
        ELSE IF(PN2.GT.DD) THEN
          PN  = SQRT(PN2)
          PNI = 1.0D0/PN
          RNX = PI(1)*PNI
          RNY = PI(2)*PNI
          RNZ = PI(3)*PNI
          RTN = PT*PNI
        ENDIF

        EP(1,1) =  RTX*RNZ/RAPP
        EP(2,1) =  RTY*RNZ/RAPP
        EP(3,1) = -RTN/RAPP
        EP(4,1) =  0.0D0
        EW(1) =  1.0D0

        EP(1,2) = -RTY/RAPP
        EP(2,2) =  RTX/RAPP
        EP(3,2) =  0.0D0
        EP(4,2) =  0.0D0
        EW(2) =  1.0D0
        NP = 2

        WRITE(6,*) 'PI(1) PI(2) ',PI(1),PI(2)
        WRITE(6,*) 'PT PTI RTY  ',PT,PTI,RTY
        WRITE(6,*) 'EP          ',EP(1,2)

        IF(LP.GE.3) THEN
          IF (APP.GT.DD) THEN
            NP = NP + 1
            RPPI  = 1.0D0/SQRT(APP)
            EP(1,NP) = 0.D0
            EP(2,NP) = 0.D0
            EP(3,NP) = 0.D0
CX          EP(4,NP) = -RAPP/PN
            EP(4,NP) = -1.D0/PN
            EW(NP) = SIGN(1.0D0, VM)
            WRITE(6,*) ' NP APP EP ',NP,APP,EP(4,NP)
          ENDIF
        ENDIF

        IF(LP.GE.4) THEN
          IF (APP.GT.DD) THEN
            NP = NP + 1
            RPPI  = 1.0D0/SQRT(APP)
            EP(1,NP) = 0.D0
            EP(2,NP) = 0.D0
            EP(3,NP) = 0.D0
            EP(4,NP) = 0.D0
            EW(NP) = 0.D0
            AM2 = AM*AM
         END IF
       END IF

        WRITE(6,*) 'NP EP  ',NP,EP(1,2)
*     CALL CTIME('SMINTV')
      RETURN
      END
************************************************************************
      SUBROUTINE SMEXTF(IO, AM, PE, PS, CE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
      INTEGER    IO
*     DOUBLE PRECISION     AM
      DIMENSION  PE(4), PS(4,2)
      DOUBLE COMPLEX CE(2, 2)
*
*   Decompose momentum of internal fermion.
*
*         PE = L1 + L2
*         L1**2 = 0
*         L2**2 = 0
*         PS(*,1) = L1
*         PS(*,2) = L2
*
*     IO : input  : 1: imcoming, u or v    2: outgoing, ub or vb
*     AM : input  : mass of the fermion.
*     PE : input  : momentum of the fermion.
*     PS : output : decomposed momenta.
*     CE : output : phase factor, only CE(*, IO) is used.
*
*-----------------------------------------------------------------------
      CALL SPLTQ(AM, PE, PS(1,2), PS(1,1))
      CALL PHASEQ(IO, PE, CE(1,IO))
*     CALL CTIME('SMEXTF')
      RETURN
      END
************************************************************************
      SUBROUTINE SMGGG(L1,L2,L3,K1,K2,K3,CPL,P1,P2,P3,E1,E2,E3,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      INTEGER    L1, L2, L3
      INTEGER    K1, K2, K3
      DOUBLE COMPLEX CPL
      DIMENSION  P1(4),    P2(4),    P3(4)
      DIMENSION  E1(4,L1), E2(4,L2), E3(4,L3)
      DOUBLE COMPLEX AV(0:LASIZE)
*     DOUBLE COMPLEX AV(0:L3*L2*L1-1)
      INTEGER    LT(0:LTSIZE)
*
*    Calculate gluon-gluon-gluon vertex.
*
*           ! 3
*           V
*      -->--+--<---
*        2     1
*
*     L1,L2,L3 : input  : number of polarization vectors (2, 3 OR 4)
*     K1,K2,K3 : input  : if incoming momentum then 1 else -1
*     CPL      : input  : coupling constant.
*     P1,P2,P3 : input  : momentum vectors
*     E1,E2,E3 : input  : set of polarization vectors
*     AV       : output : table of amplitudes
*     LT       : output : table of sizes in AV
*
      DIMENSION  PP1(4), PP2(4), PP3(4)
*-----------------------------------------------------------------------
      LT(0) = 3
      LT(1) = L1
      LT(2) = L2
      LT(3) = L3
      DO 10 J = 1, 4
        PP1(J) = K1*P1(J)
        PP2(J) = K2*P2(J)
        PP3(J) = K3*P3(J)
   10 CONTINUE
      IA = 0
      DO 500 IL3 = 1, L3
        E3P  =  E3(4,IL3)*(PP1(4)-PP2(4)) - E3(1,IL3)*(PP1(1)-PP2(1))
     &        - E3(2,IL3)*(PP1(2)-PP2(2)) - E3(3,IL3)*(PP1(3)-PP2(3))
      DO 500 IL2 = 1, L2
        E2P  =  E2(4,IL2)*(PP3(4)-PP1(4)) - E2(1,IL2)*(PP3(1)-PP1(1))
     &        - E2(2,IL2)*(PP3(2)-PP1(2)) - E2(3,IL2)*(PP3(3)-PP1(3))
        E2E3 =  E2(4,IL2)*E3(4,IL3) - E2(1,IL2)*E3(1,IL3)
     &        - E2(2,IL2)*E3(2,IL3) - E2(3,IL2)*E3(3,IL3)
      DO 500 IL1 = 1, L1
        E1E2 =  E1(4,IL1)*E2(4,IL2) - E1(1,IL1)*E2(1,IL2)
     &        - E1(2,IL1)*E2(2,IL2) - E1(3,IL1)*E2(3,IL2)
        E1E3 =  E1(4,IL1)*E3(4,IL3) - E1(1,IL1)*E3(1,IL3)
     &        - E1(2,IL1)*E3(2,IL3) - E1(3,IL1)*E3(3,IL3)
        E1P  =  E1(4,IL1)*(PP2(4)-PP3(4)) - E1(1,IL1)*(PP2(1)-PP3(1))
     &        - E1(2,IL1)*(PP2(2)-PP3(2)) - E1(3,IL1)*(PP2(3)-PP3(3))
        AV(IA) =       CPL*(E1P*E2E3 + E2P*E1E3 + E3P*E1E2)
        IA = IA + 1
  500 CONTINUE
*     CALL CTIME('SMGGG ')
      RETURN
      END
************************************************************************
      SUBROUTINE SMGGGG(L1,L2,L3,L4,CPL,E1,E2,E3,E4,LT,AV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      INTEGER    L1, L2, L3, L4
      DOUBLE COMPLEX CPL
      DIMENSION  E1(4,L1), E2(4,L2), E3(4,L3), E4(4,L4)
      DOUBLE COMPLEX AV(0:LASIZE)
*     DOUBLE COMPLEX AV(0:L2*L1-1)
      INTEGER    LT(0:LTSIZE)
*
*    Calculate 4-gluon vertex.
*
*           ! 4
*           V            -cpl*(g(1,3)*g(2,4) - g(1,4)*g(2,3))
*      -->--+--<---
*        1  ^  3
*           !2
*
*     L1,L2,L3,L4 : input  : number of polarization vectors (2, 3 OR 4)
*     CPL         : input  : coupling constant.
*     E1,E2,E3,E4 : input  : set of polarization vectors
*     AV          : output : table of amplitudes
*     LT          : output : table of sizes in AV
*-----------------------------------------------------------------------
      LT(0) = 4
      LT(1) = L1
      LT(2) = L2
      LT(3) = L3
      LT(4) = L4
      IA = 0
      DO 500 IL4 = 1, L4
      DO 500 IL3 = 1, L3
      DO 500 IL2 = 1, L2
        E2E4 =  E2(4,IL2)*E4(4,IL4) - E2(1,IL2)*E4(1,IL4)
     &        - E2(2,IL2)*E4(2,IL4) - E2(3,IL2)*E4(3,IL4)
        E2E3 =  E2(4,IL2)*E3(4,IL3) - E2(1,IL2)*E3(1,IL3)
     &        - E2(2,IL2)*E3(2,IL3) - E2(3,IL2)*E3(3,IL3)
      DO 500 IL1 = 1, L1
        E1E4 =  E1(4,IL1)*E4(4,IL4) - E1(1,IL1)*E4(1,IL4)
     &        - E1(2,IL1)*E4(2,IL4) - E1(3,IL1)*E4(3,IL4)
        E1E3 =  E1(4,IL1)*E3(4,IL3) - E1(1,IL1)*E3(1,IL3)
     &        - E1(2,IL1)*E3(2,IL3) - E1(3,IL1)*E3(3,IL3)
        AV(IA) = CPL*(E1E3*E2E4 - E1E4*E2E3)
        IA = IA + 1
  500 CONTINUE
*     CALL CTIME('SMGGGG')
      RETURN
      END
************************************************************************
      SUBROUTINE SMPRPD(APROP, AMOMQ, AMASSQ, AMAG)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE COMPLEX APROP
      DOUBLE PRECISION     AMOMQ, AMASSQ, AMAG
! This common can be everywhere, contains various switches
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      save
*
*    Calculate denominator of propagator.
*
*     APROP  : in/out : product of denominator of propagators.
*     AMOMP  : input  : square of mementum.
*     AMASSQ : input  : square of mass.
*     AMAG   : input  : mass * width.
*-----------------------------------------------------------------------
      KeyWu  = MOD(KeyPhy,1000000)/100000
      KeyZet = MOD(KeyPhy,1000)/100
      if (keywu.ne.keyzet) write(*,*) 'I do not like key-wu-zet; smprpd'
      if (keywu.ne.keyzet) stop
      IF (AMOMQ .GT. 0) THEN
       IF ((AMASSQ.GT.100D0).and.(keywu.ne.1)) THEN
        APROP = - APROP*DCMPLX(AMOMQ - AMASSQ,amomq/amassq* AMAG)
       ELSE
        APROP = - APROP*DCMPLX(AMOMQ - AMASSQ, AMAG)
       ENDIF
      ELSE
        APROP = - APROP*DCMPLX(AMOMQ - AMASSQ, 0.0D0)
!        APROP = - APROP*DCMPLX(AMOMQ - AMASSQ, AMAG)
      ENDIF
 
*     CALL CTIME('SMPRPD')
      RETURN
      END
************************************************************************
C
C      ************************************************************
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *             ============================                 *
C      *                CHANEL   (Version. 1.1)                   *
C      *             ============================                 *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                   Written by H. Tanaka                   *
C      *                                                          *
C      *                                                          *
C      *                        ---------                         *
C      *                        Reference                         *
C      *                        ---------                         *
C      *    ' Numerical Calculation of Helicity Amplitudes        *
C      *      for Processes Involving Massive Fermions'           *
C      *                                                          *
C      *      H. Tanaka, Hiroshima University preprint            *
C      *         Accepted to Comput. Phys. Commun.                *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      ************************************************************
C
C
C
C
C
C ******************************************************************
C *                                                                *
C *  SUBROUTINE POL(I:I*4, AM:R*8, P:R*8(4), EP:R*8(4), EM:R*8(4)) *
C *                                                                *
C *     Purpose: To set components of polarization vectors.        *
C *                                                                *
C *     I= : Polarization states   I=1,2 : Transverse              *
C *                                I=3   : Longitudinal            *
C *                                I=4   : Scalar                  *
C *                                                                *
C *     AM= : Mass of vector boson                                 *
C *     P = : Momentum of vector boson                             *
C *     EP= : Polarization vector for state I                      *
C *     EM= : Weight factors                                       *
C *                                                                *
C *                                                                *
C *                                                                *
C *     Note                                                       *
C *     ----                                                       *
C *                                                                *
C *     For summation of the polarization vectors I=1 to 4,        *
C *     following gauges are chosen :                              *
C *                                                                *
C *     Unitary gauge : for massive vector boson                   *
C *     Feynman gauge : for massless vector boson                  *
C *                                                                *
C *                                                                *
C *                                    written by H. Tanaka        *
C ******************************************************************
C
C
C              ============================
               SUBROUTINE POL(I,AM,P,EP,EM)
C              ============================
C
C
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        PARAMETER (ONE = 1.0)
        DIMENSION P(4),EP(4),EM(4)
        DATA DD/0.D0/
 
        PT2 = P(1)**2 + P(2)**2
        PP  = (P(4) - P(3))*(P(4) + P(3)) - PT2
        IF (PT2.LE.DD) THEN
          PT  = 0.0D0
          RTY = 1.0D0
          RTX = 0.0D0
        ELSE
          PT  = SQRT(PT2)
          PTI = 1.0D0/PT
          RTY = P(2)*PTI
          RTX = P(1)*PTI
        END IF
        PN2 = PT2 + P(3)**2
        IF (PN2.LE.DD) THEN
          PN  = 0.0D0
          RNX = 0.0D0
          RNY = 0.0D0
          RNZ = 1.0D0
          RTN = 0.0D0
        ELSE IF (PN2.GT.DD) THEN
          PN  = SQRT(PN2)
          PNI = 1.0D0/PN
          RNX = P(1)*PNI
          RNY = P(2)*PNI
          RNZ = P(3)*PNI
          RTN = PT*PNI
        END IF
        IF (I.EQ.1) THEN
          EP(1) =  RTX*RNZ
          EP(2) =  RTY*RNZ
          EP(3) = -RTN
          EP(4) =  0.0D0
          EM(1) =  1.0D0
        ELSE IF (I.EQ.2) THEN
          EP(1) = -RTY
          EP(2) =  RTX
          EP(3) =  0.0D0
          EP(4) =  0.0D0
          EM(2) =  1.0D0
        ELSE IF (I.EQ.3) THEN
          APP=ABS(PP)
          IF (APP.LE.DD) THEN
            EP(1) = 0.0D0
            EP(2) = 0.0D0
            EP(3) = 0.0D0
            EP(4) = 0.0D0
            EM(3) = 0.0D0
          ELSE
            RPPI  = 1.0D0/SQRT(APP)
            EP(1) = RNX*P(4)*RPPI
            EP(2) = RNY*P(4)*RPPI
            EP(3) = RNZ*P(4)*RPPI
            EP(4) = PN*RPPI
            EM(3) = SIGN(1.0D0, PP)
          ENDIF
        ELSE IF (I.EQ.4) THEN
          APP = ABS(PP)
          IF (APP.LE.DD) THEN
            EP(1) = 0.D0
            EP(2) = 0.D0
            EP(3) = 0.D0
            EP(4) = 0.D0
            EM(4) = 0.0D0
          ELSE
            RPPI  = 1.0D0/SQRT(APP)
            EP(1) = P(1)*RPPI
            EP(2) = P(2)*RPPI
            EP(3) = P(3)*RPPI
            EP(4) = P(4)*RPPI
            IF (AM.GT.0.D0) THEN
              EM(4)=  SIGN(ONE, PP)*(PP/AM**2-1.0D0)
            ELSE
              EM(4)= -SIGN(ONE, PP)
            END IF
          ENDIF
        END IF
        RETURN
      END
C
C ****************************************************************
C *                                                              *
C *                                                              *
C *   SUBROUTINE SPLTQ(AM:R*8, P:R*8(4), P2:R*8(4), P1:R*8(4))   *
C *                                                              *
C * Purpose: To decompose momentum of massive fermions to two    *
C *          light-like vectors                                  *
C *                                                              *
C *                                                              *
C *                                                              *
C *                                                              *
C *      AM= : Mass of fermion.                                  *
C *      P = : Momentum of fermion.                              *
C *      P1,P2= : Decomposed light-like vectors.                 *
C *                                                              *
C *                                                              *
C *                                    written by H. Tanaka      *
C ****************************************************************
C
C
C               ============================
                SUBROUTINE SPLTQ(AM,P,P2,P1)
C               ============================
C
C
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        DIMENSION P(4),P1(4),P2(4)
        DATA DD/0.0D0/
 
        PT1 = P(1)
        PT2 = P(2)
        PT3 = P(3)
        PN  = SQRT(PT1**2 + PT2**2 + PT3**2)
        P10 = (P(4) + PN)*0.5D0
        P20 = AM**2/(4.0*P10)
        IF (PN.LE.DD) THEN
 
          P10   =  AM*0.5D0
          P20   =  P10
 
          P1(1) =  0.D0
          P1(2) =  P10
          P1(3) =  0.D0
          P1(4) =  P10
 
          P2(1) =  0.D0
          P2(2) = -P20
          P2(3) =  0.D0
          P2(4) =  P20
 
        ELSE
 
          ANX=P(1)/PN
          ANY=P(2)/PN
          ANZ=P(3)/PN
 
          P1(1) =  P10*ANX
          P1(2) =  P10*ANY
          P1(3) =  P10*ANZ
          P1(4) =  P10
 
          P2(1) = -P20*ANX
          P2(2) = -P20*ANY
          P2(3) = -P20*ANZ
          P2(4) =  P20
 
        ENDIF
 
        RETURN
      END
C ********************************************************************
C *                                                                  *
C *                                                                  *
C *                                                                  *
C *   SUBROUTINE PHASEQ(I:I*4, P:R*8(4), C:C*16(2))                  *
C *                                                                  *
C *                                                                  *
C *                                                                  *
C *   Purpose: To calculate phase factors for massive fermion.       *
C *                                                                  *
C *                                                                  *
C *                                                                  *
C *         I=: I=1 for complex phase C                              *
C *             I=2 for conjugate of phase C                         *
C *         P=: Momuntum of massive fermion.                         *
C *         C=: Calculated phase factors.                            *
C *                                            written by H. Tanaka  *
C ********************************************************************
C
C
C               ========================
                SUBROUTINE PHASEQ(I,P,C)
C               ========================
C
C
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        PARAMETER (ONE = 1.0)
 
        DOUBLE COMPLEX C
        DIMENSION P(4),C(2)
 
        PTN=DSQRT(P(2)**2+P(3)**2)
        IF (PTN.LE.0.D0) THEN
          PYN=SIGN(ONE, P(1))
          PZN=0.0D0
        ELSE
          PYN=P(2)/PTN
          PZN=P(3)/PTN
        ENDIF
        IF (I.EQ.2) PZN=-PZN
        C(1)=DCMPLX(-PYN,PZN)
        C(2)=DCMPLX( PYN,PZN)
        RETURN
      END
*-----------------------------------------------------------------------
*     FFV  revised 94/04/08 by T.Ishikawa
*     AAM !=0 , AM != 0
*-----------------------------------------------------------------------
      SUBROUTINE FFVMM1(LIND,II,IO,AL,AR,CC,C,Q1,Q2,P1,P2,Q,AALL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ONE = 1.0D0)
      DOUBLE COMPLEX AALL
      DIMENSION AALL(4,2,2)
      DOUBLE COMPLEX C,CC
      DIMENSION Q(4),P1(4),P2(4),Q1(4)
      DIMENSION Q2(4),C(2),CC(2)
      INTEGER II,IO
      COMMON /CHWORK/
     .        ALL11R,ALL11I,ALL12R,ALL12I,ALL21R,ALL21I,ALL22R,ALL22I,
     .        R,RR,
     .        Q1RPP1,Q1R1Y,Q1R1Z,Q2RPP1,Q2R1Y,Q2R1Z,
     .        P1RPP2,P1R2Y,P1R2Z,P2RPP2,P2R2Y,P2R2Z,
     .        J1,J2,K1,K2,
     .        LQ1,LQ2,LP1,LP2
      LOGICAL    LQ1,LQ2,LP1,LP2

*------------------------ Entry point ----------------------------------
         ALL11R = 0.0D0
         ALL11I = 0.0D0
         ALL12R = 0.0D0
         ALL12I = 0.0D0
         ALL21R = 0.0D0
         ALL21I = 0.0D0
         ALL22R = 0.0D0
         ALL22I = 0.0D0

      DO 90 I3= 1 , 2
      DO 90 I2= 1 , 2
         AALL(LIND,I2,I3)=(0.0D0,0.0D0)
   90 CONTINUE

      IF( LIND .EQ. 1 ) THEN
          LQ1      = .TRUE.
          LQ2      = .TRUE.
          LP1      = .TRUE.
          LP2      = .TRUE.
          IF( Q1(4) .LE. 0.0D0 ) LQ1 = .FALSE.
          IF( Q2(4) .LE. 0.0D0 ) LQ2 = .FALSE.
          IF( P1(4) .LE. 0.0D0 ) LP1 = .FALSE.
          IF( P2(4) .LE. 0.0D0 ) LP2 = .FALSE.
C-
          IF( LQ1 ) THEN
               PT1        = SQRT(Q1(2)*Q1(2) + Q1(3)*Q1(3))
               IF( PT1 .LE. 0.0D0) THEN
                   Q1RR1Y    = SIGN(ONE, Q1(1))
                   Q1RR1Z    = 0.0D0
               ELSE
                   Q1RR1Y    = Q1(2)/PT1
                   Q1RR1Z    = Q1(3)/PT1
               ENDIF
               IF( Q1(1).GE.0.D0) THEN
                   PS1       = SQRT(Q1(4) + Q1(1))
                   Q1RPP1    = PT1/PS1
               ELSE
                   Q1RPP1    = SQRT(Q1(4) - Q1(1))
                   PS1       = PT1/Q1RPP1
               ENDIF
               Q1R1Y    = Q1RR1Y*PS1
               Q1R1Z    = Q1RR1Z*PS1
           ENDIF

           IF( LQ2 ) THEN
               PT1        = SQRT(Q2(2)*Q2(2) + Q2(3)*Q2(3))
               IF( PT1 .LE. 0.0D0) THEN
                   Q2RR1Y    = SIGN(ONE, Q2(1))
                   Q2RR1Z    = 0.0D0
               ELSE
                   Q2RR1Y    = Q2(2)/PT1
                   Q2RR1Z    = Q2(3)/PT1
               ENDIF
               IF( Q2(1).GE.0.D0) THEN
                   PS1       = SQRT(Q2(4) + Q2(1))
                   Q2RPP1    = PT1/PS1
               ELSE
                   Q2RPP1    = SQRT(Q2(4) - Q2(1))
                   PS1       = PT1/Q2RPP1
               ENDIF
               Q2R1Y    = Q2RR1Y*PS1
               Q2R1Z    = Q2RR1Z*PS1
           ENDIF
C-
           IF( LP1 ) THEN
               PT2     = SQRT(P1(2)*P1(2) + P1(3)*P1(3))
               IF( PT2 .LE. 0.0D0) THEN
                   P1RR2Y    = SIGN(ONE, P1(1))
                   P1RR2Z    = 0.0D0
               ELSE
                   P1RR2Y    = P1(2)/PT2
                   P1RR2Z    = P1(3)/PT2
               ENDIF
               IF( P1(1) .GE. 0.D0) THEN
                   PS2       = SQRT(P1(4) + P1(1))
                   P1RPP2    = PT2/PS2
               ELSE
                   P1RPP2    = SQRT(P1(4) - P1(1))
                   PS2       = PT2/P1RPP2
               ENDIF
               P1R2Y    = P1RR2Y*PS2
               P1R2Z    = P1RR2Z*PS2
           ENDIF

           IF( LP2 ) THEN
               PT2     = SQRT(P2(2)*P2(2) + P2(3)*P2(3))
               IF( PT2 .LE. 0.0D0) THEN
                   P2RR2Y    = SIGN(ONE, P2(1))
                   P2RR2Z    = 0.0D0
               ELSE
                   P2RR2Y    = P2(2)/PT2
                   P2RR2Z    = P2(3)/PT2
               ENDIF
               IF( P2(1) .GE. 0.D0) THEN
                   PS2       = SQRT(P2(4) + P2(1))
                   P2RPP2    = PT2/PS2
               ELSE
                   P2RPP2    = SQRT(P2(4) - P2(1))
                   PS2       = PT2/P2RPP2
               ENDIF
               P2R2Y    = P2RR2Y*PS2
               P2R2Z    = P2RR2Z*PS2
           ENDIF

           R     = DBLE(IO - 2)
           RR    = DBLE(II - 2)

C
           J1    = (5-II)/2
           J2    = 3 - J1
           K1    = (5-IO)/2
           K2    = 3 - K1

      ENDIF
C- Q1,P1
           IF( LQ1.AND.LP1 ) THEN
               ALL11R = Q1RPP1*P1RPP2*(Q(4)+Q(1))
     .                 -Q1RPP1*(Q(2)*P1R2Y+Q(3)*P1R2Z)
     .                 -P1RPP2*(Q(2)*Q1R1Y+Q(3)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Y*P1R2Y+Q1R1Z*P1R2Z)
               ALL11I = Q1RPP1*(Q(2)*P1R2Z-Q(3)*P1R2Y)
     .                 +P1RPP2*(Q(3)*Q1R1Y-Q(2)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Z*P1R2Y-Q1R1Y*P1R2Z)
           ENDIF
C- Q2,P2
           IF( LQ2.AND.LP2 ) THEN
               ALL22R = Q2RPP1*P2RPP2*(Q(4)+Q(1))
     .                 -Q2RPP1*(Q(2)*P2R2Y+Q(3)*P2R2Z)
     .                 -P2RPP2*(Q(2)*Q2R1Y+Q(3)*Q2R1Z)
     .                 +(Q(4)-Q(1))*(Q2R1Y*P2R2Y+Q2R1Z*P2R2Z)
               ALL22I = Q2RPP1*(Q(2)*P2R2Z-Q(3)*P2R2Y)
     .                 +P2RPP2*(Q(3)*Q2R1Y-Q(2)*Q2R1Z)
     .                 +(Q(4)-Q(1))*(Q2R1Z*P2R2Y-Q2R1Y*P2R2Z)
           ENDIF
C- Q1,P2
           IF( LQ1.AND.LP2 ) THEN
               ALL12R = Q1RPP1*P2RPP2*(Q(4)+Q(1))
     .                 -Q1RPP1*(Q(2)*P2R2Y+Q(3)*P2R2Z)
     .                 -P2RPP2*(Q(2)*Q1R1Y+Q(3)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Y*P2R2Y+Q1R1Z*P2R2Z)
               ALL12I = Q1RPP1*(Q(2)*P2R2Z-Q(3)*P2R2Y)
     .                 +P2RPP2*(Q(3)*Q1R1Y-Q(2)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Z*P2R2Y-Q1R1Y*P2R2Z)
           ENDIF
C- Q2,P1
           IF( LQ2.AND.LP1 ) THEN
               ALL21R = Q2RPP1*P1RPP2*(Q(4)+Q(1))
     .                 -Q2RPP1*(Q(2)*P1R2Y+Q(3)*P1R2Z)
     .                 -P1RPP2*(Q(2)*Q2R1Y+Q(3)*Q2R1Z)
     .                 +(Q(4)-Q(1))*(Q2R1Y*P1R2Y+Q2R1Z*P1R2Z)
               ALL21I = Q2RPP1*(Q(2)*P1R2Z-Q(3)*P1R2Y)
     .                 +P1RPP2*(Q(3)*Q2R1Y-Q(2)*Q2R1Z)
     .                 +(Q(4)-Q(1))*(Q2R1Z*P1R2Y-Q2R1Y*P1R2Z)
           ENDIF
C---
           AALL(LIND,J1,K1)= AL*DCMPLX(ALL11R,-ALL11I)
     &                 +(AR*R*RR*C(2)*CC(2)*DCMPLX(ALL22R, ALL22I))
           AALL(LIND,J2,K2)= AR*DCMPLX(ALL11R, ALL11I)
     &                 +(AL*R*RR*C(1)*CC(1)*DCMPLX(ALL22R,-ALL22I))
           AALL(LIND,J1,K2)= -(AR*RR*CC(2)*DCMPLX(ALL21R, ALL21I))
     &                 -(AL*R*C(1)*DCMPLX(ALL12R,-ALL12I))
           AALL(LIND,J2,K1)= -(AL*RR*CC(1)*DCMPLX(ALL21R,-ALL21I))
     &                 -(AR*R*C(2)*DCMPLX(ALL12R, ALL12I))

        RETURN
      END
C   08/03/94 403291234  MEMBER NAME  FFV0M    *.FORT        E2FORT
      SUBROUTINE FFVMM2(LIND,AL,AR,C,Q1,Q,AALL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ONE = 1.0D0)
      DOUBLE COMPLEX AALL
      DIMENSION  AALL(4,2,2)
      DOUBLE COMPLEX C
      DIMENSION  Q(4),Q1(4)
      DIMENSION  C(2)
      COMMON /CHWORK/
     .        ALL11R,ALL11I,ALL12R,ALL12I,ALL21R,ALL21I,ALL22R,ALL22I,
     .        R,RR,
     .        Q1RPP1,Q1R1Y,Q1R1Z,Q2RPP1,Q2R1Y,Q2R1Z,
     .        P1RPP2,P1R2Y,P1R2Z,P2RPP2,P2R2Y,P2R2Z,
     .        J1,J2,K1,K2,
     .        LQ1,LQ2,LP1,LP2
      LOGICAL    LQ1,LQ2,LP1,LP2
      COMMON /WRKFFX/P13RPP,P13R1Y,P13R1Z,P23RPP,P23R2Y,P23R2Z,LP13,LP23
      LOGICAL    LP13,LP23

*------------------------ Entry point ----------------------------------
         ALL11R = 0.0D0
         ALL11I = 0.0D0
         ALL12R = 0.0D0
         ALL12I = 0.0D0
         ALL21R = 0.0D0
         ALL21I = 0.0D0
         ALL22R = 0.0D0
         ALL22I = 0.0D0

      DO 90 I3= 1 , 2
      DO 90 I2= 1 , 2
         AALL(LIND,I2,I3)=(0.0D0,0.0D0)
   90 CONTINUE

      IF( LIND .EQ. 1 ) THEN
          LP13   = .TRUE.
          IF( Q1(4) .LE. 0.0D0 ) LP13 = .FALSE.
C-
          IF( LP13 ) THEN
              PT1        = SQRT(Q1(2)*Q1(2) + Q1(3)*Q1(3))
              IF( PT1 .LE. 0.0D0) THEN
                  Q1RR1Y    = SIGN(ONE, Q1(1))
                  Q1RR1Z    = 0.0D0
              ELSE
                   Q1RR1Y    = Q1(2)/PT1
                   Q1RR1Z    = Q1(3)/PT1
              ENDIF
              IF( Q1(1).GE.0.D0) THEN
                  PS1       = SQRT(Q1(4) + Q1(1))
                  P13RPP    = PT1/PS1
              ELSE
                  P13RPP    = SQRT(Q1(4) - Q1(1))
                  PS1       = PT1/P13RPP
              ENDIF
              P13R1Y    = Q1RR1Y*PS1
              P13R1Z    = Q1RR1Z*PS1
          ENDIF
      ENDIF
C- Q1,P1
           IF( LP13.AND.LP1 ) THEN
               ALL11R = P13RPP*P1RPP2*(Q(4)+Q(1))
     .                 -P13RPP*(Q(2)*P1R2Y +Q(3)*P1R2Z)
     .                 -P1RPP2*(Q(2)*P13R1Y+Q(3)*P13R1Z)
     .                 +(Q(4)-Q(1))*(P13R1Y*P1R2Y+P13R1Z*P1R2Z)
               ALL11I = P13RPP*(Q(2)*P1R2Z -Q(3)*P1R2Y)
     .                 +P1RPP2*(Q(3)*P13R1Y-Q(2)*P13R1Z)
     .                 +(Q(4)-Q(1))*(P13R1Z*P1R2Y-P13R1Y*P1R2Z)
           ENDIF
C- Q1,P2
           IF( LP13.AND.LP2 ) THEN
               ALL12R = P13RPP*P2RPP2*(Q(4)+Q(1))
     .                 -P13RPP*(Q(2)*P2R2Y +Q(3)*P2R2Z)
     .                 -P2RPP2*(Q(2)*P13R1Y+Q(3)*P13R1Z)
     .                 +(Q(4)-Q(1))*(P13R1Y*P2R2Y+P13R1Z*P2R2Z)
               ALL12I = P13RPP*(Q(2)*P2R2Z -Q(3)*P2R2Y)
     .                 +P2RPP2*(Q(3)*P13R1Y-Q(2)*P13R1Z)
     .                 +(Q(4)-Q(1))*(P13R1Z*P2R2Y-P13R1Y*P2R2Z)
           ENDIF

           AALL(LIND,1,K1)= AL*DCMPLX(ALL11R,-ALL11I)
           AALL(LIND,2,K2)= AR*DCMPLX(ALL11R, ALL11I)
           AALL(LIND,1,K2)= -AL*R*C(1)*DCMPLX(ALL12R,-ALL12I)
           AALL(LIND,2,K1)= -AR*R*C(2)*DCMPLX(ALL12R, ALL12I)

        RETURN
      END
        SUBROUTINE FFVMM3(LIND,AL,AR,CC,P1,Q,AALL)
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        PARAMETER (ONE = 1.0D0)
        DOUBLE COMPLEX AALL
        DIMENSION  AALL(4,2,2)
        DOUBLE COMPLEX CC
        DIMENSION  Q(4),P1(4)
        DIMENSION  CC(2)
        COMMON /CHWORK/
     .        ALL11R,ALL11I,ALL12R,ALL12I,ALL21R,ALL21I,ALL22R,ALL22I,
     .        R,RR,
     .        Q1RPP1,Q1R1Y,Q1R1Z,Q2RPP1,Q2R1Y,Q2R1Z,
     .        P1RPP2,P1R2Y,P1R2Z,P2RPP2,P2R2Y,P2R2Z,
     .        J1,J2,K1,K2,
     .        LQ1,LQ2,LP1,LP2
      LOGICAL    LQ1,LQ2,LP1,LP2
C
      COMMON /WRKFFX/P13RPP,P13R1Y,P13R1Z,P23RPP,P23R2Y,P23R2Z,LP13,LP23
      LOGICAL    LP13,LP23

*------------------------ Entry point ----------------------------------
         ALL11R = 0.0D0
         ALL11I = 0.0D0
         ALL12R = 0.0D0
         ALL12I = 0.0D0
         ALL21R = 0.0D0
         ALL21I = 0.0D0
         ALL22R = 0.0D0
         ALL22I = 0.0D0

      DO 90 I3= 1 , 2
      DO 90 I2= 1 , 2
         AALL(LIND,I2,I3)=(0.0D0,0.0D0)
   90 CONTINUE
      IF( LIND .EQ. 1 ) THEN
           LP23      = .TRUE.
           IF( P1(4) .LE. 0.0D0 ) LP23 = .FALSE.
C-
           IF( LP23 ) THEN
               PT2     = SQRT(P1(2)*P1(2) + P1(3)*P1(3))
               IF( PT2 .LE. 0.0D0) THEN
                   P1RR2Y    = SIGN(ONE, P1(1))
                   P1RR2Z    = 0.0D0
               ELSE
                   P1RR2Y    = P1(2)/PT2
                   P1RR2Z    = P1(3)/PT2
               ENDIF
               IF( P1(1) .GE. 0.D0) THEN
                   PS2       = SQRT(P1(4) + P1(1))
                   P23RPP    = PT2/PS2
               ELSE
                   P23RPP    = SQRT(P1(4) - P1(1))
                   PS2       = PT2/P23RPP
               ENDIF
               P23R2Y    = P1RR2Y*PS2
               P23R2Z    = P1RR2Z*PS2
           ENDIF
      ENDIF
C- Q1,P1
           IF( LQ1.AND.LP23 ) THEN
               ALL11R = Q1RPP1*P23RPP*(Q(4)+Q(1))
     .                 -Q1RPP1*(Q(2)*P23R2Y+Q(3)*P23R2Z)
     .                 -P23RPP*(Q(2)*Q1R1Y +Q(3)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Y*P23R2Y+Q1R1Z*P23R2Z)
               ALL11I = Q1RPP1*(Q(2)*P23R2Z-Q(3)*P23R2Y)
     .                 +P23RPP*(Q(3)*Q1R1Y -Q(2)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Z*P23R2Y-Q1R1Y*P23R2Z)
           ENDIF
C- Q2,P1
           IF( LQ2.AND.LP23 ) THEN
               ALL21R = Q2RPP1*P23RPP*(Q(4)+Q(1))
     .                 -Q2RPP1*(Q(2)*P23R2Y+Q(3)*P23R2Z)
     .                 -P23RPP*(Q(2)*Q2R1Y +Q(3)*Q2R1Z)
     .                 +(Q(4)-Q(1))*(Q2R1Y*P23R2Y+Q2R1Z*P23R2Z)
               ALL21I = Q2RPP1*(Q(2)*P23R2Z-Q(3)*P23R2Y)
     .                 +P23RPP*(Q(3)*Q2R1Y-Q(2)*Q2R1Z)
     .                 +(Q(4)-Q(1))*(Q2R1Z*P23R2Y-Q2R1Y*P23R2Z)
           ENDIF
C---

           AALL(LIND,J1,1) = AL*DCMPLX(ALL11R,-ALL11I)
           AALL(LIND,J2,2) = AR*DCMPLX(ALL11R, ALL11I)
           AALL(LIND,J1,2) = -AR*RR*CC(2)*DCMPLX(ALL21R, ALL21I)
           AALL(LIND,J2,1) = -AL*RR*CC(1)*DCMPLX(ALL21R,-ALL21I)

      RETURN
      END
      SUBROUTINE FFVMM4(LIND,AL,AR,Q,AALL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE COMPLEX AALL
      DIMENSION AALL(4,2,2)
      DIMENSION Q(4)
      COMMON /CHWORK/
     .        ALL11R,ALL11I,ALL12R,ALL12I,ALL21R,ALL21I,ALL22R,ALL22I,
     .        R,RR,
     .        Q1RPP1,Q1R1Y,Q1R1Z,Q2RPP1,Q2R1Y,Q2R1Z,
     .        P1RPP2,P1R2Y,P1R2Z,P2RPP2,P2R2Y,P2R2Z,
     .        J1,J2,K1,K2,
     .        LQ1,LQ2,LP1,LP2
      LOGICAL    LQ1,LQ2,LP1,LP2
      COMMON /WRKFFX/P13RPP,P13R1Y,P13R1Z,P23RPP,P23R2Y,P23R2Z,LP13,LP23
      LOGICAL    LP13,LP23

*------------------------ Entry point ----------------------------------
         ALL11R = 0.0D0
         ALL11I = 0.0D0

      DO 90 I3= 1 , 2
      DO 90 I2= 1 , 2
         AALL(LIND,I2,I3)=(0.0D0,0.0D0)
   90 CONTINUE
C- Q1,P1
           IF( LP13.AND.LP23 ) THEN
               ALL11R = P13RPP*P23RPP*(Q(4)+Q(1))
     .                 -P13RPP*(Q(2)*P23R2Y+Q(3)*P23R2Z)
     .                 -P23RPP*(Q(2)*P13R1Y+Q(3)*P13R1Z)
     .                 +(Q(4)-Q(1))*(P13R1Y*P23R2Y+P13R1Z*P23R2Z)
               ALL11I = P13RPP*(Q(2)*P23R2Z-Q(3)*P23R2Y)
     .                 +P23RPP*(Q(3)*P13R1Y-Q(2)*P13R1Z)
     .                 +(Q(4)-Q(1))*(P13R1Z*P23R2Y-P13R1Y*P23R2Z)
           ENDIF

           AALL(LIND,1,1) = AL*DCMPLX(ALL11R,-ALL11I)
           AALL(LIND,2,2) = AR*DCMPLX(ALL11R, ALL11I)

        RETURN
      END
*-----------------------------------------------------------------------
*     FFV  revised 94/08/13 by T.Ishikawa
*      AAM != 0, AM = 0
*-----------------------------------------------------------------------
      SUBROUTINE FFVMM5(LIND,II,IO,AL,AR,CC,C,Q1,Q2,P1,P2,Q,AALL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ONE = 1.0D0)
      DOUBLE COMPLEX AALL
      DIMENSION AALL(4,2,2)
      DOUBLE COMPLEX C,CC
      DIMENSION Q(4),P1(4),P2(4),Q1(4)
      DIMENSION Q2(4),C(2),CC(2)
      INTEGER II,IO
      COMMON /CHWORK/
     .        ALL11R,ALL11I,ALL12R,ALL12I,ALL21R,ALL21I,ALL22R,ALL22I,
     .        R,RR,
     .        Q1RPP1,Q1R1Y,Q1R1Z,Q2RPP1,Q2R1Y,Q2R1Z,
     .        P1RPP2,P1R2Y,P1R2Z,P2RPP2,P2R2Y,P2R2Z,
     .        J1,J2,K1,K2,
     .        LQ1,LQ2,LP1,LP2
      LOGICAL    LQ1,LQ2,LP1,LP2

*------------------------ Entry point ----------------------------------
         ALL11R = 0.0D0
         ALL11I = 0.0D0
         ALL12R = 0.0D0
         ALL12I = 0.0D0
         ALL21R = 0.0D0
         ALL21I = 0.0D0
         ALL22R = 0.0D0
         ALL22I = 0.0D0

      DO 90 I3= 1 , 2
      DO 90 I2= 1 , 2
         AALL(LIND,I2,I3)=(0.0D0,0.0D0)
   90 CONTINUE

      IF( LIND .EQ. 1 ) THEN
          LQ1      = .TRUE.
          LQ2      = .TRUE.
          LP1      = .TRUE.
          LP2      = .TRUE.
          IF( Q1(4) .LE. 0.0D0 ) LQ1 = .FALSE.
          IF( Q2(4) .LE. 0.0D0 ) LQ2 = .FALSE.
          IF( P1(4) .LE. 0.0D0 ) LP1 = .FALSE.
          IF( P2(4) .LE. 0.0D0 ) LP2 = .FALSE.
C-
          IF( LQ1 ) THEN
               PT1        = SQRT(Q1(2)*Q1(2) + Q1(3)*Q1(3))
               IF( PT1 .LE. 0.0D0) THEN
                   Q1RR1Y    = SIGN(ONE, Q1(1))
                   Q1RR1Z    = 0.0D0
               ELSE
                   Q1RR1Y    = Q1(2)/PT1
                   Q1RR1Z    = Q1(3)/PT1
               ENDIF
               IF( Q1(1).GE.0.D0) THEN
                   PS1       = SQRT(Q1(4) + Q1(1))
                   Q1RPP1    = PT1/PS1
               ELSE
                   Q1RPP1    = SQRT(Q1(4) - Q1(1))
                   PS1       = PT1/Q1RPP1
               ENDIF
               Q1R1Y    = Q1RR1Y*PS1
               Q1R1Z    = Q1RR1Z*PS1
           ENDIF

           IF( LQ2 ) THEN
               PT1        = SQRT(Q2(2)*Q2(2) + Q2(3)*Q2(3))
               IF( PT1 .LE. 0.0D0) THEN
                   Q2RR1Y    = SIGN(ONE, Q2(1))
                   Q2RR1Z    = 0.0D0
               ELSE
                   Q2RR1Y    = Q2(2)/PT1
                   Q2RR1Z    = Q2(3)/PT1
               ENDIF
               IF( Q2(1).GE.0.D0) THEN
                   PS1       = SQRT(Q2(4) + Q2(1))
                   Q2RPP1    = PT1/PS1
               ELSE
                   Q2RPP1    = SQRT(Q2(4) - Q2(1))
                   PS1       = PT1/Q2RPP1
               ENDIF
               Q2R1Y    = Q2RR1Y*PS1
               Q2R1Z    = Q2RR1Z*PS1
           ENDIF
C-
           IF( LP1 ) THEN
               PT2     = SQRT(P1(2)*P1(2) + P1(3)*P1(3))
               IF( PT2 .LE. 0.0D0) THEN
                   P1RR2Y    = SIGN(ONE, P1(1))
                   P1RR2Z    = 0.0D0
               ELSE
                   P1RR2Y    = P1(2)/PT2
                   P1RR2Z    = P1(3)/PT2
               ENDIF
               IF( P1(1) .GE. 0.D0) THEN
                   PS2       = SQRT(P1(4) + P1(1))
                   P1RPP2    = PT2/PS2
               ELSE
                   P1RPP2    = SQRT(P1(4) - P1(1))
                   PS2       = PT2/P1RPP2
               ENDIF
               P1R2Y    = P1RR2Y*PS2
               P1R2Z    = P1RR2Z*PS2
           ENDIF

           R     = DBLE(IO - 2)
           RR    = DBLE(II - 2)
C
           J1    = (5-II)/2
           J2    = 3 - J1
           K1    = (5-IO)/2
           K2    = 3 - K1

      ENDIF
C- Q1,P1
           IF( LQ1.AND.LP1 ) THEN
               ALL11R = Q1RPP1*P1RPP2*(Q(4)+Q(1))
     .                 -Q1RPP1*(Q(2)*P1R2Y+Q(3)*P1R2Z)
     .                 -P1RPP2*(Q(2)*Q1R1Y+Q(3)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Y*P1R2Y+Q1R1Z*P1R2Z)
               ALL11I = Q1RPP1*(Q(2)*P1R2Z-Q(3)*P1R2Y)
     .                 +P1RPP2*(Q(3)*Q1R1Y-Q(2)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Z*P1R2Y-Q1R1Y*P1R2Z)
           ENDIF
C- Q2,P1
           IF( LQ2.AND.LP1 ) THEN
               ALL21R = Q2RPP1*P1RPP2*(Q(4)+Q(1))
     .                 -Q2RPP1*(Q(2)*P1R2Y+Q(3)*P1R2Z)
     .                 -P1RPP2*(Q(2)*Q2R1Y+Q(3)*Q2R1Z)
     .                 +(Q(4)-Q(1))*(Q2R1Y*P1R2Y+Q2R1Z*P1R2Z)
               ALL21I = Q2RPP1*(Q(2)*P1R2Z-Q(3)*P1R2Y)
     .                 +P1RPP2*(Q(3)*Q2R1Y-Q(2)*Q2R1Z)
     .                 +(Q(4)-Q(1))*(Q2R1Z*P1R2Y-Q2R1Y*P1R2Z)
           ENDIF
C---
           AALL(LIND,J1,K1)= AL*DCMPLX(ALL11R,-ALL11I)
           AALL(LIND,J2,K2)= AR*DCMPLX(ALL11R, ALL11I)
           AALL(LIND,J1,K2)= -(AR*RR*CC(2)*DCMPLX(ALL21R, ALL21I))
           AALL(LIND,J2,K1)= -(AL*RR*CC(1)*DCMPLX(ALL21R,-ALL21I))

      RETURN
      END
C   08/03/94 403291234  MEMBER NAME  FFV0M    *.FORT        E2FORT
      SUBROUTINE FFVMM6(LIND,AL,AR,C,Q1,Q,AALL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ONE = 1.0D0)
      DOUBLE COMPLEX AALL
      DIMENSION  AALL(4,2,2)
      DOUBLE COMPLEX C
      DIMENSION  Q(4),Q1(4)
      DIMENSION  C(2)
      COMMON /CHWORK/
     .        ALL11R,ALL11I,ALL12R,ALL12I,ALL21R,ALL21I,ALL22R,ALL22I,
     .        R,RR,
     .        Q1RPP1,Q1R1Y,Q1R1Z,Q2RPP1,Q2R1Y,Q2R1Z,
     .        P1RPP2,P1R2Y,P1R2Z,P2RPP2,P2R2Y,P2R2Z,
     .        J1,J2,K1,K2,
     .        LQ1,LQ2,LP1,LP2
      LOGICAL    LQ1,LQ2,LP1,LP2
      COMMON /WRKFFX/P13RPP,P13R1Y,P13R1Z,P23RPP,P23R2Y,P23R2Z,LP13,LP23
      LOGICAL    LP13,LP23

*------------------------ Entry point ----------------------------------
         ALL11R = 0.0D0
         ALL11I = 0.0D0
         ALL12R = 0.0D0
         ALL12I = 0.0D0
         ALL21R = 0.0D0
         ALL21I = 0.0D0
         ALL22R = 0.0D0
         ALL22I = 0.0D0

      DO 90 I3= 1 , 2
      DO 90 I2= 1 , 2
         AALL(LIND,I2,I3)=(0.0D0,0.0D0)
   90 CONTINUE

      IF( LIND .EQ. 1 ) THEN
          LP13   = .TRUE.
          IF( Q1(4) .LE. 0.0D0 ) LP13 = .FALSE.
C-
          IF( LP13 ) THEN
              PT1        = SQRT(Q1(2)*Q1(2) + Q1(3)*Q1(3))
              IF( PT1 .LE. 0.0D0) THEN
                  Q1RR1Y    = SIGN(ONE, Q1(1))
                  Q1RR1Z    = 0.0D0
              ELSE
                   Q1RR1Y    = Q1(2)/PT1
                   Q1RR1Z    = Q1(3)/PT1
              ENDIF
              IF( Q1(1).GE.0.D0) THEN
                  PS1       = SQRT(Q1(4) + Q1(1))
                  P13RPP    = PT1/PS1
              ELSE
                  P13RPP    = SQRT(Q1(4) - Q1(1))
                  PS1       = PT1/P13RPP
              ENDIF
              P13R1Y    = Q1RR1Y*PS1
              P13R1Z    = Q1RR1Z*PS1
          ENDIF
      ENDIF
C- Q1,P1
           IF( LP13.AND.LP1 ) THEN
               ALL11R = P13RPP*P1RPP2*(Q(4)+Q(1))
     .                 -P13RPP*(Q(2)*P1R2Y +Q(3)*P1R2Z)
     .                 -P1RPP2*(Q(2)*P13R1Y+Q(3)*P13R1Z)
     .                 +(Q(4)-Q(1))*(P13R1Y*P1R2Y+P13R1Z*P1R2Z)
               ALL11I = P13RPP*(Q(2)*P1R2Z -Q(3)*P1R2Y)
     .                 +P1RPP2*(Q(3)*P13R1Y-Q(2)*P13R1Z)
     .                 +(Q(4)-Q(1))*(P13R1Z*P1R2Y-P13R1Y*P1R2Z)
           ENDIF

           AALL(LIND,1,K1)= AL*DCMPLX(ALL11R,-ALL11I)
           AALL(LIND,2,K2)= AR*DCMPLX(ALL11R, ALL11I)

      RETURN
      END
*-----------------------------------------------------------------------
*     FFV  revised 94/04/08 by T.Ishikawa
*     AAM =0 , AM != 0
*-----------------------------------------------------------------------
      SUBROUTINE FFVMM7(LIND,II,IO,AL,AR,CC,C,Q1,Q2,P1,P2,Q,AALL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ONE = 1.0D0)
      DOUBLE COMPLEX AALL
      DIMENSION AALL(4,2,2)
      DOUBLE COMPLEX C,CC
      DIMENSION Q(4),P1(4),P2(4),Q1(4)
      DIMENSION Q2(4),C(2),CC(2)
      INTEGER II,IO
      COMMON /CHWORK/
     .        ALL11R,ALL11I,ALL12R,ALL12I,ALL21R,ALL21I,ALL22R,ALL22I,
     .        R,RR,
     .        Q1RPP1,Q1R1Y,Q1R1Z,Q2RPP1,Q2R1Y,Q2R1Z,
     .        P1RPP2,P1R2Y,P1R2Z,P2RPP2,P2R2Y,P2R2Z,
     .        J1,J2,K1,K2,
     .        LQ1,LQ2,LP1,LP2
      LOGICAL    LQ1,LQ2,LP1,LP2

*------------------------ Entry point ----------------------------------
         ALL11R = 0.0D0
         ALL11I = 0.0D0
         ALL12R = 0.0D0
         ALL12I = 0.0D0
         ALL21R = 0.0D0
         ALL21I = 0.0D0
         ALL22R = 0.0D0
         ALL22I = 0.0D0

      DO 90 I3= 1 , 2
      DO 90 I2= 1 , 2
         AALL(LIND,I2,I3)=(0.0D0,0.0D0)
   90 CONTINUE

      IF( LIND .EQ. 1 ) THEN
          LQ1      = .TRUE.
          LQ2      = .TRUE.
          LP1      = .TRUE.
          LP2      = .TRUE.
          IF( Q1(4) .LE. 0.0D0 ) LQ1 = .FALSE.
          IF( Q2(4) .LE. 0.0D0 ) LQ2 = .FALSE.
          IF( P1(4) .LE. 0.0D0 ) LP1 = .FALSE.
          IF( P2(4) .LE. 0.0D0 ) LP2 = .FALSE.
C-
          IF( LQ1 ) THEN
               PT1        = SQRT(Q1(2)*Q1(2) + Q1(3)*Q1(3))
               IF( PT1 .LE. 0.0D0) THEN
                   Q1RR1Y    = SIGN(ONE, Q1(1))
                   Q1RR1Z    = 0.0D0
               ELSE
                   Q1RR1Y    = Q1(2)/PT1
                   Q1RR1Z    = Q1(3)/PT1
               ENDIF
               IF( Q1(1).GE.0.D0) THEN
                   PS1       = SQRT(Q1(4) + Q1(1))
                   Q1RPP1    = PT1/PS1
               ELSE
                   Q1RPP1    = SQRT(Q1(4) - Q1(1))
                   PS1       = PT1/Q1RPP1
               ENDIF
               Q1R1Y    = Q1RR1Y*PS1
               Q1R1Z    = Q1RR1Z*PS1
           ENDIF
C-
           IF( LP1 ) THEN
               PT2     = SQRT(P1(2)*P1(2) + P1(3)*P1(3))
               IF( PT2 .LE. 0.0D0) THEN
                   P1RR2Y    = SIGN(ONE, P1(1))
                   P1RR2Z    = 0.0D0
               ELSE
                   P1RR2Y    = P1(2)/PT2
                   P1RR2Z    = P1(3)/PT2
               ENDIF
               IF( P1(1) .GE. 0.D0) THEN
                   PS2       = SQRT(P1(4) + P1(1))
                   P1RPP2    = PT2/PS2
               ELSE
                   P1RPP2    = SQRT(P1(4) - P1(1))
                   PS2       = PT2/P1RPP2
               ENDIF
               P1R2Y    = P1RR2Y*PS2
               P1R2Z    = P1RR2Z*PS2
           ENDIF

           IF( LP2 ) THEN
               PT2     = SQRT(P2(2)*P2(2) + P2(3)*P2(3))
               IF( PT2 .LE. 0.0D0) THEN
                   P2RR2Y    = SIGN(ONE, P2(1))
                   P2RR2Z    = 0.0D0
               ELSE
                   P2RR2Y    = P2(2)/PT2
                   P2RR2Z    = P2(3)/PT2
               ENDIF
               IF( P2(1) .GE. 0.D0) THEN
                   PS2       = SQRT(P2(4) + P2(1))
                   P2RPP2    = PT2/PS2
               ELSE
                   P2RPP2    = SQRT(P2(4) - P2(1))
                   PS2       = PT2/P2RPP2
               ENDIF
               P2R2Y    = P2RR2Y*PS2
               P2R2Z    = P2RR2Z*PS2
           ENDIF

           R     = DBLE(IO - 2)
           RR    = DBLE(II - 2)

C
           J1    = (5-II)/2
           J2    = 3 - J1
           K1    = (5-IO)/2
           K2    = 3 - K1

      ENDIF
C- Q1,P1
           IF( LQ1.AND.LP1 ) THEN
               ALL11R = Q1RPP1*P1RPP2*(Q(4)+Q(1))
     .                 -Q1RPP1*(Q(2)*P1R2Y+Q(3)*P1R2Z)
     .                 -P1RPP2*(Q(2)*Q1R1Y+Q(3)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Y*P1R2Y+Q1R1Z*P1R2Z)
               ALL11I = Q1RPP1*(Q(2)*P1R2Z-Q(3)*P1R2Y)
     .                 +P1RPP2*(Q(3)*Q1R1Y-Q(2)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Z*P1R2Y-Q1R1Y*P1R2Z)
           ENDIF

C- Q1,P2
           IF( LQ1.AND.LP2 ) THEN
               ALL12R = Q1RPP1*P2RPP2*(Q(4)+Q(1))
     .                 -Q1RPP1*(Q(2)*P2R2Y+Q(3)*P2R2Z)
     .                 -P2RPP2*(Q(2)*Q1R1Y+Q(3)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Y*P2R2Y+Q1R1Z*P2R2Z)
               ALL12I = Q1RPP1*(Q(2)*P2R2Z-Q(3)*P2R2Y)
     .                 +P2RPP2*(Q(3)*Q1R1Y-Q(2)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Z*P2R2Y-Q1R1Y*P2R2Z)
           ENDIF

C---
           AALL(LIND,J1,K1)= AL*DCMPLX(ALL11R,-ALL11I)
           AALL(LIND,J2,K2)= AR*DCMPLX(ALL11R, ALL11I)
           AALL(LIND,J1,K2)= -(AL*R*C(1)*DCMPLX(ALL12R,-ALL12I))
           AALL(LIND,J2,K1)= -(AR*R*C(2)*DCMPLX(ALL12R, ALL12I))

      RETURN
      END
        SUBROUTINE FFVMM8(LIND,AL,AR,CC,P1,Q,AALL)
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        PARAMETER (ONE = 1.0D0)
        DOUBLE COMPLEX AALL
        DIMENSION  AALL(4,2,2)
        DOUBLE COMPLEX CC
        DIMENSION  Q(4),P1(4)
        DIMENSION  CC(2)
        COMMON /CHWORK/
     .        ALL11R,ALL11I,ALL12R,ALL12I,ALL21R,ALL21I,ALL22R,ALL22I,
     .        R,RR,
     .        Q1RPP1,Q1R1Y,Q1R1Z,Q2RPP1,Q2R1Y,Q2R1Z,
     .        P1RPP2,P1R2Y,P1R2Z,P2RPP2,P2R2Y,P2R2Z,
     .        J1,J2,K1,K2,
     .        LQ1,LQ2,LP1,LP2
      LOGICAL    LQ1,LQ2,LP1,LP2
C
      COMMON /WRKFFX/P13RPP,P13R1Y,P13R1Z,P23RPP,P23R2Y,P23R2Z,LP13,LP23
      LOGICAL    LP13,LP23

*------------------------ Entry point ----------------------------------
         ALL11R = 0.0D0
         ALL11I = 0.0D0
         ALL12R = 0.0D0
         ALL12I = 0.0D0
         ALL21R = 0.0D0
         ALL21I = 0.0D0
         ALL22R = 0.0D0
         ALL22I = 0.0D0

      DO 90 I3= 1 , 2
      DO 90 I2= 1 , 2
         AALL(LIND,I2,I3)=(0.0D0,0.0D0)
   90 CONTINUE
      IF( LIND .EQ. 1 ) THEN
           LP23      = .TRUE.
           IF( P1(4) .LE. 0.0D0 ) LP23 = .FALSE.
C-
           IF( LP23 ) THEN
               PT2     = SQRT(P1(2)*P1(2) + P1(3)*P1(3))
               IF( PT2 .LE. 0.0D0) THEN
                   P1RR2Y    = SIGN(ONE, P1(1))
                   P1RR2Z    = 0.0D0
               ELSE
                   P1RR2Y    = P1(2)/PT2
                   P1RR2Z    = P1(3)/PT2
               ENDIF
               IF( P1(1) .GE. 0.D0) THEN
                   PS2       = SQRT(P1(4) + P1(1))
                   P23RPP    = PT2/PS2
               ELSE
                   P23RPP    = SQRT(P1(4) - P1(1))
                   PS2       = PT2/P23RPP
               ENDIF
               P23R2Y    = P1RR2Y*PS2
               P23R2Z    = P1RR2Z*PS2
           ENDIF
      ENDIF
C- Q1,P1
           IF( LQ1.AND.LP23 ) THEN
               ALL11R = Q1RPP1*P23RPP*(Q(4)+Q(1))
     .                 -Q1RPP1*(Q(2)*P23R2Y+Q(3)*P23R2Z)
     .                 -P23RPP*(Q(2)*Q1R1Y +Q(3)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Y*P23R2Y+Q1R1Z*P23R2Z)
               ALL11I = Q1RPP1*(Q(2)*P23R2Z-Q(3)*P23R2Y)
     .                 +P23RPP*(Q(3)*Q1R1Y -Q(2)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Z*P23R2Y-Q1R1Y*P23R2Z)
           ENDIF
C---
           AALL(LIND,J1,1) = AL*DCMPLX(ALL11R,-ALL11I)
           AALL(LIND,J2,2) = AR*DCMPLX(ALL11R, ALL11I)

      RETURN
      END
*-----------------------------------------------------------------------
*     FFV  revised 94/08/13 by T.Ishikawa
*     AAM =0 , AM = 0
*-----------------------------------------------------------------------
      SUBROUTINE FFVMM9(LIND,II,IO,AL,AR,CC,C,Q1,Q2,P1,P2,Q,AALL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ONE = 1.0D0)
      DOUBLE COMPLEX AALL
      DIMENSION AALL(4,2,2)
      DOUBLE COMPLEX C,CC
      DIMENSION Q(4),P1(4),P2(4),Q1(4)
      DIMENSION Q2(4),C(2),CC(2)
      INTEGER II,IO
      COMMON /CHWORK/
     .        ALL11R,ALL11I,ALL12R,ALL12I,ALL21R,ALL21I,ALL22R,ALL22I,
     .        R,RR,
     .        Q1RPP1,Q1R1Y,Q1R1Z,Q2RPP1,Q2R1Y,Q2R1Z,
     .        P1RPP2,P1R2Y,P1R2Z,P2RPP2,P2R2Y,P2R2Z,
     .        J1,J2,K1,K2,
     .        LQ1,LQ2,LP1,LP2
      LOGICAL    LQ1,LQ2,LP1,LP2

*------------------------ Entry point ----------------------------------
         ALL11R = 0.0D0
         ALL11I = 0.0D0
         ALL12R = 0.0D0
         ALL12I = 0.0D0
         ALL21R = 0.0D0
         ALL21I = 0.0D0
         ALL22R = 0.0D0
         ALL22I = 0.0D0

      DO 90 I3= 1 , 2
      DO 90 I2= 1 , 2
         AALL(LIND,I2,I3)=(0.0D0,0.0D0)
   90 CONTINUE

      IF( LIND .EQ. 1 ) THEN
          LQ1      = .TRUE.
          LQ2      = .TRUE.
          LP1      = .TRUE.
          LP2      = .TRUE.
          IF( Q1(4) .LE. 0.0D0 ) LQ1 = .FALSE.
          IF( Q2(4) .LE. 0.0D0 ) LQ2 = .FALSE.
          IF( P1(4) .LE. 0.0D0 ) LP1 = .FALSE.
          IF( P2(4) .LE. 0.0D0 ) LP2 = .FALSE.
C-
          IF( LQ1 ) THEN
               PT1        = SQRT(Q1(2)*Q1(2) + Q1(3)*Q1(3))
               IF( PT1 .LE. 0.0D0) THEN
                   Q1RR1Y    = SIGN(ONE, Q1(1))
                   Q1RR1Z    = 0.0D0
               ELSE
                   Q1RR1Y    = Q1(2)/PT1
                   Q1RR1Z    = Q1(3)/PT1
               ENDIF
               IF( Q1(1).GE.0.D0) THEN
                   PS1       = SQRT(Q1(4) + Q1(1))
                   Q1RPP1    = PT1/PS1
               ELSE
                   Q1RPP1    = SQRT(Q1(4) - Q1(1))
                   PS1       = PT1/Q1RPP1
               ENDIF
               Q1R1Y    = Q1RR1Y*PS1
               Q1R1Z    = Q1RR1Z*PS1
           ENDIF

C-
           IF( LP1 ) THEN
               PT2     = SQRT(P1(2)*P1(2) + P1(3)*P1(3))
               IF( PT2 .LE. 0.0D0) THEN
                   P1RR2Y    = SIGN(ONE, P1(1))
                   P1RR2Z    = 0.0D0
               ELSE
                   P1RR2Y    = P1(2)/PT2
                   P1RR2Z    = P1(3)/PT2
               ENDIF
               IF( P1(1) .GE. 0.D0) THEN
                   PS2       = SQRT(P1(4) + P1(1))
                   P1RPP2    = PT2/PS2
               ELSE
                   P1RPP2    = SQRT(P1(4) - P1(1))
                   PS2       = PT2/P1RPP2
               ENDIF
               P1R2Y    = P1RR2Y*PS2
               P1R2Z    = P1RR2Z*PS2
           ENDIF


           R     = DBLE(IO - 2)
           RR    = DBLE(II - 2)

C
           J1    = (5-II)/2
           J2    = 3 - J1
           K1    = (5-IO)/2
           K2    = 3 - K1

      ENDIF
C- Q1,P1
           IF( LQ1.AND.LP1 ) THEN
               ALL11R = Q1RPP1*P1RPP2*(Q(4)+Q(1))
     .                 -Q1RPP1*(Q(2)*P1R2Y+Q(3)*P1R2Z)
     .                 -P1RPP2*(Q(2)*Q1R1Y+Q(3)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Y*P1R2Y+Q1R1Z*P1R2Z)
               ALL11I = Q1RPP1*(Q(2)*P1R2Z-Q(3)*P1R2Y)
     .                 +P1RPP2*(Q(3)*Q1R1Y-Q(2)*Q1R1Z)
     .                 +(Q(4)-Q(1))*(Q1R1Z*P1R2Y-Q1R1Y*P1R2Z)
           ENDIF
C---
      AALL(LIND,J1,K1)= AL*DCMPLX(ALL11R,-ALL11I)
      AALL(LIND,J2,K2)= AR*DCMPLX(ALL11R, ALL11I)

      RETURN
      END
C
C      ************************************************************
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *             ============================                 *
C      *                CHANEL   (Version. 2)                     *
C      *             ============================                 *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *                   Written by H. Tanaka                   *
C      *                                                          *
C      *                                                          *
C      *                        ---------                         *
C      *                        Reference                         *
C      *                        ---------                         *
C      *    ' Numerical Calculation of Helicity Amplitudes        *
C      *      for Processes Involving Massive Fermions'           *
C      *                                                          *
C      *      H. Tanaka, Hiroshima University preprint            *
C      *         Accepted to Comput. Phys. Commun.                *
C      *         for version 1.                                   *
C      *                                                          *
C      *                                                          *
C      *                                                          *
C      *       Version 1 should be added.                         *
C      *                                                          *
C      *                                                          *
C      ************************************************************
C
C
C ********************************************************************
C *                                                                  *
C *  SUBROUTINE SPLT(AM:R*8, P:R*8(4), S1:R*8, S2:R*8,               *
C * &                P1:R*8(4), P2:R*8(4))                           *
C *                                                                  *
C * Decompose off-shell momentum P to two on-shell momenta P1 and P2 *
C *                                                                  *
C *   P = S1*P1 + S2*P2                                              *
C *   P1**2 = 0,      S1 = +-1 (or = 0 for P1 = 0)                   *
C *   P2**2 = AM**2,  S2 = +-1 (or = 0 for P2 = 0)                   *
C *                                        ??????                    *
C *                                        consistent?               *
C *   This subroutine is not included in CHANTL version 1.1.         *
C *                                                                  *
C *                                            written by H. Tanaka  *
C ********************************************************************
C
        SUBROUTINE SPLT(AM,P,S1,S2,P1,P2)
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        PARAMETER (ONE = 1.0)
 
CT      REAL*16 PP,P0,P10,P20,AA,DEN0,DEN,A,B,P00,PPP
        DIMENSION P(4),P1(4),P2(4)
        DATA DD/1.0D-7/
 
CX      DDD=P(4)**2+P(1)**2+P(2)**2+P(1)**2
        DEN0  = P(4) - P(2)
        ADEN0 = ABS(DEN0)
CX      IF (P(4).EQ.0.0D0) THEN
CX        P10 = 0.0D0
CX        S1  = 0.0D0
CX      ELSE
          IF (ADEN0.LE.DD) THEN
            C   = 0.75D0
            A   = SQRT(C)
            B   = 0.5D0
            DEN = P(4)-A*P(1)-B*P(2)
          ELSE
            A   = 0.D0
            B   = 1.D0
            DEN = DEN0
          END IF
          PP   = P(4)*P(4) - P(1)*P(1) - P(2)*P(2) - P(3)*P(3)
          P0   = (PP - AM**2)/(2.0D0*DEN)
          P10  = ABS(P0)
          IF (P10.LE.0.0D0) THEN
            S1  = 0.D0
          ELSE
            S1  = SIGN(ONE, P0)
          END IF
CX      END IF
        P1(1) = A*P10
        P1(2) = B*P10
        P1(3) =   0.D0
        P1(4) =   P10
 
        P00=P(4)-S1*P1(4)
        P20=ABS(P00)
        IF (P20.LE.0.0D0) THEN
          S2 = 0.0D0
        ELSE
          S2 = SIGN(ONE, P00)
        END IF
 
CT      P2(1) = S2*(QEXTD(P(1)) - QEXTD(S1*P1(1)))
CT      P2(2) = S2*(QEXTD(P(2)) - QEXTD(S1*P1(2)))
CT      P2(3) = S2*(QEXTD(P(3)) - QEXTD(S1*P1(3)))
CT      P2(4) = S2*(QEXTD(P(4)) - QEXTD(S1*P1(4)))
        P2(1) = S2*(P(1) - S1*P1(1))
        P2(2) = S2*(P(2) - S1*P1(2))
        P2(3) = S2*(P(3) - S1*P1(3))
        P2(4) = S2*(P(4) - S1*P1(4))
CX      PPP   = AM**2 + P2(1)**2 + P2(2)**2 + P2(3)**2
CX      P2(4) = SQRT(PPP)
 
        RETURN
      END
C
C ********************************************************************
C *          3                                                       *
C *          |                                                       *
C *          |                                                       *
C *  1 -->---|---<-- 2    SUBROUTINE VVV                             *
C *                                                                  *
C *     Purpose: To calculate vertex amplitudes for three vector-    *
C *              boson vertex.                                       *
C *                                                                  *
C *     GG=: Coupling constant for vertex.                           *
C *     L1,L2,L3=: Polarization state of vector boson.               *
C *     P1,P2,P3=: Momenta for the vector bosons.                    *
C *     EP1,EP2,EP3=: Polarization vectors for vector bosons.        *
C *     AALL=: Calculated results of vertex amplitudes for given     *
C *            plarization states.                                   *
C *                                                                  *
C *                                                                  *
C *     Comment : The momenta of particles with vertices are         *
C *               taken to flow in.                                  *
C *                                                                  *
C *                                                                  *
C *                                      written by H. Tanaka        *
C ********************************************************************
C
C
C       ===================================================
CXXX    SUBROUTINE VVV(GG,L1,L2,L3,P1,P2,P3,EP1,EP2,EP3,AALL)
        SUBROUTINE VVV(GG,P1,P2,P3,EP1,EP2,EP3,AALL)
C       ===================================================
C
C
CX      IMPLICIT REAL*16(A-H,O-Z)
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
CXX     DOUBLE PRECISION GG,P1,P2,P3,EP1,EP2,EP3,AALL
CXX     DOUBLE COMPLEX AALL
        DIMENSION P1(4),P2(4),P3(4),EP1(4),EP2(4),EP3(4)
        E3P1P2=EP3(4)*(P1(4)-P2(4))-EP3(1)*(P1(1)-P2(1))
     *        -EP3(2)*(P1(2)-P2(2))-EP3(3)*(P1(3)-P2(3))
        E1P2P3=EP1(4)*(P2(4)-P3(4))-EP1(1)*(P2(1)-P3(1))
     *        -EP1(2)*(P2(2)-P3(2))-EP1(3)*(P2(3)-P3(3))
        E2P3P1=EP2(4)*(P3(4)-P1(4))-EP2(1)*(P3(1)-P1(1))
     *        -EP2(2)*(P3(2)-P1(2))-EP2(3)*(P3(3)-P1(3))
        E1E2=EP1(4)*EP2(4)-EP1(1)*EP2(1)-EP1(2)*EP2(2)-EP1(3)*EP2(3)
        E2E3=EP2(4)*EP3(4)-EP2(1)*EP3(1)-EP2(2)*EP3(2)-EP2(3)*EP3(3)
        E1E3=EP1(4)*EP3(4)-EP1(1)*EP3(1)-EP1(2)*EP3(2)-EP1(3)*EP3(3)
        VVVV=GG*(E3P1P2*E1E2+E1P2P3*E2E3+E2P3P1*E1E3)
        AALL=VVVV
CXX     AALL=DCMPLX(AALL,0.D0)
        RETURN
      END
C
C ********************************************************************
C *          V4                                                      *
C *          |                                                       *
C *          |                                                       *
C * V1 -->---|---<-- V3   SUBROUTINE VVVV                            *
C *          |                                                       *
C *          |                                                       *
C *         V2                                                       *
C *                                                                  *
C *     V1,V2=: Charged gauge bosons ( W+ OR W- ).                   *
C *     V3,V4=: Neutral gauge bosons ( photon or Z0 ) or             *
C *             charged gouge bosons with oposite charges to         *
C *             V1 and V2.                                           *
C *                                                                  *
C *     Purpose: To calculate vertex amplitudes for four vector-     *
C *              boson vertex.                                       *
C *                                                                  *
C *     GG=: Coupling constant for vertex.                           *
C *     L1,L2,L3,L4=: Polarization state of vector boson.            *
C *     P1,P2,P3,P4=: Momenta for the vector bosons.                 *
C *     EP1,EP2,EP3,EP4=: Polarization vectors for vector bosons.    *
C *     AALL=: Calculated results of vertex amplitudes for given     *
C *            plarization states.                                   *
C *                                                                  *
C *                                                                  *
C *     Comment : The momenta of particles with vertices are         *
C *               taken to flow in.                                  *
C *                                                                  *
C *                                                                  *
C *                                      written by H. Tanaka        *
C ********************************************************************
C
C
C       ==============================================================
CXXX    SUBROUTINE VVVV(GG,L1,L2,L3,L4,
CXXX *             P1,P2,P3,P4,EP1,EP2,EP3,EP4,AALL)
        SUBROUTINE VVVV(GG,EP1,EP2,EP3,EP4,AALL)
C       ==============================================================
C
C
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
CXX     DOUBLE COMPLEX AALL
CXX     DIMENSION P1(4),P2(4),P3(4),P4(4),EP1(4),EP2(4),EP3(4),EP4(4)
        DIMENSION EP1(4),EP2(4),EP3(4),EP4(4)
        E1E2=EP1(4)*EP2(4)-EP1(1)*EP2(1)-EP1(2)*EP2(2)-EP1(3)*EP2(3)
        E3E4=EP3(4)*EP4(4)-EP3(1)*EP4(1)-EP3(2)*EP4(2)-EP3(3)*EP4(3)
        E1E3=EP1(4)*EP3(4)-EP1(1)*EP3(1)-EP1(2)*EP3(2)-EP1(3)*EP3(3)
        E2E4=EP2(4)*EP4(4)-EP2(1)*EP4(1)-EP2(2)*EP4(2)-EP2(3)*EP4(3)
        E1E4=EP1(4)*EP4(4)-EP1(1)*EP4(1)-EP1(2)*EP4(2)-EP1(3)*EP4(3)
        E2E3=EP2(4)*EP3(4)-EP2(1)*EP3(1)-EP2(2)*EP3(2)-EP2(3)*EP3(3)
        AALL=GG*(E1E3*E2E4+E1E4*E2E3-2.D0*E1E2*E3E4)
CXX     AALL=DCMPLX(AALL,0.D0)
        RETURN
      END
C
C ********************************************************************
C *          3                                                       *
C *          S                                                       *
C *          |                                                       *
C *          |                                                       *
C *  V -->---|---<-- V    SUBROUTINE VVS                             *
C *  1               2                                               *
C *                                                                  *
C *                                                                  *
C *     Purpose: To calculate vertex amplitudes for vector boson-    *
C *              scalor boson vertex.                                *
C *                                                                  *
C *     GG=: Coupling constant for vertex.                           *
C *     L1,L2=: Polarization state of vector boson.                  *
C *     EP1,EP2=: Polarization vectors for vector bosons.            *
C *     AALL=: Calculated results of vertex amplitudes for given     *
C *            plarization states.                                   *
C *                                                                  *
C *                                                                  *
C *     Comment : The momenta of particles with vertices are         *
C *               taken to flow in.                                  *
C *                                                                  *
C *      (*) This subroutine can be also used for VVSS vertex.       *
C *                                                                  *
C *                                                                  *
C *                                                                  *
C *                                      written by H. Tanaka        *
C ********************************************************************
C
C
C            =====================================
CXX          SUBROUTINE VVS(GG,L1,L2,EP1,EP2,AALL)
             SUBROUTINE VVS(GG,EP1,EP2,AALL)
C            =====================================
C
C
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
CXX     DOUBLE COMPLEX AALL
        DIMENSION EP1(4),EP2(4)
        E1E2=EP1(4)*EP2(4)-EP1(1)*EP2(1)-EP1(2)*EP2(2)-EP1(3)*EP2(3)
        AALL=GG*E1E2
CXX     AALL=DCMPLX(AALL,0.D0)
        RETURN
      END
C ********************************************************************
C *          3                                                       *
C *          V                                                       *
C *          |                                                       *
C *          |                                                       *
C *  S --<---|---<-- S    SUBROUTINE SSV                             *
C *  1               2                                               *
C *                                                                  *
C *                                                                  *
C *     Purpose: To calculate vertex amplitudes for vector boson     *
C *              coupled to scalor boson line.                       *
C *                                                                  *
C *     GG=: Coupling constant for vertex.                           *
C *     L=: Polarization state of vector boson.                      *
C *     EP=: Polarization vectors for vector bosons.                 *
C *     P1,P2=: Momenta for scalor bosons.                           *
C *     AALL=: Calculated results of vertex amplitudes for given     *
C *            plarization states.                                   *
C *                                                                  *
C *                                                                  *
C *                                                                  *
C *                                                                  *
C *                                                                  *
C *                                                                  *
C *                                      written by H. Tanaka        *
C ********************************************************************
C
C
C            =====================================
CX           SUBROUTINE SSV(GG,L,P1,P2,EP,AALL)
             SUBROUTINE SSV(GG,P1,P2,EP,AALL)
C            =====================================
C
C
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
CXX     DOUBLE COMPLEX AALL
        DIMENSION EP(4),P1(4),P2(4)
        EP1P2=EP(4)*(P1(4)-P2(4))-EP(1)*(P1(1)-P2(1))
     *       -EP(2)*(P1(2)-P2(2))-EP(3)*(P1(3)-P2(3))
        AALL=GG*EP1P2
CXX     AALL=DCMPLX(AALL,0.D0)
        RETURN
      END
C
C
C
C ******************************************************************
C *                                                                *
C *  SUBROUTINE POLA(I:I*4, A:R*8 , AM:R*8, P:R*8(4),              *
C *                                       EP:R*8(4), EM:R*8(4))    *
C *                                                                *
C *     Purpose: To set components of polarization vectors         *
C *              for covarient gauge.                              *
C *                                                                *
C *     I= : Polarization states   I=1,2 : Transverse              *
C *                                I=3   : Longitudinal            *
C *                                I=4   : Scalar                  *
C *                                                                *
C *     A = : Gauge parametor                                      *
C *     AM= : Mass of vector boson                                 *
C *     P = : Momentum of vector boson                             *
C *     EP= : Polarization vector for state I                      *
C *     EM= : Weight factors                                       *
C *                                                                *
C *                                                                *
C *                                                                *
C *     Note                                                       *
C *     ----                                                       *
C *                                                                *
C *     For summation of the polarization vectors I=1 to 4,        *
C *     following gauges are chosen :                              *
C *                                                                *
C *     For A < 100                                                *
C *                                                                *
C *     Covarient gauge : for massive and massless vector boson    *
C *     Feynman gauge : if A*AM**2 = P*P                           *
C *                                                                *
C *     For A > 100                                                *
C *                                                                *
C *     Unitary gauge : for massive vector boson                   *
C *     Covarient gauge : for massless vector boson                *
C *                                                                *
C *                                    written by H. Tanaka        *
C ******************************************************************
C
C
C              ===================================
               SUBROUTINE POLA(I,A,AM,P,EP,EM)
C              ===================================
C
C
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        DIMENSION P(4),EP(4),EM(4)
        DATA DD/0.D0/
 
        PT2 = P(1)**2 + P(2)**2
        PP  = (P(4) - P(3))*(P(4) + P(3)) - PT2
        IF (PT2.LE.DD) THEN
          PT  = 0.0D0
          RTY = 1.0D0
          RTX = 0.0D0
        ELSE
          PT  = SQRT(PT2)
          PTI = 1.0D0/PT
          RTY = P(2)*PTI
          RTX = P(1)*PTI
        END IF
        PN2 = PT2 + P(3)**2
        IF (PN2.LE.DD) THEN
          PN  = 0.0D0
          RNX = 0.0D0
          RNY = 0.0D0
          RNZ = 1.0D0
          RTN = 0.0D0
        ELSE IF (PN2.GT.DD) THEN
          PN  = SQRT(PN2)
          PNI = 1.0D0/PN
          RNX = P(1)*PNI
          RNY = P(2)*PNI
          RNZ = P(3)*PNI
          RTN = PT*PNI
        END IF
        IF (I.EQ.1) THEN
          EP(1) =  RTX*RNZ
          EP(2) =  RTY*RNZ
          EP(3) = -RTN
          EP(4) =  0.0D0
          EM(1) =  1.0D0
CX        EM(1) =  PROP
        ELSE IF (I.EQ.2) THEN
          EP(1) = -RTY
          EP(2) =  RTX
          EP(3) =  0.0D0
          EP(4) =  0.0D0
          EM(2) =  1.0D0
CX        EM(2) =  PROP
        ELSE IF (I.EQ.3) THEN
          APP=ABS(PP)
          IF (APP.LE.DD) THEN
            EP(1) = 0.0D0
            EP(2) = 0.0D0
            EP(3) = 0.0D0
            EP(4) = 0.0D0
            EM(3) = 0.0D0
          ELSE
            RPPI  = 1.0D0/SQRT(APP)
            EP(1) = RNX*P(4)*RPPI
            EP(2) = RNY*P(4)*RPPI
            EP(3) = RNZ*P(4)*RPPI
            EP(4) = PN*RPPI
            EM(3) = SIGN(1.0D0, PP)
CX          EM(3) = SIGN(1.0D0, PP)*PROP
          ENDIF
        ELSE IF (I.EQ.4) THEN
          APP = ABS(PP)
          IF (APP.LE.DD) THEN
            EP(1) = 0.D0
            EP(2) = 0.D0
            EP(3) = 0.D0
            EP(4) = 0.D0
            EM(4) = 0.0D0
          ELSE
            RPPI  = 1.0D0/SQRT(APP)
            EP(1) = P(1)*RPPI
            EP(2) = P(2)*RPPI
            EP(3) = P(3)*RPPI
            EP(4) = P(4)*RPPI
              AM2 = AM*AM
CX          IF (A.NE.1.D0.AND.A.LT.100.D0) THEN
            IF (A.LT.100.D0) THEN
              IF (A*AM2.NE.PP) THEN
              EM(4) = SIGN(1.0D0, PP)*(AM2-PP)/(PP-A*AM2)*A
              ELSE
              EM(4)= -SIGN(1.0D0, PP)
              WRITE(6,*) ' Caution : p*p = a*m**2 '
              WRITE(6,*) '  a = 1 was taken. '
              END IF
CX          ELSE IF (A.EQ.1.D0) THEN
CX            EM(4)= -SIGN(1.0D0, PP)
            ELSE
            IF (AM.GT.0.D0) THEN
              EM(4)=  SIGN(1.0D0, PP)*(PP/AM**2-1.0D0)
            ELSE
              EM(4) =-SIGN(1.0D0, PP)*A
            END IF
            END IF
          ENDIF
        END IF
        RETURN
      END
C
C
C *******************************************************************
C *                                                                 *
C *         S                                                       *
C *         |                                                       *
C *         |                                                       *
C * F1--<---|---<---F2    SUBROUTINE FFS0                           *
C *                                                                 *
C *                                                                 *
C *                                                                 *
C *                                                                 *
C *   Purpose: To calculate vertex amplitudes for scalor boson-     *
C *            massless fermions vertex.                            *
C *                                                                 *
C *   P1,P2=: Momenta of massless fermions.                         *
C *   AALL=: Calculated results for vertex amplitudes.              *
C *                                                                 *
C *                                                                 *
C *                                                                 *
C *                                                                 *
C *                                          written by H. Tanaka   *
C *******************************************************************
C
C
C               ============================
                SUBROUTINE FFS0(P1,P2,AALL)
C               ============================
C
C
C
C    FFS0     FUNCTION S(P1,P2) AND T(P1,P2)
C
C      X(+,P1)X(-,P2)  :  2
C      X(-,P1)X(+,P2)  :  1
C
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        DOUBLE PRECISION P1,P2
        DOUBLE COMPLEX AALL
        DIMENSION P1(4),P2(4),AALL(2)
 
        IF (P1(4).LE.0.0D0 .OR. P2(4).LE.0.0D0) THEN
          AALL(1) = 0.0D0
          AALL(2) = 0.0D0
CX        WRITE(6,*) ' AALL = 0 '
          RETURN
        END IF
 
        PT1  = SQRT(P1(2)**2 + P1(3)**2)
        IF(PT1.LE.0.0D0) THEN
          RR1Y = SIGN(1.0D0, P1(1))
          RR1Z = 0.0D0
        ELSE
          RR1Y = P1(2)/PT1
          RR1Z = P1(3)/PT1
        ENDIF
        IF(P1(1).GE.0.D0) THEN
          PS1  = SQRT(P1(4) + P1(1))
          RPP1 = PT1/PS1
        ELSE
          RPP1 = SQRT(P1(4) - P1(1))
          PS1  = PT1/RPP1
        ENDIF
        R1Y = RR1Y*PS1
        R1Z = RR1Z*PS1
 
        PT2  = SQRT(P2(2)**2 + P2(3)**2)
        IF(PT2.LE.0.0D0) THEN
          RR2Y = SIGN(1.0D0, P2(1))
          RR2Z = 0.0D0
        ELSE
          RR2Y = P2(2)/PT2
          RR2Z = P2(3)/PT2
        ENDIF
        IF(P2(1).GE.0.D0) THEN
          PS2  = SQRT(P2(4) + P2(1))
          RPP2 = PT2/PS2
        ELSE
          RPP2 = SQRT(P2(4) - P2(1))
          PS2  = PT2/RPP2
        ENDIF
        R2Y = RR2Y*PS2
        R2Z = RR2Z*PS2
 
        RR  =  RPP2*R1Y-RPP1*R2Y
        RIMM = RPP2*R1Z-RPP1*R2Z
 
        AALL(2)=DCMPLX(RR,RIMM)
        AALL(1)=-DCONJG(AALL(2))
 
        RETURN
      END
C
C ********************************************************************
C *         S                                                        *
C *         |                                                        *
C *         |                                                        *
C * F1--<---|---<---F2    SUBROUTINE FFS                             *
C *                                                                  *
C *     Purpose: To calculate vertex amplitudes for scalor boson-    *
C *              massive fermions vertex.                            *
C *                                                                  *
C *     L=: Polarization state of vector boson.                      *
C *     I,II=: Indices to specify fermion(I,II=3) or antifermion     *
C *            (I,II=1) state.                                       *
C *     AM,AAM=: Masses of fermions.                                 *
C *     AL,AR=: Coupling constants for vertex.                       *
C *     C,CC=: Phase factors for massive fermions.                   *
C *     P1,P2,Q1,Q2=: Light-like vectors decomposed by subroutine    *
C *                   SPLTQ.                                         *
C *     AALL=: Calculated results of vertex amplitudes for all       *
C *            possible helicity states.                             *
C *                                                                  *
C *                                                                  *
C *                                      written by H. Tanaka        *
C ********************************************************************
C
C
C       ===========================================================
        SUBROUTINE FFS(II,I,AAM,AM,AL,AR,CC,C,Q1,Q2,P1,P2,AALL)
C       ===========================================================
C
C
C
C   FFS              CALCULATION OF FFS   MASSIVE CASE
C
C   I,II=1:V    I,II=3:U
C      J,JJ=1:HS=-   J,JJ=2:HS=+
C
CDD     SUBROUTINE FFS(L,II,I,AAM,AM,AL,AR,PP,P,AALL)
CXX     SUBROUTINE FFS(L,II,I,AAM,AM,AL,AR,CC,C,Q1,Q2,P1,P2,AALL)
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        DOUBLE COMPLEX AALL
        DOUBLE COMPLEX C,CC,AALL11,AALL22,AALL12,AALL21
        DIMENSION P1(4),P2(4),Q1(4),Q2(4),C(2),CC(2)
        DIMENSION AALL(2,2),AALL11(2),AALL22(2),AALL12(2),AALL21(2)
        CALL FFS0(Q1,P1,AALL11)
        CALL FFS0(Q2,P2,AALL22)
        CALL FFS0(Q1,P2,AALL12)
        CALL FFS0(Q2,P1,AALL21)
 
        R  = DFLOAT(I  - 2)
        RR = DFLOAT(II - 2)
        IF (AM.LE.0.0D0) THEN
          R = 0.0D0
        ENDIF
        IF (AAM.LE.0.0D0) THEN
          RR = 0.0D0
        ENDIF
 
        J1 = (5-II)/2
        J2 = 3 - J1
        K1 = (5-I)/2
        K2 = 3 - K1
 
        AALL(J1,K2)=(AR*AALL11(1))
     &               +(AL*R*RR*C(1)*CC(2)*AALL22(2))
        AALL(J2,K1)=(AL*AALL11(2))
     &               +(AR*R*RR*C(2)*CC(1)*AALL22(1))
        AALL(J2,K2)=-(AR*RR*CC(1)*AALL21(1))
     &                -(AL*R*C(1)*AALL12(2))
        AALL(J1,K1)=-(AL*RR*CC(2)*AALL21(2))
     &                -(AR*R*C(2)*AALL12(1))
 
 
CX      R=DFLOAT(I)-2.D0
CX      RR=DFLOAT(II)-2.D0
CX      IF (AM.LE.0.D0) R=0.D0
CX      IF (AAM.LE.0.D0) RR=0.D0
CX      IF (AAM.LE.0.D0) RR=0.D0
CX      IF (AM.LE.0.D0) R=0.D0
CX      IF (II.EQ.3.AND.I.EQ.3) THEN
CX      AALL(1,2)=AR*AALL11(1)+AL*R*RR*C(1)*CC(2)*AALL22(2)
CX      AALL(2,1)=AL*AALL11(2)+AR*R*RR*C(2)*CC(1)*AALL22(1)
CX      AALL(2,2)=-AR*RR*CC(1)*AALL21(1)-AL*R*C(1)*AALL12(2)
CX      AALL(1,1)=-AL*RR*CC(2)*AALL21(2)-AR*R*C(2)*AALL12(1)
CX      ELSE IF (II.EQ.3.AND.I.EQ.1) THEN
CX      AALL(1,1)=AR*AALL11(1)+AL*R*RR*C(1)*CC(2)*AALL22(2)
CX      AALL(2,2)=AL*AALL11(2)+AR*R*RR*C(2)*CC(1)*AALL22(1)
CX      AALL(2,1)=-AR*RR*CC(1)*AALL21(1)-AL*R*C(1)*AALL12(2)
CX      AALL(1,2)=-AL*RR*CC(2)*AALL21(2)-AR*R*C(2)*AALL12(1)
CX      ELSE IF (II.EQ.1.AND.I.EQ.3) THEN
CX      AALL(2,2)=AR*AALL11(1)+AL*R*RR*C(1)*CC(2)*AALL22(2)
CX      AALL(1,1)=AL*AALL11(2)+AR*R*RR*C(2)*CC(1)*AALL22(1)
CX      AALL(1,2)=-AR*RR*CC(1)*AALL21(1)-AL*R*C(1)*AALL12(2)
CX      AALL(2,1)=-AL*RR*CC(2)*AALL21(2)-AR*R*C(2)*AALL12(1)
CX      ELSE IF (II.EQ.1.AND.I.EQ.1) THEN
CX      AALL(2,1)=AR*AALL11(1)+AL*R*RR*C(1)*CC(2)*AALL22(2)
CX      AALL(1,2)=AL*AALL11(2)+AR*R*RR*C(2)*CC(1)*AALL22(1)
CX      AALL(1,1)=-AR*RR*CC(1)*AALL21(1)-AL*R*C(1)*AALL12(2)
CX      AALL(2,2)=-AL*RR*CC(2)*AALL21(2)-AR*R*C(2)*AALL12(1)
CX      END IF
C       WRITE(6,100) Q1(4),P1(4),Q(4)
CX
CX      WRITE(6,*) 'II,I,AAM,AM',II,I,AAM,AM
C100    FORMAT(2X,3D15.5)
 
        RETURN
      END
************************************************************************
cx      SUBROUTINE SawwV(L1,L2,L3,K1,K2,K3,CPL,P1,P2,P3,E1,E2,E3,LT,AV)
**    SUBROUTINE SawwV(L1,L2,L3,K1,K2,K3,CPL,g1,aeta,al,wm2,P1,P2,P3,
      SUBROUTINE smvww(L1,L2,L3,K1,K2,K3,CPL,g1,aeta,al,wm2,P1,P2,P3,
     &                 e1,e2,e3,lt,av)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0)
*   * dummy array size.
      PARAMETER (LTSIZE = 20, LASIZE = 1024)
      INTEGER    L1, L2, L3
      INTEGER    K1, K2, K3
      DOUBLE COMPLEX CPL
      DIMENSION  P1(4),    P2(4),    P3(4)
      DIMENSION  E1(4,L1), E2(4,L2), E3(4,L3)
      DOUBLE COMPLEX AV(0:LASIZE)
*     DOUBLE COMPLEX AV(0:L3*L2*L1-1)
      INTEGER    LT(0:LTSIZE)
*
*     Calculate vector-vector-vector vertex.
*
*           ! 3
*           V
*      -->--+--<---
*        2     1
*
*     L1,L2,L3 : input  : number of polarization vectors (2, 3 OR 4)
*     K1,K2,K3 : input  : if incoming momentum then 1 else -1
*     CPL      : input  : coupling constant.
*     P1,P2,P3 : input  : momentum vectors
*     E1,E2,E3 : input  : set of polarization vectors
*     AV       : output : table of amplitudes
*     LT       : output : table of sizes in AV
*
      DIMENSION  PP12(4), PP23(4), PP31(4)
      DIMENSION  PP1(4), PP2(4), PP3(4)
*-----------------------------------------------------------------------
c       common / mass / wm2
         
cx	 write(6,*) ' sawwv g1 aeta al wm2 ',g1,aeta,al,wm2
      LT(0) = 3
      LT(1) = L1
      LT(2) = L2
      LT(3) = L3
      DO 10 J = 1, 4
cx        PP12(J) = K1*P1(J) - K2*P2(J)
cx        PP23(J) = K2*P2(J) - K3*P3(J)
cx        PP31(J) = K3*P3(J) - K1*P1(J)
        PP1(J) = K1*P1(J)
        PP2(J) = K2*P2(J)
        PP3(J) = K3*P3(J)
cx	write(6,*) ' j p1 p2 p3    ',j,p1(j),p2(j),p3(j)
cx	write(6,*) ' j pp1 pp2 pp3 ',j,pp1(j),pp2(j),pp3(j)
   10 CONTINUE
cx	 write(6,*) ' g1 ae al wm2 ',g1,ae,al,wm2
      IA = 0
      DO 500 IL3 = 1, L3
cx        E3P1P2=E3(4,IL3)*PP12(4)-E3(1,IL3)*PP12(1)
cx     &        -E3(2,IL3)*PP12(2)-E3(3,IL3)*PP12(3)
      DO 500 IL2 = 1, L2
cx        E2P3P1=E2(4,IL2)*PP31(4)-E2(1,IL2)*PP31(1)
cx     &        -E2(2,IL2)*PP31(2)-E2(3,IL2)*PP31(3)
cx        E2E3=E2(4,IL2)*E3(4,IL3)-E2(1,IL2)*E3(1,IL3)
cx     &      -E2(2,IL2)*E3(2,IL3)-E2(3,IL2)*E3(3,IL3)
      DO 500 IL1 = 1, L1
cx        CALL VVV(1.0D0,PP1,PP2,PP3,E1(1,IL1),E2(1,IL2),E3(1,IL3),AV(ia))
cx        CALL VVV(1.d0,PP1,PP2,PP3,E1(1,IL1),E2(1,IL2),E3(1,IL3),AV0)
	     
cx 	 write(6,*) ' g1 aeta al wm2 ',g1,aeta,al,wm2
  	call wwva(1.d0,g1,aeta,al,wm2,pp1,pp2,pp3,e1(1,il1),e2(1,il2),
     &                   e3(1,il3),av0)  
**      call wwva(1.d0,g1,aeta,al,wm2,pp3,pp1,pp2,e3(1,il3),e1(1,il1),
**   &                   e2(1,il2),av0)  
cx        E1P2P3=E1(4,IL1)*PP23(4)-E1(1,IL1)*PP23(1)
cx     &        -E1(2,IL1)*PP23(2)-E1(3,IL1)*PP23(3)
cx        E1E2=E1(4,IL1)*E2(4,IL2)-E1(1,IL1)*E2(1,IL2)
cx     &      -E1(2,IL1)*E2(2,IL2)-E1(3,IL1)*E2(3,IL2)
cx        E1E3=E1(4,IL1)*E3(4,IL3)-E1(1,IL1)*E3(1,IL3)
cx     &      -E1(2,IL1)*E3(2,IL3)-E1(3,IL1)*E3(3,IL3)
cx        AV(IA) = CPL*(E3P1P2*E1E2+E1P2P3*E2E3+E2P3P1*E1E3)
          av(ia) = cpl*av0
          IA = IA + 1
cx	  write(6,*) ' ia av0 ',ia,av0
  500 CONTINUE
*     CALL CTIME('SMVVV ')
      RETURN
      END
C ********************************************************************
C *                                                                  *
C *          3 W(-) P3 EP3                                           *
C *          !                                                       *
C *          !                                                       *
C *  1 -->---!---<-- 2    SUBROUTINE WWVA                            *
C *  V              W(+)                                             *
C *  P1             P2                                               *
C *  EP1            EP2                                              *
C *                                                                  *
C *     Purpose: to calculate vertex amplitudes for three vector-    *
C *              boson vertex including anomalous couplings.         *
C *                                                                  *
C *     Comment : The momenta of particles with vertices are         *
C *               taken to flow in.                                  *
C *                                                                  *
C *     The constructed amplitude has the following form:            *
C *                                                                  *
C *     AALL=GG*( G1*(   (EP3.(P1-P2))*(EP1.EP2)                     *
C *                    + (EP1.(P2-P3))*(EP2.EP3)                     *
C *                    + (EP2.(P3-P1))*(EP1.EP3) )                   *
C *                                                                  *
C *         + AE*((EP1.EP3)*(EP2.P1) - (EP1.EP2)*(EP3.P1))           *
C *         + AL/WM2*((EP2.EP3)*((EP1.P3)*(P1.P2)-(EP1.P2)*(P1.P3))  *
C *                  +(EP1.EP3)*((EP2.P1)*(P2.P3)-(EP2.P3)*(P1.P2))  *
C *                  +(EP1.EP2)*((EP3.P2)*(P1.P3)-(EP3.P1)*(P2.P3))  *
C *                  +(EP1.P2)*(EP2.P3)*(EP3.P1)                     *
C *                  -(EP1.P3)*(EP2.P1)*(EP3.P2))                    *
C *                                                                  *
C *                                                                  *
C *     GG : coupling constant for the standard model.               *
C *     G1 :                                                         *
C *     AE : G1 - K                                                  *
C *     AL :                                                         *
C *     WM2: W boson mass squared.                                   *
C *     P1,P2,P3=: momenta of the vector bosons.                     *
C *     EP1,EP2,EP3=: polarization vectors of vector bosons.         *
C *     AALL=: calculated results of vertex amplitudes for given     *
C *            plarization states.                                   *
C *                                                                  *
C *                                                                  *
C *                                                                  *
C *                                                                  *
C *                                      written by H. Tanaka        *
C ********************************************************************
C
C
C
C        GG : COUPLING FOR STANDARD MODEL
C        G1 :
C        AE : G1 - K
C        AL :
C        WM2: W BOSON MASS SQUARED
C
C        ==========================================================
         SUBROUTINE WWVA(GG,G1,AE,AL,WM2,P1,P2,P3,EP1,EP2,EP3,AALL)
C        ==========================================================
C
         IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!         DIMENSION P1(4),P2(4),P3(3),EP1(4),EP2(4),EP3(4)
! this bug was corrected by M. Skrzypek Jun 8 1986. 
         DIMENSION P1(4),P2(4),P3(4),EP1(4),EP2(4),EP3(4)
         GG0 = 1.D0
         CALL VVV(GG0,P1,P2,P3,EP1,EP2,EP3,AALL0)
 
        E1E2=EP1(4)*EP2(4)-EP1(1)*EP2(1)-EP1(2)*EP2(2)-EP1(3)*EP2(3)
        E1E3=EP1(4)*EP3(4)-EP1(1)*EP3(1)-EP1(2)*EP3(2)-EP1(3)*EP3(3)
        E2E3=EP2(4)*EP3(4)-EP2(1)*EP3(1)-EP2(2)*EP3(2)-EP2(3)*EP3(3)
 
        P1P2=P1(4)*P2(4)-P1(1)*P2(1)-P1(2)*P2(2)-P1(3)*P2(3)
        P1P3=P1(4)*P3(4)-P1(1)*P3(1)-P1(2)*P3(2)-P1(3)*P3(3)
        P2P3=P2(4)*P3(4)-P2(1)*P3(1)-P2(2)*P3(2)-P2(3)*P3(3)
 
        E1P2=EP1(4)*P2(4)-EP1(1)*P2(1)-EP1(2)*P2(2)-EP1(3)*P2(3)
        E1P3=EP1(4)*P3(4)-EP1(1)*P3(1)-EP1(2)*P3(2)-EP1(3)*P3(3)
        E2P3=EP2(4)*P3(4)-EP2(1)*P3(1)-EP2(2)*P3(2)-EP2(3)*P3(3)
 
        E2P1=EP2(4)*P1(4)-EP2(1)*P1(1)-EP2(2)*P1(2)-EP2(3)*P1(3)
        E3P1=EP3(4)*P1(4)-EP3(1)*P1(1)-EP3(2)*P1(2)-EP3(3)*P1(3)
        E3P2=EP3(4)*P2(4)-EP3(1)*P2(1)-EP3(2)*P2(2)-EP3(3)*P2(3)
 
        AALL1 = E1E3*E2P1 - E1E2*E3P1
        AALL2 = E2E3*(E1P3*P1P2 - E1P2*P1P3)
        AALL3 = E1E3*(E2P1*P2P3 - E2P3*P1P2)
        AALL4 = E1E2*(E3P2*P1P3 - E3P1*P2P3)
        AALL5 = E1P2*E2P3*E3P1 - E1P3*E2P1*E3P2
 
        AALL = GG*(G1*AALL0+AE*AALL1
     &       + AL/WM2*(AALL2+AALL3+AALL4+AALL5))
 
        RETURN
        END
C
C ********************************************************************
C *          V4 EP4                                                  *
C *          |                                                       *
C *          |                                                       *
C * W- -->---|---<-- V3   SUBROUTINE WWVVA                           *
C * EP1      |      EP3                                              *
C *          |                                                       *
C *         W+ EP2                                                   *
C *                                                                  *
C *                                                                  *
C *     Purpose: to calculate vertex amplitudes for three vector-    *
C *              boson vertex including anomalous couplings.         *
C *                                                                  *
C *                                                                  *
C *     Comment : the constructed amplitude has the following form:  *
C *                                                                  *
C *    AALL=GG*(EP1.EP3*EP2.EP4+EP1.EP4*EP2.EP3-2.*EP1.EP2*EP3.EP4). *
C *                                                                  *
C *     GG=: coupling constant for vertex.                           *
C *     EP1,EP2,EP3,EP4=: polarization vectors for vector bosons.    *
C *     AALL=: calculated result of vertex amplitude for given       *
C *            plarization states.                                   *
C *                                                                  *
C *                                      written by H. Tanaka        *
C ********************************************************************
C
C
C       ==============================================================
        SUBROUTINE WWVVA(GG,G1,AE,AL,WM2,P1,P2,P3,P4,
     &                                         EP1,EP2,EP3,EP4,AALL)
C       ==============================================================
C
C
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        DIMENSION EP1(4),EP2(4),EP3(4),EP4(4)
        DIMENSION P1(4),P2(4),P3(4),P4(4) ,PS(4)
 
         PS(1) = P1(1)+P2(1)
         PS(2) = P1(2)+P2(2)
         PS(3) = P1(3)+P2(3)
         PS(4) = P1(4)+P2(4)
 
         GG0 = 1.D0
         CALL VVVV(GG0,EP1,EP2,EP3,EP4,AALL0)
CX       WRITE(6,*) ' GG G1 AE AL ',GG,G1,AE,AL
CX       WRITE(6,*) ' AALL0 ',AALL0
 
        E1E2=EP1(4)*EP2(4)-EP1(1)*EP2(1)-EP1(2)*EP2(2)-EP1(3)*EP2(3)
        E3E4=EP3(4)*EP4(4)-EP3(1)*EP4(1)-EP3(2)*EP4(2)-EP3(3)*EP4(3)
        E1E3=EP1(4)*EP3(4)-EP1(1)*EP3(1)-EP1(2)*EP3(2)-EP1(3)*EP3(3)
        E2E4=EP2(4)*EP4(4)-EP2(1)*EP4(1)-EP2(2)*EP4(2)-EP2(3)*EP4(3)
        E1E4=EP1(4)*EP4(4)-EP1(1)*EP4(1)-EP1(2)*EP4(2)-EP1(3)*EP4(3)
        E2E3=EP2(4)*EP3(4)-EP2(1)*EP3(1)-EP2(2)*EP3(2)-EP2(3)*EP3(3)
 
        P1P2=P1(4)*P2(4)-P1(1)*P2(1)-P1(2)*P2(2)-P1(3)*P2(3)
        P3P4=P3(4)*P4(4)-P3(1)*P4(1)-P3(2)*P4(2)-P3(3)*P4(3)
        P1P3=P1(4)*P3(4)-P1(1)*P3(1)-P1(2)*P3(2)-P1(3)*P3(3)
        P2P4=P2(4)*P4(4)-P2(1)*P4(1)-P2(2)*P4(2)-P2(3)*P4(3)
        P1P4=P1(4)*P4(4)-P1(1)*P4(1)-P1(2)*P4(2)-P1(3)*P4(3)
        P2P3=P2(4)*P3(4)-P2(1)*P3(1)-P2(2)*P3(2)-P2(3)*P3(3)
        PSPS=PS(4)*PS(4)-PS(1)*PS(1)-PS(2)*PS(2)-PS(3)*PS(3)
 
        E1P2=EP1(4)*P2(4)-EP1(1)*P2(1)-EP1(2)*P2(2)-EP1(3)*P2(3)
        E3P4=EP3(4)*P4(4)-EP3(1)*P4(1)-EP3(2)*P4(2)-EP3(3)*P4(3)
        E1P3=EP1(4)*P3(4)-EP1(1)*P3(1)-EP1(2)*P3(2)-EP1(3)*P3(3)
        E2P4=EP2(4)*P4(4)-EP2(1)*P4(1)-EP2(2)*P4(2)-EP2(3)*P4(3)
        E1P4=EP1(4)*P4(4)-EP1(1)*P4(1)-EP1(2)*P4(2)-EP1(3)*P4(3)
        E2P3=EP2(4)*P3(4)-EP2(1)*P3(1)-EP2(2)*P3(2)-EP2(3)*P3(3)
        E1PS=EP1(4)*PS(4)-EP1(1)*PS(1)-EP1(2)*PS(2)-EP1(3)*PS(3)
        E2PS=EP2(4)*PS(4)-EP2(1)*PS(1)-EP2(2)*PS(2)-EP2(3)*PS(3)
        E3PS=EP3(4)*PS(4)-EP3(1)*PS(1)-EP3(2)*PS(2)-EP3(3)*PS(3)
        E4PS=EP4(4)*PS(4)-EP4(1)*PS(1)-EP4(2)*PS(2)-EP4(3)*PS(3)
 
        E2P1=EP2(4)*P1(4)-EP2(1)*P1(1)-EP2(2)*P1(2)-EP2(3)*P1(3)
        E4P3=EP4(4)*P3(4)-EP4(1)*P3(1)-EP4(2)*P3(2)-EP4(3)*P3(3)
        E3P1=EP3(4)*P1(4)-EP3(1)*P1(1)-EP3(2)*P1(2)-EP3(3)*P1(3)
        E4P2=EP4(4)*P2(4)-EP4(1)*P2(1)-EP4(2)*P2(2)-EP4(3)*P2(3)
        E4P1=EP4(4)*P1(4)-EP4(1)*P1(1)-EP4(2)*P1(2)-EP4(3)*P1(3)
        E3P2=EP3(4)*P2(4)-EP3(1)*P2(1)-EP3(2)*P2(2)-EP3(3)*P2(3)
 
        AALL1 = E3E4*PSPS
        AALL2 = E1E3*E2E4*(P2P3+P1P4)+E1E4*E2E3*(P1P3+P2P4)
        AALL3 = E1E2*(E4P3*E3PS+E3P4*E4PS)
        AALL4 = E3E4*(E2P1*E1PS+E1P2*E2PS)
        AALL5 = E2E4*(E1P3*E3P2+E1P4*E3P1-E1P4*E3P2+E3P4*E1P2)
        AALL6 = E1E4*(-E2P4*E3P1+E3P4*E2P1+E2P3*E3P1+E2P4*E3P2)
        AALL7 = E2E3*(-E4P2*E1P3+E1P2*E4P3+E4P1*E1P3+E4P2*E1P4)
        AALL8 = E1E3*(-E4P1*E2P3+E2P1*E4P3+E2P3*E4P2+E2P4*E4P1)
 
CX       WRITE(6,*) ' AALL0 ',AALL0
        AALL = GG*( G1*AALL0
     &              + AL/WM2*(AALL1+AALL2+AALL3
CX   &              - AL/WM2*(AALL1+AALL2+AALL3 ???
     &                          - AALL4-AALL5-AALL6-AALL7-AALL8))
CX       WRITE(6,*) ' AALL  ',AALL
        RETURN
        END
C ******************************************************************
C *                                                                *
C *  SUBROUTINE POLP(I:I*4, A:R*8 , AM:R*8, P:R*8(4),              *
C *                                       EP:R*8(4), EM:R*8(4))    *
C *                                                                *
C *     Purpose: To set components of polarization vectors         *
C *              for QED vertex with on-shell fermions.            *
C *                                                                *
C *              -                                                 *
C *              U(P2)(EP)U(P1) vertex with P1**2=P2**2=m**2       *
C *                                                                *
C *                                                                *
C *     I= : Polarization states   I=1,2 : Transverse              *
C *                                I=3   : Longitudinal            *
C *                                I=4   : Scalar                  *
C *                                                                *
C *     (*) I=5: EP(P)=P for massless gauge bosons.                *
C *         ( For check of gouge invarience for external           *
C *           massless gouge boson legs.)                          *
C *                                                                *
C * X   A = : Gauge parametor                                      *
C *     AM= : Mass of vector boson                                 *
C *     P = : Momentum of vector boson                             *
C *     EP= : Polarization vector for state I                      *
C *     EM= : Weight factors                                       *
C *                                                                *
C *                                                                *
C *                                                                *
C *     Note                                                       *
C *     ----                                                       *
C *                                                                *
C *     Use only to QED vertex.  Gauge invarience can not          *
C *     be cheched in this subroutine.                             *
C *                                    written by H. Tanaka        *
C ******************************************************************
C
C
C              ===================================
               SUBROUTINE POLP(I,A,AM,P,EP,EM)
C              ===================================
C
C
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        DIMENSION P(4),EP(4),EM(4)
        DATA DD/0.D0/

        PT2 = P(1)**2 + P(2)**2
        PP  = (P(4) - P(3))*(P(4) + P(3)) - PT2
        IF (PT2.LE.DD) THEN
          PT  = 0.0D0
          RTY = 1.0D0
          RTX = 0.0D0
        ELSE
          PT  = SQRT(PT2)
          PTI = 1.0D0/PT
          RTY = P(2)*PTI
          RTX = P(1)*PTI
        END IF

        PN2 = PT2 + P(3)**2
        IF (PN2.LE.DD) THEN
          PN  = 0.0D0
          RNX = 0.0D0
          RNY = 0.0D0
          RNZ = 1.0D0
          RTN = 0.0D0
        ELSE IF (PN2.GT.DD) THEN
          PN  = SQRT(PN2)
          PNI = 1.0D0/PN
          RNX = P(1)*PNI
          RNY = P(2)*PNI
          RNZ = P(3)*PNI
          RTN = PT*PNI
        END IF

        IF (I.EQ.1) THEN
          EP(1) =  RTX*RNZ
          EP(2) =  RTY*RNZ
          EP(3) = -RTN
          EP(4) =  0.0D0
          EM(1) =  1.0D0
CX        EM(1) =  PROP
        ELSE IF (I.EQ.2) THEN
          EP(1) = -RTY
          EP(2) =  RTX
          EP(3) =  0.0D0
          EP(4) =  0.0D0
          EM(2) =  1.0D0
CX        EM(2) =  PROP
        ELSE IF (I.EQ.3) THEN
          APP=ABS(PP)
          IF (APP.LE.DD) THEN
            EP(1) = 0.0D0
            EP(2) = 0.0D0
            EP(3) = 0.0D0
            EP(4) = 0.0D0
            EM(3) = 0.0D0
          ELSE
            EP(1) = 0.D0
            EP(2) = 0.D0
            EP(3) = 0.D0
            EP(4) = -SQRT(APP)/PN
            EM(3) = SIGN(1.0D0, PP)
CX          EM(3) = SIGN(1.0D0, PP)*PROP
          ENDIF
        ELSE IF (I.EQ.4) THEN
            EP(1) = 0.D0
            EP(2) = 0.D0
            EP(3) = 0.D0
            EP(4) = 0.D0
            EM(4) = 0.0D0
        ELSE IF (I.EQ.5) THEN
          EP(1) = P(1)
          EP(2) = P(2)
          EP(3) = P(3)
          EP(4) = P(4)
        END IF

        RETURN
        END
* File smcpol.f
* 
*          Fortran source code generator
* 
************************************************************************
      subroutine smcpol(la, lvt, avt)
      implicit DOUBLE PRECISION(a-h,o-z)
      parameter (ltsize = 20, lasize = 1024 )
**    include 'incl1.f'
**    include 'inclk.f'
**    integer    lvt(0:nextn)
      integer    lvt(0:ltsize)
**    DOUBLE COMPLEX avt(0:lag-1)
      DOUBLE COMPLEX avt(0:lasize)
*
**    integer j(nextn), jv(nextn)
      integer j(ltsize), jv(ltsize)
      DOUBLE COMPLEX zi, zr, ztv, ztw
      data ifst/0/
      save ifst, zr, zi
*-----------------------------------------------------------------------
      if(ifst.eq.0) then
        ifst = 1
        r2 = 1.0d0/sqrt(2.0d0)
        zr = dcmplx(1.0d0,0.0d0)*r2
        zi = dcmplx(0.0d0,1.0d0)*r2
      endif

      nextn = lvt(0)
      print *,'smcpol:nextn',nextn
      ja = la
*     if(lt(ja) .ne. 2) then
*       write(*,*) 'smcpol: cannot calculate circlar polarization'
*       write(*,*) 'lt = ', lt
*       write(*,*) 'lt(',ja,') = ', lt(ja), ' <> 2'
*       stop
*     endif
      if(lvt(ja) .ne. 2) then
        write(*,*) 'smcpol: cannot calculate circlar polarization'
        write(*,*) 'lt = ', lvt
        write(*,*) 'lt(',ja,') = ', lvt(ja), ' <> 2'
        stop
      endif

      ibas  = 1
      do 10 i = 1, nextn
        jv(i) = ibas
        ibas  = ibas*lvt(i)
        j(i)  = 0
   10 continue

  100 continue
        iv = 0
        do 110 i = 1, nextn
          iv = iv +   jv(i)*j(i)
  110   continue
        iw = iv + jv(ja)

        ztv =   zr*avt(iv) + zi*avt(iw)
        ztw =   zr*avt(iv) - zi*avt(iw)
        avt(iv) = ztv
        avt(iw) = ztw

c       write(*,*) 'smcpol:', j
c       write(*,*) 'smcpol:', iv, iw, avt(iv)
c       write(*,*) 'smcpol:', iw, iv, avt(iw)

        ii = 1
  120   continue
          if(ii .eq. ja) then
            ii = ii + 1
            if(ii.gt.nextn) then
              goto 190
            endif
          endif
          j(ii) = j(ii) + 1
          if(j(ii).ge.lvt(ii)) then
            j(ii) = 0
            ii = ii + 1
            if(ii.le.nextn) then
              goto 120
            else
              goto 190
            endif
          endif
        goto 100
  190 continue
      return
      end

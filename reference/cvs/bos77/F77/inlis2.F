      FUNCTION INLIS2(K1,K2,LIST,M,N)

*     SEARCH LIST(1...M,1) ... LIST(1...M,N) FOR ENTRY WITH
*     K1=LIST(1,I) AND K2=LIST(2,I). IF FOUND, INDEX I IS RETURNED.
*     IF NOT FOUND, RETURN NEGATIVE VALUE OF INDEX, THE ENTRY K1,K2
*     WOULD GET, IF IT IS ADDED TO THE LIST.
*     POTENTIAL VALUES OF THE FUNCTION ARE 1...N AND -N-1...-1.
*     THE LIST HAS TO BE SORTED.

      INTEGER K1,K2,LIST(M,N)

      IF(N.EQ.0.OR.K1.LT.LIST(1,1).
     +   OR.(K1.EQ.LIST(1,1).AND.K2.LT.LIST(2,1))) THEN
         I=-1
      ELSE IF(K1.GT.LIST(1,N).
     +    OR.(K1.EQ.LIST(1,N).AND.K2.GT.LIST(2,N))) THEN
         I=-N-1
      ELSE
*        ENTER SEARCH LOOP
         IA=1
         IB=N
   10    IF(IB-IA.GT.10) THEN
*           BINARY SEARCH
            II=(IA+IB)/2
            IF (K1.GT.LIST(1,II).
     +      OR.(K1.EQ.LIST(1,II).AND.K2.GE.LIST(2,II))) THEN
               IA=II
            ELSE
               IB=II
            END IF
            GOTO 10
         ELSE
*           SEQUENTIAL COMPARISON
            DO 20 I=IA,IB
            IF (K1.LT.LIST(1,I).
     +      OR.(K1.EQ.LIST(1,I).AND.K2.LE.LIST(2,I))) GOTO 30
   20       CONTINUE
   30       IF(K1.NE.LIST(1,I).OR.K2.NE.LIST(2,I)) I=-I
         END IF
      END IF
      INLIS2=I
  100 RETURN
      END

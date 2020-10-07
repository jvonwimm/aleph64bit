      FUNCTION INLIST(K,LIST,N)

*     SEARCH LIST(1)...LIST(N) FOR ENTRY K AND RETURN INDEX, IF FOUND.
*     THE LIST HAS TO BE SORTED IN SCENDING ORDER,
*     IF NOT FOUND, RETURN NEGATIVE VALUE OF INDEX, THE ENTRY K WOULD
*     GET, IF IT IS ADDED TO THE LIST.
*     POTENTIAL VALUES OF THE FUNCTION ARE 1...N AND -N-1...-1.

*     CODE TO INSERT NEW ENTRY
*     IF(I.LT.0) THEN
*        I=-I
*        DO 40 J=N,I,-1
*  40    LIST(J+1)=LIST(J)
*        LIST(I)=K
*        N=N+1
*     END IF

      INTEGER K,LIST(*)

      IF(N.EQ.0.OR.K.LT.LIST(1)) THEN
         I=-1
      ELSE IF(K.GT.LIST(N)) THEN
         I=-N-1
      ELSE
*        ENTER SEARCH LOOP
         IA=1
         IB=N
   10    IF(IB-IA.GT.10) THEN
*           BINARY SEARCH
            II=(IA+IB)/2
            IF(K.GE.LIST(II)) THEN
               IA=II
            ELSE
               IB=II
            END IF
            GOTO 10
         ELSE
*           SEQUENTIAL COMPARISON
            DO 20 I=IA,IB
            IF(K.LE.LIST(I)) GOTO 30
   20       CONTINUE
   30       IF(K.NE.LIST(I)) I=-I
         END IF
      END IF
      INLIST=I
  100 RETURN
      END

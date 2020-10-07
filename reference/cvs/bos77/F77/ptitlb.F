      SUBROUTINE PTITLB(IND)
      COMMON/HCS/KW(100)

      IF(KW(IND+3).EQ.0) THEN
         WRITE(KW(6),101)
      ELSE
         WRITE(KW(6),101) (KW(IND+I),I=3,20)
      END IF
      IF(KW(IND+1).EQ.0) THEN
         WRITE(KW(6),102) KW(IND-3),KW(IND-2)
      ELSE
         ID=MOD(KW(IND-2),100)
         WRITE(KW(6),103) KW(IND-3),KW(IND+1),KW(IND+2),ID
      END IF
  100 RETURN
  101 FORMAT('0------------------ ',18A4)
  102 FORMAT(' U',A4,2X,I11)
  103 FORMAT(' U',A4,2X,A4,A4,I3)
      END

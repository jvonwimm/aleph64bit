C --------- Start of /QNMACR/ : statement functions for the NANO -------
      KTNO(I)=IAND(IW(KONDTK+LMHLEN+(I-KFCHT)*KCNDTK+JNDTTA),255)
      QV0PCH(I)=FLOAT(IAND(IW(KONDV0+LMHLEN+(I-KFV0T)*KCNDV0+JNDVTT)
     &  ,65335))/10.0
      QV0NCH(I)=FLOAT(IAND(ISHFT(IW(KONDV0+LMHLEN+(I-KFV0T)*KCNDV0
     &  +JNDVTT),-16),65335))/10.0
C ----------- End of /QNMACR/ : statement functions for the NANO -------

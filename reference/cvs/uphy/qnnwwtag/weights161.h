C
C.....Number of layers
      DATA NIVMAX161/3/
C
C.....NN architecture : NEURON161
      DATA(NEURON161(I),I=1,3)/17,15,1/          
C
C
C.....minima and maxima of inputs : XMIN161, XMAX161
      DATA (XMIN161(I),I=1,17)/
     & -.20236039E+01, -.13387649E+01, -.17792292E+01, -.63913852E+00,
     & -.11446266E+01, -.29451702E+01, -.21491005E+01, -.22405429E+01,
     & -.26794624E+01, -.19968712E+01, -.19796196E+01, -.27074947E+01,
     & -.18956158E+01, -.11005187E+01, -.10856961E+01, -.12663407E+01,
     & -.16731943E+01/      
       DATA (XMAX161(I),I=1,17)/
     &  .48866057E+01,  .50873027E+01,  .61401505E+01,  .41865330E+01,
     &  .14975133E+02,  .35604715E+01,  .30328522E+01,  .29008322E+01,
     &  .29021628E+01,  .37519150E+01,  .39637418E+01,  .49309931E+01,
     &  .23961115E+01,  .31961763E+01,  .41640482E+01,  .32054739E+01,
     &  .21462069E+01/
C
C
C.....Mean values of 17 Inputs
      DATA (SOMOY161(I),I=1,17)/
     &  .25793794E+00,  .15294807E+00,  .16856425E+00,  .64270506E+01,
     &  .13252462E+02,  .32560177E+02,  .39993675E+02,  .45192642E+02,
     &  .54110298E+02,  .14199500E+02,  .11991667E+02,  .53103783E+02,
     &  .43298319E+00,  .24484414E+00, -.60707533E+00, -.43975127E+00,
     & -.13067810E+00/
C
C
C.....Standard deviations of 17 Inputs
      DATA (SOM161(I),I=1,17)/
     &  .13280238E-01,  .12754641E-01,  .75648255E-02,  .10111916E+03,
     &  .12939885E+03,  .11614143E+03,  .24650600E+03,  .26225992E+03,
     &  .27881613E+03,  .43693298E+02,  .30829294E+02,  .73568733E+02,
     &  .51695779E-01,  .49455673E-01,  .13096577E+00,  .19573145E+00,
     &  .26991597E+00/  
C
C
C......Weights : WPOIDS161
C      NN weights for : Input layer --> Hidden layer
       DATA ((WPOIDS161(1,I,J),J=1,15),I=1,18)/
C      Weights between Input 1 and Hidden layer
     &  .62226E+00,-.28066E+01,-.41506E+00, .30788E+01,-.93083E+00,
     & -.73552E-01,-.98520E+00, .13321E+01, .12833E+01, .43583E+01,
     &  .53258E-01, .10806E+01, .14168E+01, .22626E+01, .10786E+01,
C      Weights between Input 2 and Hidden layer 
     &  .13649E+01,-.40954E+01, .32600E+01, .38661E+01,-.21319E+01,
     &  .24556E+01,-.22109E+01, .26577E+01, .27024E+01, .37491E+01,
     &  .13583E+00, .24150E+01, .29988E+01, .81082E+01, .23485E+01,
C      Weights between Input 3 and Hidden layer 
     &  .13778E+00,-.28962E+01,-.30031E+01, .19624E+01,-.26416E+00,
     & -.15752E+01,-.32063E+00, .41718E+00, .38705E+00, .24918E+01,
     & -.56477E-02, .27755E+00, .45752E+00, .41100E+00, .33931E+00, 
C      Weights between Input 4 and Hidden layer
     &  .37997E-01,-.70866E+00,-.48995E+00,-.15239E+01,-.59513E-01,
     &  .32799E-01,-.45385E-01,-.10148E+00,-.47281E-01, .68607E+00,
     &  .22362E-01,-.13320E-01,-.47606E-01,-.98826E+00, .95648E-02,  
C      Weights between Input 5 and Hidden layer
     & -.79002E-01,-.51659E+00,-.10778E+01, .22072E-01, .23176E+00,
     &  .21514E+01, .11306E+00,-.57563E-01,-.12865E+00,-.12716E+01,
     & -.38313E-01,-.25893E+00,-.25968E+00,-.13593E+01,-.13847E+00,
C      Weights between Input 6 and Hidden layer
     & -.28987E+00,-.11528E+00,-.25551E+01,-.56175E+00, .48632E+00,
     &  .68460E+00, .45123E+00,-.74304E+00,-.68902E+00,-.12472E+01,
     & -.16405E-01,-.58676E+00,-.73372E+00,-.42974E+00,-.56647E+00,
C      Weights between Input 7 and Hidden layer
     & -.52085E+00, .11614E+01,-.30144E+01,-.10757E+01, .67887E+00,
     & -.53417E+00, .73500E+00,-.11346E+01,-.10789E+01,-.50648E+01,
     & -.93656E-02,-.78738E+00,-.11633E+01,-.17320E+01,-.78856E+00,
C      Weights between Input 8 and Hidden layer
     & -.42976E+00, .17815E+01,-.33672E+01,-.15286E+01, .58786E+00,
     & -.60538E+01, .68567E+00,-.83851E+00,-.84131E+00,-.83417E+00,
     &  .11895E-01,-.69477E+00,-.89223E+00,-.27422E+01,-.69423E+00,
C      Weights between Input 9 and Hidden layer
     & -.38058E+00, .11781E+01,-.20321E+01,-.14751E+01, .59889E+00,
     & -.44567E+01, .64076E+00,-.89002E+00,-.90705E+00,-.22668E+01,
     & -.50865E-01,-.74263E+00,-.92478E+00,-.29681E+00,-.71478E+00,
C      Weights between Input 10 and Hidden layer
     & -.13065E+00,-.82319E+00,-.84263E+00,-.19756E+00, .17884E+00,
     & -.71881E+01, .87862E-01,-.18381E+00,-.14396E+00, .53544E+01,
     & -.67766E-01,-.28068E+00,-.24070E+00,-.75238E+00,-.16751E+00,
C      Weights between Input 11 and Hidden layer
     & -.20800E+00, .40288E+01, .13030E+00,-.12416E+01, .29134E+00,
     &  .10672E+01, .36941E+00,-.40547E+00,-.36045E+00, .61154E+01,
     & -.42344E-01,-.35846E+00,-.36820E+00,-.19872E+01,-.36437E+00,
C      Weights between Input 12 and Hidden layer
     & -.10105E+01, .25461E+01,-.81181E+00,-.10919E+01, .14021E+01,
     & -.16468E+01, .15411E+01,-.19479E+01,-.19094E+01,-.50445E+01,
     & -.27101E-01,-.15964E+01,-.20990E+01,-.36991E+01,-.16340E+01,
C      Weights between Input 13 and Hidden layer
     &  .50219E+00,-.14824E+01, .56188E+01, .28739E+01,-.71260E+00,
     &  .50295E+00,-.79910E+00, .90012E+00, .92706E+00, .11640E+01,
     & -.13426E-01, .85935E+00, .10943E+01, .11973E+01, .90427E+00,
C      Weights between Input 14 and Hidden layer
     & -.64603E+00, .20805E+01,-.44311E+00, .33225E+00, .96000E+00,
     & -.32382E+01, .11082E+01,-.14212E+01,-.14543E+01,-.19943E+01,
     & -.14780E-01,-.11440E+01,-.15764E+01,-.12518E+01,-.11697E+01,
C      Weights between Input 15 and Hidden layer
     &  .11778E+00, .36718E+00,-.61071E+00,-.10965E+01,-.11323E+00,
     &  .68879E+01,-.19132E+00, .27647E+00, .26536E+00,-.88280E+00,
     & -.43040E-01, .23126E+00, .34317E+00, .33252E+00, .26092E+00,
C      Weights between Input 16 and Hidden layer
     &  .40499E+00,-.22162E+01, .11529E+01, .49865E+00,-.54761E+00,
     &  .25638E+01,-.54438E+00, .73741E+00, .76232E+00, .90460E+00,
     &  .58210E-02, .62964E+00, .87638E+00, .96434E+00, .62553E+00,
C      Weights between Input 17 and Hidden layer
     &  .46774E+00,-.23139E+01, .37155E+01, .23027E+01,-.64111E+00,
     &  .35296E+01,-.71025E+00, .85873E+00, .91880E+00, .11261E+00,
     & -.21844E-01, .68727E+00, .10102E+01,-.11716E+01, .72643E+00,
C      Weights between threshold Input and Hidden layer
     &  .55474E+00,-.94717E+00, .19601E+01, .17378E+01,-.82665E+00,
     & -.18662E+01,-.94645E+00, .11292E+01, .11180E+01, .27252E+01,
     & -.14286E-02, .91181E+00, .12677E+01, .29628E+01, .10459E+01/
C
C
C......NN weights for : Hidden layer --> Output layer
       DATA (WPOIDS161(2,I,1),I=1,16)/
C      Weight between Hidden neuron 1 and Output layer
     & -.56301E-01,
C      Weight between Hidden neuron 2 and Output layer
     &  .10775E+01,
C      Weight between Hidden neuron 3 and Output layer
     & -.10107E+01,
C      Weight between Hidden neuron 4 and Output layer
     & -.68014E+00,
C      Weight between Hidden neuron 5 and Output layer
     &  .12727E+00 ,
C      Weight between Hidden neuron 6 and Output layer
     & -.17650E+01,
C      Weight between Hidden neuron 7 and Output layer
     &  .14052E+00, 
C      Weight between Hidden neuron 8 and Output layer
     & -.20598E+00,
C      Weight between Hidden neuron 9 and Output layer
     & -.18954E+00,
C      Weight between Hidden neuron 10 and Output layer
     & -.11909E+01,
C      Weight between Hidden neuron 11 and Output layer
     & -.48586E-01,
C      Weight between Hidden neuron 12 and Output layer
     & -.75418E-01,
C      Weight between Hidden neuron 13 and Output layer
     & -.20917E+00,
C      Weight between Hidden neuron 14 and Output layer
     & -.91044E+00,
C      Weight between Hidden neuron 15 and Output layer
     & -.14910E+00,
C      Weight between threshold Hidden neuron and Output layer
     & -.18476E+01/ 
C

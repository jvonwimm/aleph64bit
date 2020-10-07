#ifdef __unix__
#include "unix_externals_defs.h"
#include "unix_common_defs.h"
#endif /* unix */
/*----------------------------------------------------------------------------*/
struct dcftvt_s
{
	char tfildc[8];
	char titldc[10];
	char tdefdc[3];
	char tnamdc[4];
};
extern struct dcftvt_s dcftvt;
/*----------------------------------------------------------------------------*/
struct ddefco_s
{
	float rdcodd[128],grcodd[128],blcodd[128];
	int ltabdd[128];
};
extern struct ddefco_s ddefco;
struct ddefcn_s
{
        int numcdd;
};
extern struct ddefcn_s ddefcn;
/*----------------------------------------------------------------------------*/
#define mpnpdd 154
struct ddeco1_s
{
	int mxsadd,mxludd,mxvddd,mxitdd,mxtpdd,iclvdd,icbpdd,
        icsadd,icludd,icvddd,icitdd,ictpdd,icecdd,icmadd,
        ichcdd,icm1dd,icm2dd,icgldd,icbgdd,icbadd,iccndd,
        modedd,nupodd,istydd,lidtdd,litrdd,ligldd,isthdd,
        icfrdd,ictxdd,icaldd,ichddd,icbwdd,icrzdd,iclfdd,
        icvxdd,lcdudd,lcvddd,lcv1dd,lcitdd,lciudd,lcnbdd,
        lcncdd,lcnidd,lcshdd,lcsldd,lcsadd,lcsrdd,lcssdd,
        lcsddd,lctpdd,lctbdd,lctcdd,lctfdu,lctudd,lctrdd,
        lctwdd,lclgdd,lclcdd,lclhdd,lclkdd[3]           ,
        lceodd,lcegdd,lcecdd,lceldd,lcekdd[3]           ,
        lcesdd,lcwedd[3]           ,lcetdd,lcmudd[2]    ,
        lchgdd,lchcdd,lchldd,lchtdd[4]                  ,
        ncoldd[8]                  ,ldumdd[5]    ,moltdd,
        jstrdd,jssadd,jsvddd,jsitdd,jstpdd,jslcdd,jsecdd,
        jseodd,jshcdd,jsd1dd[5]                         ,
        jsd2dd[4]                  ,kcbpdd,kcsadd,kcludd,
        kcvddd,kcitdd,kctpdd,kcecdd,kcdudd,kchcdd,kcm1dd,
        kcm2dd,kclgdd,k129dd,ksdbdd,ksaidd,ksaodd,kslbdd,
        k134dd[6],                                k140dd,
        nfvhdd,nfvcdd,nfvedd,nfitdd,nftpdd,nfecdd,nfhtdd,
        n148dd,nftrdd,n150dd,n151dd,n152dd,n153dd,n154dd;
};
extern struct ddeco1_s ddeco1;
struct ddeco2_s
{
	float pdcodd[4][mpnpdd];
};
extern struct ddeco2_s ddeco2;
struct ddeco3_s
{
	float dlindd;
};
extern struct ddeco3_s ddeco3;
struct ddecot_s
{
	char tdcodd[mpnpdd][2];
};
extern struct ddecot_s ddecot;
/*----------------------------------------------------------------------------*/
struct devtic_s
{
	int nfilde,irunde[2],ievtde[2],lninde[2],lclsde;
};
extern struct devtic_s devtic;
/*----------------------------------------------------------------------------*/
struct dgrdec_s
{
	float hmindg[15],vmindg[15],
		hlowdg[15],vlowdg[15],hhghdg[15],vhghdg[15];
};
extern struct dgrdec_s dgrdec;
struct dgrdet_s
{
	char tgradg[3];
};
extern struct dgrdet_s dgrdet;
/*----------------------------------------------------------------------------*/
struct dmacrc_s
{
	logical fmacdm;
	int nletdm;
	logical fmopdm,fmindm,fanidm,fntadm,finmdm;
};
extern struct dmacrc_s dmacrc;
/*----------------------------------------------------------------------------*/
struct dopr1c_s
{
	int imaxdo,idu1do[2],iaredo,ipicdo,izomdo,idu2do[2];
};
extern struct dopr1c_s dopr1c;
/*----------------------------------------------------------------------------*/
struct dopr2t_s
{
	char taredo[15][2];
};
extern struct dopr2t_s dopr2t;
/*----------------------------------------------------------------------------*/
struct dtvccc_s
{
	float dumydt[4],humidt,humadt,vumidt,vumadt;
	int nsqudt,nobjdt,nocldt;
	float hposdt,vposdt,fposdt,tposdt,arotdt,rposdt;
	int ntvidt;
	logical fpridt,fretdt,fblwdt,fmondt,f256dt,fx11dt;
};
extern struct dtvccc_s dtvccc;
/*----------------------------------------------------------------------------*/
#define mpnpdu 60
struct dusdac_s
{
	int nunidu;
	float waitdu,ecchdu,boxsdu,ecsqdu,hcsqdu,	/* The float array */
		himxdu,rescdu,slowdu,wisudu,settdu,		/* paradu[mpnpdu]  */
		uslvdu,sylrdu,sdatdu,rzdhdu,rotadu,		/* is equivalent to*/
		scmmdu,clmmdu,cummdu,wimidu,ccmidu,		/* these floats -  */
		ecmidu,hcmidu,shtfdu[2]    ,ftdzdu,		/* ignored because */
		frnldu,dacudu,shecdu,shhcdu,drlidu,		/* paradu is not   */
		sqlcdu,chlcdu,headdu,vrdzdu,chhcdu,		/* used in fortran */
		areadu,dfwidu[2]    ,dflgdu,pbugdu,		/* version of this */
		chtfdu,rncldu,pr43du,pr44du,pr45du,		/* file.           */
		dsofdu,tpofdu,rovddu,roecdu,rohcdu,
		hstrdu,realdu,pr53du,pr54du,pr55du,
        pr56du[4],                  pr60du;
};
extern struct dusdac_s dusdac;
/*----------------------------------------------------------------------------*/
#define mposdw 30
#define mpnwdw 12
#define mnuwdw 14  /* Must be mpnwdw + 2 */
struct dwindc_s
{
	int nwindw[mpnwdw+1][mpnwdw+1],
		nextdw[mpnwdw+1],lastdw[mpnwdw+1];
	float posidw[mposdw];
};
extern struct dwindc_s dwindc;
struct dwindo_s
{
	char twindw[mnuwdw+1];
};
extern struct dwindo_s dwindo;
/*----------------------------------------------------------------------------*/
struct dwinsu_s
{
	float wisudw;
};
extern struct dwinsu_s dwinsu;
/*----------------------------------------------------------------------------*/
/* Now follow the commons from dali_uis.inc and the other *uis.for files.     */
/*----------------------------------------------------------------------------*/
#define mwindu 6
#define mpnwdu 13
#define mersdu 10
struct duiscc_s
{
	int indvcm,idcmdu,idvddu[mwindu],idwddu[mwindu];
	logical fuisdu;
	int idsgdu[mpnwdu+1],idesdu[mersdu],nersdu,
		iwesdu[mersdu],lersdu,iocldu[7 *(mpnwdu+1)];
	logical fersdu,fpuidu,fopsdu,fuixdu,fnrzdu,fmetdu,fpswdu;
};
extern struct duiscc_s duiscc;
/*----------------------------------------------------------------------------*/
#define mxcudx 5
struct dxpntr_s
{
	int nxcudx;
	logical fupodx[mxcudx * 2];
	int hsphdx[mxcudx],hspvdx[mxcudx];
	char poindx[mxcudx][2][64];
};
extern struct dxpntr_s dxpntr;
/*----------------------------------------------------------------------------*/
struct helptx2_s
{
	logical flthlp;  /* Note this common has been changed extensively */
};
extern struct helptx2_s helptx2;
/*----------------------------------------------------------------------------*/
struct cmflag_s
{
	char commnd[8];
	char ton;
};
extern struct cmflag_s cmflag;
/*----------------------------------------------------------------------------*/
struct astcm1_s
{
	int butsta, keybf1[4];
	logical fbox;
	float lowx, lowy, pxx[5], pyy[5], hpopos, vpopos;
};
extern struct astcm1_s astcm1;
/*----------------------------------------------------------------------------*/

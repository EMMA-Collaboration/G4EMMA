/*  -- translated by f2c (version 20100827).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif

#include <f2c.h>
//#include "/usr/local/include/f2c.h"


/* Common Block Declarations */

struct {
    doublereal a, b, phi, alpha, beta, xcr1, xcr2;
} mitray_dipo__;

#define mitray_dipo__1 mitray_dipo__

struct {
    doublereal rb, xc_offset__, zc_offset__;
} mitray24_;

#define mitray24_1 mitray24_

union {
    struct {
	doublereal bx, by, bz, k, tc[6], dtc[6];
    } _1;
    struct {
	doublereal bx, by, bz, tc[6], dtc[6];
    } _2;
} mitray10_;

#define mitray10_1 (mitray10_._1)
#define mitray10_2 (mitray10_._2)

union {
    struct {
	doublereal ndx, bet1, gama, delt;
    } _1;
    struct {
	doublereal ec2, ec4, we, wc;
    } _2;
} mitray20_;

#define mitray20_1 (mitray20_._1)
#define mitray20_2 (mitray20_._2)

struct {
    doublereal rca, dels, br, s2, s3, s4, s5, s6, s7, s8;
} mitray21_;

#define mitray21_1 mitray21_

union {
    struct {
	doublereal d__, dg, s, bf, bt, wdip;
    } _1;
    struct {
	doublereal d__, dg, s, ef, et, dum;
    } _2;
} mitray22_;

#define mitray22_1 (mitray22_._1)
#define mitray22_2 (mitray22_._2)

struct {
    doublereal c0, c1, c2, c3, c4, c5;
} mitray23_;

#define mitray23_1 mitray23_

union {
    struct {
	integer in, mtyp, nsrf, imap, ir;
    } _1;
    struct {
	integer in, mtyp, nsrf, idum1, idum2;
    } _2;
} mitray25_;

#define mitray25_1 (mitray25_._1)
#define mitray25_2 (mitray25_._2)

struct {
    doublereal xa, ya, za, xb, yb, zb, xc, yc, zc;
} mitray_axes__;

#define mitray_axes__1 mitray_axes__

struct {
    doublereal xbmin, xbmax, xcmin, xcmax;
} mitray_bounds__;

#define mitray_bounds__1 mitray_bounds__

struct {
    integer idebug, idemin, idemax, itest, idrun, idevt, ieorun, ieotri, 
	    ievent, iswit[10], ifinit[20], nevent, nrndm[2];
} gcflag_;

#define gcflag_1 gcflag_

struct {
    logical batch, nolog;
} gcflax_;

#define gcflax_1 gcflax_

struct {
    integer lin, lout, nunits, lunits[5];
} gcunit_;

#define gcunit_1 gcunit_

struct {
    char chmail[132];
} gcmail_;

#define gcmail_1 gcmail_

struct {
    logical ldiag;
} mitray_diag__;

#define mitray_diag__1 mitray_diag__

struct {
    integer jevent, jstop, jslit;
} diag_;

#define diag_1 diag_

struct {
    doublereal xo, zo, ss, dcs, dsn;
} mitrayblsdip_;

#define mitrayblsdip_1 mitrayblsdip_

struct {
    integer jmap[5], ix, iz, idum;
    doublereal bzmap[102010]	/* was [101][101][2][5] */;
} mitray26_;

#define mitray26_1 mitray26_

struct {
    doublereal ex, ey, ez, qmc;
    integer ivec;
} mitray11_;

#define mitray11_1 mitray11_

struct {
    doublereal a, b, phi;
} mitray_edipo__;

#define mitray_edipo__1 mitray_edipo__

struct {
    doublereal xa, ya, za, vxa, vya, vza;
} mitray5_;

#define mitray5_1 mitray5_

struct {
    doublereal d__, s, bt, grad1, grad2, grad3, grad4, grad5;
} mitray90_;

#define mitray90_1 mitray90_

struct {
    doublereal c0, c1, c2, c3, c4, c5;
} mitray91_;

#define mitray91_1 mitray91_

struct {
    integer in;
} mitray92_;

#define mitray92_1 mitray92_

struct {
    doublereal dh, do__, dd, ddd, dsh, dso, dsd, dsdd;
} mitray93_;

#define mitray93_1 mitray93_

struct {
    doublereal bf, al, rad;
} mitray30_;

#define mitray30_1 mitray30_

struct {
    doublereal s, bt;
} mitray31_;

#define mitray31_1 mitray31_

/* Table of constant values */

static integer c__1 = 1;
static integer c__9 = 9;
static integer c__3 = 3;
static integer c__0 = 0;
static integer c__2 = 2;
static integer c_n1 = -1;
static integer c_n2 = -2;
static doublereal c_b202 = 1.;
static doublereal c_b203 = 70.;
static integer c__5 = 5;
static real c_b503 = (float)0.;
static integer c__4 = 4;
static integer c__6 = 6;

/* *********************************************************************** */
/*                          DIPOLE SUBROUTINES */
/* *********************************************************************** */

/* Subroutine */ int mitray_dipo_atob__(doublereal *xa, doublereal *ya, 
	doublereal *za, doublereal *xb, doublereal *yb, doublereal *zb)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    static doublereal cosa, sina;


/* 	TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD. */
/* 	I.E. FROM SYSTEM A TO SYSTEM B OF STANDARD LAYOUT, FOR A DIPOLE. */
/* 	ALPHA IS THE POLEFACE ROTATION ANGLE OF THE ENTRANCE FACE */

/* 	INPUT:  XA, YA, ZA */
/* 	OUTPUT: XB, YB, ZB */


    cosa = cos(mitray_dipo__1.alpha / 57.29577951);
    sina = sin(mitray_dipo__1.alpha / 57.29577951);

    *xb = (mitray_dipo__1.a - *za) * sina - *xa * cosa;
    *yb = *ya;
    *zb = (mitray_dipo__1.a - *za) * cosa + *xa * sina;

/* 	COORDINATE SYSTEM B IS DISPLACED BY XCR1 IN THE +XB DIRECTION */

    *xb -= mitray_dipo__1.xcr1;

/* 	ALL COORDINATES ARE NOW IN TERMS OF COORDINATE SYSTEM B */

    return 0;
} /* mitray_dipo_atob__ */


/* ======================================================================= */

/* Subroutine */ int mitray_dipo_btoc__(doublereal *xb, doublereal *yb, 
	doublereal *zb, doublereal *xc, doublereal *yc, doublereal *zc)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    static doublereal xt, zt, sip2, copab, sipab, cospb, sinpb;


/* 	TRANFORM FROM SYSTEM B TO SYSTEM C COORDINATES OF A DIPOLE */

    copab = cos((mitray_dipo__1.phi - mitray_dipo__1.alpha - 
	    mitray_dipo__1.beta) / 57.29577951);
    sipab = sin((mitray_dipo__1.phi - mitray_dipo__1.alpha - 
	    mitray_dipo__1.beta) / 57.29577951);
    cospb = cos((mitray_dipo__1.phi / (float)2. - mitray_dipo__1.beta) / 
	    57.29577951);
    sinpb = sin((mitray_dipo__1.phi / (float)2. - mitray_dipo__1.beta) / 
	    57.29577951);
    sip2 = sin(mitray_dipo__1.phi / (float)2. / 57.29577951);

/* 	THE ORIGIN OF THE B SYSTEM IS DISPLACED FROM THE CENTRAL RAY */
/* 	BY AMOUNT XCR1 ALONG THE +XB AXIS */

    xt = *xb;
    zt = *zb;

/* 	TRANSFORM BACK TO B COORDINATE SYSTEM CENTERED ON CENTRAL RAY */

    xt += mitray_dipo__1.xcr1;

/* 	NOW ROTATE/TRANSLATE TO GET COORDINATES IN TERMS OF C SYSTEM. */

    *zc = -zt * copab + xt * sipab - mitray24_1.rb * (float)2. * sip2 * cospb;
    *xc = -zt * sipab - xt * copab - mitray24_1.rb * (float)2. * sip2 * sinpb;

/* 	SHIFT C COORDINATE AXES FROM CENTRAL RAY BY AMOUNT XCR2 ALONG */
/* 	+XC AXIS */

    *xc -= mitray_dipo__1.xcr2;
    *yc = *yb;
    
/* 	COORDINATES ARE NOW IN TERMS OF THE NEW, SHIFTED C SYSTEM. */

    return 0;
} /* mitray_dipo_btoc__ */


/* ======================================================================= */

/* Subroutine */ int mitray_dipo_bbtoba__(doublereal *bxb, doublereal *byb, 
	doublereal *bzb, doublereal *bxa, doublereal *bya, doublereal *bza)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    static doublereal cosa, sina;


/* 	SUBROUTINE TO TRANSFORM B FIELD FROM A TO B COORD SYSTEMS */
/*       OF A DIPOLE, I.E. FROM SYSTEM A TO SYSTEM B OF STANDARD LAYOUT. */
/* 	ALPHA IS THE POLEFACE ROTATION ANGLE OF THE ENTRANCE FACE */

/* 	INPUT:  BXB, BYB, BZB */
/* 	OUTPUT: BXA, BYA, BZA */



    cosa = cos(-mitray_dipo__1.alpha / 57.29577951);
    sina = sin(-mitray_dipo__1.alpha / 57.29577951);

    *bxa = -(*bzb) * sina - *bxb * cosa;
    *bya = *byb;
    *bza = -(*bzb) * cosa + *bxb * sina;

/* 	COORDINATE SYSTEM B IS DISPLACED BY XCR1 IN THE +XB DIRECTION */
/* 	ALL B-FIELD COMPONENTS ARE NOW IN TERMS OF COORDINATE SYSTEM A */

    return 0;
} /* mitray_dipo_bbtoba__ */


/* ======================================================================= */

/* Subroutine */ int mitray_dipo_bctobb__(doublereal *bxc, doublereal *byc, 
	doublereal *bzc, doublereal *bxb, doublereal *byb, doublereal *bzb)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    static doublereal sip2, copab, sipab, cospb, sinpb;


/* 	TRANFORM B-FIELD COMPONENTS FROM SYSTEM C TO SYSTEM B COORDINATES */
/*       OF A DIPOLE. */

/* 	INPUT:  BXC, BYC, BZC   B-FIELD IN C-AXIS SYSTEM */
/* 	OUTPUT: BXB, BYB, BZB   B-FIELD IN B-AXIS SYSTEM */



    copab = cos(-(mitray_dipo__1.phi - mitray_dipo__1.alpha - 
	    mitray_dipo__1.beta) / 57.29577951);
    sipab = sin(-(mitray_dipo__1.phi - mitray_dipo__1.alpha - 
	    mitray_dipo__1.beta) / 57.29577951);
    cospb = cos(-(mitray_dipo__1.phi / (float)2. - mitray_dipo__1.beta) / 
	    57.29577951);
    sinpb = sin(-(mitray_dipo__1.phi / (float)2. - mitray_dipo__1.beta) / 
	    57.29577951);
    sip2 = sin(-(mitray_dipo__1.phi / (float)2.) / 57.29577951);

/* 	NOW ROTATE TO GET COORDINATES IN TERMS OF C SYSTEM. */

    *bzb = -(*bzc) * copab + *bxc * sipab;
    *bxb = -(*bzc) * sipab - *bxc * copab;
    *byb = *byc;

/* 	B-FIELD COMPONENTS ARE NOW IN TERMS OF THE B SYSTEM. */

    return 0;
} /* mitray_dipo_bctobb__ */


/* ======================================================================= */

/* Subroutine */ int mitray_dipole__(doublereal *data, doublereal *xpos, 
	doublereal *bfld)
{
    /* Format strings */
    static char fmt_900[] = "(/1x,50(\002-\002)/\002 ENTER SUBROUTINE DIPOL\
E\002/\002 XA,YA,ZA=\002,3f10.3/)";
    static char fmt_901[] = "(\002 XB,YB,ZB=\002,3f10.3)";
    static char fmt_902[] = "(\002 XC,YC,ZC=\002,3f10.3)";
    static char fmt_903[] = "(\002 Z11=\002,f10.3,\002  Z12=\002,f10.3,\002 \
 Z21=\002,f10.3,\002  Z22=\002,f10.3/\002 XBMIN=\002,f10.3,\002  XBMAX=\002,\
f10.3,\002  XCMIN=\002,f10.3,\002  XCMAX=\002,f10.3)";
    static char fmt_904[] = "(1x,a,\002  BX,BY,BZ=\002,3f10.3)";

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    /* Subroutine */ void s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(), 
	    s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle();
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int mitray_dipo_atob__(doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *), 
	    mitray_dipo_btoc__(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static integer i__;
    static doublereal z11, z12, z21, z22, lf1, lf2, br1, br2, rb2, rb3, rb4, 
	    lu1;
    extern /* Subroutine */ int mitray_dipo_bbtoba__(doublereal *, doublereal 
	    *, doublereal *, doublereal *, doublereal *, doublereal *), 
	    mitray_dipo_bctobb__(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal bxa, bya, bza, bxb, wde, byb, bzb, bxc, byc, bzc, wdx;
    extern /* Subroutine */ int mitray_bdip__();
    static doublereal scor;
    static char region[2];

    /* Fortran I/O blocks */
    static cilist io___18 = { 0, 0, 0, fmt_900, 0 };
    static cilist io___31 = { 0, 0, 0, fmt_901, 0 };
    static cilist io___32 = { 0, 0, 0, fmt_902, 0 };
    static cilist io___33 = { 0, 0, 0, fmt_903, 0 };
    static cilist io___34 = { 0, 0, 0, 0, 0 };
    static cilist io___41 = { 0, 0, 0, 0, 0 };
    static cilist io___42 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___43 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___47 = { 0, 0, 0, 0, 0 };
    static cilist io___48 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___52 = { 0, 0, 0, 0, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___54 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___55 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___59 = { 0, 0, 0, 0, 0 };
    static cilist io___60 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___61 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___62 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___63 = { 0, 0, 0, 0, 0 };
    static cilist io___64 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___65 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___66 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___67 = { 0, 0, 0, 0, 0 };
    static cilist io___68 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___69 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___70 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___71 = { 0, 0, 0, 0, 0 };



/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
/*                                                                      C */
/*     Subroutine for dipole, in GEANT implementation of MIT-RAYTRACE   C */
/*     adapted from:                                                    C */
/*     Subroutine DIPOLE ( NO, NP, T, TP ,NUM ) by S. Kowalski          C */
/*                                                                      C */
/*      TC(1) to  TC(6) =  (  X,  Y,  Z, VX, VY, VZ )                   C */
/*     DTC(1) to DTC(6) =  ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )          C */
/*                                                                      C */
/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */





/* geant */



/* geant */
/* * GCUNIT */

/* local */

/* *** MITRAY_DIAG COMMON BLOCK */




/*     SY  Addition Feb 10/98 */
/*     Extract coordinates of the field point, */
/*     in the A-axis system of the device. */

/* local */

/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
/*                                                                      C */
/*           File diagnostic.inc - keeps diagnostic flags               C */
/*                                                                      C */
/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */



    /* Parameter adjustments */
    --bfld;
    --xpos;
    --data;

    /* Function Body */
    //mitray_diag__1.ldiag = FALSE_;
    mitray_diag__1.ldiag = FALSE_;
    mitray_axes__1.xa = xpos[1];
    mitray_axes__1.ya = xpos[2];
    mitray_axes__1.za = xpos[3];

    s_copy(region, " ", (ftnlen)2, (ftnlen)1);
    if (mitray_diag__1.ldiag) {
	io___18.ciunit = gcunit_1.lout;
	s_wsfe(&io___18);
	do_fio(&c__1, (char *)&mitray_axes__1.xa, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&mitray_axes__1.ya, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&mitray_axes__1.za, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

/*     EXTRACT THE PARAMETERS FOR THE DIPOLE MAGNET */

    mitray25_1.ir = 0;
    lf1 = data[1];
    lu1 = data[2];
    lf2 = data[3];
    mitray22_1.dg = data[4];
    mitray25_1.mtyp = (integer) data[5];
    mitray25_1.imap = (integer) data[6];
    mitray_dipo__1.a = data[11];
    mitray_dipo__1.b = data[12];
    mitray22_1.d__ = data[13];
    mitray24_1.rb = data[14];
    mitray22_1.bf = data[15];
    mitray_dipo__1.phi = data[16];
    mitray_dipo__1.alpha = data[17];
    mitray_dipo__1.beta = data[18];
    mitray20_1.ndx = data[19];
    mitray20_1.bet1 = data[20];
    mitray20_1.gama = data[21];
    mitray20_1.delt = data[22];
    z11 = data[25];
    z12 = data[26];
    z21 = data[27];
    z22 = data[28];
    br1 = data[41];
    br2 = data[42];
    mitray_dipo__1.xcr1 = data[43];
    mitray_dipo__1.xcr2 = data[44];
    wde = data[49];
    wdx = data[50];

/*     SY  Addition Feb 10/98 */
/*     These parameters define the bounds of the entrance fringe */
/*     field, in the XB axis system.  The entrance fringe field */
/*     is defined by XBMIN < XB < XBMAX  .AND.  Z12 < ZB < Z11 */

    mitray_bounds__1.xbmax = wde / (float)2.;
    mitray_bounds__1.xbmin = -mitray_bounds__1.xbmax;

/*     Similarly, the bounds of the exit fringe field region are */
/*     defined in the XC axis system.  The exit fringe field is */
/*     defined by XCMIN < XC < XCMAX .AND. Z21 < ZC < Z22 */

    mitray_bounds__1.xcmax = wdx / (float)2.;
    mitray_bounds__1.xcmin = -mitray_bounds__1.xcmax;

    if (mitray25_1.mtyp == 0) {
	mitray25_1.mtyp = 1;
    }

/*     SY  Addition Feb 10/98 */
/*     We first zero the B-field components in case we need to abort */

    for (i__ = 1; i__ <= 3; ++i__) {
	bfld[i__] = (float)0.;
    }

    mitray10_1.bx = (float)0.;
    mitray10_1.by = (float)0.;
    mitray10_1.bz = (float)0.;
    mitray22_1.bt = (float)0.;
    mitray22_1.s = (float)0.;
    mitray21_1.br = br1;

/*     Transform from the A to the B coordinate system */
    mitray_dipo_atob__(&mitray_axes__1.xa, &mitray_axes__1.ya, &
	    mitray_axes__1.za, &mitray_axes__1.xb, &mitray_axes__1.yb, &
	    mitray_axes__1.zb);
/*     Transform from the B to the C coordinate system */
    mitray_dipo_btoc__(&mitray_axes__1.xb, &mitray_axes__1.yb, &
	    mitray_axes__1.zb, &mitray_axes__1.xc, &mitray_axes__1.yc, &
	    mitray_axes__1.zc);

/*     Print out coordinates if diagnostic mode */

    if (mitray_diag__1.ldiag) {
	io___31.ciunit = gcunit_1.lout;
	s_wsfe(&io___31);
	do_fio(&c__1, (char *)&mitray_axes__1.xb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&mitray_axes__1.yb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&mitray_axes__1.zb, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___32.ciunit = gcunit_1.lout;
	s_wsfe(&io___32);
	do_fio(&c__1, (char *)&mitray_axes__1.xc, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&mitray_axes__1.yc, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&mitray_axes__1.zc, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___33.ciunit = gcunit_1.lout;
	s_wsfe(&io___33);
	do_fio(&c__1, (char *)&z11, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&z12, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&z21, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&z22, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&mitray_bounds__1.xbmin, (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&mitray_bounds__1.xbmax, (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&mitray_bounds__1.xcmin, (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&mitray_bounds__1.xcmax, (ftnlen)sizeof(
		doublereal));
	e_wsfe();
	io___34.ciunit = gcunit_1.lout;
	s_wsle(&io___34);
	do_lio(&c__9, &c__1, " ", (ftnlen)1);
	e_wsle();
    }

/*   Now determine what region we are in -- */
/*   Before start of the entrance fringe field ("entrance far field", IN=-99 ) */
/*   entrance fringe field (IN=1), */
/*   "uniform" field region (IN=2), */
/*   exit fringe field region (IN=3), */
/*   after the end of the exit fringe field ("exit far field", IN=+99) */
/*   Because of the possibility of confusing entrance and exit regions, we */
/*   first check for entrance/exit fringe field, then uniform field, then */
/*   entrance/exit far field regions, to be sure that we get the most */
/*   important regions first */

    if (mitray_axes__1.zb <= z11 && mitray_axes__1.zb > z12 && 
	    mitray_axes__1.xb >= mitray_bounds__1.xbmin && mitray_axes__1.xb 
	    <= mitray_bounds__1.xbmax) {

/*        ************************* */
/*        *                       * */
/*        * ENTRANCE FRINGE FIELD * */
/*        *                       * */
/*        ************************* */

/*        Entrance fringe field region, B-axis coordinates are used. */

	mitray25_1.ir = 1;
	mitray25_1.in = 1;
	mitray24_1.xc_offset__ = mitray24_1.rb * cos(mitray_dipo__1.alpha / (
		float)57.29578);
	mitray24_1.zc_offset__ = -mitray24_1.rb * sin(mitray_dipo__1.alpha / (
		float)57.29578);

/*        Load the B-axis coordinates into TC(1),TC(2),TC(3), because this */
/*        is where subroutine BDIP expects to find the coordinates. */

	mitray10_1.tc[0] = mitray_axes__1.xb;
	mitray10_1.tc[1] = mitray_axes__1.yb;
	mitray10_1.tc[2] = mitray_axes__1.zb;

	mitray23_1.c0 = data[29];
	mitray23_1.c1 = data[30];
	mitray23_1.c2 = data[31];
	mitray23_1.c3 = data[32];
	mitray23_1.c4 = data[33];
	mitray23_1.c5 = data[34];
	mitray21_1.dels = data[45];
	mitray21_1.rca = data[47];
	mitray22_1.wdip = data[49];

/*        S2...S8 are the coefficients for the entrance face curvature */
/*        SY Feb 20/98   Calculate powers of RB, use DATA(57)/RB4/RB3 */
/*        instead of RB**7 to avoid exponent overflow in case of large RB */

/* Computing 2nd power */
	d__1 = mitray24_1.rb;
	rb2 = d__1 * d__1;
	rb3 = rb2 * mitray24_1.rb;
	rb4 = rb3 * mitray24_1.rb;
	mitray21_1.s2 = data[51] / mitray24_1.rb + mitray21_1.rca / 2.;
	mitray21_1.s3 = data[52] / rb2;
/* Computing 3rd power */
	d__1 = mitray21_1.rca;
	mitray21_1.s4 = data[53] / rb3 + d__1 * (d__1 * d__1) / 8.;
	mitray21_1.s5 = data[54] / rb4;
/* Computing 5th power */
	d__1 = mitray21_1.rca, d__2 = d__1, d__1 *= d__1;
	mitray21_1.s6 = data[55] / rb3 / rb2 + d__2 * (d__1 * d__1) / 16.;
	mitray21_1.s7 = data[56] / rb3 / rb3;
/* Computing 7th power */
	d__1 = mitray21_1.rca, d__2 = d__1, d__1 *= d__1, d__2 *= d__1;
	mitray21_1.s8 = data[57] / rb4 / rb3 + d__2 * (d__1 * d__1) / 25.6;

/*        CHECK IF WE HAVE A FLAT BOUNDARY */
/*                 NSRF=0 FLAT */
/*                     =1 CURVED */

	mitray25_1.nsrf = 1;
	if (mitray21_1.s2 == (float)0. && mitray21_1.s3 == (float)0. && 
		mitray21_1.s4 == (float)0. && mitray21_1.s5 == (float)0. && 
		mitray21_1.s6 == (float)0. && mitray21_1.s7 == (float)0. && 
		mitray21_1.s8 == (float)0.) {
	    mitray25_1.nsrf = 0;
	}

/*        Call BDIP to calculate the B-field components */

	mitray_bdip__();

/* 	BX, BY, BZ ARE IN B-AXIS SYSTEM;  TRANSFORM TO A-AXIS SYSTEM */

	mitray_dipo_bbtoba__(&mitray10_1.bx, &mitray10_1.by, &mitray10_1.bz, &
		bxa, &bya, &bza);

	if (mitray_diag__1.ldiag) {
	    io___41.ciunit = gcunit_1.lout;
	    s_wsle(&io___41);
	    do_lio(&c__9, &c__1, "ENTRANCE FRINGE FIELD REGION", (ftnlen)28);
	    e_wsle();
	    io___42.ciunit = gcunit_1.lout;
	    s_wsfe(&io___42);
	    do_fio(&c__1, "B SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray10_1.bx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.by, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.bz, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___43.ciunit = gcunit_1.lout;
	    s_wsfe(&io___43);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	bfld[1] = bxa;
	bfld[2] = bya;
	bfld[3] = bza;
	return 0;

/*        ------------------------- */

    } else if (mitray_axes__1.zc > z21 && mitray_axes__1.zc <= z22 && 
	    mitray_axes__1.xc >= mitray_bounds__1.xcmin && mitray_axes__1.xc 
	    <= mitray_bounds__1.xcmax) {

/*        ********************* */
/*        *                   * */
/*        * EXIT FRINGE FIELD * */
/*        *                   * */
/*        ********************* */

/*        Exit fringe field region, C-axis coordinates are used. */
/*        Setup for second fringe field and integration */
/*        IN=3 designates exit fringe field */
	mitray25_1.in = 3;
	mitray25_1.ir = 2;
	mitray24_1.xc_offset__ = -mitray24_1.rb * cos(mitray_dipo__1.beta / 
		57.29577951);
	mitray24_1.zc_offset__ = -mitray24_1.rb * sin(mitray_dipo__1.beta / 
		57.29577951);

/*        Load the C axis coordinates into array TC, because this is */
/*        where subroutine BDIP expects to find the coordinates. */

	mitray10_1.tc[0] = mitray_axes__1.xc;
	mitray10_1.tc[1] = mitray_axes__1.yc;
	mitray10_1.tc[2] = mitray_axes__1.zc;

	mitray21_1.br = br2;
/*        C0,...,C5 are the expansion coefficients for the exit fringe field */
	mitray23_1.c0 = data[35];
	mitray23_1.c1 = data[36];
	mitray23_1.c2 = data[37];
	mitray23_1.c3 = data[38];
	mitray23_1.c4 = data[39];
	mitray23_1.c5 = data[40];
	mitray21_1.dels = data[46];
/*        RCA is inverse radius of curvature of exit boundary */
	mitray21_1.rca = data[48];
	mitray22_1.wdip = data[50];

/*        S2...S8 are expansion coefficients for shape of exit */
/*        face of dipole */
/*        SY Feb 20, 1998  Calculate powers of RB, use DATA(64)/RB4/RB3 */
/*        to avoid calculating RB**7, which could suffer exponent overflow */
/*        in case of very large RB value, as in clamshell dipole. */

/* Computing 2nd power */
	d__1 = mitray24_1.rb;
	rb2 = d__1 * d__1;
	rb3 = rb2 * mitray24_1.rb;
	rb4 = rb3 * mitray24_1.rb;
	mitray21_1.s2 = data[58] / mitray24_1.rb + mitray21_1.rca / 2.;
	mitray21_1.s3 = data[59] / rb2;
/* Computing 3rd power */
	d__1 = mitray21_1.rca;
	mitray21_1.s4 = data[60] / rb3 + d__1 * (d__1 * d__1) / 8.;
	mitray21_1.s5 = data[61] / rb4;
/* Computing 5th power */
	d__1 = mitray21_1.rca, d__2 = d__1, d__1 *= d__1;
	mitray21_1.s6 = data[62] / rb3 / rb2 + d__2 * (d__1 * d__1) / 16.;
	mitray21_1.s7 = data[63] / rb3 / rb3;
/* Computing 7th power */
	d__1 = mitray21_1.rca, d__2 = d__1, d__1 *= d__1, d__2 *= d__1;
	mitray21_1.s8 = data[64] / rb4 / rb3 + d__2 * (d__1 * d__1) / 25.6;

/*        CHECK IF WE HAVE A FLAT BOUNDARY */
/*                 NSRF=0 FLAT */
/*                     =1 CURVED */

	mitray25_1.nsrf = 1;
	if (mitray21_1.s2 == (float)0. && mitray21_1.s3 == (float)0. && 
		mitray21_1.s4 == (float)0. && mitray21_1.s5 == (float)0. && 
		mitray21_1.s6 == (float)0. && mitray21_1.s7 == (float)0. && 
		mitray21_1.s8 == (float)0.) {
	    mitray25_1.nsrf = 0;
	}

/*        Call BDIP to calculate magnetic field components */

	mitray_bdip__();

/*        BX, BY, BZ ARE IN C-AXIS SYSTEM;  FIRST TRANSFORM TO B-AXIS SYSTEM */

	mitray_dipo_bctobb__(&mitray10_1.bx, &mitray10_1.by, &mitray10_1.bz, &
		bxb, &byb, &bzb);

/*        THEN TRANSFORM FROM B TO A-AXIS SYSTEM */

	mitray_dipo_bbtoba__(&bxb, &byb, &bzb, &bxa, &bya, &bza);

	if (mitray_diag__1.ldiag) {
	    io___47.ciunit = gcunit_1.lout;
	    s_wsle(&io___47);
	    do_lio(&c__9, &c__1, "EXIT FRINGE FIELD REGION", (ftnlen)24);
	    e_wsle();
	    io___48.ciunit = gcunit_1.lout;
	    s_wsfe(&io___48);
	    do_fio(&c__1, "C SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray10_1.bx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.by, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.bz, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___49.ciunit = gcunit_1.lout;
	    s_wsfe(&io___49);
	    do_fio(&c__1, "B SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&byb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bzb, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___50.ciunit = gcunit_1.lout;
	    s_wsfe(&io___50);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	bfld[1] = bxa;
	bfld[2] = bya;
	bfld[3] = bza;
	return 0;

/*        ------------------------- */

    } else if (mitray_axes__1.zb <= z12 && mitray_axes__1.zc <= z21) {

/*        ************************ */
/*        *                      * */
/*        * UNIFORM FIELD REGION * */
/*        *                      * */
/*        ************************ */

/*       UNIFORM FIELD REGION;  C-AXIS COORDINATES ARE USED */

	mitray22_1.s = (float)0.;
	mitray25_1.in = 2;
	mitray24_1.xc_offset__ = -mitray24_1.rb * cos(mitray_dipo__1.beta / 
		57.29577951);
	mitray24_1.zc_offset__ = -mitray24_1.rb * sin(mitray_dipo__1.beta / 
		57.29577951);

/*        Load the C axis coordinates into array TC, because this is where */
/*        subroutine BDIP expects to find the coordinates. */
	mitray10_1.tc[0] = mitray_axes__1.xc;
	mitray10_1.tc[1] = mitray_axes__1.yc;
	mitray10_1.tc[2] = mitray_axes__1.zc;

	mitray21_1.dels = (float)0.;
	scor = (float)0.;

	mitray_bdip__();

/*        BX, BY, BZ ARE IN C-AXIS SYSTEM.  TRANSFORM FIRST TO B-AXIS SYSTEM */

	mitray_dipo_bctobb__(&mitray10_1.bx, &mitray10_1.by, &mitray10_1.bz, &
		bxb, &byb, &bzb);

/*        THEN TRANSFORM FIELD FROM B TO A-AXIS SYSTEM */

	mitray_dipo_bbtoba__(&bxb, &byb, &bzb, &bxa, &bya, &bza);

	if (mitray_diag__1.ldiag) {
	    io___52.ciunit = gcunit_1.lout;
	    s_wsle(&io___52);
	    do_lio(&c__9, &c__1, "UNIFORM FIELD REGION", (ftnlen)20);
	    e_wsle();
	    io___53.ciunit = gcunit_1.lout;
	    s_wsfe(&io___53);
	    do_fio(&c__1, "C SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray10_1.bx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.by, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.bz, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___54.ciunit = gcunit_1.lout;
	    s_wsfe(&io___54);
	    do_fio(&c__1, "B SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&byb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bzb, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___55.ciunit = gcunit_1.lout;
	    s_wsfe(&io___55);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	bfld[1] = bxa;
	bfld[2] = bya;
	bfld[3] = bza;

    } else if (mitray_axes__1.zb > z11 && mitray_axes__1.xb >= 
	    mitray_bounds__1.xbmin && mitray_axes__1.xb <= 
	    mitray_bounds__1.xbmax) {

/*        ********************** */
/*        *                    * */
/*        * ENTRANCE FAR FIELD * */
/*        *                    * */
/*        ********************** */

	mitray25_1.in = -99;
	bxc = (float)0.;
	byc = (float)0.;
	bzc = (float)0.;
	bxb = (float)0.;
	byb = (float)0.;
	bzb = (float)0.;
	bxa = (float)0.;
	bya = (float)0.;
	bza = (float)0.;

	if (mitray_diag__1.ldiag) {
	    io___59.ciunit = gcunit_1.lout;
	    s_wsle(&io___59);
	    do_lio(&c__9, &c__1, "DIPOLE ENTRANCE FAR FIELD REGION", (ftnlen)
		    32);
	    e_wsle();
	    io___60.ciunit = gcunit_1.lout;
	    s_wsfe(&io___60);
	    do_fio(&c__1, "C SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxc, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&byc, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bzc, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___61.ciunit = gcunit_1.lout;
	    s_wsfe(&io___61);
	    do_fio(&c__1, "B SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&byb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bzb, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___62.ciunit = gcunit_1.lout;
	    s_wsfe(&io___62);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	bfld[1] = bxa;
	bfld[2] = bya;
	bfld[3] = bza;
	return 0;

/*        ------------------------- */

    } else if (mitray_axes__1.zc > z22 && mitray_axes__1.xc > 
	    mitray_bounds__1.xcmin && mitray_axes__1.xc <= 
	    mitray_bounds__1.xcmax) {

/*        ****************** */
/*        *                * */
/*        * EXIT FAR FIELD * */
/*        *                * */
/*        ****************** */

	mitray25_1.in = 99;
	mitray25_1.in = -99;
	bxc = (float)0.;
	byc = (float)0.;
	bzc = (float)0.;
	bxb = (float)0.;
	byb = (float)0.;
	bzb = (float)0.;
	bxa = (float)0.;
	bya = (float)0.;
	bza = (float)0.;

	if (mitray_diag__1.ldiag) {
	    io___63.ciunit = gcunit_1.lout;
	    s_wsle(&io___63);
	    do_lio(&c__9, &c__1, "DIPOLE EXIT FAR FIELD REGION", (ftnlen)28);
	    e_wsle();
	    io___64.ciunit = gcunit_1.lout;
	    s_wsfe(&io___64);
	    do_fio(&c__1, "C SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxc, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&byc, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bzc, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___65.ciunit = gcunit_1.lout;
	    s_wsfe(&io___65);
	    do_fio(&c__1, "B SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&byb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bzb, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___66.ciunit = gcunit_1.lout;
	    s_wsfe(&io___66);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	bfld[1] = bxa;
	bfld[2] = bya;
	bfld[3] = bza;
	return 0;

/*        ------------------------- */

    } else {
/*        UNSPECIFIED FIELD REGION, RETURN WITH ZERO FIELD COMPONENTS */
	if (mitray_diag__1.ldiag) {
	    io___67.ciunit = gcunit_1.lout;
	    s_wsle(&io___67);
	    do_lio(&c__9, &c__1, "UNKNOWN DIPOLE REGION", (ftnlen)21);
	    e_wsle();
	    io___68.ciunit = gcunit_1.lout;
	    s_wsfe(&io___68);
	    do_fio(&c__1, "C SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxc, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&byc, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bzc, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___69.ciunit = gcunit_1.lout;
	    s_wsfe(&io___69);
	    do_fio(&c__1, "B SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&byb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bzb, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___70.ciunit = gcunit_1.lout;
	    s_wsfe(&io___70);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&bxa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&bza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___71.ciunit = gcunit_1.lout;
	    s_wsle(&io___71);
	    do_lio(&c__9, &c__1, "!!! Abort current event !!!", (ftnlen)27);
	    e_wsle();
	}
	diag_1.jstop = 1;
	gcflag_1.ieotri = 1;
	bfld[1] = (float)0.;
	bfld[2] = (float)0.;
	bfld[3] = (float)0.;
	return 0;
    }
    return 0;
} /* mitray_dipole__ */


/* ======================================================================= */

/* Subroutine */ int mitray_bdip__()
{
    /* Format strings */
    static char fmt_8[] = "(\0020 ERROR -GO TO -  IN BFUN   IN=    \002,i5)";

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();
    double sqrt(doublereal);

    /* Local variables */
    static doublereal x, y, z__, b0, b1, b2, b3, b4, b5, b6, b7, b8, s0, b9, 
	    b10, b11, b12, yg1, yg2, yg3, yg4;
    extern /* Subroutine */ int mitray_bdmp__(doublereal *, doublereal *, 
	    doublereal *), mitray_bdpp__(doublereal *, doublereal *, 
	    doublereal *), mitray_ndip__(), mitray_bdppx__(doublereal *, 
	    integer *, integer *), mitray_bpretz__();

    /* Fortran I/O blocks */
    static cilist io___72 = { 0, 0, 0, 0, 0 };
    static cilist io___73 = { 0, 0, 0, 0, 0 };
    static cilist io___74 = { 0, 0, 0, 0, 0 };
    static cilist io___75 = { 0, 0, 0, fmt_8, 0 };


/* **** */
/* **** */
/* **** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION */
/* **** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE */
/* **** MTYP=3  :    NONUNIFORM FIELD STANDARD APPROXIMATION */
/* **** MTYP=4  :    NONUNIFORM FIELD  B=BF/(1+N*DR/R) */
/* **** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION */
/* **** MTYP=6  :    PRETZEL MAGNET */
/* **** */
/* **** THE RELATIONSHIP BETWEEN B0, ......... B12 AND B(I,J) RELATIVE TO */
/* **** AXES (Z,X) IS GIVEN BY */
/* **** */
/* **** */
/* **** */
/* **** B0  = B( 0, 0 ) */
/* **** B1  = B( 1, 0 ) */
/* **** B2  = B( 2, 0 ) */
/* **** B3  = B( 1, 1 ) */
/* **** B4  = B( 1,-1 ) */
/* **** B5  = B( 0, 1 ) */
/* **** B6  = B( 0, 2 ) */
/* **** B7  = B( 0,-1 ) */
/* **** B8  = B( 0,-2 ) */
/* **** B9  = B(-1, 0 ) */
/* **** B10 = B(-2, 0 ) */
/* **** B11 = B(-1, 1 ) */
/* **** B12 = B(-1,-1 ) */
/* **** */
/* **** */



/* geant */
/* * GCUNIT */

/* **** */
/* **** */
    switch (mitray25_1.mtyp) {
	case 1:  goto L10;
	case 2:  goto L10;
	case 3:  goto L6;
	case 4:  goto L6;
	case 5:  goto L10;
	case 6:  goto L21;
    }
    io___72.ciunit = gcunit_1.lout;
    s_wsle(&io___72);
    do_lio(&c__9, &c__1, "**error** in MITRAY_BDIP", (ftnlen)24);
    e_wsle();
    io___73.ciunit = gcunit_1.lout;
    s_wsle(&io___73);
    do_lio(&c__9, &c__1, "          Illegal value MTYP=", (ftnlen)29);
    do_lio(&c__3, &c__1, (char *)&mitray25_1.mtyp, (ftnlen)sizeof(integer));
    e_wsle();
    io___74.ciunit = gcunit_1.lout;
    s_wsle(&io___74);
    do_lio(&c__9, &c__1, "!!! Abort current run !!!", (ftnlen)25);
    e_wsle();
    s_stop("", (ftnlen)0);
L6:
    mitray_ndip__();
    return 0;
L21:
    mitray_bpretz__();
    return 0;
/* **** */
/* **** MTYP = 1 , 2, 5 */
/* **** UNIFORM FIELD MAGNETS */
/* **** */
L10:
    switch (mitray25_1.in) {
	case 1:  goto L2;
	case 2:  goto L1;
	case 3:  goto L2;
	case 4:  goto L4;
    }
/* L7: */
    io___75.ciunit = gcunit_1.lout;
    s_wsfe(&io___75);
    do_fio(&c__1, (char *)&mitray25_1.in, (ftnlen)sizeof(integer));
    e_wsfe();
L1:
    mitray10_1.bx = (float)0.;
    mitray10_1.by = mitray22_1.bf;
    mitray10_1.bz = (float)0.;
    mitray22_1.bt = mitray22_1.bf;
/*      WRITE(lout,*)'!!! Abort current run !!!' */
/*      STOP */
    return 0;
/* **** */
/* **** */
L2:
    x = mitray10_1.tc[0];
    y = mitray10_1.tc[1];
    z__ = mitray10_1.tc[2];
/* **** */
/* **** MTYP=1,2,5 MAP ROUTINES/INTERPOLATE */
/* **** */
    if (mitray25_1.imap == 0) {
	goto L5;
    }
    mitray_bdmp__(&b0, &z__, &x);
    s0 = (float)0.;
    if (y != (float)0.) {
	goto L11;
    }
    mitray10_1.bx = (float)0.;
    mitray10_1.by = b0;
    mitray10_1.bz = (float)0.;
    mitray22_1.bt = b0;
    return 0;
L11:
    d__1 = z__ + mitray22_1.dg;
    mitray_bdmp__(&b1, &d__1, &x);
    d__1 = z__ + mitray22_1.dg * (float)2.;
    mitray_bdmp__(&b2, &d__1, &x);
    d__1 = z__ + mitray22_1.dg;
    d__2 = x + mitray22_1.dg;
    mitray_bdmp__(&b3, &d__1, &d__2);
    d__1 = z__ + mitray22_1.dg;
    d__2 = x - mitray22_1.dg;
    mitray_bdmp__(&b4, &d__1, &d__2);
    d__1 = x + mitray22_1.dg;
    mitray_bdmp__(&b5, &z__, &d__1);
    d__1 = x + mitray22_1.dg * (float)2.;
    mitray_bdmp__(&b6, &z__, &d__1);
    d__1 = x - mitray22_1.dg;
    mitray_bdmp__(&b7, &z__, &d__1);
    d__1 = x - mitray22_1.dg * (float)2.;
    mitray_bdmp__(&b8, &z__, &d__1);
    d__1 = z__ - mitray22_1.dg;
    mitray_bdmp__(&b9, &d__1, &x);
    d__1 = z__ - mitray22_1.dg * (float)2.;
    mitray_bdmp__(&b10, &d__1, &x);
    d__1 = z__ - mitray22_1.dg;
    d__2 = x + mitray22_1.dg;
    mitray_bdmp__(&b11, &d__1, &d__2);
    d__1 = z__ - mitray22_1.dg;
    d__2 = x - mitray22_1.dg;
    mitray_bdmp__(&b12, &d__1, &d__2);
    goto L9;
/* **** */
/* **** MTYP = 1,2,5   STANDARD ROUTINES */
/* **** */
L5:
    mitray_bdpp__(&b0, &z__, &x);
    s0 = mitray22_1.s;
    if (y != (float)0.) {
	goto L3;
    }
    mitray10_1.bx = (float)0.;
    mitray10_1.by = b0;
    mitray10_1.bz = (float)0.;
    mitray22_1.bt = b0;
    return 0;
/* **** */
/* **** */
L3:
/* **** */
/* **** */
    if (mitray25_1.mtyp == 2) {
	goto L12;
    }
/* **** */
/* **** */
/* **** MTYP = 1,5 */
/* **** NON-MIDPLANE FRINGING FIELD REGION */
/* **** */
    d__1 = z__ + mitray22_1.dg;
    mitray_bdpp__(&b1, &d__1, &x);
    d__1 = z__ + mitray22_1.dg * (float)2.;
    mitray_bdpp__(&b2, &d__1, &x);
    d__1 = z__ + mitray22_1.dg;
    d__2 = x + mitray22_1.dg;
    mitray_bdpp__(&b3, &d__1, &d__2);
    d__1 = z__ + mitray22_1.dg;
    d__2 = x - mitray22_1.dg;
    mitray_bdpp__(&b4, &d__1, &d__2);
    d__1 = x + mitray22_1.dg;
    mitray_bdpp__(&b5, &z__, &d__1);
    d__1 = x + mitray22_1.dg * (float)2.;
    mitray_bdpp__(&b6, &z__, &d__1);
    d__1 = x - mitray22_1.dg;
    mitray_bdpp__(&b7, &z__, &d__1);
    d__1 = x - mitray22_1.dg * (float)2.;
    mitray_bdpp__(&b8, &z__, &d__1);
    d__1 = z__ - mitray22_1.dg;
    mitray_bdpp__(&b9, &d__1, &x);
    d__1 = z__ - mitray22_1.dg * (float)2.;
    mitray_bdpp__(&b10, &d__1, &x);
    d__1 = z__ - mitray22_1.dg;
    d__2 = x + mitray22_1.dg;
    mitray_bdpp__(&b11, &d__1, &d__2);
    d__1 = z__ - mitray22_1.dg;
    d__2 = x - mitray22_1.dg;
    mitray_bdpp__(&b12, &d__1, &d__2);
    goto L9;
/* **** */
/* **** MTYP = 2 */
/* **** NON-MIDPLANE FRINGING FIELD REGION */
/* **** */
L12:
    mitray_bdppx__(&b1, &c__1, &c__0);
    mitray_bdppx__(&b2, &c__2, &c__0);
    mitray_bdppx__(&b3, &c__1, &c__1);
    mitray_bdppx__(&b4, &c__1, &c_n1);
    mitray_bdppx__(&b5, &c__0, &c__1);
    mitray_bdppx__(&b6, &c__0, &c__2);
    mitray_bdppx__(&b7, &c__0, &c_n1);
    mitray_bdppx__(&b8, &c__0, &c_n2);
    mitray_bdppx__(&b9, &c_n1, &c__0);
    mitray_bdppx__(&b10, &c_n2, &c__0);
    mitray_bdppx__(&b11, &c_n1, &c__1);
    mitray_bdppx__(&b12, &c_n1, &c_n1);
/* **** */
/* **** CALCULATE BX, BY, AND BZ */
/* **** */
L9:
    mitray22_1.s = s0;
    yg1 = y / mitray22_1.dg;
    yg2 = yg1 * yg1;
    yg3 = yg2 * yg1;
    yg4 = yg3 * yg1;
    mitray10_1.bx = yg1 * ((b5 - b7) * (float)2. / (float)3. - (b6 - b8) / (
	    float)12.) + yg3 * ((b5 - b7) / (float)6. - (b6 - b8) / (float)
	    12. - (b3 + b11 - b4 - b12 - b5 * (float)2. + b7 * (float)2.) / (
	    float)12.);
    mitray10_1.by = b0 - yg2 * ((b1 + b9 + b5 + b7 - b0 * (float)4.) * (float)
	    2. / (float)3. - (b2 + b10 + b6 + b8 - b0 * (float)4.) / (float)
	    24.) + yg4 * (-(b1 + b9 + b5 + b7 - b0 * (float)4.) / (float)6. + 
	    (b2 + b10 + b6 + b8 - b0 * (float)4.) / (float)24. + (b3 + b11 + 
	    b4 + b12 - b1 * (float)2. - b9 * (float)2. - b5 * (float)2. - b7 *
	     (float)2. + b0 * (float)4.) / (float)12.);
    mitray10_1.bz = yg1 * ((b1 - b9) * (float)2. / (float)3. - (b2 - b10) / (
	    float)12.) + yg3 * ((b1 - b9) / (float)6. - (b2 - b10) / (float)
	    12. - (b3 + b4 - b11 - b12 - b1 * (float)2. + b9 * (float)2.) / (
	    float)12.);
    mitray22_1.bt = sqrt(mitray10_1.bx * mitray10_1.bx + mitray10_1.by * 
	    mitray10_1.by + mitray10_1.bz * mitray10_1.bz);
    return 0;
/* **** */
/* **** CONSTANT FIELD REGION */
/* **** */
L4:
    mitray10_1.bx = (float)0.;
    mitray10_1.by = mitray21_1.br;
    mitray10_1.bz = (float)0.;
    mitray22_1.bt = mitray21_1.br;
    return 0;
} /* mitray_bdip__ */


/* ======================================================================= */

/* Subroutine */ int mitray_bdpp__0_(int n__, doublereal *bfld, doublereal *
	z__, doublereal *x, integer *i__, integer *j)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), sqrt(doublereal), exp(
	    doublereal);

    /* Local variables */
    static doublereal a, e, p0, db, cs;
    extern /* Subroutine */ int mitray_sij__(integer *, integer *), 
	    mitray_sdip__(doublereal *, doublereal *);
    extern doublereal xmitray_zefb__(doublereal *);

/* **** */
/* **** */
/* **** */
/* **** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION */
/* **** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE */
/* ****              MORE ACCURATE 3'RD AND HIGHER ORDER CURVATURES */
/* **** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION */
/* **** */
/* **** */
/* **** */
    switch(n__) {
	case 1: goto L_mitray_bdppx;
	}

    switch (mitray25_1.mtyp) {
	case 1:  goto L10;
	case 2:  goto L2;
	case 3:  goto L6;
	case 4:  goto L6;
	case 5:  goto L11;
	case 6:  goto L6;
    }
L6:
    return 0;
/* **** */
/* **** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION */
/* **** */
L10:
    mitray22_1.s = (*z__ - xmitray_zefb__(x)) / mitray22_1.d__ + 
	    mitray21_1.dels;
    goto L13;
/* **** */
/* **** MTYP=2  :    UNIFORM FIELD, ITERATIVE CALCULATION */
/* **** */
L2:
    mitray_sdip__(x, z__);
    goto L13;
/* **** */
/* **** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION */
/* **** */
L11:
    if (abs(mitray21_1.rca) >= 1e-8) {
	goto L12;
    }
    mitray22_1.s = *z__ / mitray22_1.d__ + mitray21_1.dels;
    goto L13;
L12:
    a = (float)1. / mitray21_1.rca;
/* Computing 2nd power */
    d__1 = *z__ + a;
    mitray22_1.s = (d_sign(&c_b202, &a) * sqrt(d__1 * d__1 + *x * *x) - a) / 
	    mitray22_1.d__ + mitray21_1.dels;
    goto L13;
/* **** */
/* **** ENTRY FOR OFF MIDPLANE FIELD */
/* **** */

L_mitray_bdppx:
    mitray_sij__(i__, j);
L13:
    cs = mitray23_1.c0 + mitray22_1.s * (mitray23_1.c1 + mitray22_1.s * (
	    mitray23_1.c2 + mitray22_1.s * (mitray23_1.c3 + mitray22_1.s * (
	    mitray23_1.c4 + mitray22_1.s * mitray23_1.c5))));
    if (abs(cs) > (float)70.) {
	cs = d_sign(&c_b203, &cs);
    }
    e = exp(cs);
    p0 = e + (float)1.;
    db = mitray22_1.bf - mitray21_1.br;
    *bfld = mitray21_1.br + db / p0;
/* **** */
/* **** WRITE(6,100)X, Y, Z,  DR, S, BFLD */
/* *100 FORMAT( 1P6D15.4 ) */
/* **** */
    return 0;
} /* mitray_bdpp__ */

/* Subroutine */ int mitray_bdpp__(doublereal *bfld, doublereal *z__, 
	doublereal *x)
{
    return mitray_bdpp__0_(0, bfld, z__, x, (integer *)0, (integer *)0);
    }

/* Subroutine */ int mitray_bdppx__(doublereal *bfld, integer *i__, integer *
	j)
{
    return mitray_bdpp__0_(1, bfld, (doublereal *)0, (doublereal *)0, i__, j);
    }


/* ======================================================================= */

/* Subroutine */ int mitray_ndip__()
{
    /* Format strings */
    static char fmt_8[] = "(\0020 ERROR -GO TO -  IN BFUN   IN=\002,i3,\002 \
  MTYP=\002,i4)";

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static doublereal x, y, z__, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, 
	    b11, b12, dr, dx, dz, rp, xp, dr1, dr2, dr3, dr4, dr5, dr6, dr7, 
	    dr8, dr9, yg1, yg2, yg3, yg4, rr1, rr2, rr3, yr1, yr2, yr3, yr4, 
	    dr10, dr11, dr12, zfb, brr, dnr1, dnr2, dnr3, dnr4, drr1, drr2, 
	    drr3, drr4, dnr5;
    extern /* Subroutine */ int mitray_bdmp__(doublereal *, doublereal *, 
	    doublereal *), mitray_ndpp__(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern doublereal xmitray_zefb__(doublereal *);
    extern /* Subroutine */ int mitray_ndppx__(doublereal *, integer *, 
	    integer *, doublereal *);

    /* Fortran I/O blocks */
    static cilist io___109 = { 0, 6, 0, fmt_8, 0 };


/* **** */
/* **** */
/* **** MTYP = 3 OR 4 */
/* **** THIS VERSION OF BFUN IS MAINLY FOR NONUNIFORM FIELD MAGNETS */
/* **** THE CENTRAL FIELD REGION IS REPRESENTED TO 3'RD ORDER ON-AND- */
/* **** OFF THE MIDPLANE BY ANALYTIC EXPRESSIONS. SEE SLAC NO. 75 */
/* **** FRINGE FIELD REGIONS REPRESENTED BY FERMI TYPE FALL-OFF */
/* **** ALONG WITH RADIAL FALL-OFF */
/* **** COMPONENTS OF 'B' IN FRINGE REGION EVALUATED BY NUMERICAL METHODS */
/* **** */
/* **** */
/* **** THE RELATIONSHIP BETWEEN B0, ......... B12 AND B(I,J) RELATIVE TO */
/* **** AXES (Z,X) IS GIVEN BY */
/* **** */
/* **** */
/* **** B0  = B( 0, 0 ) */
/* **** B1  = B( 1, 0 ) */
/* **** B2  = B( 2, 0 ) */
/* **** B3  = B( 1, 1 ) */
/* **** B4  = B( 1,-1 ) */
/* **** B5  = B( 0, 1 ) */
/* **** B6  = B( 0, 2 ) */
/* **** B7  = B( 0,-1 ) */
/* **** B8  = B( 0,-2 ) */
/* **** B9  = B(-1, 0 ) */
/* **** B10 = B(-2, 0 ) */
/* **** B11 = B(-1, 1 ) */
/* **** B12 = B(-1,-1 ) */
/* **** */
/* **** */
    x = mitray10_1.tc[0];
    y = mitray10_1.tc[1];
    z__ = mitray10_1.tc[2];
    dx = x - mitray24_1.xc_offset__;
    dz = z__ - mitray24_1.zc_offset__;
    rp = sqrt(dx * dx + dz * dz);
    dr = rp - mitray24_1.rb;
    switch (mitray25_1.in) {
	case 1:  goto L1;
	case 2:  goto L2;
	case 3:  goto L1;
	case 4:  goto L14;
    }
L7:
    s_wsfe(&io___109);
    do_fio(&c__1, (char *)&mitray25_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&mitray25_1.mtyp, (ftnlen)sizeof(integer));
    e_wsfe();
    s_stop("", (ftnlen)0);
L2:
    drr1 = dr / mitray24_1.rb;
    drr2 = drr1 * drr1;
    drr3 = drr2 * drr1;
    drr4 = drr3 * drr1;
    if (y != (float)0.) {
	goto L4;
    }
/* **** */
/* **** MID-PLANE UNIFORM FIELD REGION */
/* **** */
    mitray10_1.bx = (float)0.;
    mitray10_1.by = (float)0.;
    if (mitray25_1.mtyp == 3) {
	mitray10_1.by = mitray22_1.bf * ((float)1. - mitray20_1.ndx * drr1 + 
		mitray20_1.bet1 * drr2 + mitray20_1.gama * drr3 + 
		mitray20_1.delt * drr4);
    }
    if (mitray25_1.mtyp == 4) {
	mitray10_1.by = mitray22_1.bf / (mitray20_1.ndx * drr1 + (float)1.);
    }
    mitray10_1.bz = (float)0.;
    mitray22_1.bt = mitray10_1.by;
    return 0;
/* **** */
/* **** NON MID-PLANE UNIFORM FIELD REGION */
/* **** */
L4:
    yr1 = y / mitray24_1.rb;
    yr2 = yr1 * yr1;
    yr3 = yr2 * yr1;
    yr4 = yr3 * yr1;
    rr1 = mitray24_1.rb / rp;
    rr2 = rr1 * rr1;
    rr3 = rr2 * rr1;
    if (mitray25_1.mtyp == 3) {
	goto L11;
    }
    if (mitray25_1.mtyp == 4) {
	goto L12;
    }
    goto L7;
/* **** */
/* **** MTYP = 3 */
/* **** */
L11:
    brr = mitray22_1.bf * ((-mitray20_1.ndx + mitray20_1.bet1 * (float)2. * 
	    drr1 + mitray20_1.gama * (float)3. * drr2 + mitray20_1.delt * (
	    float)4. * drr3) * yr1 - (mitray20_1.ndx * rr2 + mitray20_1.bet1 *
	     (float)2. * rr1 * ((float)1. - rr1 * drr1) + mitray20_1.gama * (
	    float)3. * (rr1 * (float)2. * drr1 + (float)2. - rr2 * drr2) + 
	    mitray20_1.delt * (float)4. * (drr1 * (float)6. + rr1 * (float)3. 
	    * drr2 - rr2 * drr3)) * yr3 / (float)6.);
    mitray10_1.by = mitray22_1.bf * ((float)1. - mitray20_1.ndx * drr1 + 
	    mitray20_1.bet1 * drr2 + mitray20_1.gama * drr3 + mitray20_1.delt 
	    * drr4 - yr2 * (float).5 * (-mitray20_1.ndx * rr1 + 
	    mitray20_1.bet1 * (float)2. * (rr1 * drr1 + (float)1.) + 
	    mitray20_1.gama * (float)3. * drr1 * (rr1 * drr1 + (float)2.) + 
	    mitray20_1.delt * (float)4. * drr2 * (rr1 * drr1 + (float)3.)) + 
	    yr4 * (-mitray20_1.ndx * rr3 + mitray20_1.bet1 * (float)2. * (rr3 
	    * drr1 - rr2) + mitray20_1.gama * (float)3. * (rr1 * (float)4. - 
	    rr2 * (float)2. * drr1 + rr3 * drr2) + mitray20_1.delt * (float)
	    4. * (rr1 * (float)12. * drr1 + (float)6. - rr2 * (float)3. * 
	    drr2 + rr3 * drr3)) / (float)24.);
    goto L13;
/* **** */
/* **** MTYP = 4 */
/* **** */
L12:
    dnr1 = mitray20_1.ndx * drr1 + (float)1.;
    dnr2 = dnr1 * dnr1;
    dnr3 = dnr2 * dnr1;
    dnr4 = dnr3 * dnr1;
    dnr5 = dnr4 * dnr1;
    brr = mitray22_1.bf * mitray20_1.ndx * (-yr1 / dnr2 + yr3 * (
	    mitray20_1.ndx * (float)6. * mitray20_1.ndx / dnr4 - 
	    mitray20_1.ndx * (float)2. * rr1 / dnr3 - rr2 / dnr2) / (float)6.)
	    ;
/* Computing 3rd power */
    d__1 = mitray20_1.ndx;
    mitray10_1.by = mitray22_1.bf * ((float)1. / dnr1 + yr2 * (float).5 * 
	    mitray20_1.ndx * (mitray20_1.ndx * (float)-2. / dnr3 + rr1 / dnr2)
	     + yr4 * mitray20_1.ndx * (d__1 * (d__1 * d__1) * (float)24. / 
	    dnr5 - mitray20_1.ndx * (float)12. * mitray20_1.ndx * rr1 / dnr4 
	    - mitray20_1.ndx * (float)2. * rr2 / dnr3 - rr3 / dnr2) / (float)
	    24.);
/* **** */
/* **** */
L13:
    mitray10_1.bx = brr * dx / rp;
    mitray10_1.bz = brr * dz / rp;
    mitray22_1.bt = sqrt(mitray10_1.bx * mitray10_1.bx + mitray10_1.by * 
	    mitray10_1.by + mitray10_1.bz * mitray10_1.bz);
    return 0;
/* **** */
/* **** FRINGING FIELD ZONES */
/* **** */
/* **** CHECK IF FIELD MAP CALCULATED */
/* **** */
L1:
    if (mitray25_1.imap == 0) {
	goto L3;
    }
/* **** */
/* **** MTYP=3,4 MAP ROUTINES/INTERPOLATE */
/* **** */
/* **** */
    mitray_bdmp__(&b0, &z__, &x);
    if (y != (float)0.) {
	goto L5;
    }
    mitray10_1.bx = (float)0.;
    mitray10_1.by = b0;
    mitray10_1.bz = (float)0.;
    mitray22_1.bt = b0;
    return 0;
L5:
    d__1 = z__ + mitray22_1.dg;
    mitray_bdmp__(&b1, &d__1, &x);
    d__1 = z__ + mitray22_1.dg * (float)2.;
    mitray_bdmp__(&b2, &d__1, &x);
    d__1 = z__ + mitray22_1.dg;
    d__2 = x + mitray22_1.dg;
    mitray_bdmp__(&b3, &d__1, &d__2);
    d__1 = z__ + mitray22_1.dg;
    d__2 = x - mitray22_1.dg;
    mitray_bdmp__(&b4, &d__1, &d__2);
    d__1 = x + mitray22_1.dg;
    mitray_bdmp__(&b5, &z__, &d__1);
    d__1 = x + mitray22_1.dg * (float)2.;
    mitray_bdmp__(&b6, &z__, &d__1);
    d__1 = x - mitray22_1.dg;
    mitray_bdmp__(&b7, &z__, &d__1);
    d__1 = x - mitray22_1.dg * (float)2.;
    mitray_bdmp__(&b8, &z__, &d__1);
    d__1 = z__ - mitray22_1.dg;
    mitray_bdmp__(&b9, &d__1, &x);
    d__1 = z__ - mitray22_1.dg * (float)2.;
    mitray_bdmp__(&b10, &d__1, &x);
    d__1 = z__ - mitray22_1.dg;
    d__2 = x + mitray22_1.dg;
    mitray_bdmp__(&b11, &d__1, &d__2);
    d__1 = z__ - mitray22_1.dg;
    d__2 = x - mitray22_1.dg;
    mitray_bdmp__(&b12, &d__1, &d__2);
    goto L15;
/* **** */
/* **** MTYP=3, 4  STANDARD ROUTINES */
/* **** */
L3:
    zfb = xmitray_zefb__(&x);
    if (z__ > zfb) {
/* Computing 2nd power */
	d__1 = zfb - mitray24_1.zc_offset__;
	dr = sqrt(dx * dx + d__1 * d__1) - mitray24_1.rb;
    }
    mitray_ndpp__(&b0, &z__, &x, &dr);
    if (y != (float)0.) {
	goto L6;
    }
/* **** */
/* **** MID-PLANE FRINGING FIELD REGION */
/* **** */
    mitray10_1.bx = (float)0.;
    mitray10_1.by = b0;
    mitray10_1.bz = (float)0.;
    mitray22_1.bt = b0;
    return 0;
/* **** */
/* **** NON MID-PLANE FRINGING FIELD REGION */
/* **** */
L6:
    if (z__ > zfb) {
	goto L9;
    }
/* Computing 2nd power */
    d__1 = dz + mitray22_1.dg;
    dr1 = sqrt(dx * dx + d__1 * d__1) - mitray24_1.rb;
/* Computing 2nd power */
    d__1 = dz + mitray22_1.dg * (float)2.;
    dr2 = sqrt(dx * dx + d__1 * d__1) - mitray24_1.rb;
/* Computing 2nd power */
    d__1 = dx + mitray22_1.dg;
/* Computing 2nd power */
    d__2 = dz + mitray22_1.dg;
    dr3 = sqrt(d__1 * d__1 + d__2 * d__2) - mitray24_1.rb;
/* Computing 2nd power */
    d__1 = dx - mitray22_1.dg;
/* Computing 2nd power */
    d__2 = dz + mitray22_1.dg;
    dr4 = sqrt(d__1 * d__1 + d__2 * d__2) - mitray24_1.rb;
/* Computing 2nd power */
    d__1 = dx + mitray22_1.dg;
    dr5 = sqrt(d__1 * d__1 + dz * dz) - mitray24_1.rb;
/* Computing 2nd power */
    d__1 = dx + mitray22_1.dg * (float)2.;
    dr6 = sqrt(d__1 * d__1 + dz * dz) - mitray24_1.rb;
/* Computing 2nd power */
    d__1 = dx - mitray22_1.dg;
    dr7 = sqrt(d__1 * d__1 + dz * dz) - mitray24_1.rb;
/* Computing 2nd power */
    d__1 = dx - mitray22_1.dg * (float)2.;
    dr8 = sqrt(d__1 * d__1 + dz * dz) - mitray24_1.rb;
/* Computing 2nd power */
    d__1 = dz - mitray22_1.dg;
    dr9 = sqrt(dx * dx + d__1 * d__1) - mitray24_1.rb;
/* Computing 2nd power */
    d__1 = dz - mitray22_1.dg * (float)2.;
    dr10 = sqrt(dx * dx + d__1 * d__1) - mitray24_1.rb;
/* Computing 2nd power */
    d__1 = dx + mitray22_1.dg;
/* Computing 2nd power */
    d__2 = dz - mitray22_1.dg;
    dr11 = sqrt(d__1 * d__1 + d__2 * d__2) - mitray24_1.rb;
/* Computing 2nd power */
    d__1 = dx - mitray22_1.dg;
/* Computing 2nd power */
    d__2 = dz - mitray22_1.dg;
    dr12 = sqrt(d__1 * d__1 + d__2 * d__2) - mitray24_1.rb;
    goto L10;
L9:
    dr1 = dr;
    dr2 = dr;
    dr9 = dr;
    dr10 = dr;
    xp = x + mitray22_1.dg;
    zfb = xmitray_zefb__(&xp);
    dx = xp - mitray24_1.xc_offset__;
/* Computing 2nd power */
    d__1 = zfb - mitray24_1.zc_offset__;
    dr3 = sqrt(dx * dx + d__1 * d__1) - mitray24_1.rb;
    dr5 = dr3;
    dr11 = dr3;
    xp = x - mitray22_1.dg;
    zfb = xmitray_zefb__(&xp);
    dx = xp - mitray24_1.xc_offset__;
/* Computing 2nd power */
    d__1 = zfb - mitray24_1.zc_offset__;
    dr4 = sqrt(dx * dx + d__1 * d__1) - mitray24_1.rb;
    dr7 = dr4;
    dr12 = dr4;
    xp = x + mitray22_1.dg * (float)2.;
    zfb = xmitray_zefb__(&xp);
    dx = xp - mitray24_1.xc_offset__;
/* Computing 2nd power */
    d__1 = zfb - mitray24_1.zc_offset__;
    dr6 = sqrt(dx * dx + d__1 * d__1) - mitray24_1.rb;
    xp = x - mitray22_1.dg * (float)2.;
    zfb = xmitray_zefb__(&xp);
    dx = xp - mitray24_1.xc_offset__;
/* Computing 2nd power */
    d__1 = zfb - mitray24_1.zc_offset__;
    dr8 = sqrt(dx * dx + d__1 * d__1) - mitray24_1.rb;
/* **** */
/* **** */
L10:
/* **** CALL NDPP ( B1 , Z + DG, X  , DR1 ) */
/* **** CALL NDPP ( B2 , Z + 2.*DG, X  , DR2 ) */
/* **** CALL NDPP ( B3 , Z + DG, X + DG  , DR3 ) */
/* **** CALL NDPP ( B4 , Z + DG, X - DG  , DR4 ) */
/* **** CALL NDPP ( B5 , Z , X + DG , DR5 ) */
/* **** CALL NDPP ( B6 , Z , X + 2.*DG  , DR6 ) */
/* **** CALL NDPP ( B7 , Z , X - DG , DR7 ) */
/* **** CALL NDPP ( B8 , Z , X - 2.*DG  , DR8 ) */
/* **** CALL NDPP ( B9 , Z - DG, X  , DR9 ) */
/* **** CALL NDPP ( B10, Z - 2.*DG, X, DR10 ) */
/* **** CALL NDPP ( B11, Z - DG, X + DG  , DR11 ) */
/* **** CALL NDPP ( B12, Z - DG, X - DG  , DR12 ) */
/* **** */
/* **** */
    mitray_ndppx__(&b1, &c__1, &c__0, &dr1);
    mitray_ndppx__(&b2, &c__2, &c__0, &dr2);
    mitray_ndppx__(&b3, &c__1, &c__1, &dr3);
    mitray_ndppx__(&b4, &c__1, &c_n1, &dr4);
    mitray_ndppx__(&b5, &c__0, &c__1, &dr5);
    mitray_ndppx__(&b6, &c__0, &c__2, &dr6);
    mitray_ndppx__(&b7, &c__0, &c_n1, &dr7);
    mitray_ndppx__(&b8, &c__0, &c_n2, &dr8);
    mitray_ndppx__(&b9, &c_n1, &c__0, &dr9);
    mitray_ndppx__(&b10, &c_n2, &c__0, &dr10);
    mitray_ndppx__(&b11, &c_n1, &c__1, &dr11);
    mitray_ndppx__(&b12, &c_n1, &c_n1, &dr12);
/* **** */
/* **** OFF-MIDPLANE FIELD COMPONENTS BX, BY, AND BZ */
/* **** */
L15:
    yg1 = y / mitray22_1.dg;
    yg2 = yg1 * yg1;
    yg3 = yg2 * yg1;
    yg4 = yg3 * yg1;
    mitray10_1.bx = yg1 * ((b5 - b7) * (float)2. / (float)3. - (b6 - b8) / (
	    float)12.) + yg3 * ((b5 - b7) / (float)6. - (b6 - b8) / (float)
	    12. - (b3 + b11 - b4 - b12 - b5 * (float)2. + b7 * (float)2.) / (
	    float)12.);
    mitray10_1.by = b0 - yg2 * ((b1 + b9 + b5 + b7 - b0 * (float)4.) * (float)
	    2. / (float)3. - (b2 + b10 + b6 + b8 - b0 * (float)4.) / (float)
	    24.) + yg4 * (-(b1 + b9 + b5 + b7 - b0 * (float)4.) / (float)6. + 
	    (b2 + b10 + b6 + b8 - b0 * (float)4.) / (float)24. + (b3 + b11 + 
	    b4 + b12 - b1 * (float)2. - b9 * (float)2. - b5 * (float)2. - b7 *
	     (float)2. + b0 * (float)4.) / (float)12.);
    mitray10_1.bz = yg1 * ((b1 - b9) * (float)2. / (float)3. - (b2 - b10) / (
	    float)12.) + yg3 * ((b1 - b9) / (float)6. - (b2 - b10) / (float)
	    12. - (b3 + b4 - b11 - b12 - b1 * (float)2. + b9 * (float)2.) / (
	    float)12.);
    mitray22_1.bt = sqrt(mitray10_1.bx * mitray10_1.bx + mitray10_1.by * 
	    mitray10_1.by + mitray10_1.bz * mitray10_1.bz);
    return 0;
L14:
    mitray10_1.bx = (float)0.;
    mitray10_1.by = mitray21_1.br;
    mitray10_1.bz = (float)0.;
    mitray22_1.bt = mitray21_1.br;
    return 0;
} /* mitray_ndip__ */


/* ======================================================================= */

/* Subroutine */ int mitray_ndpp__0_(int n__, doublereal *bfld, doublereal *
	z__, doublereal *x, doublereal *dr, integer *i__, integer *j)
{
    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), exp(doublereal);

    /* Local variables */
    static doublereal e, p0, db, cs;
    extern /* Subroutine */ int mitray_sij__(integer *, integer *);
    static doublereal drr1, drr2, drr3, drr4;
    extern /* Subroutine */ int mitray_sdip__(doublereal *, doublereal *);

/* **** */
/* **** */
/* **** */
/* **** */
/* **** */
/* **** */
    switch(n__) {
	case 1: goto L_mitray_ndppx;
	}

    mitray_sdip__(x, z__);
    goto L1;
/* **** */
/* **** */
/* **** ENTRY FOR OFF MIDPLANE FIELDS */
/* **** */

L_mitray_ndppx:
    mitray_sij__(i__, j);
L1:
    drr1 = *dr / mitray24_1.rb;
    drr2 = drr1 * drr1;
    drr3 = drr2 * drr1;
    drr4 = drr3 * drr1;
    cs = mitray23_1.c0 + mitray22_1.s * (mitray23_1.c1 + mitray22_1.s * (
	    mitray23_1.c2 + mitray22_1.s * (mitray23_1.c3 + mitray22_1.s * (
	    mitray23_1.c4 + mitray22_1.s * mitray23_1.c5))));
    if (abs(cs) > (float)70.) {
	cs = d_sign(&c_b203, &cs);
    }
    e = exp(cs);
    p0 = e + (float)1.;
    db = mitray22_1.bf - mitray21_1.br;
    *bfld = (float)0.;
    if (mitray25_1.mtyp == 3) {
	*bfld = mitray21_1.br + ((float)1. - mitray20_1.ndx * drr1 + 
		mitray20_1.bet1 * drr2 + mitray20_1.gama * drr3 + 
		mitray20_1.delt * drr4) * db / p0;
    }
    if (mitray25_1.mtyp == 4) {
	*bfld = mitray21_1.br + (float)1. / (mitray20_1.ndx * drr1 + (float)
		1.) * db / p0;
    }
/* **** */
/* **** WRITE(6,100) X, Y, Z,  DR, S, BFLD */
/* *100 FORMAT( 1P6D15.4 ) */
/* **** */
    return 0;
} /* mitray_ndpp__ */

/* Subroutine */ int mitray_ndpp__(doublereal *bfld, doublereal *z__, 
	doublereal *x, doublereal *dr)
{
    return mitray_ndpp__0_(0, bfld, z__, x, dr, (integer *)0, (integer *)0);
    }

/* Subroutine */ int mitray_ndppx__(doublereal *bfld, integer *i__, integer *
	j, doublereal *dr)
{
    return mitray_ndpp__0_(1, bfld, (doublereal *)0, (doublereal *)0, dr, i__,
	     j);
    }


/* ======================================================================= */

/* Subroutine */ int mitray_bpretz__()
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    static doublereal y, z__, g1, by0, by1, by2, by3, by4;

/* **** */
/* **** */
/* **** MTYP=6 */
/* **** */
/* **** */
/* **** PRETZEL MAGNET FIELD COMPONENTS */
/* **** DG = SMALL NEGATIVE NUMBER */
/* **** */
/* **** */
/* **** */
/* **** */
    g1 = mitray22_1.bf / mitray22_1.d__;
    y = mitray10_1.tc[1];
    z__ = mitray10_1.tc[2];
    if (z__ <= mitray22_1.dg) {
	goto L1;
    }
    mitray10_1.bx = (float)0.;
    mitray10_1.by = (float)0.;
    mitray10_1.bz = (float)0.;
    return 0;
L1:
    d__1 = abs(z__);
    by0 = g1 * pow_dd(&d__1, &mitray20_1.ndx);
    by1 = by0 * mitray20_1.ndx / z__;
    by2 = by1 * (mitray20_1.ndx - (float)1.) / z__;
    by3 = by2 * (mitray20_1.ndx - (float)2.) / z__;
    by4 = by3 * (mitray20_1.ndx - (float)3.) / z__;
    mitray10_1.bx = (float)0.;
/* Computing 4th power */
    d__1 = y, d__1 *= d__1;
    mitray10_1.by = by0 - y * y * by2 / (float)2. + d__1 * d__1 * by4 / (
	    float)24.;
/* Computing 3rd power */
    d__1 = y;
    mitray10_1.bz = y * by1 - d__1 * (d__1 * d__1) * by3 / (float)6.;
    mitray22_1.bt = sqrt(mitray10_1.bx * mitray10_1.bx + mitray10_1.by * 
	    mitray10_1.by + mitray10_1.bz * mitray10_1.bz);
    return 0;
} /* mitray_bpretz__ */


/* ======================================================================= */

/* Subroutine */ int mitray_sdip__0_(int n__, doublereal *x, doublereal *z__, 
	integer *iz, integer *jx)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), sqrt(doublereal), atan(
	    doublereal), cos(doublereal), sin(doublereal);

    /* Local variables */
    static doublereal a;
    static integer i__, j;
    static doublereal r1, r2, x1, cc, dd, dp, az, x12, z12, ro, cx, rr, xp, 
	    zp, dp2, dsd, xdi, zdi;
    static integer ixp;
    static doublereal xpm, xpo, zpm, zpo, xxp, zzp;
    extern doublereal xmitray_zefb__(doublereal *);
    static doublereal rinv4;
    extern doublereal xmitray_dzdx__(doublereal *);
    static doublereal delta, azmax, costh, dzdxo, zsign, deltax;

/* **** */
/* **** */
/* **** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE */
/* **** MTYP=3  :    NONUNIFORM FIELD STANDARD APPROXIMATION */
/* **** MTYP=4  :    NONUNIFORM FIELD  B=BF/(1+N*DR/R) */
/* **** */
/* **** */
/* **** */
/* **** */
/* **** */
/* **** */
/* **** MTYP=2,3,4  : */
/* **** */
/* **** */
/* **** FIELD POINT (X,Z) */
/* **** */
/* **** */
/* **** CHECK TO SEE IF BOUNDARY IS FLAT */
/* **** */
    switch(n__) {
	case 1: goto L_mitray_sij;
	}

    if ((real) mitray25_1.nsrf != (float)0.) {
	goto L1;
    }
    mitray22_1.s = *z__ / mitray22_1.d__ + mitray21_1.dels;
    mitrayblsdip_1.ss = mitray22_1.s;
    mitrayblsdip_1.dcs = (float)1.;
    mitrayblsdip_1.dsn = (float)0.;
    mitrayblsdip_1.zo = xmitray_zefb__(x);
    return 0;
/* **** */
/* **** FIND POINT ON EFFECTIVE FIELD BOUNDARY THROUGH FIELD POINT */
/* **** PARALLEL TO Z-AXIS */
/* **** */
L1:
    zp = xmitray_zefb__(x);
/* **** */
/* **** INTERVAL OF SEARCH, AZ */
/* **** */
    az = (*z__ - zp) / 5.;
    zsign = d_sign(&c_b202, &az);
    azmax = sqrt(*x * *x + *z__ * *z__) / 5.;
    if (az > azmax) {
	az = azmax;
    }
/* **** */
/* **** */
    az = abs(az);
    xp = *x - az * 5;
    ixp = 1;
    dp = 1e15;
    for (i__ = 1; i__ <= 11; ++i__) {
	zp = xmitray_zefb__(&xp);
	xxp = *x - xp;
	zzp = *z__ - zp;
	dd = xxp * xxp + zzp * zzp;
	if (dd >= dp) {
	    goto L3;
	}
	ixp = i__;
	dp = dd;
L3:
	xp += az;
/* L2: */
    }
/* **** */
/* ****  DIVIDE INTERVAL AND REPEAT FOR MORE EXACT */
/* ****  SHORTEST DISTANCE. */
/* **** */
    x1 = *x + az * (ixp - 6);
    az /= 5.;
    xp = x1 - az * 5;
    ixp = 1;
    dp = 1e15;
    for (i__ = 1; i__ <= 11; ++i__) {
	zp = xmitray_zefb__(&xp);
	xxp = *x - xp;
	zzp = *z__ - zp;
	dd = xxp * xxp + zzp * zzp;
	if (dd >= dp) {
	    goto L5;
	}
	ixp = i__;
	dp = dd;
L5:
	xp += az;
/* L4: */
    }
/* **** */
/* **** */
    mitrayblsdip_1.xo = x1 + az * (ixp - 6);
    mitrayblsdip_1.zo = xmitray_zefb__(&mitrayblsdip_1.xo);
    xpo = *x - mitrayblsdip_1.xo;
    zpo = *z__ - mitrayblsdip_1.zo;
    ro = xpo * xpo + zpo * zpo;
/* **** */
/* **** INTERPOLATE FOR MORE ACCURATE LOCATION */
/* **** */
    if (ixp == 1 || ixp == 11) {
	goto L8;
    }
    xp = mitrayblsdip_1.xo + az;
    zp = xmitray_zefb__(&xp);
    xxp = *x - xp;
    zzp = *z__ - zp;
    r1 = xxp * xxp + zzp * zzp;
/* **** */
/* **** CALCULATE POINT ON THE OTHER SIDE */
/* **** */
    xpm = mitrayblsdip_1.xo - az;
    zpm = xmitray_zefb__(&xpm);
    xxp = *x - xpm;
    zzp = *z__ - zpm;
    r2 = xxp * xxp + zzp * zzp;
    if (r1 <= r2) {
	goto L9;
    }
/* **** */
/* **** SWAP POINTS */
/* **** */
    xp = mitrayblsdip_1.xo;
    zp = mitrayblsdip_1.zo;
    r1 = ro;
    mitrayblsdip_1.xo = xpm;
    mitrayblsdip_1.zo = zpm;
    ro = r2;
L9:
    x12 = xp - mitrayblsdip_1.xo;
    z12 = zp - mitrayblsdip_1.zo;
    cc = x12 * x12 + z12 * z12;
    mitrayblsdip_1.xo += (cc + ro - r1) * az / (cc * 2);
    mitrayblsdip_1.zo = xmitray_zefb__(&mitrayblsdip_1.xo);
    xpo = *x - mitrayblsdip_1.xo;
    zpo = *z__ - mitrayblsdip_1.zo;
    ro = xpo * xpo + zpo * zpo;
L8:
/* **** */
/* **** */
    if (ro < 1e-15) {
	ro = 1e-15;
    }
    if (ro > 1e15) {
	ro = 1e15;
    }
    dzdxo = xmitray_dzdx__(&mitrayblsdip_1.xo);
    costh = sqrt((float)1. / (dzdxo * dzdxo + (float)1.));
    deltax = sqrt(ro) * costh / 4.;
/* **** */
/* **** */
/* **** WRITE(6,100) X, Z, XO, ZO, COSTH, DELTAX */
/* **** */
/* **** PREPARE TO CALCULATE A PAIR OF EQUALLY SPACED IN X */
/* **** DISTANCES ON EITHER SIDE OF RO */
/* **** */
    rinv4 = 1. / (ro * ro);
/* **** */
/* **** CALCULATE REPRESENTATIVE DISTANCE */
/* **** */
    cx = mitrayblsdip_1.xo - deltax * 2;
    for (j = 1; j <= 5; ++j) {
	if (j == 3) {
	    goto L7;
	}
	zp = xmitray_zefb__(&cx);
	xdi = *x - cx;
	zdi = *z__ - zp;
	rr = xdi * xdi + zdi * zdi;
	if (rr < 1e-15) {
	    rr = 1e-15;
	}
	if (rr > 1e15) {
	    rr = 1e15;
	}
	rinv4 += 1. / (rr * rr);
L7:
	cx += deltax;
/* L6: */
    }
    dp2 = sqrt(1. / rinv4);
    dp = sqrt(dp2);
/* **** */
/* **** */
    mitray22_1.s = zsign * 1.41875 * dp / mitray22_1.d__ + mitray21_1.dels;
/* **** */
/* **** Parameters for off midplane calculation */
/* **** */
    mitrayblsdip_1.ss = mitray22_1.s;
    delta = atan(xmitray_dzdx__(&mitrayblsdip_1.xo));
    mitrayblsdip_1.dcs = cos(delta);
    mitrayblsdip_1.dsn = sin(delta);
/* **** */
/* *100 FORMAT( 1P6D15.4 ) */
/* **** WRITE(6,100) X, Z, DELS, S */
/* **** */
    return 0;
/* **** */
/* **** ENTRY FOR NON MIDPLANE 'S' */
/* **** */

L_mitray_sij:
/* **** */
/* **** */
    a = (*jx * mitrayblsdip_1.dcs + *iz * mitrayblsdip_1.dsn) * mitray22_1.dg;
    d__1 = mitrayblsdip_1.xo + a * mitrayblsdip_1.dcs;
    dsd = -mitrayblsdip_1.dcs * (xmitray_zefb__(&d__1) - mitrayblsdip_1.zo - 
	    a * mitrayblsdip_1.dsn);
    mitray22_1.s = mitrayblsdip_1.ss + ((*iz * mitrayblsdip_1.dcs - *jx * 
	    mitrayblsdip_1.dsn) * mitray22_1.dg + dsd) / mitray22_1.d__;
    return 0;
} /* mitray_sdip__ */

/* Subroutine */ int mitray_sdip__(doublereal *x, doublereal *z__)
{
    return mitray_sdip__0_(0, x, z__, (integer *)0, (integer *)0);
    }

/* Subroutine */ int mitray_sij__(integer *iz, integer *jx)
{
    return mitray_sdip__0_(1, (doublereal *)0, (doublereal *)0, iz, jx);
    }


/* ======================================================================= */

doublereal xmitray_zefb__(doublereal *xp)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static doublereal xp2, xp3, xp4, zefb;



    xp2 = *xp * *xp;
    xp3 = xp2 * *xp;
    xp4 = xp3 * *xp;
    zefb = -(mitray21_1.s2 * xp2 + mitray21_1.s3 * xp3 + mitray21_1.s4 * xp4 
	    + mitray21_1.s5 * xp4 * *xp + mitray21_1.s6 * xp4 * xp2 + 
	    mitray21_1.s7 * xp4 * xp3 + mitray21_1.s8 * xp4 * xp4);
    ret_val = zefb;
    return ret_val;
} /* xmitray_zefb__ */


/* ======================================================================= */

doublereal xmitray_dzdx__(doublereal *xp)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static doublereal xp2, xp3, xp4, dzdx;




    xp2 = *xp * *xp;
    xp3 = xp2 * *xp;
    xp4 = xp3 * *xp;
    dzdx = -(mitray21_1.s2 * (float)2. * *xp + mitray21_1.s3 * (float)3. * 
	    xp2 + mitray21_1.s4 * (float)4. * xp3 + mitray21_1.s5 * (float)5. 
	    * xp4 + mitray21_1.s6 * (float)6. * xp4 * *xp + mitray21_1.s7 * (
	    float)7. * xp4 * xp2 + mitray21_1.s8 * (float)8. * xp4 * xp3);
    ret_val = dzdx;
    return ret_val;
} /* xmitray_dzdx__ */


/* ======================================================================= */

doublereal xmitray_dzdx2__(doublereal *xp)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static doublereal xp2, xp3, xp4, dzdx2;



    xp2 = *xp * *xp;
    xp3 = xp2 * *xp;
    xp4 = xp3 * *xp;
    dzdx2 = -(mitray21_1.s2 * (float)2. + mitray21_1.s3 * (float)6. * *xp + 
	    mitray21_1.s4 * (float)12. * xp2 + mitray21_1.s5 * (float)20. * 
	    xp3 + mitray21_1.s6 * (float)30. * xp4 + mitray21_1.s7 * (float)
	    42. * xp4 * *xp + mitray21_1.s8 * (float)56. * xp4 * xp2);
    ret_val = dzdx2;
    return ret_val;
} /* xmitray_dzdx2__ */


/* ======================================================================= */

/* Subroutine */ int mitray_bdmp__(doublereal *bzz, doublereal *z__, 
	doublereal *x)
{
    static integer i__;
    static doublereal a1, a2, a3, a4, c1, c2, b00, dx, dz, fz[3], px, qz, bm1,
	     bm2, bp1, bp2, qz2, qz3, qz4;
    static integer nxp, nzq, nxx;


/* **** */
/* **** */
/* **** */
    dx = mitray26_1.ix + *x / mitray22_1.dg;
    dz = mitray26_1.iz + *z__ / mitray22_1.dg;
/* **** NXP = DX */
/* **** NZQ = DZ */
    nxp = (integer) (dx + (float).5);
    nzq = (integer) (dz + (float).5);
    px = dx - nxp;
    qz = dz - nzq;
/* **** */
/* **** */
/* **** 6-POINT BIVARIATE INTERPOLATION 'ABRAMOWITZ' */
/* **** */
/* **** */
/* ****      BZZ =  BF*( ( QZ*(QZ-1.) * BZMAP( NXP, NZQ-1, IR, IMAP ) + */
/* ****     1        PX*(PX-1.) * BZMAP( NXP-1, NZQ, IR, IMAP ) + */
/* ****     2        PX*(PX-2.*QZ+1.) * BZMAP( NXP+1, NZQ, IR, IMAP ) + */
/* ****     3        QZ*(QZ-2.*PX+1.)*BZMAP( NXP, NZQ+1, IR, IMAP )  )/2. */
/* ****     4        PX*QZ * BZMAP( NXP+1, NZQ+1, IR, IMAP ) + */
/* ****     5        (1.+PX*QZ-PX*PX-QZ*QZ) * BZMAP( NXP, NZQ, IR, IMAP ) */
/* **** */
/* **** */
    qz2 = qz * qz;
    qz3 = qz2 * qz;
    qz4 = qz3 * qz;
    for (i__ = 1; i__ <= 3; ++i__) {
	nxx = nxp - 2 + i__;
	bm2 = mitray26_1.bzmap[nxx + (nzq - 2 + (mitray25_1.ir + (
		mitray25_1.imap << 1)) * 101) * 101 - 30705];
	bm1 = mitray26_1.bzmap[nxx + (nzq - 1 + (mitray25_1.ir + (
		mitray25_1.imap << 1)) * 101) * 101 - 30705];
	b00 = mitray26_1.bzmap[nxx + (nzq + (mitray25_1.ir + (mitray25_1.imap 
		<< 1)) * 101) * 101 - 30705];
	bp1 = mitray26_1.bzmap[nxx + (nzq + 1 + (mitray25_1.ir + (
		mitray25_1.imap << 1)) * 101) * 101 - 30705];
	bp2 = mitray26_1.bzmap[nxx + (nzq + 2 + (mitray25_1.ir + (
		mitray25_1.imap << 1)) * 101) * 101 - 30705];
	a1 = ((bp1 - bm1) * 8 - bp2 + bm2) / 12;
	a2 = ((bp1 + bm1) * 16 - bp2 - bm2 - b00 * 30) / 24;
	a3 = ((bm1 - bp1) * 2 + bp2 - bm2) / 12;
	a4 = ((bp1 + bm1) * -4 + bp2 + bm2 + b00 * 6) / 24;
	fz[i__ - 1] = b00 + a1 * qz + a2 * qz2 + a3 * qz3 + a4 * qz4;
/* L1: */
    }
    c1 = (fz[2] - fz[0]) / 2;
    c2 = (fz[2] + fz[0] - fz[1] * 2) / 2;
    *bzz = mitray22_1.bf * (fz[1] + c1 * px + c2 * px * px);
/* **** */
/* **** */
    return 0;
} /* mitray_bdmp__ */


/* ======================================================================= */

/* Subroutine */ int mitray_edipol__(doublereal *data, doublereal *xpos, 
	doublereal *efld)
{
    /* Format strings */
    static char fmt_900[] = "(/1x,50(\002-\002)/\002 ENTER SUBROUTINE EDIP\
OLE\002/\002 XA,YA,ZA=\002,3f10.3/)";
    static char fmt_901[] = "(\002 XB,YB,ZB=\002,3f10.3)";
    static char fmt_902[] = "(\002 XC,YC,ZC=\002,3f10.3)";
    static char fmt_903[] = "(\002 Z11=\002,f10.3,\002  Z12=\002,f10.3,\002 \
 Z21=\002,f10.3,\002  Z22=\002,f10.3/\002 XBMIN=\002,f10.3,\002  XBMAX=\002,\
f10.3,\002 XCMIN=\002,f10.3,\002  XCMAX=\002,f10.3)";
    static char fmt_904[] = "(1x,a,\002  EX,EY,EZ=\002,3f10.3)";
    static char fmt_905[] = "(1x,a,3f12.4)";

    /* Builtin functions */
    /* Subroutine */ void s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();
    double cos(doublereal), sin(doublereal);
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle();

    /* Local variables */
    static integer i__;
    static doublereal xa, ya, za, z11, z12, z21, z22, xb, yb, zb, zc, xc, yc, 
	    lf1, lf2, lu1;
    extern /* Subroutine */ int mitray_edip_ectoea__(doublereal *, doublereal 
	    *, doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal eff, exa, eya, eza;
    extern /* Subroutine */ int mitray_edip__();
    static doublereal sip2, copab, sipab, cospb, sinpb, xbmin, xbmax, xcmax, 
	    xcmin;
    static char region[2];

    /* Fortran I/O blocks */
    static cilist io___248 = { 0, 0, 0, fmt_900, 0 };
    static cilist io___273 = { 0, 0, 0, fmt_901, 0 };
    static cilist io___274 = { 0, 0, 0, fmt_902, 0 };
    static cilist io___275 = { 0, 0, 0, fmt_903, 0 };
    static cilist io___276 = { 0, 0, 0, 0, 0 };
    static cilist io___280 = { 0, 0, 0, 0, 0 };
    static cilist io___281 = { 0, 0, 0, 0, 0 };
    static cilist io___282 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___283 = { 0, 0, 0, 0, 0 };
    static cilist io___284 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___285 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___286 = { 0, 0, 0, 0, 0 };
    static cilist io___287 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___288 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___289 = { 0, 0, 0, 0, 0 };
    static cilist io___290 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___291 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___292 = { 0, 0, 0, 0, 0 };
    static cilist io___293 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___294 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___295 = { 0, 0, 0, 0, 0 };
    static cilist io___296 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___297 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___298 = { 0, 0, 0, 0, 0 };
    static cilist io___299 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___300 = { 0, 0, 0, 0, 0 };
    static cilist io___301 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___302 = { 0, 0, 0, 0, 0 };
    static cilist io___303 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___304 = { 0, 0, 0, 0, 0 };
    static cilist io___305 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___306 = { 0, 0, 0, 0, 0 };
    static cilist io___307 = { 0, 0, 0, fmt_905, 0 };
    static cilist io___308 = { 0, 0, 0, fmt_905, 0 };
    static cilist io___309 = { 0, 0, 0, fmt_905, 0 };
    static cilist io___310 = { 0, 0, 0, 0, 0 };
    static cilist io___311 = { 0, 0, 0, 0, 0 };
    static cilist io___312 = { 0, 0, 0, fmt_905, 0 };
    static cilist io___313 = { 0, 0, 0, fmt_905, 0 };
    static cilist io___314 = { 0, 0, 0, fmt_905, 0 };
    static cilist io___315 = { 0, 0, 0, 0, 0 };



/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
/*                                                                      C */
/*     Subroutine for electric dipole, in GEANT implementation of       C */
/*     MIT-RAYTRACE                                                     C */
/*     adapted from:                                                    C */
/*     Subroutine EDIPL (NO, NP, T, TP, NUM)  by S. Kowalski            C */
/*                                                                      C */
/*      TC(1) to  TC(6) =  (  X,  Y,  Z, VX, VY, VZ )                   C */
/*     DTC(1) to DTC(6) =  ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )          C */
/*     T = TIME                                                         C */
/*                                                                      C */
/*     Input:  DATA(i)    array containing parameters of the            C */
/*                        electric dipole                               C */
/*             XPOS(i),   i=1,3  contain the X,Y,Z coordinates of the   C */
/*                        field point, in the A-axis coordinate system  C */
/*                        of the electric dipole.                       C */
/*                                                                      C */
/*     Output: EFLD(i),   i=1,3  contain the electric field components  C */
/*                        Ex, Ey, Ez at the specified field point, in   C */
/*                        the A-axis coordinate system of the E-dipole. C */
/*                                                                      C */
/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */





/* geant */



/* geant */
/* * GCUNIT */


/*     S.Yen addition  July 31, 1999 */
/*     Extract coordinates of the field point in the */
/*     A-axis coordinate system of the electric dipole */

/* 	  LDIAG=.true. */
/* local */

/* *** MITRAY_DIAG COMMON BLOCK */



    /* Parameter adjustments */
    --efld;
    --xpos;
    --data;

    /* Function Body */
    mitray_diag__1.ldiag = FALSE_;
    xa = xpos[1];
    ya = xpos[2];
    za = xpos[3];
    s_copy(region, " ", (ftnlen)2, (ftnlen)1);
    if (mitray_diag__1.ldiag) {
	io___248.ciunit = gcunit_1.lout;
	s_wsfe(&io___248);
	do_fio(&c__1, (char *)&xa, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ya, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&za, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

/*     Extract the parameters for the electric dipole */

    lf1 = data[1];
    lu1 = data[2];
    lf2 = data[3];
    mitray22_2.dg = data[4];
    mitray_edipo__1.a = data[11];
    mitray_edipo__1.b = data[12];
    mitray22_2.d__ = data[13];
    mitray24_1.rb = data[14];
    eff = data[15];
    mitray_edipo__1.phi = data[16];
    mitray20_2.ec2 = data[17];
    mitray20_2.ec4 = data[18];
    mitray20_2.we = data[19];
    mitray20_2.wc = data[20];
    z11 = data[25];
    z12 = data[26];
    z21 = data[27];
    z22 = data[28];

/*     SY Addition Sept 20/99 */
/*     We must define the bounds of the entrance fringe field */
/*     in the XB axis system.  The entrance fringe field region is */
/*     defined by ( XBMIN < XB < XBMAX )  &  ( Z12 < ZB < Z11 ) */
/*     If the field point is outside the gap width between the */
/*     electrodes, then we assume that the field is zero. */

    xbmax = mitray22_2.d__ * (float).5;
    xbmin = -xbmax;

/*     Similarly the bounds of the exit fringe field are defined in */
/*     the XC axis system.  The exit fringe field is defined by */
/*     ( XCMIN < XC < XCMAX ) & ( Z21 < ZC < Z22 ) */

    xcmax = xbmax;
    xcmin = xbmin;

/*     SY Addition Sept 20/99 */
/*     First zero the E-field components in case we need to abort */

    for (i__ = 1; i__ <= 3; ++i__) {
	efld[i__] = (float)0.;
    }

    if (mitray20_2.we == (float)0.) {
	mitray20_2.we = mitray24_1.rb * (float)1e3;
    }
    mitray10_1.bx = (float)0.;
    mitray10_1.by = (float)0.;
    mitray10_1.bz = (float)0.;
    mitray11_1.ex = (float)0.;
    mitray11_1.ey = (float)0.;
    mitray11_1.ez = (float)0.;
    mitray22_2.et = (float)0.;
    mitray22_2.s = (float)0.;

/*     TRANSFORM FROM INITIAL ENTRANCE (A-SYSTEM) COORDINATES TO ENTRANCE */
/*     EFB (B-SYSTEM) COORDINATES. */

    xb = -xa;
    yb = ya;
    zb = mitray_edipo__1.a - za;

/*     TRANSFORM FROM THE B-SYSTEM COORDINATES TO THE EXIT EFB (C-SYSTEM) */
/*     COORDINATES. */

/*     (These are the same equations used for B-->C tranformation for */
/*      the magnetic dipole, but with ALPHA=BETA=0) */

    copab = cos(mitray_edipo__1.phi / (float)57.29578);
    sipab = sin(mitray_edipo__1.phi / (float)57.29578);
    cospb = cos(mitray_edipo__1.phi / (float)2. / (float)57.29578);
    sinpb = sin(mitray_edipo__1.phi / (float)2. / (float)57.29578);
    sip2 = sin(mitray_edipo__1.phi / (float)2. / (float)57.29578);
    zc = -zb * copab + xb * sipab - mitray24_1.rb * (float)2. * sip2 * cospb;
    xc = -zb * sipab - xb * copab - mitray24_1.rb * (float)2. * sip2 * sinpb;
    yc = yb;

/*     Print out coordinates if in diagostic mode */

    if (mitray_diag__1.ldiag) {
	io___273.ciunit = gcunit_1.lout;
	s_wsfe(&io___273);
	do_fio(&c__1, (char *)&xb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&yb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&zb, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___274.ciunit = gcunit_1.lout;
	s_wsfe(&io___274);
	do_fio(&c__1, (char *)&xc, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&yc, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&zc, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___275.ciunit = gcunit_1.lout;
	s_wsfe(&io___275);
	do_fio(&c__1, (char *)&z11, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&z12, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&z21, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&z22, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&xbmin, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&xbmax, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&xcmin, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&xcmax, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___276.ciunit = gcunit_1.lout;
	s_wsle(&io___276);
	do_lio(&c__9, &c__1, " ", (ftnlen)1);
	e_wsle();
    }

/*     Now determine which region we are in.  Choices are: */
/*     Before start of entrance fringe field ("entrance far field") IN=-99 */
/*     entrance fringe field region  (IN=1) */
/*     "uniform" field region (IN=2) */
/*     exit fringe field region (IN=3) */
/*     after the end of the exit fringe field ("exit far field") IN=+99 */
/*     Because of the possibility of confusing entrance and exit regions, */
/*     we first check for entrance/exit fringe field, then uniform field, */
/*     then entrance/exit far field regions, to make sure that we get the */
/*     most important regions first. */

/* **** IN DESIGNATES MAGNET REGIONS FOR BFUN */

    if (zb <= z11 && zb > z12 && xb >= xbmin && xb <= xbmax) {

/*        ************************* */
/*        *                       * */
/*        * ENTRANCE FRINGE FIELD * */
/*        *                       * */
/*        ************************* */

/*        Entrance fringe field region, B-axis coordinates are used. */

	mitray25_2.in = 1;
	mitray24_1.xc_offset__ = mitray24_1.rb;
	mitray24_1.zc_offset__ = (float)0.;
	mitray22_2.ef = eff;

/*       Get Enge coefficients Cn for entrance fringe field. */

	mitray23_1.c0 = data[29];
	mitray23_1.c1 = data[30];
	mitray23_1.c2 = data[31];
	mitray23_1.c3 = data[32];
	mitray23_1.c4 = data[33];
	mitray23_1.c5 = data[34];

/*       Load the B-axis coordinates into TC(1), TC(2), TC(3), because this */
/*       is where the subroutine EDIP expects to find the coordinates */

	mitray10_1.tc[0] = xb;
	mitray10_1.tc[1] = yb;
	mitray10_1.tc[2] = zb;

/*       Call subroutine EDIP to calculate the E-field components */

	mitray_edip__();

/*       The E-field components EX, EY, EZ have been passed back */
/*       via common block /MITRAY11/, and are in the B-axis system. */
/*       Transform the E-field components back to the A-axis system. */
/*       Actually there is no change, since the A-axis and B-axis systems */
/*       are related by a simple translation for the electrostatic dipole, */
/*       with no rotation. */

	exa = mitray11_1.ex;
	eya = mitray11_1.ey;
	eza = mitray11_1.ez;

/*       Print out diagnostics if required */

	if (mitray_diag__1.ldiag) {
	    io___280.ciunit = gcunit_1.lout;
	    s_wsle(&io___280);
	    do_lio(&c__9, &c__1, "ENTRANCE FRINGE FIELD REGION", (ftnlen)28);
	    e_wsle();
	    io___281.ciunit = gcunit_1.lout;
	    s_wsle(&io___281);
	    do_lio(&c__9, &c__1, "B SYSTEM", (ftnlen)8);
	    do_lio(&c__5, &c__1, (char *)&mitray11_1.ex, (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&mitray11_1.ey, (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&mitray11_1.ez, (ftnlen)sizeof(
		    doublereal));
	    e_wsle();
	    io___282.ciunit = gcunit_1.lout;
	    s_wsfe(&io___282);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&exa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___283.ciunit = gcunit_1.lout;
	    s_wsle(&io___283);
	    do_lio(&c__9, &c__1, "ENTRANCE FRINGE FIELD REGION", (ftnlen)28);
	    e_wsle();
	    io___284.ciunit = gcunit_1.lout;
	    s_wsfe(&io___284);
	    do_fio(&c__1, "B SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray11_1.ex, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray11_1.ey, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray11_1.ez, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___285.ciunit = gcunit_1.lout;
	    s_wsfe(&io___285);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&exa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}

/*       Load EXA, EYA, EZA into array EFLD and exit. */

	efld[1] = exa;
	efld[2] = eya;
	efld[3] = eza;
	return 0;

/*     ------------------------------------- */

    } else if (zc > z21 && zc <= z22 && xc >= xcmin && xc <= xcmax) {

/*        ********************* */
/*        *                   * */
/*        * EXIT FRINGE FIELD * */
/*        *                   * */
/*        ********************* */

/*        SET INDICATOR IN=3 for exit fringe field */

	mitray25_2.in = 3;
	mitray24_1.xc_offset__ = -mitray24_1.rb;
/* ADDED NOV 23/99 */
	mitray24_1.zc_offset__ = (float)0.;
/* ADDED NOV 23/99 */
	mitray22_2.ef = -eff;

/*        Get Enge coefficients Cn for the exit fringe field */

/* ADDED NOV 23/99 */
	mitray23_1.c0 = data[35];
	mitray23_1.c1 = data[36];
	mitray23_1.c2 = data[37];
	mitray23_1.c3 = data[38];
	mitray23_1.c4 = data[39];
	mitray23_1.c5 = data[40];

/*        LOAD THE C AXIS COORDINATES INTO ARRAY TC, BECAUSE THIS IS */
/*        WHERE SUBROUTINE BDIP EXPECTS TO FIND THEM. */

	mitray10_1.tc[0] = xc;
	mitray10_1.tc[1] = yc;
	mitray10_1.tc[2] = zc;

/*        Call subroutine EDIP to calculate the E-field components */
/*        in the C-axis system */

	mitray_edip__();

/*        The electric field components Ex, Ey, Ez are in the C-axis */
/*        system.  Transform them to the A-axis system. */

	mitray_edip_ectoea__(&mitray11_1.ex, &mitray11_1.ey, &mitray11_1.ez, &
		exa, &eya, &eza);

/*        Print out diagnostics if required */

	if (mitray_diag__1.ldiag) {
	    io___286.ciunit = gcunit_1.lout;
	    s_wsle(&io___286);
	    do_lio(&c__9, &c__1, "EXIT FRINGE FIELD REGION", (ftnlen)24);
	    e_wsle();
	    io___287.ciunit = gcunit_1.lout;
	    s_wsfe(&io___287);
	    do_fio(&c__1, "C SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray11_1.ex, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray11_1.ey, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray11_1.ez, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___288.ciunit = gcunit_1.lout;
	    s_wsfe(&io___288);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&exa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___289.ciunit = gcunit_1.lout;
	    s_wsle(&io___289);
	    do_lio(&c__9, &c__1, "EXIT FRINGE FIELD REGION", (ftnlen)24);
	    e_wsle();
	    io___290.ciunit = gcunit_1.lout;
	    s_wsfe(&io___290);
	    do_fio(&c__1, "C SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray11_1.ex, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray11_1.ey, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray11_1.ez, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___291.ciunit = gcunit_1.lout;
	    s_wsfe(&io___291);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&exa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}

/*       Load EXA, EYA, EZA into array EFLD and exit. */

	efld[1] = exa;
	efld[2] = eya;
	efld[3] = eza;
	return 0;

/*       -------------------------------------- */

    } else if (zb <= z12 && zc <= z21) {

/*        ************************ */
/*        *                      * */
/*        * UNIFORM FIELD REGION * */
/*        *                      * */
/*        ************************ */

/*        Set indicator IN=2 for uniform field region */

	mitray25_2.in = 2;
	mitray24_1.xc_offset__ = -mitray24_1.rb;
	mitray24_1.zc_offset__ = (float)0.;
	mitray22_2.ef = -eff;
	mitray22_2.s = (float)0.;

/*        LOAD THE C-AXIS COORDINATES INTO ARRAY TC BECAUSE THIS */
/*        IS WHERE SUBROUTINE EDIP EXPECTS TO FIND THEM */

	mitray10_1.tc[0] = xc;
	mitray10_1.tc[1] = yc;
	mitray10_1.tc[2] = zc;

/*        CALL SUBROUTINE EDIP TO CALCULATE THE E-FIELD COMPONENTS IN */
/*        THE C-AXIS SYSTEM */

	mitray_edip__();

/*        EX, EY, EZ ARE THE E-FIELD COMPONENTS IN THE C-SYSTEM. */
/*        TRANSFORM THEM TO THE A-SYSTEM */

	mitray_edip_ectoea__(&mitray11_1.ex, &mitray11_1.ey, &mitray11_1.ez, &
		exa, &eya, &eza);

/*        Print out diagnostics if required */

	if (mitray_diag__1.ldiag) {
	    io___292.ciunit = gcunit_1.lout;
	    s_wsle(&io___292);
	    do_lio(&c__9, &c__1, "UNIFORM FIELD REGION", (ftnlen)20);
	    e_wsle();
	    io___293.ciunit = gcunit_1.lout;
	    s_wsfe(&io___293);
	    do_fio(&c__1, "C SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray11_1.ex, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray11_1.ey, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray11_1.ez, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___294.ciunit = gcunit_1.lout;
	    s_wsfe(&io___294);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&exa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___295.ciunit = gcunit_1.lout;
	    s_wsle(&io___295);
	    do_lio(&c__9, &c__1, "UNIFORM FIELD REGION", (ftnlen)20);
	    e_wsle();
	    io___296.ciunit = gcunit_1.lout;
	    s_wsfe(&io___296);
	    do_fio(&c__1, "C SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray11_1.ex, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray11_1.ey, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray11_1.ez, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___297.ciunit = gcunit_1.lout;
	    s_wsfe(&io___297);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&exa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}

/*        Load EXA, EYA, EZA into array EFLD and exit. */

	efld[1] = exa;
	efld[2] = eya;
	efld[3] = eza;
	return 0;

/*     ----------------------------------------- */

    } else if (zb > z11 && xb >= xbmin && xb <= xbmax) {

/*        ********************** */
/*        *                    * */
/*        * ENTRANCE FAR FIELD * */
/*        *                    * */
/*        ********************** */

/*        SET INDICATOR IN=-99 */

	mitray25_2.in = -99;
	exa = (float)0.;
	eya = (float)0.;
	eza = (float)0.;

/*        PRINT DIAGNOSTICS IF REQUIRED */

	if (mitray_diag__1.ldiag) {
	    io___298.ciunit = gcunit_1.lout;
	    s_wsle(&io___298);
	    do_lio(&c__9, &c__1, "DIPOLE ENTRANCE FAR FIELD REGION", (ftnlen)
		    32);
	    e_wsle();
	    io___299.ciunit = gcunit_1.lout;
	    s_wsfe(&io___299);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&exa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___300.ciunit = gcunit_1.lout;
	    s_wsle(&io___300);
	    do_lio(&c__9, &c__1, "DIPOLE ENTRANCE FAR FIELD REGION", (ftnlen)
		    32);
	    e_wsle();
	    io___301.ciunit = gcunit_1.lout;
	    s_wsfe(&io___301);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&exa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}

/*        LOAD E-FIELD COMPONENTS INTO ARRAY EFLD AND EXIT */

	efld[1] = exa;
	efld[2] = eya;
	efld[3] = eza;
	return 0;

/*        ----------------------------------------- */

    } else if (zc > z22 && xc > xcmin && xc <= xcmax) {

/*           ****************** */
/*           *                * */
/*           * EXIT FAR FIELD * */
/*           *                * */
/*           ****************** */

/*        SET INDICATOR IN=+99 */

	mitray25_2.in = 99;
	exa = (float)0.;
	eya = (float)0.;
	eza = (float)0.;

/*        PRINT DIAGNOSTICS IF REQUIRED */

	if (mitray_diag__1.ldiag) {
	    io___302.ciunit = gcunit_1.lout;
	    s_wsle(&io___302);
	    do_lio(&c__9, &c__1, "DIPOLE EXIT FAR FIELD REGION", (ftnlen)28);
	    e_wsle();
	    io___303.ciunit = gcunit_1.lout;
	    s_wsfe(&io___303);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&exa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___304.ciunit = gcunit_1.lout;
	    s_wsle(&io___304);
	    do_lio(&c__9, &c__1, "DIPOLE EXIT FAR FIELD REGION", (ftnlen)28);
	    e_wsle();
	    io___305.ciunit = gcunit_1.lout;
	    s_wsfe(&io___305);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&exa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&eza, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}

/*        LOAD E-FIELD COMPONENTS INTO ARRAY EFLD AND EXIT */

	efld[1] = exa;
	efld[2] = eya;
	efld[3] = eza;
	return 0;

/*      ---------------------------------------------------- */

    } else {
/*        UNSPECIFIED FIELD REGION, RETURN WITH ZERO FIELD COMPONENTS */

	if (mitray_diag__1.ldiag) {
/* 	IF (1 .eq. 2)THEN */
	    io___306.ciunit = gcunit_1.lout;
	    s_wsle(&io___306);
	    do_lio(&c__9, &c__1, "UNKNOWN ELECTIC DIPOLE REGION", (ftnlen)29);
	    e_wsle();
	    io___307.ciunit = gcunit_1.lout;
	    s_wsfe(&io___307);
	    do_fio(&c__1, "A SYSTEM XA,YA,ZA=", (ftnlen)18);
	    do_fio(&c__1, (char *)&xa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&za, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___308.ciunit = gcunit_1.lout;
	    s_wsfe(&io___308);
	    do_fio(&c__1, "B SYSTEM XB,YB,ZB=", (ftnlen)18);
	    do_fio(&c__1, (char *)&xb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&yb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&zb, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___309.ciunit = gcunit_1.lout;
	    s_wsfe(&io___309);
	    do_fio(&c__1, "C SYSTEM XC,YC,ZC=", (ftnlen)18);
	    do_fio(&c__1, (char *)&xc, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&yc, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&zc, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___310.ciunit = gcunit_1.lout;
	    s_wsle(&io___310);
	    do_lio(&c__9, &c__1, "RETURN E-FIELD EXA=0, EYA=0, EZA=0", (
		    ftnlen)34);
	    e_wsle();
	    io___311.ciunit = gcunit_1.lout;
	    s_wsle(&io___311);
	    do_lio(&c__9, &c__1, "UNKNOWN ELECTIC DIPOLE REGION", (ftnlen)29);
	    e_wsle();
	    io___312.ciunit = gcunit_1.lout;
	    s_wsfe(&io___312);
	    do_fio(&c__1, "A SYSTEM XA,YA,ZA=", (ftnlen)18);
	    do_fio(&c__1, (char *)&xa, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ya, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&za, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___313.ciunit = gcunit_1.lout;
	    s_wsfe(&io___313);
	    do_fio(&c__1, "B SYSTEM XB,YB,ZB=", (ftnlen)18);
	    do_fio(&c__1, (char *)&xb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&yb, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&zb, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___314.ciunit = gcunit_1.lout;
	    s_wsfe(&io___314);
	    do_fio(&c__1, "C SYSTEM XC,YC,ZC=", (ftnlen)18);
	    do_fio(&c__1, (char *)&xc, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&yc, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&zc, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___315.ciunit = gcunit_1.lout;
	    s_wsle(&io___315);
	    do_lio(&c__9, &c__1, "RETURN E-FIELD EXA=0, EYA=0, EZA=0", (
		    ftnlen)34);
	    e_wsle();

/*           SET ALL E-FIELD COMPONENTS TO ZERO */

	    efld[1] = 0.;
	    efld[2] = 0.;
	    efld[3] = 0.;
	    return 0;
	}
    }
    return 0;
} /* mitray_edipol__ */


/* ========================================================================= */

/* Subroutine */ int mitray_edip__()
{
    /* Format strings */
    static char fmt_100[] = "(\002 ** ERROR ** -GO TO-  IN SUBROUTINE MITRAY\
_EDIP\002,/\002               INVALID VALUE  IN = \002,i5//)";
    static char fmt_101[] = "(\002 **ERROR ** IN SUBROUTINE MITRAY_EDIP\002\
/\002               X=\002,f12.3,\002  XC_OFFSET=\002,f12.3//)";

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();
    double sqrt(doublereal), asin(doublereal);

    /* Local variables */
    static doublereal x, y, z__, g1, g2, g3, g4, g5, g6, dr, dx, rp, re, rp2, 
	    efr, eft, drr, drr2, drr3;
    extern /* Subroutine */ int mitray_edpp__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal drr4;
    extern /* Subroutine */ int exit_();
    static doublereal cost, sint, theta;

    /* Fortran I/O blocks */
    static cilist io___321 = { 0, 6, 0, fmt_100, 0 };
    static cilist io___323 = { 0, 6, 0, fmt_101, 0 };


/* **** */
/* **** CALCULATES E-FIELD COMPONENTS FOR A CYLINDRICAL */
/* **** ELECTROSTATIC DEFLECTOR */
/* **** */



/* geant */
/* * GCUNIT */

/* **** */
/* **** */
/* **** */
/* **** */
/*       ADDED BY SY OCT 30/99  INITIAL VALUES OF E-FIELD */

/* L100: */
/* L101: */
    mitray11_1.ex = (float)0.;
    mitray11_1.ey = (float)0.;
    mitray11_1.ez = (float)0.;

    x = mitray10_1.tc[0];
    y = mitray10_1.tc[1];
    z__ = mitray10_1.tc[2];
    dx = x - mitray24_1.xc_offset__;
    rp2 = dx * dx + z__ * z__;
    switch (mitray25_2.in) {
	case 1:  goto L1;
	case 2:  goto L2;
	case 3:  goto L1;
    }
    s_wsfe(&io___321);
    do_fio(&c__1, (char *)&mitray25_2.in, (ftnlen)sizeof(integer));
    e_wsfe();
/*       ADDED BY SY  OCT 30/99 RETURN WITH ZERO FIELD IN CASE OF ILLEGAL */
/*       VALUE OF 'IN' */
    return 0;
/* **** */
/* ****   UNIFORM FIELD REGION   IN=2 */
/* **** */
L2:
    mitray11_1.ex = mitray22_2.ef * mitray24_1.rb * dx / rp2;
    mitray11_1.ey = (float)0.;
    mitray11_1.ez = mitray22_2.ef * mitray24_1.rb * z__ / rp2;
    mitray22_2.et = sqrt(mitray11_1.ex * mitray11_1.ex + mitray11_1.ez * 
	    mitray11_1.ez);
    return 0;
/* **** */
/* ****   FRINGE FIELD REGION    IN=1 OR IN=3 */
/* **** */
L1:
    rp = sqrt(rp2);
    if (abs(x) < abs(mitray24_1.xc_offset__)) {
	goto L3;
    }
    s_wsfe(&io___323);
    do_fio(&c__1, (char *)&x, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&mitray24_1.xc_offset__, (ftnlen)sizeof(doublereal))
	    ;
    e_wsfe();
    exit_();
L3:
    dr = rp - mitray24_1.rb;
    sint = z__ / rp;
    cost = (d__1 = mitray24_1.xc_offset__ - x, abs(d__1)) / rp;
    theta = asin(sint);
/* Computing 4th power */
    d__1 = y / mitray20_2.we, d__1 *= d__1;
    mitray22_2.s = theta * mitray24_1.rb / mitray22_2.d__ + mitray20_2.ec2 * 
	    y * y / (mitray20_2.we * mitray20_2.we) + mitray20_2.ec4 * (d__1 *
	     d__1);
    mitray_edpp__(&mitray22_2.d__, &mitray22_2.s, &re, &g1, &g2, &g3, &g4, &
	    g5, &g6);
    drr = dr / mitray24_1.rb;
    drr2 = drr * drr;
    drr3 = drr2 * drr;
    drr4 = drr3 * drr;
    efr = mitray22_2.ef * mitray24_1.rb * (re - drr2 * g2 / (float)2. + drr3 *
	     g2 / (float)2. + drr4 * (g4 - g2 * (float)11.) / (float)24.) / 
	    rp;
    eft = mitray22_2.ef * mitray24_1.rb * (drr * g1 - drr2 * g1 / (float)2. + 
	    drr3 * (g1 * (float)2. - g3) / (float)6. - drr4 * (g1 - g3) / (
	    float)4.) / rp;
    mitray11_1.ex = efr * cost - eft * sint;
    mitray11_1.ez = efr * sint + eft * cost;
    mitray22_2.et = sqrt(mitray11_1.ex * mitray11_1.ex + mitray11_1.ey * 
	    mitray11_1.ey + mitray11_1.ez * mitray11_1.ez);
    if (mitray25_2.in == 1) {
	mitray11_1.ez = -mitray11_1.ez;
    }
    return 0;
} /* mitray_edip__ */


/* ======================================================================== */

/* Subroutine */ int mitray_edpp__(doublereal *d__, doublereal *s, doublereal 
	*re, doublereal *g1, doublereal *g2, doublereal *g3, doublereal *g4, 
	doublereal *g5, doublereal *g6)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), exp(doublereal);

    /* Local variables */
    static doublereal e, s2, s3, s4, s5, cs, cp1, cp2, cp3, cp4, cp12, cp13, 
	    rbd, cp14, cp22, ere, ere1, ere2, ere3, ere4;

/* **** */
/* **** CALCULATE S; DETERMINE E-FIELD IN FRINGE REGIONS */
/* **** */



    s2 = *s * *s;
    s3 = s2 * *s;
    s4 = s2 * s2;
    s5 = s4 * *s;
    cs = mitray23_1.c0 + mitray23_1.c1 * *s + mitray23_1.c2 * s2 + 
	    mitray23_1.c3 * s3 + mitray23_1.c4 * s4 + mitray23_1.c5 * s5;
    rbd = mitray24_1.rb / *d__;
    cp1 = (mitray23_1.c1 + mitray23_1.c2 * (float)2. * *s + mitray23_1.c3 * (
	    float)3. * s2 + mitray23_1.c4 * (float)4. * s3 + mitray23_1.c5 * (
	    float)5. * s4) * rbd;
    cp2 = (mitray23_1.c2 * (float)2. + mitray23_1.c3 * (float)6. * *s + 
	    mitray23_1.c4 * (float)12. * s2 + mitray23_1.c5 * (float)20. * s3)
	     * rbd * rbd;
/* Computing 3rd power */
    d__1 = rbd;
    cp3 = (mitray23_1.c3 * (float)6. + mitray23_1.c4 * (float)24. * *s + 
	    mitray23_1.c5 * (float)60. * s2) * (d__1 * (d__1 * d__1));
/* Computing 4th power */
    d__1 = rbd, d__1 *= d__1;
    cp4 = (mitray23_1.c4 * (float)24. + mitray23_1.c5 * (float)120. * *s) * (
	    d__1 * d__1);
/* **** */
    if (abs(cs) > (float)70.) {
	cs = d_sign(&c_b203, &cs);
    }
    e = exp(cs);
    *re = (float)1. / (e + (float)1.);
    ere = e * *re;
    ere1 = ere * *re;
    ere2 = ere * ere1;
    ere3 = ere * ere2;
    ere4 = ere * ere3;
/* **** */
    cp12 = cp1 * cp1;
    cp13 = cp1 * cp12;
    cp14 = cp12 * cp12;
    cp22 = cp2 * cp2;
/* **** */
    *g1 = -cp1 * ere1;
/* **** */
    *g2 = -(cp2 + cp12) * ere1 + cp12 * (float)2. * ere2;
    *g3 = -(cp3 + cp1 * (float)3. * cp2 + cp13) * ere1 + (cp1 * cp2 + cp13) * 
	    (float)6. * ere2 - cp13 * (float)6. * ere3;
/* **** */
/* L1: */
    *g4 = -(cp4 + cp1 * (float)4. * cp3 + cp22 * (float)3. + cp12 * (float)6. 
	    * cp2 + cp14) * ere1 + (cp1 * (float)8. * cp3 + cp12 * (float)36. 
	    * cp2 + cp22 * (float)6. + cp14 * (float)14.) * ere2 - (cp12 * 
	    cp2 + cp14) * (float)36. * ere3 + cp14 * (float)24. * ere4;
    return 0;
} /* mitray_edpp__ */


/* ============================================================================ */

/* Subroutine */ int mitray_edip_ectoea__(doublereal *exc, doublereal *eyc, 
	doublereal *ezc, doublereal *exa, doublereal *eya, doublereal *eza)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    static doublereal ezb, exb, eyb, copab, sipab;


/*     TRANSFORM E-FIELD COMPONENTS FROM SYSTEM C TO SYSTEM A COORDINATES */
/*     OF AN ELECTOSTATIC DIPOLE. */

/*     INPUT:  EXC, EYC, EZC     E-FIELD COMPONENTS IN C-AXIS SYSTEM */
/*     OUTPUT: EXA, EYA, EZA     E-FIELD COMPONENTS IN A-AXIS SYSTEM */



/*     We use here the same formulae as for the magnetic dipole, but */
/*     with ALPHA=BETA=0 */

    copab = cos(-mitray_edipo__1.phi / 57.29577951);
    sipab = sin(-mitray_edipo__1.phi / 57.29577951);

/*     Next 3 lines not needed */

/*     COSPB = DCOS(-PHI/2. /57.29577951D0) */
/*     SINPB = DSIN(-PHI/2. /57.29577951D0) */
/*     SIP2 = DSIN(-PHI/2. /57.29577951D0) */

/*    NOW ROTATE TO GET E-FIELD COMPONENTS IN B-SYSTEM, IN TERMS OF C-SYSTEM */

    ezb = -(*ezc) * copab + *exc * sipab;
    exb = -(*ezc) * sipab - *exc * copab;
    eyb = *eyc;

/*     A-axis and B-axis systems are related by simple translation and */
/*     no rotation for the electrostatic dipole, so the E-field components */
/*     are the same in both B-axis and A- axis systems. */

    *exa = exb;
    *eya = eyb;
    *eza = ezb;
    return 0;
} /* mitray_edip_ectoea__ */


/* *********************************************************************** */
/*                          MULTI-POLES SUBROUTINE */
/* *********************************************************************** */

/* Subroutine */ int mitray_poles__(doublereal *data, doublereal *xpos, 
	doublereal *bfld)
{
    /* Format strings */
    static char fmt_900[] = "(1x,50(\002-\002)/\002 ENTER SUBROUTINE POLE\
S\002/\002 XA,YA,ZA=\002,3f10.3/)";
    static char fmt_901[] = "(\002 XB,YB,ZB=\002,3f10.3)";
    static char fmt_902[] = "(\002 XC,YC,ZC=\002,3f10.3)";
    static char fmt_903[] = "(\002 Z11=\002,f10.3,\002  Z12=\002,f10.3,\002 \
 Z21=\002,f10.3,\002  Z22=\002,f10.3)";
    static char fmt_904[] = "(1x,a,\002  BX,BY,BZ=\002,3f10.3)";

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(), 
	    s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle();

    /* Local variables */
    static doublereal a, b;
    static integer i__;
    static doublereal l, z11, z12, z21, z22, xb, yb, zb, xc, yc, zc, lf1, lf2,
	     lu1, bdc, bdd, boc, rad, bqd, frd, frh, bhx, fro;
    extern /* Subroutine */ int mitray_zone__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *)
	    ;
    static doublereal frdd;
    static integer izone;
    static doublereal grad1c, grad2c, grad3c, grad4c, grad5c;
    extern /* Subroutine */ int mitray_bpoles__();

    /* Fortran I/O blocks */
    static cilist io___393 = { 0, 0, 0, fmt_900, 0 };
    static cilist io___394 = { 0, 0, 0, fmt_901, 0 };
    static cilist io___395 = { 0, 0, 0, fmt_902, 0 };
    static cilist io___396 = { 0, 0, 0, fmt_903, 0 };
    static cilist io___397 = { 0, 0, 0, 0, 0 };
    static cilist io___399 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___405 = { 0, 0, 0, 0, 0 };
    static cilist io___406 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___407 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___408 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___409 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___410 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___411 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___412 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___413 = { 0, 0, 0, fmt_904, 0 };
    static cilist io___414 = { 0, 0, 0, 0, 0 };
    static cilist io___415 = { 0, 0, 0, 0, 0 };



/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
/*                                                                      C */
/*     Subroutine for multipoles, GEANT implementation of MIT-RAYTRACE  C */
/*     adapted from:                                                    C */
/*     Subroutine POLES (NO, NP, T, TP, NUM) by S. Kowalski             C */
/*                                                                      C */
/*      TC(1) to  TC(6) =  (  X,  Y,  Z, VX, VY, VZ )                   C */
/*     DTC(1) to DTC(6) =  ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )          C */
/*                                                                      C */
/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */

/*     Modification History */
/*     -------------------- */
/*     Mar 26, 1998   S. Yen */
/*     Apr  5, 1999   S. Yen  Calculate overlapping entrance and exit fringe */
/*                            fields for very short multipole. */




/* geant */



/* geant */
/* * GCUNIT */

/* local */

/* *** MITRAY_DIAG COMMON BLOCK */




/* local */

/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
/*                                                                      C */
/*           File diagnostic.inc - keeps diagnostic flags               C */
/*                                                                      C */
/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */




/*     First zero the output B-field in case of abort */

    /* Parameter adjustments */
    --bfld;
    --xpos;
    --data;

    /* Function Body */
    mitray_diag__1.ldiag = FALSE_;
    for (i__ = 1; i__ <= 3; ++i__) {
	bfld[i__] = (float)0.;
    }

/*     EXTRACT THE MULTIPOLE PARAMETERS FROM THE INPUT DATA ARRAY */

    lf1 = data[1];
    lu1 = data[2];
    lf2 = data[3];
    a = data[10];
    b = data[11];
    l = data[12];
    rad = data[13];
    bqd = data[14];
    bhx = data[15];
    boc = data[16];
    bdc = data[17];
    bdd = data[18];
    z11 = data[19];
    z12 = data[20];
    z21 = data[21];
    z22 = data[22];
    frh = data[35];
    fro = data[36];
    frd = data[37];
    frdd = data[38];
    mitray93_1.dsh = data[39];
    mitray93_1.dso = data[40];
    mitray93_1.dsd = data[41];
    mitray93_1.dsdd = data[42];
/*     DTF1= LF1/ VEL */
/*     DTF2= LF2/ VEL */
/*     DTU = LU1/ VEL */
    mitray90_1.d__ = rad * (float)2.;
    if (frh == (float)0.) {
	frh = 1.;
    }
    if (fro == (float)0.) {
	fro = 1.;
    }
    if (frd == (float)0.) {
	frd = 1.;
    }
    if (frdd == (float)0.) {
	frdd = 1.;
    }
    mitray93_1.dh = frh * mitray90_1.d__;
    mitray93_1.do__ = fro * mitray90_1.d__;
    mitray93_1.dd = frd * mitray90_1.d__;
    mitray93_1.ddd = frdd * mitray90_1.d__;

/*     Extract the A-axis coordinates from the XPOS array */

    mitray5_1.xa = xpos[1];
    mitray5_1.ya = xpos[2];
    mitray5_1.za = xpos[3];

/*     Calculate the B-axis coordinates (entrance VFB coordinates) */

    xb = -mitray5_1.xa;
    yb = mitray5_1.ya;
    zb = a - mitray5_1.za;

/*     Calculate the C-axis coordinates (exit VFB coordinates) */

    xc = -xb;
    yc = yb;
    zc = -zb - l;


    mitray10_1.bx = (float)0.;
    mitray10_1.by = (float)0.;
    mitray10_1.bz = (float)0.;
    mitray90_1.bt = (float)0.;
    mitray90_1.s = (float)0.;

    if (mitray_diag__1.ldiag) {
	io___393.ciunit = gcunit_1.lout;
	s_wsfe(&io___393);
	do_fio(&c__1, (char *)&mitray5_1.xa, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&mitray5_1.ya, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&mitray5_1.za, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }

/*     Print out coordinates if diagnostic mode */

    if (mitray_diag__1.ldiag) {
	io___394.ciunit = gcunit_1.lout;
	s_wsfe(&io___394);
	do_fio(&c__1, (char *)&xb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&yb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&zb, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___395.ciunit = gcunit_1.lout;
	s_wsfe(&io___395);
	do_fio(&c__1, (char *)&xc, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&yc, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&zc, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___396.ciunit = gcunit_1.lout;
	s_wsfe(&io___396);
	do_fio(&c__1, (char *)&z11, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&z12, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&z21, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&z22, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___397.ciunit = gcunit_1.lout;
	s_wsle(&io___397);
	do_lio(&c__9, &c__1, " ", (ftnlen)1);
	e_wsle();
    }

/*     Determine which zone of the multipole we are in by calling MITRAY_ZONE. */
/*     IZONE=0  far entrance or exit (B=0) */
/*     IZONE=1  entrance fringe field */
/*     IZONE=2  uniform field region */
/*     IZONE=3  exit fringe field */
/*     IZONE=4  overlapping entrance and exit fringe fields for short magnet */
/*              In this case, we calculate for the B-field at any point */
/*              B=B(entrance) + B(exit) - B(uniform).  This simulates */
/*              action of MIT-RAYTRACE in integrating *backwards* from end of */
/*              the entrance fringe field, to the start of the exit fringe */
/*              field, using the field of the uniform field region. */
/*     IZONE=-1 error */

    mitray_zone__(&zb, &zc, &z11, &z12, &z21, &z22, &izone);

/*     For each zone, set variable IN accordingly, and call MITRAY_BPOLES */
/*     to evaluate the magnetic field for that zone. */
/*     Note that for IZONE=4 (overlapping entrance and exit fringe fields, */
/*     for a very short multipole), the B-field that we want is */
/*     B(total) = B(entrance fringe) + B(exit fringe) - B(uniform region) */

    if (izone == 0) {

/*        *********************************** */
/*        * FAR ENTRANCE OR EXIT ZONES, B=0 * */
/*        *********************************** */

	if (mitray_diag__1.ldiag) {
	    io___399.ciunit = gcunit_1.lout;
	    s_wsfe(&io___399);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&c_b503, (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&c_b503, (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&c_b503, (ftnlen)sizeof(real));
	    e_wsfe();
	}
	return 0;
    }


/*     Calculate the gradients for the various multipole components. */
/*     The gradients evaluated below (with no minus signs) are correct when */
/*     we are working in the C-axis system */
/*     (uniform field and exit fringe field regions), but the signs of */
/*     the quadrupole, octopole and dodecapole gradients must be changed */
/*     when we work in the B-axis system (i.e. in the entrance fringe field */
/*     region). */

    grad1c = bqd / rad;
/* Computing 2nd power */
    d__1 = rad;
    grad2c = bhx / (d__1 * d__1);
/* Computing 3rd power */
    d__1 = rad;
    grad3c = boc / (d__1 * (d__1 * d__1));
/* Computing 4th power */
    d__1 = rad, d__1 *= d__1;
    grad4c = bdc / (d__1 * d__1);
/* Computing 5th power */
    d__1 = rad, d__2 = d__1, d__1 *= d__1;
    grad5c = bdd / (d__2 * (d__1 * d__1));

    if (izone == 2 || izone == 4) {

/*        Come here for either uniform field region (IZONE=1) or */
/*        overlapping entrance and exit fringe fields (IZONE=4). */

/*        ***************************************************** */
/*        * INTERIOR ("UNIFORM FIELD") ZONE FIELD CALCULATION * */
/*        ***************************************************** */

/*        Load the C-axis coordinates into array TC(i), which is what */
/*        subroutine BPOLES is expecting for the uniform field region */
/*        IN designates the region that we are in. */
	mitray92_1.in = 2;
	mitray90_1.s = (float)0.;
	mitray10_1.tc[0] = xc;
	mitray10_1.tc[1] = yc;
	mitray10_1.tc[2] = zc;

/*        We are working in the C-axis system, so use C-axis gradients */
	mitray90_1.grad1 = grad1c;
	mitray90_1.grad2 = grad2c;
	mitray90_1.grad3 = grad3c;
	mitray90_1.grad4 = grad4c;
	mitray90_1.grad5 = grad5c;

/*        CALL BPOLES TO CALCULATE B-FIELD IN C-AXIS SYSTEM */

	mitray_bpoles__();

	if (mitray_diag__1.ldiag) {
	    io___405.ciunit = gcunit_1.lout;
	    s_wsle(&io___405);
	    do_lio(&c__9, &c__1, "UNIFORM FIELD REGION CALC.", (ftnlen)26);
	    e_wsle();
	    io___406.ciunit = gcunit_1.lout;
	    s_wsfe(&io___406);
	    do_fio(&c__1, "C SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray10_1.bx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.by, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.bz, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___407.ciunit = gcunit_1.lout;
	    s_wsfe(&io___407);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray10_1.bx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.by, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.bz, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}

/*        THE B-FIELD COMPONENTS ARE IN THE C-AXIS SYSTEM; WHICH IS THE */
/*        SAME AS IN THE A-AXIS SYSTEM.  STORE THEM IN OUTPUT ARRAY BFLD(i). */

	if (izone == 2) {
	    bfld[1] = mitray10_1.bx;
	    bfld[2] = mitray10_1.by;
	    bfld[3] = mitray10_1.bz;
	    return 0;
	} else {
/*          IZONE=4  overlapping entrance & exit fringe fields */
/*          We add the uniform field components BX,BY,BZ to the total */
/*          field BFLD(i). */
/*          Note minus sign, since B=B(entrance)+B(exit)-B(uniform) */
	    bfld[1] -= mitray10_1.bx;
	    bfld[2] -= mitray10_1.by;
	    bfld[3] -= mitray10_1.bz;
	}
    }



    if (izone == 1 || izone == 4) {

/*        Come here for either pure entrance fringe field (IZONE=1) or */
/*        overlapping entrance and exit fringe fields (IZONE=4). */

/*        ****************************************** */
/*        * ENTRANCE FRINGE FIELD ZONE CALCULATION * */
/*        ****************************************** */

/*        Set IN=1 to designate entrance fringe field, and extract the */
/*        fringing field coefficients */
	mitray92_1.in = 1;
	mitray91_1.c0 = data[23];
	mitray91_1.c1 = data[24];
	mitray91_1.c2 = data[25];
	mitray91_1.c3 = data[26];
	mitray91_1.c4 = data[27];
	mitray91_1.c5 = data[28];

/*        Load the B-axis coordinates into the array TC(i), which is */
/*        what subroutine BPOLES is expecting */
	mitray10_1.tc[0] = xb;
	mitray10_1.tc[1] = yb;
	mitray10_1.tc[2] = zb;

/*        We are working in the B-axis system, so change the signs of */
/*        the quadrupole, octopole and dodecapole gradients from those */
/*        of the C-axis system. */
	mitray90_1.grad1 = -grad1c;
	mitray90_1.grad2 = grad2c;
	mitray90_1.grad3 = -grad3c;
	mitray90_1.grad4 = grad4c;
	mitray90_1.grad5 = -grad5c;


/*        Call BPOLES to calculate B-field in B-axis system */

	mitray_bpoles__();

	if (mitray_diag__1.ldiag) {
	    io___408.ciunit = gcunit_1.lout;
	    s_wsfe(&io___408);
	    do_fio(&c__1, "ENTRANCE FRINGE FIELD CALC.", (ftnlen)27);
	    e_wsfe();
	    io___409.ciunit = gcunit_1.lout;
	    s_wsfe(&io___409);
	    do_fio(&c__1, "B SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray10_1.bx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.by, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.bz, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___410.ciunit = gcunit_1.lout;
	    s_wsfe(&io___410);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    d__1 = -mitray10_1.bx;
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.by, (ftnlen)sizeof(doublereal));
	    d__2 = -mitray10_1.bz;
	    do_fio(&c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}

/*        THE B-FIELD COMPONENTS BX,BY,BZ ARE IN THE B-AXIS SYSTEM; CONVERT TO */
/*        THE A-AXIS SYSTEM.  STORE THEM IN OUTPUT ARRAY BFLD(i). */

	if (izone == 1) {
	    bfld[1] = -mitray10_1.bx;
	    bfld[2] = mitray10_1.by;
	    bfld[3] = -mitray10_1.bz;
	    return 0;
	} else {
/*          ! IZONE.EQ.4  overlapping entrance & exit fringe fields */
/*          We add the entrance field components BX,BY,BZ to the total */
/*          field BFLD(i), since B(total)=B(entrance)+B(exit)-B(uniform) */
/*          Note the sign changes for BX and BZ since these are in the B-axis */
/*          system and we need to change them to the A-axis system. */
	    bfld[1] -= mitray10_1.bx;
	    bfld[2] += mitray10_1.by;
	    bfld[3] -= mitray10_1.bz;
	}
    }



    if (izone == 3 || izone == 4) {

/*        Come here for either pure exit fringe field (IZONE=3) or */
/*        overlapping entrance and exit fringe fields (IZONE=4). */

/*        ************************************** */
/*        * EXIT FRINGE FIELD ZONE CALCULATION * */
/*        ************************************** */

/*        Set IN=3 to designate exit fringe field, and extract fringing */
/*        coefficients for the exit fringe field. */
	mitray92_1.in = 3;
	mitray91_1.c0 = data[29];
	mitray91_1.c1 = data[30];
	mitray91_1.c2 = data[31];
	mitray91_1.c3 = data[32];
	mitray91_1.c4 = data[33];
	mitray91_1.c5 = data[34];

/*        Load the C-axis coordinates into the array TC(i), which is */
/*        what subroutine BPOLES is expecting */
	mitray10_1.tc[0] = xc;
	mitray10_1.tc[1] = yc;
	mitray10_1.tc[2] = zc;

/*        We are working in the C-axis system, so use C-axis gradients */
	mitray90_1.grad1 = grad1c;
	mitray90_1.grad2 = grad2c;
	mitray90_1.grad3 = grad3c;
	mitray90_1.grad4 = grad4c;
	mitray90_1.grad5 = grad5c;


/*        Call BPOLES to calculate B-field in C-axis system */

	mitray_bpoles__();

	if (mitray_diag__1.ldiag) {
	    io___411.ciunit = gcunit_1.lout;
	    s_wsfe(&io___411);
	    do_fio(&c__1, "EXIT FRINGE FIELD CALC.", (ftnlen)23);
	    e_wsfe();
	    io___412.ciunit = gcunit_1.lout;
	    s_wsfe(&io___412);
	    do_fio(&c__1, "C SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray10_1.bx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.by, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.bz, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	    io___413.ciunit = gcunit_1.lout;
	    s_wsfe(&io___413);
	    do_fio(&c__1, "A SYSTEM", (ftnlen)8);
	    do_fio(&c__1, (char *)&mitray10_1.bx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.by, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&mitray10_1.bz, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}

/*        THE B-FIELD COMPONENTS BX,BY,BZ ARE IN THE C-AXIS SYSTEM; CONVERT TO */
/*        THE A-AXIS SYSTEM (which is the same). */
/*        STORE THEM IN OUTPUT ARRAY BFLD(i). */

	if (izone == 3) {
	    bfld[1] = mitray10_1.bx;
	    bfld[2] = mitray10_1.by;
	    bfld[3] = mitray10_1.bz;
	    return 0;
	} else {
/*          ! IZONE=4 overlapping entrance and exit fringe fields */
/*          We add the exit field components BX,BY,BZ to the total */
/*          field BFLD(i) since B(total)=B(entrance)+B(exit)-B(uniform) */
	    bfld[1] += mitray10_1.bx;
	    bfld[2] += mitray10_1.by;
	    bfld[3] += mitray10_1.bz;
	    return 0;
	}
    }

    if (izone == -1) {
/*        UNKNOWN FIELD REGION, RETURN WITH B=0 */
	io___414.ciunit = gcunit_1.lout;
	s_wsle(&io___414);
	do_lio(&c__9, &c__1, "UNKNOWN MULTIPOLE FIELD REGION", (ftnlen)30);
	e_wsle();
	io___415.ciunit = gcunit_1.lout;
	s_wsle(&io___415);
	do_lio(&c__9, &c__1, "!!! Abort current event !!!", (ftnlen)27);
	e_wsle();
	diag_1.jstop = 1;
	gcflag_1.ieotri = 1;
	return 0;
    }
    return 0;
} /* mitray_poles__ */


/* ======================================================================= */

/* Subroutine */ int mitray_bpoles__()
{
    /* Format strings */
    static char fmt_3[] = "(\002  ERROR IN BPOLES IN= \002,i5///)";

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();
    /* Subroutine */ int s_stop(char *, ftnlen);
    double sqrt(doublereal);

    /* Local variables */
    static doublereal x, y, z__, g1, g2, g3, g4, g5, g6, x2, x3, x4, x5, x6, 
	    x7, y2, y3, y4, y5, y6, y7, re, ss, b2x, b2y, b3x, b3y, b4x, b4y, 
	    b5x, b5y, b6x, b6y, b2z, b3z, b4z, b5z, b6z;
    extern /* Subroutine */ int mitray_bpls__(integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);

    /* Fortran I/O blocks */
    static cilist io___431 = { 0, 6, 0, fmt_3, 0 };


/* **** */
/* **** CALCULATION OF MULTIPOLE(POLES) FIELD COMPONENTS */
/* **** */
/* **** */
/* **** */
/* **** 2 - QUADRUPOLE  (GRAD1) */
/* **** 3 - HEXAPOLE    (GRAD2) */
/* **** 4 - OCTAPOLE    (GRAD3) */
/* **** 5 - DECAPOLE    (GRAD4) */
/* **** 6 - DODECAPOLE  (GRAD5) */
/* **** */
/* **** */
    x = mitray10_1.tc[0];
    y = mitray10_1.tc[1];
    z__ = mitray10_1.tc[2];
    x2 = x * x;
    x3 = x2 * x;
    x4 = x3 * x;
    x5 = x4 * x;
    x6 = x5 * x;
    x7 = x6 * x;
    y2 = y * y;
    y3 = y2 * y;
    y4 = y3 * y;
    y5 = y4 * y;
    y6 = y5 * y;
    y7 = y6 * y;
    switch (mitray92_1.in) {
	case 1:  goto L2;
	case 2:  goto L1;
	case 3:  goto L2;
    }
    s_wsfe(&io___431);
    do_fio(&c__1, (char *)&mitray92_1.in, (ftnlen)sizeof(integer));
    e_wsfe();
    s_stop("", (ftnlen)0);
L1:
    b2x = mitray90_1.grad1 * y;
    b2y = mitray90_1.grad1 * x;
    b3x = mitray90_1.grad2 * (float)2. * x * y;
    b3y = mitray90_1.grad2 * (x2 - y2);
    b4x = mitray90_1.grad3 * (x2 * (float)3. * y - y3);
    b4y = mitray90_1.grad3 * (x3 - x * (float)3. * y2);
    b5x = mitray90_1.grad4 * (float)4. * (x3 * y - x * y3);
    b5y = mitray90_1.grad4 * (x4 - x2 * (float)6. * y2 + y4);
    b6x = mitray90_1.grad5 * (x4 * (float)5. * y - x2 * (float)10. * y3 + y5);
    b6y = mitray90_1.grad5 * (x5 - x3 * (float)10. * y2 + x * (float)5. * y4);
    mitray10_1.bx = b2x + b3x + b4x + b5x + b6x;
    mitray10_1.by = b2y + b3y + b4y + b5y + b6y;
    mitray10_1.bz = (float)0.;
    mitray90_1.bt = sqrt(mitray10_1.bx * mitray10_1.bx + mitray10_1.by * 
	    mitray10_1.by);
    return 0;
/* **** */
/* **** */
/* **** QUADRUPOLE */
/* **** */
L2:
    mitray90_1.s = z__ / mitray90_1.d__;
    mitray_bpls__(&c__2, &mitray90_1.d__, &mitray90_1.s, &re, &g1, &g2, &g3, &
	    g4, &g5, &g6);
    b2x = mitray90_1.grad1 * (re * y - g2 / (float)12. * (x2 * (float)3. * y 
	    + y3) + g4 / (float)384. * (x4 * (float)5. * y + x2 * (float)6. * 
	    y3 + y5) - g6 / (float)23040. * (x6 * (float)7. * y + x4 * (float)
	    15. * y3 + x2 * (float)9. * y5 + y7));
    b2y = mitray90_1.grad1 * (re * x - g2 / (float)12. * (x3 + x * (float)3. *
	     y2) + g4 / (float)384. * (x5 + x3 * (float)6. * y2 + x * (float)
	    5. * y4) - g6 / (float)23040. * (x7 + x5 * (float)9. * y2 + x3 * (
	    float)15. * y4 + x * (float)7. * y6));
    b2z = mitray90_1.grad1 * (g1 * x * y - g3 / (float)12. * (x3 * y + x * y3)
	     + g5 / (float)384. * (x5 * y + x3 * (float)2. * y3 + x * y5));
/* **** */
/* **** HEXAPOLE */
/* **** */
    ss = z__ / mitray93_1.dh + mitray93_1.dsh;
    mitray_bpls__(&c__3, &mitray93_1.dh, &ss, &re, &g1, &g2, &g3, &g4, &g5, &
	    g6);
    b3x = mitray90_1.grad2 * (re * (float)2. * x * y - g2 / (float)48. * (x3 *
	     (float)12. * y + x * (float)4. * y3));
    b3y = mitray90_1.grad2 * (re * (x2 - y2) - g2 / (float)48. * (x4 * (float)
	    3. + x2 * (float)6. * y2 - y4 * (float)5.));
    b3z = mitray90_1.grad2 * (g1 * (x2 * y - y3 / (float)3.) - g3 / (float)
	    48. * (x4 * (float)3. * y + x2 * (float)2. * y3 - y5));
/* **** */
/* **** OCTAPOLE */
/* **** */
    ss = z__ / mitray93_1.do__ + mitray93_1.dso;
    mitray_bpls__(&c__4, &mitray93_1.do__, &ss, &re, &g1, &g2, &g3, &g4, &g5, 
	    &g6);
    b4x = mitray90_1.grad3 * (re * (x2 * (float)3. * y - y3) - g2 / (float)
	    80. * (x4 * (float)20. * y - y5 * (float)4.));
    b4y = mitray90_1.grad3 * (re * (x3 - x * (float)3. * y2) - g2 / (float)
	    80. * (x5 * (float)4. - x * (float)20. * y4));
    b4z = mitray90_1.grad3 * g1 * (x3 * y - x * y3);
/* **** */
/* **** DECAPOLE */
/* **** */
    ss = z__ / mitray93_1.dd + mitray93_1.dsd;
    mitray_bpls__(&c__5, &mitray93_1.dd, &ss, &re, &g1, &g2, &g3, &g4, &g5, &
	    g6);
    b5x = mitray90_1.grad4 * re * (x3 * (float)4. * y - x * (float)4. * y3);
    b5y = mitray90_1.grad4 * re * (x4 - x2 * (float)6. * y2 + y4);
    b5z = mitray90_1.grad4 * g1 * (x4 * y - x2 * (float)2. * y3 + y5 / (float)
	    5.);
/* **** */
/* **** DODECAPOLE */
/* **** */
    ss = z__ / mitray93_1.ddd + mitray93_1.dsdd;
    mitray_bpls__(&c__6, &mitray93_1.ddd, &ss, &re, &g1, &g2, &g3, &g4, &g5, &
	    g6);
    b6x = mitray90_1.grad5 * re * (x4 * (float)5. * y - x2 * (float)10. * y3 
	    + y5);
    b6y = mitray90_1.grad5 * re * (x5 - x3 * (float)10. * y2 + x * (float)5. *
	     y4);
    b6z = (float)0.;
/* **** */
/* **** TOTAL FIELD */
/* **** */
    mitray10_1.bx = b2x + b3x + b4x + b5x + b6x;
    mitray10_1.by = b2y + b3y + b4y + b5y + b6y;
    mitray10_1.bz = b2z + b3z + b4z + b5z + b6z;
    mitray90_1.bt = sqrt(mitray10_1.bx * mitray10_1.bx + mitray10_1.by * 
	    mitray10_1.by + mitray10_1.bz * mitray10_1.bz);
    return 0;
} /* mitray_bpoles__ */


/* ======================================================================= */

/* Subroutine */ int mitray_bpls__(integer *igp, doublereal *d__, doublereal *
	s, doublereal *re, doublereal *g1, doublereal *g2, doublereal *g3, 
	doublereal *g4, doublereal *g5, doublereal *g6)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), exp(doublereal);

    /* Local variables */
    static doublereal e, s2, s3, s4, s5, cs, cp1, cp2, cp3, cp4, cp5, cp12, 
	    cp13, cp14, cp22, cp15, cp16, ere, cp23, cp32, ere1, ere2, ere3, 
	    ere4, ere5, ere6;

/* **** */
/* **** */
/* **** */
/* **** */
/* **** */
/* **** */
/* **** */
    s2 = *s * *s;
    s3 = s2 * *s;
    s4 = s2 * s2;
    s5 = s4 * *s;
    cs = mitray91_1.c0 + mitray91_1.c1 * *s + mitray91_1.c2 * s2 + 
	    mitray91_1.c3 * s3 + mitray91_1.c4 * s4 + mitray91_1.c5 * s5;
    cp1 = (mitray91_1.c1 + mitray91_1.c2 * (float)2. * *s + mitray91_1.c3 * (
	    float)3. * s2 + mitray91_1.c4 * (float)4. * s3 + mitray91_1.c5 * (
	    float)5. * s4) / *d__;
    cp2 = (mitray91_1.c2 * (float)2. + mitray91_1.c3 * (float)6. * *s + 
	    mitray91_1.c4 * (float)12. * s2 + mitray91_1.c5 * (float)20. * s3)
	     / (*d__ * *d__);
/* Computing 3rd power */
    d__1 = *d__;
    cp3 = (mitray91_1.c3 * (float)6. + mitray91_1.c4 * (float)24. * *s + 
	    mitray91_1.c5 * (float)60. * s2) / (d__1 * (d__1 * d__1));
/* Computing 4th power */
    d__1 = *d__, d__1 *= d__1;
    cp4 = (mitray91_1.c4 * (float)24. + mitray91_1.c5 * (float)120. * *s) / (
	    d__1 * d__1);
/* **** */
/* Computing 5th power */
    d__1 = *d__, d__2 = d__1, d__1 *= d__1;
    cp5 = mitray91_1.c5 * (float)120. / (d__2 * (d__1 * d__1));
/* **** */
/* **** */
/* **** */
    if (abs(cs) > (float)70.) {
	cs = d_sign(&c_b203, &cs);
    }
    e = exp(cs);
    *re = (float)1. / (e + (float)1.);
    ere = e * *re;
    ere1 = ere * *re;
    ere2 = ere * ere1;
    ere3 = ere * ere2;
    ere4 = ere * ere3;
/* **** */
    ere5 = ere * ere4;
    ere6 = ere * ere5;
/* **** */
/* **** */
    cp12 = cp1 * cp1;
    cp13 = cp1 * cp12;
    cp14 = cp12 * cp12;
    cp22 = cp2 * cp2;
/* **** */
    cp15 = cp12 * cp13;
    cp16 = cp13 * cp13;
    cp23 = cp2 * cp22;
    cp32 = cp3 * cp3;
/* **** */
/* **** */
    if (*igp == 6) {
	return 0;
    }
    *g1 = -cp1 * ere1;
/* **** */
/* **** */
    if (*igp == 5) {
	return 0;
    }
    *g2 = -(cp2 + cp12) * ere1 + cp12 * (float)2. * ere2;
    if (*igp == 4) {
	return 0;
    }
    *g3 = -(cp3 + cp1 * (float)3. * cp2 + cp13) * ere1 + (cp1 * cp2 + cp13) * 
	    (float)6. * ere2 - cp13 * (float)6. * ere3;
/* **** */
/* **** */
    if (*igp == 3) {
	return 0;
    }
/* L1: */
    *g4 = -(cp4 + cp1 * (float)4. * cp3 + cp22 * (float)3. + cp12 * (float)6. 
	    * cp2 + cp14) * ere1 + (cp1 * (float)8. * cp3 + cp12 * (float)36. 
	    * cp2 + cp22 * (float)6. + cp14 * (float)14.) * ere2 - (cp12 * 
	    cp2 + cp14) * (float)36. * ere3 + cp14 * (float)24. * ere4;
/* **** */
/* **** */
    if (*igp != 2) {
	return 0;
    }
    *g5 = (-cp5 - cp1 * (float)5. * cp4 - cp2 * (float)10. * cp3 - cp12 * (
	    float)10. * cp3 - cp1 * (float)15. * cp22 - cp13 * (float)10. * 
	    cp2 - cp15) * ere1 + (cp1 * (float)10. * cp4 + cp2 * (float)20. * 
	    cp3 + cp12 * (float)60. * cp3 + cp1 * (float)90. * cp22 + cp13 * (
	    float)140. * cp2 + cp15 * (float)30.) * ere2 + (cp12 * (float)
	    -60. * cp3 - cp1 * (float)90. * cp22 - cp13 * (float)360. * cp2 - 
	    cp15 * (float)150.) * ere3 + (cp13 * (float)240. * cp2 + cp15 * (
	    float)240.) * ere4 + cp15 * (float)-120. * ere5;
    *g6 = (cp1 * (float)-6. * cp5 - cp2 * (float)15. * cp4 - cp12 * (float)
	    15. * cp4 - cp32 * (float)10. - cp1 * (float)60. * cp2 * cp3 - 
	    cp13 * (float)20. * cp3 - cp23 * (float)15. - cp12 * (float)45. * 
	    cp22 - cp14 * (float)15. * cp2 - cp16) * ere1 + (cp1 * (float)12. 
	    * cp5 + cp2 * (float)30. * cp4 + cp12 * (float)90. * cp4 + cp32 * 
	    (float)20. + cp1 * (float)360. * cp2 * cp3 + cp13 * (float)280. * 
	    cp3 + cp23 * (float)90. + cp12 * (float)630. * cp22 + cp14 * (
	    float)450. * cp2 + cp16 * (float)62.) * ere2 + (cp12 * (float)
	    -90. * cp4 - cp1 * (float)360. * cp2 * cp3 - cp13 * (float)720. * 
	    cp3 - cp23 * (float)90. - cp12 * (float)1620. * cp22 - cp14 * (
	    float)2250. * cp2 - cp16 * (float)540.) * ere3 + (cp13 * (float)
	    480. * cp3 + cp12 * (float)1080. * cp22 + cp14 * (float)3600. * 
	    cp2 + cp16 * (float)1560.) * ere4 + (cp14 * (float)-1800. * cp2 - 
	    cp16 * (float)1800.) * ere5 + cp16 * (float)720. * ere6;
/* **** */
    return 0;
} /* mitray_bpls__ */


/* *********************************************************************** */
/*                               SASP SUBROUTINES */
/* *********************************************************************** */

/* Subroutine */ int mitray_sasp__(doublereal *data, doublereal *xpos, 
	doublereal *bfld)
{
    /* Format strings */
    static char fmt_100[] = "(\002 **ERROR** IN SUBROUTINE SASP\002/\002 NEE\
D MTYP=4 FOR SASP\002/\002 BUT MTYP = \002,i5,\002 WAS SPECIFIED\002)";

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int mitray_saspratio__(doublereal *, doublereal *,
	     doublereal *);
    static doublereal bfld_ideal__[3], xa, ya, za, bf0;
    static integer mtyp;
    static doublereal ratio;
    extern /* Subroutine */ int mitray_dipole__(doublereal *, doublereal *, 
	    doublereal *);

    /* Fortran I/O blocks */
    static cilist io___487 = { 0, 0, 0, fmt_100, 0 };



/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
/*                                                                      C */
/*     SUBROUTINE TO CALCULATE FIELD IN SASP DIPOLE.                    C */
/*                                                                      C */
/*     This is the same as a normal MTYP=4 (clamshell) dipole except    C */
/*     that sagging of the high end magnetic field due to saturation    C */
/*     is taken into account empirically.  The variation of the         C */
/*     B-field with XA is measured with scans of the Rawson-Lush        C */
/*     rotating coil Gaussmeter along the central axis of the dipole.   C */
/*                                                                      C */
/*     S. Yen (TRIUMF)  e-mail  STAN@TRIUMF.CA                          C */
/*                                                                      C */
/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */




/* geant */
/* * GCUNIT */


/*     ZERO THE OUTPUT B-FIELD VECTOR */

/* local */

/* *** MITRAY_DIAG COMMON BLOCK */



    /* Parameter adjustments */
    --bfld;
    --xpos;
    --data;

    /* Function Body */
    for (i__ = 1; i__ <= 3; ++i__) {
	bfld[i__] = (float)0.;
    }

/*     Extract the A-axis coordinates */
    xa = xpos[1];
    ya = xpos[2];
    za = xpos[3];

/*     EXTRACT THE PARAMETERS FOR THE DIPOLE MAGNET */

    mtyp = (integer) data[5];
    bf0 = data[15];

/*     Check that MTYP=4 as required for SASP dipole */

    if (mtyp != 4) {
	io___487.ciunit = gcunit_1.lout;
	s_wsfe(&io___487);
	do_fio(&c__1, (char *)&mtyp, (ftnlen)sizeof(integer));
	e_wsfe();
	s_stop("", (ftnlen)0);
    }

/*     Calculate the B-field at point (XA,YA,ZA)=(XPOS(1),XPOS(2),XPOS(3)) */
/*     as if it were an ideal clamshell with no saturation */

    mitray_dipole__(&data[1], &xpos[1], bfld_ideal__);

/*     We assume that the field departs from the ideal shape as a function */
/*     of XA only. We calculate the ratio: */
/*                 RATIO=B_MEAS(XA,YA=0,ZA=0)/B_IDEAL(XA,YA=0,ZA=0) */
/*     and apply this correction to the ideal B-field. */

    mitray_saspratio__(&bf0, &xa, &ratio);

    for (i__ = 1; i__ <= 3; ++i__) {
	bfld[i__] = bfld_ideal__[i__ - 1] * ratio;
    }

    return 0;
} /* mitray_sasp__ */


/* ======================================================================= */

/* Subroutine */ int mitray_saspratio__(doublereal *bmeas0, doublereal *xa, 
	doublereal *ratio)
{
    /* Initialized data */

    static doublereal bxeq0[10] = { 2.00225,3.57472,6.92151,9.86762,12.2436,
	    14.06667,15.80831,16.48228,17.47663,18.02981 };
    static doublereal c1[10] = { .001962394,.001922171,.001962442,.00208478,
	    .002007846,.002323137,.002777501,.003066965,.003613984,.003992692 
	    };
    static doublereal c2[10] = { -2.293013e-4,-2.163218e-4,-2.25595e-4,
	    -2.388581e-4,-2.227859e-4,-2.495408e-4,-2.666869e-4,-2.809122e-4,
	    -3.05941e-4,-3.185566e-4 };
    static doublereal c3[10] = { 1.234833e-5,1.130863e-5,1.223721e-5,
	    1.280996e-5,1.177408e-5,1.319004e-5,1.364456e-5,1.428083e-5,
	    1.525917e-5,1.554105e-5 };
    static doublereal c4[10] = { -3.537459e-7,-3.149187e-7,-3.549376e-7,
	    -3.653301e-7,-3.317632e-7,-3.747899e-7,-3.827231e-7,-4.005838e-7,
	    -4.234547e-7,-4.266711e-7 };
    static doublereal c5[10] = { 5.551259e-9,4.811879e-9,5.644852e-9,
	    5.698301e-9,5.115515e-9,5.839129e-9,5.921277e-9,6.196709e-9,
	    6.482142e-9,6.491555e-9 };
    static doublereal c6[10] = { -4.500169e-11,-3.808138e-11,-4.635942e-11,
	    -4.587436e-11,-4.072955e-11,-4.697908e-11,-4.742479e-11,
	    -4.957102e-11,-5.130691e-11,-5.120422e-11 };
    static doublereal c7[10] = { 1.472579e-13,1.220355e-13,1.535346e-13,
	    1.490287e-13,1.309286e-13,1.525184e-13,1.5345e-13,1.600426e-13,
	    1.639181e-13,1.632841e-13 };

    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle();
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static integer i__, j;
    static doublereal xiestimate, xrl, diff, frac, xrlb, xrlc, slope, absbf0;
    static integer istop, index1, index2;
    static doublereal ratio1, ratio2, ratiob, ratioc;
    static integer istart;
    static doublereal ratioib, ratioic, ratiojb, ratiojc;

    /* Fortran I/O blocks */
    static cilist io___506 = { 0, 0, 0, 0, 0 };
    static cilist io___507 = { 0, 0, 0, 0, 0 };



/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
/*                                                                      C */
/*     Subroutine to calculate the ratio of measured to ideal B-field   C */
/*     strength                                                         C */
/*                                                                      C */
/*       RATIO = B_MEAS / B_IDEAL                                       C */
/*                                                                      C */
/*     at the specified value of XA, for the TRIUMF SASP dipole.        C */
/*     RATIO departs most markedly from 1 for large values of BF0,      C */
/*     because of saturation of the steel in the SASP dipole.           C */
/*                                                                      C */
/*     input                                                            C */
/*     -----                                                            C */
/*     BMEAS0   actual measured B-field at XA=0, as measured with the   C */
/*              Rawson-Lush rotating Gaussmeter (kiloGauss)             C */
/*     XA       X-position of the field point, in the dipole            C */
/*              XA system (cm). The "X=0" reference point of the        C */
/*              RL-probe corresponds to XA=0.                           C */
/*                                                                      C */
/*     output                                                           C */
/*     ------                                                           C */
/*     RATIO    = B_MEAS / B_IDEAL                                      C */
/*                                                                      C */
/*     We now have to interpolate the following table to get the        C */
/*     ratio of measured to ideal fields.                               C */
/*                                                                      C */
/*     file usr0:[mrsbeam.map]rl_parameters.dat                         C */
/*     also file prv1:[stan.map]rl_parameters.dat                       C */
/*                                                                      C */
/*     These are parameterizations of the field of the SASP dipole as   C */
/*     measured by the Rawson-Lush probe, along the center line of the  C */
/*     dipole.  The data can be found in the files                      C */
/*             ERICH::PRV1:[STAN.MAP]RL_PROFILE_???AMPS.DAT.            C */
/*     PLOTDATA macro ERICH::PRV1:[STAN.MAP]FITRL.PCM was used to fit   C */
/*     these.                                                           C */
/*     First the region x>40 cm (which is least saturated) was fitted   C */
/*     with a function of the ideal clamshell form                      C */
/*                                                                      C */
/*     B(x) = B0/(1+n*x/R), where n=1300 and R=220000.                  C */
/*                                                                      C */
/*     where B0 is varied to achieve the best fit.                      C */
/*                                                                      C */
/*     Next, B0 is held fixed,and the entire field is fit with the form C */
/*                                                                      C */
/*     B(x) = B0/(1+n*x/R) * (a0 + a1*x + ... + a7*x**7)                C */
/*                                                                      C */
/*     This procedure is able to reproduce the data to better than      C */
/*     5.E-4 (1 part in 2000) over the most of the region of interest.  C */
/*                                                                      C */
/* current    B0      a0          a1            a2            a3            a4            a5            a6    
        a7 */

/*  100   2.010635  .9958276  +1.954237E-3  -2.283502E-4  +1.229721E-5  -3.522838E-7  +5.528341E-9  -4.481606E
-11  +1.466508E-13 */
/*  180   3.590952  .9954798  +1.913474E-3  -2.153433E-4  +1.125749E-5  -3.134953E-7  +4.790139E-9  -3.790939E
-11  +1.214844E-13 */
/*  350   6.955757  .9950759  +1.952778E-3  -2.244842E-4  +1.217696E-5  -3.531899E-7  +5.617057E-9  -4.613114E
-11  +1.527785E-13 */
/*  500   9.919624  .9947577  +2.073851E-3  -2.376060E-4  +1.274280E-5  -3.634150E-7  +5.668429E-9  -4.563387E
-11  +1.482475E-13 */
/*  625   12.31721  .9940234  +1.995845E-3  -2.214544E-4  +1.170371E-5  -3.297804E-7  +5.084942E-9  -4.048612E
-11  +1.301461E-13 */
/*  725   14.18361  .9917550  +2.303983E-3  -2.474834E-4  +1.308128E-5  -3.716997E-7  +5.790985E-9  -4.659174E
-11  +1.512609E-13 */
/*  829   16.04223  .9854187  +2.737002E-3  -2.627983E-4  +1.344560E-5  -3.771425E-7  +5.834937E-9  -4.673327E
-11  +1.512125E-13 */
/*  875   16.80113  .9810218  +3.008759E-3  -2.755810E-4  +1.400980E-5  -3.929814E-7  +6.079107E-9  -4.863025E
-11  +1.570053E-13 */
/*  950   17.97734  .9721475  +3.513326E-3  -2.974198E-4  +1.483416E-5  -4.116604E-7  +6.301599E-9  -4.987789E
-11  +1.593526E-13 */
/* 1000   18.66637  .965898   +3.856533E-3  -3.076932E-4  +1.501107E-5  -4.121208E-7  +6.270180E-9  -4.945806E
-11  +1.577158E-13 */
/*                                                                      C */
/*     Since B(x) = B0 / (1+n*x/R) * (a0 + a1*x + ... + a7*x**7),       C */
/*     we pull out a constant factor a0 and multiply it with B0 to give C */
/*                                                                      C */
/*     BMEAS0 = B0*a0, and C1=a1/a0, C2=a2/a0, C3=a3/a0, ..., C7=a7/a0. C */
/*                                                                      C */
/*     Now                                                              C */
/*                                                                      C */
/*     B(x) = BMEAS0 / (1+n*x/R) * (1 + C1*x + C2*x**2 + ... + C7*x**7) C */
/*                                                                      C */
/*     where BMEAS0 is the measured value of the B-field at reference   C */
/*     point x=0.                                                       C */
/*                                                                      C */
/*  I      BF          C1          C2            C3              C4            C5           C6            C7 
*/
/* amps   kGauss */
/* 100.   2.00225  1.962394E-03 -2.293013E-04  1.234833E-05 -3.537459E-07  5.551259E-09 -4.500169E-11  1.47257
9E-13 */
/* 180.   3.57472  1.922171E-03 -2.163218E-04  1.130863E-05 -3.149187E-07  4.811879E-09 -3.808138E-11  1.22035
5E-13 */
/* 350.   6.92151  1.962442E-03 -2.255950E-04  1.223721E-05 -3.549376E-07  5.644852E-09 -4.635942E-11  1.53534
6E-13 */
/* 500.   9.86762  2.084780E-03 -2.388581E-04  1.280996E-05 -3.653301E-07  5.698301E-09 -4.587436E-11  1.49028
7E-13 */
/* 625.  12.24360  2.007846E-03 -2.227859E-04  1.177408E-05 -3.317632E-07  5.115515E-09 -4.072955E-11  1.30928
6E-13 */
/* 725.  14.06667  2.323137E-03 -2.495408E-04  1.319004E-05 -3.747899E-07  5.839129E-09 -4.697908E-11  1.52518
4E-13 */
/* 829.  15.80831  2.777501E-03 -2.666869E-04  1.364456E-05 -3.827231E-07  5.921277E-09 -4.742479E-11  1.53450
0E-13 */
/* 875.  16.48228  3.066965E-03 -2.809122E-04  1.428083E-05 -4.005838E-07  6.196709E-09 -4.957102E-11  1.60042
6E-13 */
/* 950.  17.47663  3.613984E-03 -3.059410E-04  1.525917E-05 -4.234547E-07  6.482142E-09 -5.130691E-11  1.63918
1E-13 */
/* 1000.  18.02981  3.992692E-03 -3.185566E-04  1.554105E-05 -4.266711E-07  6.491555E-09 -5.120422E-11  1.6328
41E-13 */
/*                                                                      C */
/*     Since the measured field has been parameterized with the form    C */
/*                                                                      C */
/*     B_meas (x) = BMEAS0 / (1+n*x/R) * (1 + C1*x + C2*x**2 + ... + C7*x**7) */
/*                                                                      C */
/*     and the ideal field has the form                                 C */
/*                                                                      C */
/*     B_ideal (x) = BMEAS0 / (1+n*x/R)                                 C */
/*                                                                      C */
/*     The ratio of B_meas/B_ideal is simply                            C */
/*                                                                      C */
/*     RATIO = (1 + C1*x + C2*x**2 + ... + C7*x**7)                     C */
/*                                                                      C */
/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */




/* geant */
/* * GCUNIT */


/* local */

/* *** MITRAY_DIAG COMMON BLOCK */













/*     For an MTYP=4 dipole (Clamshell dipole), the XA coordinate is the */
/*     same as the RL-probe coordinate. */

    xrl = *xa;

/*     Find the value of INDEX such that BXEQ0(INDEX) <= BF0 <= BXEQ0(INDEX+1) */
/*     INDEX1 and INDEX2 are the two values to use for interpolation */

    absbf0 = abs(*bmeas0);
    if (absbf0 <= bxeq0[0]) {
	index1 = 1;
	index2 = 1;
	goto L100;
    } else if (absbf0 >= bxeq0[9]) {
	index1 = 10;
	index2 = 10;
	goto L100;
    }

/*     MAKE AN ESTIMATE OF WHERE IN THE TABLE ONE SHOULD LOOK */
/*     GIVEN A VALUE OF B-FIELD STRENGTH ABSBF0, THIS POLYNOMIAL ESTIMATES */
/*     THE INDEX CORRECTLY TO WITHIN +/- 0.59 .  WE KEEP IT A FLOATING */
/*     POINT NUMBER FOR NOW */

    xiestimate = (absbf0 * (float).0239218 + (float).0175226) * absbf0 + (
	    float)1.3203 + (float).5;

/*     CALCULATE THE RANGE OF INDEX VALUES OVER WHICH TO SEARCH. */
/*     ROUND DOWN ON ISTART, SO DON'T NEED TO DO ANYTHING */

    istart = (integer) (xiestimate - (float).6);

/*     ROUND UP ON ISTOP, SO ADD ANOTHER 0.5 TO 0.6 TO GIVE +1.1 */

    istop = (integer) (xiestimate + (float)1.1);
    if (istart < 1) {
	istart = 1;
    }
    if (istop > 9) {
	istop = 9;
    }
    i__1 = istop;
    for (i__ = istart; i__ <= i__1; ++i__) {
	if (absbf0 >= bxeq0[i__ - 1] && absbf0 < bxeq0[i__]) {
	    index1 = i__;
	    index2 = i__ + 1;
	    goto L100;
	}
    }

/*     Should never come here */

    io___506.ciunit = gcunit_1.lout;
    s_wsle(&io___506);
    do_lio(&c__9, &c__1, "**error** in subroutine MITRAY_SASPRATIO", (ftnlen)
	    40);
    e_wsle();
    io___507.ciunit = gcunit_1.lout;
    s_wsle(&io___507);
    do_lio(&c__9, &c__1, "          failure of interpolation routine", (
	    ftnlen)42);
    e_wsle();

    *ratio = (float)0.;
    s_stop("", (ftnlen)0);

L100:
    i__ = index1;
    j = index2;

/*     FRAC=fraction of distance between BXEQ0(I) and BXEQ0(J) where the */
/*          actual B(x=0) value lies */

    if (i__ == j) {
	frac = (float)0.;
    } else {
	frac = (absbf0 - bxeq0[i__ - 1]) / (bxeq0[j - 1] - bxeq0[i__ - 1]);
    }

/*     THE POLYNOMIAL PARAMETERIZATION IS GOOD ONLY FOR XRL>0. */
/*     AND CAN WILDLY DIVERGE FOR XRL<0. */

    if (xrl >= (float)0.) {
/* Computing 2nd power */
	d__1 = xrl;
/* Computing 3rd power */
	d__2 = xrl;
/* Computing 4th power */
	d__3 = xrl, d__3 *= d__3;
/* Computing 5th power */
	d__4 = xrl, d__5 = d__4, d__4 *= d__4;
/* Computing 6th power */
	d__6 = xrl, d__6 *= d__6;
/* Computing 7th power */
	d__7 = xrl, d__8 = d__7, d__7 *= d__7, d__8 *= d__7;
	ratio1 = c1[i__ - 1] * xrl + (float)1. + c2[i__ - 1] * (d__1 * d__1) 
		+ c3[i__ - 1] * (d__2 * (d__2 * d__2)) + c4[i__ - 1] * (d__3 *
		 d__3) + c5[i__ - 1] * (d__5 * (d__4 * d__4)) + c6[i__ - 1] * 
		(d__6 * (d__6 * d__6)) + c7[i__ - 1] * (d__8 * (d__7 * d__7));
/* Computing 2nd power */
	d__1 = xrl;
/* Computing 3rd power */
	d__2 = xrl;
/* Computing 4th power */
	d__3 = xrl, d__3 *= d__3;
/* Computing 5th power */
	d__4 = xrl, d__5 = d__4, d__4 *= d__4;
/* Computing 6th power */
	d__6 = xrl, d__6 *= d__6;
/* Computing 7th power */
	d__7 = xrl, d__8 = d__7, d__7 *= d__7, d__8 *= d__7;
	ratio2 = c1[j - 1] * xrl + (float)1. + c2[j - 1] * (d__1 * d__1) + c3[
		j - 1] * (d__2 * (d__2 * d__2)) + c4[j - 1] * (d__3 * d__3) + 
		c5[j - 1] * (d__5 * (d__4 * d__4)) + c6[j - 1] * (d__6 * (
		d__6 * d__6)) + c7[j - 1] * (d__8 * (d__7 * d__7));

	diff = ratio2 - ratio1;
	*ratio = ratio1 + frac * diff;
	return 0;

    } else {

/*       COME HERE IF XRL<0. */
/*       For XRL<0, there is no RL-probe data available to define the */
/*       shape of the SASP dipole magnetic field. */
/*       At the entrance and exit ends of SASP, we need values of XRL down */
/*       to as low as XRL=-25 cm. */
/*       There is some indication that the data points very near XRL=0 */
/*       suffer from some "falling off" due to the proximity of the */
/*       dipole edge.  We will therefore determine RATIO at XRL=3. cm */
/*       and XRL=10. cm and use this slope to do a linear extrapolation */
/*       for XRL<0. */

	xrlb = (float)3.;
	xrlc = (float)10.;

/*       Now determine RATIO for XRL=XRLB (point B), for the two */
/*       dipole current settings I and J */

/* Computing 2nd power */
	d__1 = xrlb;
/* Computing 3rd power */
	d__2 = xrlb;
/* Computing 4th power */
	d__3 = xrlb, d__3 *= d__3;
/* Computing 5th power */
	d__4 = xrlb, d__5 = d__4, d__4 *= d__4;
/* Computing 6th power */
	d__6 = xrlb, d__6 *= d__6;
/* Computing 7th power */
	d__7 = xrlb, d__8 = d__7, d__7 *= d__7, d__8 *= d__7;
	ratioib = c1[i__ - 1] * xrlb + (float)1. + c2[i__ - 1] * (d__1 * d__1)
		 + c3[i__ - 1] * (d__2 * (d__2 * d__2)) + c4[i__ - 1] * (d__3 
		* d__3) + c5[i__ - 1] * (d__5 * (d__4 * d__4)) + c6[i__ - 1] *
		 (d__6 * (d__6 * d__6)) + c7[i__ - 1] * (d__8 * (d__7 * d__7))
		;
/* Computing 2nd power */
	d__1 = xrlb;
/* Computing 3rd power */
	d__2 = xrlb;
/* Computing 4th power */
	d__3 = xrlb, d__3 *= d__3;
/* Computing 5th power */
	d__4 = xrlb, d__5 = d__4, d__4 *= d__4;
/* Computing 6th power */
	d__6 = xrlb, d__6 *= d__6;
/* Computing 7th power */
	d__7 = xrlb, d__8 = d__7, d__7 *= d__7, d__8 *= d__7;
	ratiojb = c1[j - 1] * xrlb + (float)1. + c2[j - 1] * (d__1 * d__1) + 
		c3[j - 1] * (d__2 * (d__2 * d__2)) + c4[j - 1] * (d__3 * d__3)
		 + c5[j - 1] * (d__5 * (d__4 * d__4)) + c6[j - 1] * (d__6 * (
		d__6 * d__6)) + c7[j - 1] * (d__8 * (d__7 * d__7));

	ratiob = ratioib + frac * (ratiojb - ratioib);


/*       Now determine RATIO for XRL=XRLC (point C), for the two */
/*       dipole current settings I and J */

/* Computing 2nd power */
	d__1 = xrlc;
/* Computing 3rd power */
	d__2 = xrlc;
/* Computing 4th power */
	d__3 = xrlc, d__3 *= d__3;
/* Computing 5th power */
	d__4 = xrlc, d__5 = d__4, d__4 *= d__4;
/* Computing 6th power */
	d__6 = xrlc, d__6 *= d__6;
/* Computing 7th power */
	d__7 = xrlc, d__8 = d__7, d__7 *= d__7, d__8 *= d__7;
	ratioic = c1[i__ - 1] * xrlc + (float)1. + c2[i__ - 1] * (d__1 * d__1)
		 + c3[i__ - 1] * (d__2 * (d__2 * d__2)) + c4[i__ - 1] * (d__3 
		* d__3) + c5[i__ - 1] * (d__5 * (d__4 * d__4)) + c6[i__ - 1] *
		 (d__6 * (d__6 * d__6)) + c7[i__ - 1] * (d__8 * (d__7 * d__7))
		;
/* Computing 2nd power */
	d__1 = xrlc;
/* Computing 3rd power */
	d__2 = xrlc;
/* Computing 4th power */
	d__3 = xrlc, d__3 *= d__3;
/* Computing 5th power */
	d__4 = xrlc, d__5 = d__4, d__4 *= d__4;
/* Computing 6th power */
	d__6 = xrlc, d__6 *= d__6;
/* Computing 7th power */
	d__7 = xrlc, d__8 = d__7, d__7 *= d__7, d__8 *= d__7;
	ratiojc = c1[j - 1] * xrlc + (float)1. + c2[j - 1] * (d__1 * d__1) + 
		c3[j - 1] * (d__2 * (d__2 * d__2)) + c4[j - 1] * (d__3 * d__3)
		 + c5[j - 1] * (d__5 * (d__4 * d__4)) + c6[j - 1] * (d__6 * (
		d__6 * d__6)) + c7[j - 1] * (d__8 * (d__7 * d__7));

	ratioc = ratioic + frac * (ratiojc - ratioic);

/*       NOW WE HAVE THE TWO POINTS (XRL, RATIO) =  (XRLB, RATIOB), */
/*       AND (XRLC,RATIOC) . */
	slope = (ratiob - ratioc) / (xrlb - xrlc);

/*       EXTRAPOLATE USING LINEAR EXTRAPOLATION, SUCH THAT RATIO=1.0 FOR XRL=0 */

	*ratio = xrl * slope + (float)1.;
	return 0;
    }
    return 0;
} /* mitray_saspratio__ */


/* *********************************************************************** */
/*                         SOLENOID SUBROUTINES */
/* *********************************************************************** */

/* Subroutine */ int mitray_solnd__(doublereal *data, doublereal *xpos, 
	doublereal *bfld)
{
    static doublereal a, b, d__, l, xa, ya, za, z11, z22;
    extern /* Subroutine */ int mitray_bsol__();


/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
/*                                                                      C */
/*     Subroutine for solenoid, in GEANT implementation of MIT-RAYTRACE C */
/*     adapted from:                                                    C */
/*     Subroutine SOLND (NO, NP, T, TP ,NUM ) by S. Kowalski            C */
/*     by Stanley Yen (TRIUMF)                                          C */
/*                                                                      C */
/*      TC(1) to  TC(6) =  (  X,  Y,  Z, VX, VY, VZ )                   C */
/*     DTC(1) to DTC(6) =  ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )          C */
/*                                                                      C */
/*     BF (positive) : Solenoid field in beam direction                 C */
/*                                                                      C */
/*     MODIFICATION HISTORY                                             C */
/*     --------------------                                             C */
/*                                                                      C */
/*     Mar 16, 1998    S.Yen     Original adaptation from MIT-RAYTRACE  C */
/*                                                                      C */
/*     S. Yen (TRIUMF)  e-mail  STAN@TRIUMF.CA                          C */
/*                                                                      C */
/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */







/*     Extract the A-axes coordinates from XPOS array */

    /* Parameter adjustments */
    --bfld;
    --xpos;
    --data;

    /* Function Body */
    xa = xpos[1];
    ya = xpos[2];
    za = xpos[3];

/*     EXTRACT THE SOLENOID PARAMETERS FROM THE ARRAY DATA(i) */

    a = data[10];
    b = data[11];
    l = data[12];
    d__ = data[13];
    mitray30_1.bf = data[14];
    z11 = data[15];
    z22 = data[16];

    mitray30_1.al = l / (float)2.;
    mitray30_1.rad = d__ / (float)2.;
    mitray10_2.bx = (float)0.;
    mitray10_2.by = (float)0.;
    mitray10_2.bz = (float)0.;
    mitray31_1.bt = (float)0.;
    mitray31_1.s = (float)0.;

/*     Load position coordinates into TC(1),TC(2),TC(3) since this is */
/*     what subroutine BSOL expects.  Note that these are relative to */
/*     the geometric center of the solenoid. */

    mitray10_2.tc[0] = xa;
    mitray10_2.tc[1] = ya;
    mitray10_2.tc[2] = za - a - mitray30_1.al;

/*     If outside the integration regions defined by Z11 before the */
/*     entrance edge of the solenoid, to Z22 beyond the exit edge of */
/*     the solenoid, then return without calculating anything */

    if (mitray10_2.tc[2] < -(mitray30_1.al + z11) || mitray10_2.tc[2] > 
	    mitray30_1.al + z22) {
	return 0;
    }

/*     CALL BSOL TO CALCULATE B-FIELD COMPONENTS BX,BY,BZ, */
/*     RETURNED IN COMMON BLOCK */

    mitray_bsol__();

    bfld[1] = mitray10_2.bx;
    bfld[2] = mitray10_2.by;
    bfld[3] = mitray10_2.bz;

    return 0;
} /* mitray_solnd__ */


/* ======================================================================= */

/* Subroutine */ int mitray_bsol__()
{
    /* Initialized data */

    static doublereal pi4 = 12.566370616;

    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal p, r__, x, y, z__, r1, br, zz, ves, vks, brs1, brs2, 
	    bzs1, bzs2, r1sq, aamr, aapr, cosa, cosb, radr, rcsq, rksq;
    extern /* Subroutine */ int mitray_fb01ad__(doublereal *, doublereal *, 
	    doublereal *), mitray_fb03ad__(doublereal *, doublereal *, 
	    doublereal *);


/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
/*                                                                      C */
/*     Routine valid for fields outside central zone of elemental       C */
/*                              Solenoid                                C */
/*                                                                      C */
/*     BF    = field at center of infinite solenoid; curr. den. (NI/M)  C */
/*                                                                      C */
/*     M.W.GARRETTT  JOURNAL OF APP. PHYS. 34,(1963),P2567              C */
/*                                                                      C */
/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */






    x = mitray10_2.tc[0];
    y = mitray10_2.tc[1];
    z__ = mitray10_2.tc[2];

    r__ = sqrt(x * x + y * y);

    if (r__ < mitray30_1.rad / 1e4) {
	goto L5;
    }

    radr = mitray30_1.rad + r__;
    aapr = mitray30_1.rad * 4. / radr;
    aamr = (mitray30_1.rad - r__) / (mitray30_1.rad * 2.);
    rcsq = mitray30_1.rad * 4. * r__ / (radr * radr);

/* *** Solenoid left hand source */

    zz = -(mitray30_1.al + z__);
    r1sq = radr * radr + zz * zz;
    r1 = sqrt(r1sq);
    rksq = mitray30_1.rad * 4. * r__ / r1sq;

    mitray_fb01ad__(&rksq, &vks, &ves);
    mitray_fb03ad__(&rcsq, &rksq, &p);

    bzs1 = aapr * zz * (vks + aamr * (p - vks)) / r1;
    brs1 = r1 * ((vks - ves) * 2. - rksq * vks);

/* *** Solenoid right hand source */

    zz = mitray30_1.al - z__;
    r1sq = radr * radr + zz * zz;
    r1 = sqrt(r1sq);
    rksq = mitray30_1.rad * 4. * r__ / r1sq;

    mitray_fb01ad__(&rksq, &vks, &ves);
    mitray_fb03ad__(&rcsq, &rksq, &p);

    bzs2 = aapr * zz * (vks + aamr * (p - vks)) / r1;
    brs2 = r1 * ((vks - ves) * 2. - rksq * vks);

    mitray10_2.bz = mitray30_1.bf * (bzs2 - bzs1) / pi4;
    br = mitray30_1.bf * (brs2 - brs1) / (r__ * pi4);
    mitray10_2.bx = br * x / r__;
    mitray10_2.by = br * y / r__;
/* Computing 2nd power */
    d__1 = mitray10_2.bx;
/* Computing 2nd power */
    d__2 = mitray10_2.by;
/* Computing 2nd power */
    d__3 = mitray10_2.bz;
    mitray31_1.bt = sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3);

    return 0;

L5:

/* Computing 2nd power */
    d__1 = mitray30_1.al - z__;
    cosa = (mitray30_1.al - z__) / sqrt(mitray30_1.rad * mitray30_1.rad + 
	    d__1 * d__1);
/* Computing 2nd power */
    d__1 = mitray30_1.al + z__;
    cosb = -(mitray30_1.al + z__) / sqrt(mitray30_1.rad * mitray30_1.rad + 
	    d__1 * d__1);

    mitray10_2.bx = (float)0.;
    mitray10_2.by = (float)0.;
    mitray10_2.bz = mitray30_1.bf * (cosa - cosb) / 2.;
    mitray31_1.bt = abs(mitray10_2.bz);

    return 0;
} /* mitray_bsol__ */


/* ======================================================================= */

/* Subroutine */ int mitray_fb01ad__(doublereal *c__, doublereal *vk, 
	doublereal *ve)
{
    /* Initialized data */

    static doublereal xlg = 9223372036854775800.;

    /* Builtin functions */
    double log(doublereal);

    /* Local variables */
    static doublereal d__, e;



/*      REAL * 8 XLG/'7FFFFFFFFFFFFFFF'X/    ! modified by O Kirsebom Jan 17 2013 */

    d__ = 1. - *c__;
    if (d__ > 0.) {
	e = -log(d__);
    }

/* *** Harwell version of FB01AD */

    if (*c__ >= 1.) {
	goto L2;
    }
    *ve = e * ((((((((((d__ * 3.18591956555015718e-5 + 9.89833284622538479e-4)
	     * d__ + .00643214658643830177) * d__ + .016804023346363385) * 
	    d__ + .0261450147003138789) * d__ + .0334789436657616262) * d__ + 
	    .0427178905473830956) * d__ + .0585936612555314917) * d__ + 
	    .0937499997212031407) * d__ + .249999999999901772) * d__) + ((((((
	    (((d__ * 1.49466217571813268e-4 + .00246850333046072273) * d__ + 
	    .00863844217360407443) * d__ + .0107706350398664555) * d__ + 
	    .00782040406095955417) * d__ + .00759509342255943228) * d__ + 
	    .0115695957452954022) * d__ + .0218318116761304816) * d__ + 
	    .0568051945675591566) * d__ + .443147180560889526) * d__ + 1.;

/* *** Routine modified to calculate VD and VE always */

    *vk = e * ((((((((((d__ * 2.97002809665556121e-5 + 9.21554634963249846e-4)
	     * d__ + .00597390429915542916) * d__ + .0155309416319772039) * 
	    d__ + .0239319133231107901) * d__ + .0301248490128989303) * d__ + 
	    .0373777397586236041) * d__ + .048828041906862398) * d__ + 
	    .0703124997390383521) * d__ + .124999999999908081) * d__ + .5) + (
	    ((((((((d__ * 1.39308785700664673e-4 + .00229663489839695869) * 
	    d__ + .00800300398064998537) * d__ + .00984892932217689377) * d__ 
	    + .00684790928262450512) * d__ + .00617962744605331761) * d__ + 
	    .00878980187455506468) * d__ + .0149380135326871652) * d__ + 
	    .0308851462713051899) * d__ + .0965735902808562554) * d__ + 
	    1.38629436111989062;

    return 0;

L2:
    *ve = 1.;
    *vk = xlg;

    return 0;
} /* mitray_fb01ad__ */


/* ======================================================================= */

/* Subroutine */ int mitray_fb02ad__(doublereal *caysq, doublereal *sinp, 
	doublereal *cosp, doublereal *e, doublereal *f)
{
    /* Builtin functions */
    double atan(doublereal), sqrt(doublereal), log(doublereal);

    /* Local variables */
    static doublereal a, h__;
    static integer n;
    static doublereal h1, t1, t2, cfi, cfj, cfl, cfm, cfn, phi, cfi1, cfj1, 
	    del1, del2, del3, del4, sig1, sig2, sig3, sig4, sin2, fact, crit, 
	    term, fact1, flog1, factm, factn, recip, caymod, factor, factro, 
	    caydsq;



    phi = atan(*sinp / *cosp);

    if (*caysq * *sinp * *sinp - .5 <= 0.) {
	goto L1;
    } else {
	goto L5;
    }

L1:
    h__ = 1.;
    a = phi;
    n = 0;
    sig1 = 0.;
    sig2 = 0.;
    sin2 = *sinp * *sinp;
    term = *sinp * *cosp * .5;
    crit = phi;

L2:
    ++n;

    recip = 1. / n;
    fact = (n - .5) * recip;
    h1 = h__;
    h__ = fact * *caysq * h__;
    a = fact * a - term * recip;
    term *= sin2;
    crit *= sin2;
    del1 = h__ * a;
    del2 = recip * -.5 * *caysq * h1 * a;
    sig1 += del1;
    sig2 += del2;

    if (abs(del1) - 4e-16 >= 0.) {
	goto L3;
    } else {
	goto L4;
    }

L3:
    if (abs(crit) - abs(a) >= 0.) {
	goto L2;
    } else {
	goto L4;
    }

L4:
    *f = phi + sig1;
    *e = phi + sig2;
    goto L8;

L5:
    cfi = 1.;
    cfj = 1.;
    cfl = 0.;
    cfm = 0.;
    cfn = 0.;
    sig1 = 0.;
    sig2 = 0.;
    sig3 = 0.;
    sig4 = 0.;

    n = 0;

    fact1 = 1. - *caysq * *sinp * *sinp;
    factor = *cosp * .5 * sqrt(*caysq / fact1);
    factro = factor + factor;
    caydsq = 1. - *caysq;

L6:
    ++n;

    recip = 1. / n;
    factn = recip * (n - .5);
    factm = (n + .5) / (n + 1.);
    factor *= fact1;
    cfi1 = cfi;
    cfj1 = cfj;
    cfi *= factn;
    cfj = cfj * factn * factn * caydsq;
    cfl += .5 / (n * (n - .5));
    cfm = (cfm - factor * recip * cfi) * factm * factm * caydsq;
    cfn = (cfn - factor * recip * cfi1) * factn * factm * caydsq;
    del1 = cfm - cfj * cfl;
    del2 = cfn - (factn * cfl - recip * .25 * recip) * caydsq * cfj1;
    del3 = cfj;
    del4 = factm * cfj;
    sig1 += del1;
    sig2 += del2;
    sig3 += del3;
    sig4 += del4;

    if (abs(del1) - 4e-16 >= 0.) {
	goto L6;
    } else {
	goto L7;
    }

L7:
    caymod = sqrt(*caysq);
    flog1 = log(4. / (sqrt(fact1) + caymod * *cosp));
    t1 = (sig3 + 1.) * flog1 + factro * log(caymod * .5 * abs(*sinp) + .5);
    t2 = (sig4 + .5) * caydsq * flog1 + 1. - factro * (1. - caymod * abs(*
	    sinp));
    *f = t1 + sig1;
    *e = t2 + sig2;

L8:
    return 0;
} /* mitray_fb02ad__ */


/* ======================================================================= */

/* Subroutine */ int mitray_fb03ad__(doublereal *gn, doublereal *caca, 
	doublereal *p)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal e, f, br, pi, cth, sth, cada, cape, capk;
    extern /* Subroutine */ int mitray_fb01ad__(doublereal *, doublereal *, 
	    doublereal *), mitray_fb02ad__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);



    if (*gn >= 0.) {
	goto L2;
    } else {
	goto L1;
    }

L1:
    if (*caca <= 0.) {
	goto L3;
    } else {
	goto L4;
    }

L3:
    *p = (float)1.5707963268 / sqrt(1. - *gn);
    return 0;

L4:
    sth = sqrt(-(*gn) / (*caca - *gn));
    cth = sqrt(1. - sth * sth);
    cada = 1. - *caca;

    mitray_fb01ad__(caca, &capk, &cape);
    mitray_fb02ad__(&cada, &sth, &cth, &e, &f);

    br = cape * f - capk * (f - e);
    *p = capk * cth * cth + sth * br / sqrt(1. - *gn);
    return 0;

L2:
    if ((d__1 = *gn - *caca) < 0.) {
	goto L10;
    } else if (d__1 == 0) {
	goto L30;
    } else {
	goto L20;
    }

L10:
    sth = sqrt(*gn / *caca);
    cth = sqrt(1. - sth * sth);

    mitray_fb01ad__(caca, &capk, &cape);
    mitray_fb02ad__(caca, &sth, &cth, &e, &f);

    br = capk * e - cape * f;
    *p = capk + br * sth / (cth * sqrt(1. - *gn));
    return 0;

L30:
    mitray_fb01ad__(caca, &capk, &cape);
    *p = cape / (1. - *caca);
    return 0;

L20:
    cada = 1. - *caca;
    pi = (float)3.1415926536;
    sth = sqrt((1. - *gn) / cada);
    cth = sqrt(1. - sth * sth);

    mitray_fb01ad__(caca, &capk, &cape);
    mitray_fb02ad__(&cada, &sth, &cth, &e, &f);

    br = pi / (float)2. + capk * (f - e) - cape * f;
    *p = capk + br * sqrt(*gn) / (cada * sth * cth);

    return 0;
} /* mitray_fb03ad__ */


/* ******************************************************************************* */
/*                        ZONE SELECTION SUBROUTINE */
/* ******************************************************************************* */

/* Subroutine */ int mitray_zone__(doublereal *zb, doublereal *zc, doublereal 
	*z11, doublereal *z12, doublereal *z21, doublereal *z22, integer *
	izone)
{
    /* Format strings */
    static char fmt_200[] = "(\002 OVERLAPPING ENTRANCE & EXIT FRINGE FIELD\
S\002,\002 (SHORT MAGNET)\002/\002 TOTAL FIELD = ENTRANCE + EXIT - UNIFORM F\
IELD\002//)";
    static char fmt_210[] = "(\002 **ERROR** IN GU_MITRAY_ZONE\002/\002     \
      ERROR DETERMINING WHICH ZONE WE ARE IN\002/\002           ZB=\002,f12.\
4,\002    ZC=\002,f12.4/\002           Z11=\002,f12.4,\002   Z12=\002,f12.4\
,\002  Z21=\002,f12.4,\002Z22=\002,f12.4)";

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(), s_wsfe(cilist *), e_wsfe(), do_fio(integer *, char *, 
	    ftnlen);

    /* Local variables */
    static logical lentr, lexit;

    /* Fortran I/O blocks */
    static cilist io___601 = { 0, 0, 0, 0, 0 };
    static cilist io___602 = { 0, 0, 0, 0, 0 };
    static cilist io___605 = { 0, 0, 0, 0, 0 };
    static cilist io___606 = { 0, 0, 0, 0, 0 };
    static cilist io___607 = { 0, 0, 0, fmt_200, 0 };
    static cilist io___608 = { 0, 0, 0, fmt_210, 0 };



/*     This subroutine determines which zone of the magnet we are in */
/*     (entrance fringe field, uniform region, exit fringe field, or */
/*     overlapping entrance/exit fringe fields). */

/*     INPUT (REAL*8) */
/*     -------------- */
/*     ZB, ZC        z-coordinate in the B and C axis systems */
/*     Z11, Z12      boundaries of entrance fringe field in B axis system */
/*     Z21, Z22      boundaries of exit fringe field in C axis system */

/*     OUTPUT (INTEGER*4) */
/*     ------------------ */
/*     IZONE         = 0 for entrance/exit far field */
/*                   = 1 for pure entrance fringe field (no overlap with */
/*                       exit fringe field) */
/*                   = 2 for interior ("uniform field") region */
/*                   = 3 for pure exit fringe field (no overlap with */
/*                       entrance fringe field */
/*                   = 4 for case where the entrance and exit fringe fields */
/*                       overlap */
/*                   = -1  error */




/* geant */



/* geant */
/* * GCUNIT */

/* local */

/* *** MITRAY_DIAG COMMON BLOCK */




/*     DEFAULT VALUE OF -1 FOR IZONE */
/* local */

/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
/*                                                                      C */
/*           File diagnostic.inc - keeps diagnostic flags               C */
/*                                                                      C */
/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */



    *izone = -1;

/* ---------------------------------------------------------------------------- */

/*     Now figure out which zone we are in, from the B and C coordinates */

/*     Check for entrance/exit far zone */
    if (*zb > *z11 || *zc > *z22) {
	*izone = 0;
	if (mitray_diag__1.ldiag) {
	    io___601.ciunit = gcunit_1.lout;
	    s_wsle(&io___601);
	    do_lio(&c__9, &c__1, "FAR ENTRANCE/EXIT REGION", (ftnlen)24);
	    e_wsle();
	}
	return 0;
    }

/*     Check for uniform field zone */
    if (*zb <= *z12 && *zc <= *z21) {
	*izone = 2;
	if (mitray_diag__1.ldiag) {
	    io___602.ciunit = gcunit_1.lout;
	    s_wsle(&io___602);
	    do_lio(&c__9, &c__1, "UNIFORM FIELD REGION", (ftnlen)20);
	    e_wsle();
	}
	return 0;
    }

/*     Check for entrance or exit fringe field */
    if (*zb <= *z11 && *zb > *z12) {
	lentr = TRUE_;
	*izone = 1;
    } else {
	lentr = FALSE_;
    }
    if (*zc <= *z22 && *zc > *z21) {
	lexit = TRUE_;
	*izone = 3;
    } else {
	lexit = FALSE_;
    }

/*     For the special case of a very short magnet, where the entrance */
/*     and exit fringe fields overlap, and there is NO uniform field */
/*     region, both LENTR and LEXIT will be true from the above tests. */
/*     We signal this by setting IZONE=4 */

    if (lentr && lexit) {
	*izone = 4;
    }

/*     LDIAG=.TRUE. means we want diagnostic printout at each step */

    if (*izone == 1) {
	if (mitray_diag__1.ldiag) {
	    io___605.ciunit = gcunit_1.lout;
	    s_wsle(&io___605);
	    do_lio(&c__9, &c__1, "ENTRANCE FRINGE FIELD REGION", (ftnlen)28);
	    e_wsle();
	}
    } else if (*izone == 3) {
	if (mitray_diag__1.ldiag) {
	    io___606.ciunit = gcunit_1.lout;
	    s_wsle(&io___606);
	    do_lio(&c__9, &c__1, "EXIT FRINGE FIELD REGION", (ftnlen)24);
	    e_wsle();
	}
    } else if (*izone == 4) {
	if (mitray_diag__1.ldiag) {
	    io___607.ciunit = gcunit_1.lout;
	    s_wsfe(&io___607);
	    e_wsfe();
	}
    } else if (*izone == -1) {
	io___608.ciunit = gcunit_1.lout;
	s_wsfe(&io___608);
	do_fio(&c__1, (char *)&(*zb), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*zc), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*z11), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*z12), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*z21), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*z22), (ftnlen)sizeof(doublereal));
	e_wsfe();
	diag_1.jstop = 1;
	gcflag_1.ieotri = 1;
    }
    return 0;
} /* mitray_zone__ */

#ifdef __cplusplus
	}
#endif

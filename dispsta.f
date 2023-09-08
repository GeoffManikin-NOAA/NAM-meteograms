      PROGRAM PLTRFC
C
C   2/4/99  m. baldwin
C        program to display meteogram stations and create
C        a WWW map file
C
      CHARACTER TITLE*45,fname*80,radid*6,date*6,chour*2
      CHARACTER sti*5,str1*1,str3*3,outdir*80
      CHARACTER STR*36,STR15*15,STR11*12,STR5*6,str2*2
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM,ISSL
       RADDEG=3.14159265/180.
       DEG=180./3.14159265
c
c  initialize ncar graphics
c
       id=8
       idc=9
        CALL GOPKS(6,0)
        CALL GOPWK(ID,IDC,20) ! this makes a gmeta8.ps file
c       CALL GOPWK(ID,IDC,1)  ! this makes a gmeta file
        CALL GACWK(ID)
        call agseti('FRAME.',2)
c
c   set up map
c
	CALL MAPSTI ('MV',8)
        CALL MAPSTC ('OU','PS')
        CALL MAPROJ('ST',90.,-95.,0.) ! polar stereo 100W ref long
       alatl=36.1          ! lower left corner lat
       alonl=256.8         ! lower left corner lon
       alatr=49.9          ! upper right corner lat
       alonr=283.4         ! upper right corner lon
        CALL MAPSET('CO',alatl,alonl,alatr,alonr)
c
c  plot map
c
        CALL MAPSTI ('GR',0)   !  lat/lon line interval in deg (0 means none)
        ICOUN=0
        call newpen(8)   !  black
	CALL MAPINT
        CALL MAPPOS(.02,.98,.08,.92)
        CALL MAPDRW
        call getset(vpl,vpr,vpb,vpt,wdl,wdr,wdb,wdt,ls)
C            getset determines the "mapping" from the "working" coordinates
C            to the "viewport" coordinates.  this is used later to
C            find the pixel position of the station sites for the WWW
C            map file.

c
c  plot station sites
c
        call newpen(6)   !  red
 13   READ(5,33,END=96) RADID,rlat,rlon
 33   FORMAT(A6,F6.2,1x,f7.2)   ! format of the staids.parm file
	 CALL MAPTRN(rLAT,360.-rLON,UVAL,VVAL)   
C             MAPTRN transforms from lat/lon coord to "working" coord
	 IF (UVAL.GT.UMAX.OR.UVAL.LT.UMIN) GOTO 244
	 IF (VVAL.GT.VMAX.OR.VVAL.LT.VMIN) GOTO 244
        i1=vpl*541. + (uval-umin)*(vpr-vpl)/(umax-umin)*541. + 0.5
        j1=vpb*541. + (vval-vmin)*(vpt-vpb)/(vmax-vmin)*541. + 0.5
         j1=542-j1

C    i1,j1 are pixel coord on the final gif file.  1,1 is the upper left
C     and 541,541 is the lower right, so the y-axis gets flipped

         write(6,302) radid,i1-5,j1-5,i1+5,j1+5
 302     format('rect  /mmb/srefmeteograms/images/',a6,'.meteogram.gif '
     &   ,i3.3,',',i3.3,1x,i3.3,',',i3.3)

C  THIS PLOTS A STAR                    SIZE 
         CALL plchhq(uval,vval,':KGU:,',0.0185,0.,0.)
C  THIS PLOTS A STAR                    SIZE 

 244    CONTINUE
        goto 13
 96     call newpen(8)    ! black
 7887   CALL SETUSV('LW',1000)
	CALL SET(.01,.99,.01,.99,0.,1.,0.,1.,1)
      CALL WTSTR(0.50,0.98,'NORTH MIDWEST METEOGRAM LOCATIONS',3,0,0)   ! title at top
      CALL FRAME
      CALL GDAWK(ID)
      CALL GCLWK(ID)
      CALL GCLKS
      STOP
      END


      SUBROUTINE newpen(inp)
      COMMON/SPLT/ LNOP,NPLTDV
      common /compen/ ipx,xold,yold
      character*8  ctype
      common /comgks/ ctype
      DIMENSION red(8),green(8),blue(8)
      data red  /.50,.40,.30,.66,. 0,. 0,.9,0.0/
      data green/.90,.00,.50,.00,.80,.05,.3,0.0/
      data blue /. 0,.40,.70,.00,.40,.50,.3,0.0/
      DATA  ICALL/0/,ctype/'GKSCOLOR'/
      IF( ICALL .EQ. 0 ) THEN
          ICALL=1
        call gsfais(1)
        jshc = 1
      do 100 ic = 1, 8
        iishc = ic
        rr = red(ic)
        gg = green(ic)
        bb = blue(ic)
        call gscr(8,ic,rr,gg,bb)
 100    continue
        call gscr(8,0,1.,1.,1.)
      ENDIF
      call gstxci(inp)
      call gsplci(inp)
      RETURN
      END


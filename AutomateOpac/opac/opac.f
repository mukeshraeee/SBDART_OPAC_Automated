      program opac
CCCCC ------------------------------------------------------------------C
C     extracts and calculates optical properties of aerosols and clouds C
C     from the data stored in directory ../optdat/.                     C
C                                                                       C
C     Version 3.0                                                       C
C     ------------                                                      C
C     01.10.97 new version based on Version 2.0 of 17.04.97             C
C                                                                       C
C     Version 3.16                                                      C
C     ------------                                                      C
C     21.10.97 new data format in ../optdat/                            C
C     30.10.97 corrected: units [1/m] -> [1/km]                         C
C     06.11.97 opt.depth for clouds corrected.                          C
C     14.11.97 tested with Linux, SunOS, Dec Ultrix, HP Unix, DOS       C
C     08.09.09 debugging and reformatting by F. Veroustraete (FVo)      C
C     10.10.09 Aerosols specific mass table included (FVo)              C
C     10.10.09 Mass calculations included  (FVo)                        C
C     26.10.09 PM calculations included  (FVo)                          C
C     29.10.09 Code Completely reformatted (FVo)                        C
C     06.11.09 Angstrom coefficient added according to Globaerosol      C
C              definition (ATBD v1p0)                                   C
C                                                                       C
C     Version: 06.11.09                                          (FVo)  C
CCCCC ------------------------------------------------------------------C
C     Variable definitions
      logical wa,wb
      character*2  chum
      character*4  comnam
      character*8  inp
      character*7  res
      character*8  file,optnam,optun,opanam
      character*10 opt
      character*20 infile
      character*20 rname
      character*30 typnam,tname,tnew
      integer nlt(10)
      real mixrat,numden
C     COMMON Statements
      common /prog/   nprog
      common /file/   lfile,file,rname,infile,inp,res,opt
      common /numdis/ sigma(10),rmin(10,8),rmax(10,8),rmod(10,8),
     &                mixrat(10),dens(10,8)
      common /mipaco/ sigmac(20),rminc(20,8),rmaxc(20,8),rmodc(20,8),
     &                densc(20,8)
      common /mipaty/ nco(20),mco(20,10),dnumb(20,10),nlay(20),
     &                hmint(20,4),hmaxt(20,4),part(20,4),tname(20)
      common /input/  mtyp,nuco(10),dnum(10),mcom,
     &                hmin(5),hmax(5),hpar(5),
     &                nwavea,indwa(100),wavea(100),
     &                nwaveb,indwb(100),waveb(100),
     &                nhc,indhum(8),nopt,indopt(20),tnew
      common /atyp/   natyp,mcomp,ncomp(10),numden,
     &                typnam,comnam(20)
      common /layer/  mlay,nltyp(10),parlay(10,5),boundl(10),
     &                boundu(10)
      common /opar/   mopar,jnopar(20),nop,opanam(20),optnam(20),
     &                optun(20)
      common /wavel/  mlamb,alamb(61),niw,wlamb(61),
     &                il035,il05,il055,il08,alambp(61),niwp
      common /hum/    khum(8),ahum(8),nih,nhum(8),mhum,chum(8)
      common /norm/   norm,mixnor
C     Data statements
      data norm  /1/
      data nprog /0/
      data nlt   /1,2,3,4,5,0,0,0,0,0/
      data nhum  /0,50,70,80,90,95,98,99/,mhum/8/
      data chum  /'00','50','70','80','90','95','98','99'/
      data optnam /'ext.coef','sca.coef','abs.coef','sisc.alb',
     &             'asym.par','op.depth',
     &             '        ','turb.fac','li.ratio','pha.func',
     &             'ext.rat ','abs.rat ',
     &             '        ','ext.norm','        ','        ',
     &             'visibil ','ref.real',
     &             'ref.imag','        '/
      data optun  /' [1/km] ',' [1/km] ',' [1/km] ','        ',
     &             '        ','        ',
     &             '        ','        ','  [sr]  ',' [1/km] ',
     &             ' [m2/g] ',' [m2/g] ',
     &             '        ','        ','        ','        ',
     &             '  [km]  ','        ',
     &             '        ','        '/
      data comnam /'inso','waso','soot','ssam','sscm','minm','miam',
     &             'micm','mitr','suso','stco','stma','cucc','cucp',
     &             'cuma','fogr','cir1','cir2','cir3','    '/
C
C     Onscreen title
C
      print*,'*********************************************************'
      print*,'* OPAC v3.16: Optical Properties of Aerosols and Clouds *'
      print*,'* ----------------------------------------------------  *'
      print*,'*                                                       *'
      print*,'*  The contents of OPAC is described in:                *'
      print*,'*  M. Hess, P. Koepke and I. Schult (1997):             *'
      print*,'*  "Optical Properties of Aerosols and Clouds:          *'
      print*,'*   The software package OPAC"                          *'
      print*,'*  Published in Bull. Am. Meteor. Soc.                  *'
      print*,'*                                                       *'
      print*,'* Coding: M. Hess                              14.11.97 *'
      print*,'* Additions/debugging: F.Veroustraete - Vito   06.11.09 *'
      print*,'*********************************************************'
      print*,' '
      print*,' '
C
CCCCC -----------------------------------------------------------------C
C     Read data from configuration file opac.cfg                       C
CCCCC -----------------------------------------------------------------C
C
      call readcfg
C
CCCCC ------------------------------------------------------------------C
C     Read data from input file                                        C
CCCCC ------------------------------------------------------------------C
      call readinp
C
CCCCC ------------------------------------------------------------------C
C     Prepare data selection + check input data                         C
C     1. mixture of type                                                C
CCCCC ------------------------------------------------------------------C
C     print*,mtyp
      if(mtyp.eq.0) then
         typnam=tnew
C
         numden=0.
         mcomp=0
         do ic=1,5
            if (dnum(ic).ne.0.) then
               mcomp=mcomp+1
               numden=numden+dnum(ic)
            end if
         end do
         do ic=1,mcomp
            ncomp(ic)=nuco(ic)
            mixrat(ic)=dnum(ic)/numden
            sigma(ic)=sigmac(nuco(ic))
         end do
            else
            typnam=tname(mtyp)
            mcomp=nco(mtyp)
C
         numden=0.
         do ic=1,mcomp
            numden=numden+dnumb(mtyp,ic)
         end do
         do ic=1,mcomp
            ncomp(ic)=mco(mtyp,ic)
            mixrat(ic)=dnumb(mtyp,ic)/numden
            sigma(ic)=sigmac(mco(mtyp,ic))
         end do
      end if
C
C     print*,typnam,numden
C
      if (mcomp.gt.1) then
         do ic=1,mcomp
            if (ncomp(ic).ge.11) then
               print*,' '
               print*,'    !! ATTENTION !!'
               print*,' '
               print*,' in the present version, clouds may not',
     &                ' be mixed with other particles'
               print*,' '
               print*, ' Please change input file!'
               stop
            end if
         end do
      end if
C
CCCCC ------------------------------------------------------------------C
C     2. relative humidity                                              C
CCCCC ------------------------------------------------------------------C
      nih=0
      do ih=1,nhc
         if(indhum(ih).ne.0) then
            nih=nih+1
            khum(nih)=ih
            ahum(nih)=nhum(ih)
         end if
      end do
C
      if (nih.ne.0) then
         do ih=1,nih
            do ic=1,mcomp
               dens(ic,ih)=densc(ncomp(ic),khum(ih))
               rmin(ic,ih)=rminc(ncomp(ic),khum(ih))
               rmax(ic,ih)=rmaxc(ncomp(ic),khum(ih))
               rmod(ic,ih)=rmodc(ncomp(ic),khum(ih))
C
C     print*,khum(ih),rmin(ic,ih),rmax(ic,ih),rmod(ic,ih)
C
            end do
         end do
      else
         print*,' '
         print*,'    !! ATTENTION !!'
         print*,' '
         print*,' no relative humidity selected!'
         print*,' '
         print*, ' Please change input file!'
         stop
      end if
C
CCCCC ------------------------------------------------------------------C
C     3. height profile                                                 C
CCCCC ------------------------------------------------------------------C
      ht=0.
      do il=1,5
         ht=ht+(hmax(il)-hmin(il))
      end do
C
      if (mtyp.eq.0.or.ht.ne.0.) then
         mlay=0
         do il=1,5
            if (hmax(il).ne.hmin(il)) then
               mlay=mlay+1
               nltyp(mlay)=nlt(il)
               boundl(mlay)=hmin(il)
               boundu(mlay)=hmax(il)
               parlay(mlay,1)=hpar(il)
            end if   
         end do
      else
         mlay=nlay(mtyp)
         do il=1,mlay
            if (mlay.eq.1) then  ! for clouds
               nltyp(1)=5
            else
               nltyp(il)=nlt(il)
            end if   
            boundl(il)=hmint(mtyp,il)
            boundu(il)=hmaxt(mtyp,il)
            parlay(il,1)=part(mtyp,il)
         end do
      end if
C
CCCCC ------------------------------------------------------------------C
C     4. optical parameters                                             C
CCCCC ------------------------------------------------------------------C
      mopar=nopt
      nop=0
      do iop=1,mopar
         jnopar(iop)=indopt(iop)
         if (jnopar(iop).eq.1) then
            nop=nop+1
            opanam(nop)=optnam(iop)
         end if
      end do
      if (jnopar(15).eq.1) nop=nop-1   ! not wavelength dependent
      if (jnopar(16).eq.1) nop=nop-1   !           "
      if (jnopar(17).eq.1) nop=nop-1   !           "
      if (jnopar(18).eq.1) nop=nop+1   ! refr.index has 2 values
C
      do ic=1,mcomp
         if (jnopar(11).eq.1.and.ncomp(ic).ge.11.or.
     &       jnopar(12).eq.1.and.ncomp(ic).ge.11) then
            print*,' '
            print*,'    !! ATTENTION !!'
            print*,' '
            print*,' in the present version, no mass related',
     &             ' parameters may be calculated'
            print*,' for clouds'
            print*,' '
            print*, ' Please change input file!'
            stop
         end if
      end do
C
      if (jnopar(15).eq.1.and.jnopar(1).eq.0.or.
     &    jnopar(15).eq.1.and.jnopar(4).eq.0.or.
     &    jnopar(15).eq.1.and.jnopar(5).eq.0) then
C
         print*,' '
         print*,'    !! ATTENTION !!'
         print*,' '
         print*,' extinction coefficient, single scattering albedo and'
         print*,' asymmetry parameter must be selected too'
         print*,' for calculation of solar and terrestrial integrated'
         print*,' parameters'
         print*,' '
         print*, ' Please change input file!'
         stop
      end if
C
      if (jnopar(16).eq.1.and.jnopar(1).eq.0) then
         print*,' '
         print*,'    !! ATTENTION !!'
         print*,' '
         print*,' extinction coefficient must be selected, too'
         print*,' for calculation of Angstrom coefficients'
         print*,' '
         print*, ' Please change input file!'
         stop
      end if
C
      if (jnopar(18).eq.1.and.mcomp.gt.1) then
         print*,' '
         print*,'    !! ATTENTION !!'
         print*,' '
         print*,' refractive indices can only be printed for '
         print*,' single components'
         print*,' '
         print*, ' Please change input file!'
         stop
      end if
C
      if (jnopar(7).eq.1.or.jnopar(13).eq.1) then
         print*,' '
         print*,'    !! ATTENTION !!'
         print*,' '
         print*,' you selected an invalid optical parameter'
         print*,' '
         print*, ' Please change input file!'
         stop
      end if
C
CCCCC ------------------------------------------------------------------C
C     5. Wavelengths                                                    C
CCCCC ------------------------------------------------------------------C
      wa=.false.
      wb=.false.
      do iw=1,nwavea
         if (indwa(iw).ne.0) wa=.true.
      end do
      do iw=1,nwaveb
         if (indwb(iw).ne.0) wb=.true.
      end do
      if (wa.and.wb) then
         print*,' '
         print*,'    !! ATTENTION !!'
         print*,' '
         print*,' you selected wavelengths in both wavelength tables!'
         print*,' '
         print*, ' Please change input file!'
         stop
      else if (wa) then
         niw=0
         mlamb=nwavea
         do il=1,mlamb
            wlamb(il)=wavea(il)
            if (indwa(il).ne.0) then
               niw=niw+1
               alamb(niw)=wlamb(il)
            end if
         end do
      else if (wb) then
         niw=0
         mlamb=nwaveb
         do il=1,mlamb
            wlamb(il)=waveb(il)
            if (indwb(il).ne.0) then
               niw=niw+1
               alamb(niw)=wlamb(il)
            end if
         end do
      end if
      il05=0
      il055=0
      il035=0
      il08=0
      do iw=1,niw
         if (alamb(iw).eq.0.35) il035=iw
         if (alamb(iw).eq.0.5)   il05=iw
         if (alamb(iw).eq.0.55) il055=iw
         if (alamb(iw).eq.0.8)   il08=iw
      end do
      niwp=niw
C
      if (jnopar(15).eq.1) then    ! with solar/terrestrial integrated quantities
         niwp=niw                  ! all wavelengths are input, but not printed
         do il=1,niwp
            alambp(il)=alamb(il)
         end do
         niw=mlamb
         do il=1,niw
            alamb(il)=wlamb(il)
            if (alamb(il).eq.0.35) il035=il
            if (alamb(il).eq.0.5)  il05=il
            if (alamb(il).eq.0.55) il055=il
            if (alamb(il).eq.0.8)  il08=il
         end do
      end if
C
      if (jnopar(14).eq.1.and.il055.eq.0) then
         print*,' '
         print*,'    !! ATTENTION !!'
         print*,' '
         print*,' wavelength of 0.55 æm must be selected'
         print*,' for calculation of normalized extinction coefficients'
         print*,' '
         print*, ' Please change input file!'
         stop
      end if
C
      if (jnopar(16).eq.1.and.il035.eq.0.or.
     &    jnopar(16).eq.1.and.il05.eq.0.or.
     &    jnopar(16).eq.1.and.il08.eq.0) then
         print*,' '
         print*,'       !! ATTENTION !!'
         print*,' '
         print*,' wavelengths 0.35, 0.5 and 0.8 æm must be selected'
         print*,' for calculation of Angstrom coefficients'
         print*,' '
         print*, ' Please change input file!'
         stop
      end if
C
CCCCC ------------------------------------------------------------------C
C     Split into subprogramme RAWOPT for the calculation of             C
C     optical properties                                                C
CCCCC ------------------------------------------------------------------C
      call rawopt
C
      print*, ' '
      print*,'End of the OPAC fairy tale? No!'
      print*,'You can start post processing the ASCII output files now.'
      print*,'They are "opac.out" and "opac.dis".'
      print*,'Have fun.'
C
      stop
      end                    ! END OF MAIN ROUTINE
CCCCC ------------------------------------------------------------------C
C                                                                       C
C                            SUBROUTINES AND FUNCTIONS                  C
C                                                                       C
CCCCC ------------------------------------------------------------------C
C
      subroutine comment (com,ifile)
C
CCCCC ------------------------------------------------------------------C
C     Subroutine comment is used for file numerical value reading       C
C     based on the occurence of the # sign                              C
C     Version: 04.09.09                                       (FVo)     C
CCCCC ------------------------------------------------------------------C
C
      character*1 com,dum
 1011 read (ifile,'(a1)') dum
         if (dum.eq.com) then
            goto 1011
         else
         backspace(ifile)
         end if
      return
      end
C
C
      subroutine readcfg
CCCCC ------------------------------------------------------------------C
C     Read microphysical parameters of components and types from        C
C     file "opac.cfg"                                                   C
C     Version: 30.09.97                                       M. Hess   C
CCCCC ------------------------------------------------------------------C
C     Variable definitions
      character*1  dum
      character*4  os
      character*8  inp
      character*7  res
      character*8  file
      character*10 opt
      character*20 infile
      character*20 rname
      character*30 tname
C     COMMON Statements
      common /file/   lfile,file,rname,infile,inp,res,opt
      common /mipaco/ sigmac(20),rminc(20,8),rmaxc(20,8),rmodc(20,8),
     &                densc(20,8)
      common /mipaty/ nco(20),mco(20,10),dnumb(20,10),nlay(20),
     &                hmint(20,4),hmaxt(20,4),part(20,4),tname(20)
C
C     Start subroutine readcfg
C
      open (8,file='opac.cfg')
C
           call comment('#',8)
C
      read (8,'(a4)') os
      print *,'Operating system    : ',os
C
      if (os.eq.'dos ') then
         inp=' '
         res=' '
         opt=' '
      else if (os.eq.'unix') then
         inp=' '
         res=' '
         opt=' '
      else
         stop ' wrong operating system!'
      end if
C
          call comment('#',8)
C
      read (8,*) ncom
      print *,'Total aero+clouds   : ',ncom
      read (8,*) ntyp
C
          call comment('#',8)
C
      do icom=1,ncom
         read(8,'(a1)') dum
         read(8,'(a1)') dum
         read(8,'(a1)') dum
C     print*,'component nr: ',icom
         do ih=1,8
            if (icom.le.10) then
               read (8,*) nh,rminc(icom,ih),rmaxc(icom,ih),
     &                    rmodc(icom,ih),densc(icom,ih),
     &                    sigmac(icom)
            else
               read (8,*) nh,rminc(icom,ih),rmaxc(icom,ih),
     &                    rmodc(icom,ih),densc(icom,ih)
            end if
            read (8,'(a1)') dum
            if (ih.eq.1.and.dum.eq.'#') then
               do ihd=2,8
                  rminc(icom,ihd)=rminc(icom,1)
                  rmaxc(icom,ihd)=rmaxc(icom,1)
                  rmodc(icom,ihd)=rmodc(icom,1)
                  densc(icom,ihd)=densc(icom,1)
               end do
               goto 1111
            else
               backspace(8)
            end if
         end do
C
         call comment('#',8)
C
 1111    continue
      end do
C
          call comment('#',8)
C
C     Read mixtures in particles/cm-3
C
      do ityp=1,ntyp
C
         call comment('#',8)
C
         read (8,200) nt,tname(ityp)
  200 format(i2,2x,a30)
         read (8,*) nco(ityp)
         do ico=1,nco(ityp)
            read (8,100) mco(ityp,ico),dnumb(ityp,ico)
  100 format(i2,8x,e10.3)
C      print*,nt,tname(ityp),mco(ityp,ico),dnumb(ityp,ico)
         end do
C
C     Read height layers
C
         read (8,*) nlay(ityp)
         do ilay=1,nlay(ityp)
            read (8,*) hmint(ityp,ilay),hmaxt(ityp,ilay),
     &                 part(ityp,ilay)
C     print*,ityp,ilay,hmint(ityp,ilay),hmaxt(ityp,ilay),
C     &      part(ityp,ilay)
         end do
      end do
      close(8)
C
      return
      end !of readcfg
C
C
      subroutine readinp
CCCCC ------------------------------------------------------------------C
C     read input file *.inp which data to extract and calculate         C
C     from the database.                                                C
C     Version: 21.09.97                                       M. Hess   C
CCCCC ------------------------------------------------------------------C
C     Variable definitions
C
      character*8  inp
      character*7  res
      character*8  file
      character*10 opt
      character*20 infile
      character*20 rname
      character*30 tnew
C     COMMON statements
C
      common /file/   lfile,file,rname,infile,inp,res,opt
      common /input/  mtyp,nuco(10),dnum(10),mcom,
     &                hmin(5),hmax(5),hpar(5),
     &                nwavea,indwa(100),wavea(100),
     &                nwaveb,indwb(100),waveb(100),
     &                nhc,indhum(8),nopt,indopt(20),tnew
C
C     Start readinp
C
      open (9,file='opac.inp')
      rname(1:7)=res
      rname(8:(8+(lfile-1)))=file(1:lfile)
      rname((9+(lfile-1)):(13+(lfile-2)))='.out'
C
         call comment('#',9)
C
C     Read nr. of aerosol types
C
      read (9,*) mtyp
      print *,'Total aerosol types : ',mtyp
C
         call comment('#',9)
C
      read (9,'(a30)') tnew
C      print *,'Aerosol type: ',tnew
C
         call comment('#',9)
C
C     Read aerosol mixture of certain types
C
      mcom=0
      do ic=1,5
         read (9,*) nuco(ic),dnum(ic)
         if (dnum(ic).gt.0.) mcom=mcom+1
C
C     print*,'aerosol mix: ',ic, nuco(ic),dnum(ic)
C
      end do
C
         call comment('#',9)
C
C     Read height profile                                               C
C
      do il=1,5
         read (9,*) hmin(il),hmax(il),hpar(il)
C
C     print*,'height profile: ',il, hmin(il),hmax(il),hpar(il)
C
      end do
C
         call comment('#',9)
C
C     Read wavelengths                                                  C
C
      read (9,*) nwavea
      do iw=1,nwavea
         read (9,100) indwa(iw),wavea(iw)
  100 format(i1,32x,f10.3)
C     print*,'wavel aer+waterclouds: ',iw, indwa(iw),wavea(iw)
      end do
C
         call comment('#',9)
C
      read (9,*) nwaveb
      do iw=1,nwaveb
         read (9,100) indwb(iw),waveb(iw)
C
C     print*,'wavel cirrusclouds: ',iw, indwb(iw),waveb(iw)
C
      end do
C
          call comment('#',9)
C
C     Read relative humidity classes                                    C
C
      read (9,*) nhc
      do ih=1,nhc
         read (9,*) indhum(ih)
C
C     print*,'humidity classes: ',ih, indhum(ih)
C
      end do
C
         call comment('#',9)
C
C     Read selected optical parameters                                  C
C
      read (9,*) nopt
      do iopt=1,nopt
         read(9,*) indopt(iopt)
C
C     print*,'optical parameters: ',iopt, indopt(iopt)
C
      end do
C
C     print*,'end of input reading'
C
      close(9)
      return
      end !of subroutine readinp
C
C
      subroutine rawopt
CCCCC -----------------------------------------------------------------C
C     Calculation of optical parameters of an aerosol type for         C
C     multiple wavelengths and humidities                              C
C                                                                      C
C     The following subroutines are called:                            C
C                                                                      C
C     - RAWMIC                                                         C
C     - HEAD2                                                          C
C     - OPTRAW                                                         C
C     - OPTPAR                                                         C
C     - OUT2                                                           C
C                                                                      C
C     This version belongs to a limited OPAC database and              C
C     falls back on RAWOPT version of 04.11.93                         C
C                                                                      C
C     13.05.94 Reading of background Extinction.                       C
C     12.01.96 Mass values are always calculated                       C
C     01.10.97 Changed reading of 'extback.dat'                        C
C                                                                      C
C     Version: 01.10.97                                        M. Hess C
CCCCC -----------------------------------------------------------------C
C     Definition of variables
      character*1  dum
      character*2  chum
      character*3  atn,pat,typnam*30
      character*4  comnam
      integer      prnr,acnr,rht
      real         mixrat,n,numden
C     COMMON statements
      common /prog/   nprog
      common /numdis/ sigma(10),rmin(10,8),rmax(10,8),rmod(10,8),
     &                mixrat(10),dens(10,8)
      common /FTASTR/ EXTFTA(61),EXTSTR(61),extmit(61)
      common /atyp/   natyp,mcomp,ncomp(10),numden,
     &                typnam,comnam(20)
      common /wavel/  mlamb,alamb(61),niw,wlamb(61),
     &                il035,il05,il055,il08,alambp(61),niwp
      common /hum/    khum(8),ahum(8),nih,nhum(8),mhum,chum(8)
      common /mipoi/  latx,lonx,nl,prnr,rht(2),n(2),
     &                njc(2),acnr(5,2),acmr(5,2),nh(2),atn(2),pat(2)
      common /out/    oparam(16,2),phaf(167,2),indop(16)
C
CCCCC -----------------------------------------------------------------C
C     Call of RAWMIC for combined optical/microphysical quantities     C
CCCCC -----------------------------------------------------------------C
C
      call rawmic
C
CCCCC -----------------------------------------------------------------C
C     Definition of the quantities, used in the subprogrammes          C
CCCCC -----------------------------------------------------------------C
C
      n(1)=numden
      nl=1
      njc(1)=mcomp
      do ic=1,mcomp
         acnr(ic,1)=ncomp(ic)
         acmr(ic,1)=mixrat(ic)
      end do
C
C     Loop for all specified humidity classes                          C
C
      if (nih.eq.0) then
         njh=1
      else
         njh=nih
      end if
      do ih=1,njh  !start humidity loop
C
C     read the extinction coefficients of the tropospheric             C
C     and stratospheric backgrounds and of the minerals transported    C
C
      open (9,file='extback.dat')
      IL=1
      read(9,'(a1)') dum
      read(9,'(a1)') dum
      do IWL=1,mlamb
         read(9,*) WAVE,EXTFT,EXTST,extmi
         do ila=1,niw
            if (WAVE.EQ.alamb(ila)) then
               EXTFTA(IL)=EXTFT
               EXTSTR(IL)=EXTST
               extmit(il)=extmi
               IL=IL+1
            end if
C
C     print*,'ext coeff of background', IWL,ila,WAVE,EXTFT,EXTST,extmi
C
         end do
      end do
C
      close (9)
C
C     reading of the raw optical data in file OPTDAT
C
         call optraw (ih)
C
C     write output file
C
         call head2 (ih)
C
C     loop over all specified wavelengths                              C
C
      do il=1,niw          !start wavelength loop
C
C     Selection of the cdata needed according to the input files       C
C
         call optdat(il)
C
C     Calculation of the optical parameters                            C
C
         call optpar(il,ih)
C
      end do ! end of wavelength loop
C
C     Calculation of the solar and terrestrial integrated values,      C
C     of the Angstrom coefficients and visibility                      C
C
         call intco
C
      end do ! end of humidity loop
C
      return
      end ! end of rawmic routine
C
C
      subroutine head2 (ih)
C     -----------------------------------------------------------------C
C     version OPAC 3.16                                                C
C                                                                      C
C     write output file header                                         C
C                                                                      C
C     Version: 06.11.09                                          FVo   C
CCCCC -----------------------------------------------------------------C
C     Variable difinitions
      character*2  chum
      character*4  comnam
      character*8  inp
      character*7  res
      character*8  file,optun,optnam,opanam
      character*10 opt
      character*20 infile
      character*20 rname
      character*30 typnam
      real         mixrat,numden
C     COMMON statements
      common /file/   lfile,file,rname,infile,inp,res,opt
      common /hum/    khum(8),ahum(8),nih,nhum(8),mhum,chum(8)
      common /numdis/ sigma(10),rmin(10,8),rmax(10,8),rmod(10,8),
     &                mixrat(10),dens(10,8)
      common /masse/  smas(10,8),smag(8),vsum(10,8),vges(8),xabmax
      common /atyp/   natyp,mcomp,ncomp(10),numden,
     &                typnam,comnam(20)
      common /opar/   mopar,jnopar(20),nop,opanam(20),optnam(20),
     &                optun(20)
      common /out/    oparam(16,2),phaf(167,2),indop(16)
      common /angle/  jnangle(167),angle(167),nia,ntheta
      common /wavel/  mlamb,alamb(61),niw,wlamb(61),
     &                il035,il05,il055,il08,alambp(61),niwp
C
      open (10,file='opac.out')
C
C     write output file header                                         C
C
      if (ih.eq.1) then
         write(*, 119) typnam,mcomp,nih,niwp
         write(10,120) typnam,mcomp,nih,niwp
         write(10,108)
         write(10,112)
         write(10,108)
      do ihu=1,nih
         do ic=1,mcomp
           if (ic.eq.1) then
              write(10,124) ahum(ihu),comnam(ncomp(ic)),
     &                      numden*mixrat(ic),
     &                      vges(ihu)*vsum(ic,ihu),
     &                      smag(ihu)*smas(ic,ihu),
     &                      dens(ic,ihu),mixrat(ic),vsum(ic,ihu),
     &                      smas(ic,ihu)
           else
              write(10,121) comnam(ncomp(ic)),
     &                      numden*mixrat(ic),
     &                      vges(ihu)*vsum(ic,ihu),
     &                      smag(ihu)*smas(ic,ihu),
     &                      dens(ic,ihu),mixrat(ic),vsum(ic,ihu),
     &                      smas(ic,ihu)
           end if
         end do
      end do
      write (10,108)
      write (10,110) xabmax
      if (jnopar(10).eq.1) then
         write (10,102) ntheta
         write (10,103) (angle(ia),ia=1,ntheta)
      end if
      end if
      write (10,107)
C
C     Format statements
  110 format (1x,'only particles up to ',f5.1,' micrometer are ',
     &               'considered for mass')
  102 format(i4,' scattering angles for phase functions below:')
  103 format(8f10.3)
  107 format('===================================================',
     &       '=========================')
  108 format('---------------------------------------------------',
     &       '-------------------------')
  112 format(' RH  COMP NUMBER    VOLUME    MASS      DENSITY  ',
     &       'NUM.MIX  VOL.MIX   MAS.MIX'/
     &       ' [%]     [1/cm3]   [um3/m3]   [ug/m3]   [g/cm3]  ',
     &       'RATIO    RATIO     RATIO')
  119 format(' aerosol type        : ',a30,/
     &       ' components          : ',i3,/
     &       ' relative humidities : ',i3,/
     &       ' wavelengths         : ',i3)
  120 format(12x,
     &       'Optical Properties of Aerosols and Clouds (OPAC) v3.16'/
     &       ,11x,
     &       '------------------------------------------------------'/
     &       ' Aerosol type        : ',a30,/
     &       ' Components          : ',i3,/
     &       ' Relative humidities : ',i3,/
     &       ' Wavelengths         : ',i3)
  121 format (5x,a4,1p3e10.3,1x,0pf4.2,3x,1p3e10.3)
  124 format (f4.0,1x,a4,1p3e10.3,1x,0pf4.2,3x,1p3e10.3)
C
C     write the different relative humidity values
C
      if (ih.ne.1) then
         write (10,108)
      end if
      write(10,4000) ahum(ih)
C
      return
C
      entry names
C
      if (jnopar(10).eq.1) then
         kop=nop-1
      else
         kop=nop
      end if
      if (kop.le.7) then
         write(10,4001) (optnam(indop(in)),in=1,kop)
         write(10,4003) (optun(indop(in)), in=1,kop)
      else
         write(10,4001) (optnam(indop(in)),in=1,7)
         write(10,4002) (optun(indop(in)), in=1,7)
         write(10,4002) (optnam(indop(in)),in=8,kop)
         write(10,4002) (optun(indop(in)), in=8,kop)
      end if
C     Format statements
 4000 format('#',F4.0,' % Relative Humidity ')
 4001 format('# wavel.   ',7(1x,a8,1x))
 4003 format('#  [um]    ',7(1x,a8,1x))
 4002 format('#          ',7(1x,a8,1x))
C
      return            !end of head2 subroutine
      end
C
C
      subroutine out2(ilp,il,iop)
CCCCC -----------------------------------------------------------------C
C     output subroutine                                                C
C                                                                      C
C     Version 29.09.97                                         M. Hess C
CCCCC -----------------------------------------------------------------C
C     Variable definitions
      character*8 optun,optnam,opanam
C     COMMON statements
      common /wavel/  mlamb,alamb(61),niw,wlamb(61),
     &                il035,il05,il055,il08,alambp(61),niwp
      common /out/    oparam(16,2),phaf(167,2),indop(16)
      common /opar/   mopar,jnopar(20),nop,opanam(20),optnam(20),
     &                optun(20)
      common /angle/  jnangle(167),angle(167),nia,ntheta
C
C     Start output subroutine
      if ((jnopar(10).eq.1.and.ilp.gt.1).or.ilp.eq.1) then
C
         call names
C
      end if
C
      if (jnopar(10).eq.1) then
         iop=nop-1
      else
         iop=nop
      end if
      if (iop.le.7) then
         write(10,4010) alamb(il),(oparam(ip,1),ip=1,iop)
      else
         write(10,4010) alamb(il),(oparam(ip,1),ip=1,7)     ! 1. Zeile
         write(10,4020) (oparam(ip,1),ip=8,iop)             ! 2. Zeile
      end if
      if (jnopar(10).eq.1) then
         write(10,4002)
         write(10,4010) (phaf(it,1),it=1,ntheta)
      end if
C     Format statements
 4002 format('   phase function [1/km]')
 4010 format(1p8e10.3)
 4020 format(10x,1p7e10.3)
C
      return
      end !of subroutine out2
C
C
      subroutine rawmic
CCCCC ------------------------------------------------------------------C
C     Calculation of the particle size distribution within the selected C
C     interval boundaries.                                              C
C     Output of the datacouples and the microphysical parameters        C
C     of the distributions of all components of the aerosol type        C
C     The sum of the distributions of all aerosol components            C
C     is calculated as well.                                            C
C                                                                       C
C     19.11.92 Calculation of the volume distribution and the total     C
C              volume                                                   C
C     03.11.93 Mass of the components and total mass                    C
C              interactive reading of the changed radius limits         c
C                                                                       C
C     from then on the next versions are for OPAC.                      C
C                                                                       C
C     08.11.93 Interactive reading canceled                             C
C     04.12.95 Calculation of total mass only till max. radius of 7.5   C
C     15.01.96 The radiusfield is established with SORT1                C
C     17.01.96 Cut-off radius not larger than Rmax                      C
C     22.01.96 No mass calculation for clouds                           c
C                                                                       C
C     Version: 22.01.96                                         M. Hess c
CCCCC ------------------------------------------------------------------C
C     Variable definitions
      character*1  ent
      character*2  chum
      character*4  comnam
      character*8  inp
      character*7  res
      character*8  file
      character*10 opt
      character*20 infile
      character*20 rname
      character*30 typnam
      integer nx2(8),indx(220)
      integer nxs(8),nxc(10,8)
      real xr(220),xr2(220,8),dnsum(220,8),dn(220,10,8),numden, mixrat
      real xr3(220),dvsum(220,8),dv(220,10,8),r(220),v(220),m(220)
      real xr4(220),xab(10),dmsum(220,8),dm(220,10,8)
      real smag10(8),smag2_5(8),smag1_0(8),smag0_1(8),smag10m2_5(8)
C     COMMON Statements
      common /file/   lfile,file,rname,infile,inp,res,opt
      common /prog/   nprog
      common /numdis/ sigma(10),rmin(10,8),rmax(10,8),rmod(10,8),
     &                mixrat(10),dens(10,8)
      common /masse/  smas(10,8),smag(8),vsum(10,8),vges(8),xabmax
      common /hum/    khum(8),ahum(8),nih,nhum(8),mhum,chum(8)
      common /atyp/   natyp,mcomp,ncomp(10),numden,
     &                typnam,comnam(20)
C     DATA Statements
      data deltar /0.015/
      data xrmin  /0.01/,xrmax /10./
      data xabmax /7/
C     Constant definition
      pi=4.*atan(1.) ! Define PI
C
C     Run rawmic routine
C
      open (11,file='opac.dis')
C
C     Change radius limits when RAWOPT is called                        C
C
      if (nprog.eq.1) then
         write (*,*) ' The following radius limits are selected:'
         do ih=1,nih
            do ic=1,mcomp
               write (*,111) ahum(ih),ncomp(ic),rmin(ic,ih),rmax(ic,ih)
            end do
         end do
         write (*,112)
         read (*,'(a1)') ent
         if (ent.eq.'j') then
            do ih=1,nih
               do ic=1,mcomp
                  write (*,113) ahum(ih),ncomp(ic)
                  read(*,*) rmin(ic,ih)
                  write (*,114) ahum(ih),ncomp(ic)
                  read(*,*) rmax(ic,ih)
               end do
            end do
         end if
      end if
C
C     Format statements
  111 format(' f= ',f3.0,'%',' Component  ',i2,' rmin=',f6.3,
     &       ' rmax= ',f6.3)
  112 format (/' Should they be changed? (j/n) ')
  113 format(/' f= ',f3.0,'%',' Component  ',i2,' rmin= ')
  114 format(' f= ',f3.0,'%',' Component  ',i2,' rmax= ')
C
C     Calculation of radius lattices                                    C
C
      xr(1)=xrmin
      ix=2
      xranf=alog10(xrmin)
      do while (xr(ix-1).lt.xrmax)
         xrl=xranf+deltar*(ix-1.)
         xr(ix)=10.**xrl
         ix=ix+1
      end do
      nx=ix-1
C
C     Start humidity loop                                               C
C
      do ih=1,nih
         do ix=1,nx
            xr2(ix,ih)=xr(ix)
         end do
C
C     Insertion of the radius limits for the components                 C
C     and the cut-off radius of the lattice                             C
C
         kx=nx
         do ic=1,mcomp
            xab(ic)=xabmax
            kx=kx+1
            xr2(kx,ih)=rmin(ic,ih)
            kx=kx+1
            xr2(kx,ih)=rmax(ic,ih)
            if (rmax(ic,ih).lt.xabmax) then
               xab(ic)=rmax(ic,ih)
            end if
            kx=kx+1
            xr2(kx,ih)=xab(ic)
         end do
         do ix=1,kx
            xr3(ix)=xr2(ix,ih)
         end do
C
         call sort1(xr3,kx,xr4,indx)
C
         xr2(1,ih)=xr4(1)
         lx=0
         do ix=2,kx
            if (xr4(ix).ne.xr4(ix-1)) then
               xr2(ix-lx,ih)=xr4(ix)
            else
               lx=lx+1
            end if
         end do
C
         nx2(ih)=kx-lx
C
C     Loop over components
         do ic=1,mcomp
            if (ncomp(ic).gt.10) goto 1101 ! No mass calc. for clouds
C
C     Calculation of size distribution an den Stuetzpunkten
            do ix=1,nx2(ih)
               if (xr2(ix,ih).ge.rmin(ic,ih).and.
     &            xr2(ix,ih).le.rmax(ic,ih)) then
                  dn(ix,ic,ih)=rlogn(sigma(ic),rmod(ic,ih),
     &                         numden*mixrat(ic),d,e,xr2(ix,ih))
                  dv(ix,ic,ih)=vlogn(sigma(ic),rmod(ic,ih),
     &                         numden*mixrat(ic),pi,e,xr2(ix,ih))
                  dv(ix,ic,ih)=dv(ix,ic,ih)*10.**(6)
                  dm(ix,ic,ih)=dv(ix,ic,ih)*dens(ic,ih)*10.**(-6)
                  nxc(ic,ih)=nxc(ic,ih)+1
               else
                  dn(ix,ic,ih)=0.
                  dv(ix,ic,ih)=0.
                  dm(ix,ic,ih)=0.
               end if
C
C     Calculation of the sum over all components                        C
C
               dnsum(ix,ih)=dnsum(ix,ih)+dn(ix,ic,ih)    !Number
               dvsum(ix,ih)=dvsum(ix,ih)+dv(ix,ic,ih)    !Volume
               dmsum(ix,ih)=dmsum(ix,ih)+dm(ix,ic,ih)    !Mass
C
            end do
CCCCC ------------------------------------------------------------------C
C     Volume and Mass are calculated only to the radius XABMAX          C
C     Calculation of the aerosol volume and mass of the components      C
C     Calculation of the aerosol PM values mass of the component mass   C
C     Volume in (um3/m3)                                                C
C     Mass in (ug/m3)                                                   C
C     Volume and Mass are calculated only to the radius XABMAX          C
C     PM Mass sums are calculated for the radii less or equal to        C
C     0.1, 1, 2.5, 10 and the interval 10 - 2,5 æm                      C
CCCCC ------------------------------------------------------------------C
C     PM10
         do ix=1,nx2(ih)
            r(ix)=xr2(ix,ih)
            if (r(ix).le.10.0) then
               v(ix)=dv(ix,ic,ih)/(r(ix)*alog(10.))
            else
               v(ix)=0.
            end if
         end do
C
         call gerin(r,v,erg,1,nx2(ih),ier)
C
         vsum(ic,ih)=erg
         smas(ic,ih)=vsum(ic,ih)*dens(ic,ih)*10.**(-6)
         vges(ih)=vges(ih)+vsum(ic,ih) !total volume in æmü/mü
         smag10(ih)=smag10(ih)+smas(ic,ih) !total mass (<10 æm) in æg/æmü
C
C     PM2.5
         do ix=1,nx2(ih)
            r(ix)=xr2(ix,ih)
            if (r(ix).le.2.5) then
               v(ix)=dv(ix,ic,ih)/(r(ix)*alog(10.))
            else
               v(ix)=0.
            end if
         end do
C
         call gerin(r,v,erg,1,nx2(ih),ier)
C
         vsum(ic,ih)=erg
         smas(ic,ih)=vsum(ic,ih)*dens(ic,ih)*10.**(-6)
         vges(ih)=vges(ih)+vsum(ic,ih) !total volume in æmü/mü
         smag2_5(ih)=smag2_5(ih)+smas(ic,ih) !total mass (<2.5 æm) in æg/æmü
C
C     PM1
         do ix=1,nx2(ih)
            r(ix)=xr2(ix,ih)
            if (r(ix).le.1.0) then
               v(ix)=dv(ix,ic,ih)/(r(ix)*alog(10.))
            else
               v(ix)=0.
            end if
         end do
C
         call gerin(r,v,erg,1,nx2(ih),ier)
C
         vsum(ic,ih)=erg
         smas(ic,ih)=vsum(ic,ih)*dens(ic,ih)*10.**(-6)
         vges(ih)=vges(ih)+vsum(ic,ih) !total volume in æmü/mü
         smag1_0(ih)=smag1_0(ih)+smas(ic,ih) !total mass (<1.0 æm) in æg/æmü
C
C     PM0.1
         do ix=1,nx2(ih)
            r(ix)=xr2(ix,ih)
            if (r(ix).le.0.1) then
               v(ix)=dv(ix,ic,ih)/(r(ix)*alog(10.))
            else
               v(ix)=0.
            end if
         end do
C
         call gerin(r,v,erg,1,nx2(ih),ier)
C
         vsum(ic,ih)=erg
         smas(ic,ih)=vsum(ic,ih)*dens(ic,ih)*10.**(-6)
         vges(ih)=vges(ih)+vsum(ic,ih) !total volume in æmü/mü
         smag0_1(ih)=smag0_1(ih)+smas(ic,ih) !total mass (<1.0 æm) in æg/æmü
C
C     PM10 - PM2.5
         do ix=1,nx2(ih)
            r(ix)=xr2(ix,ih)
            if (r(ix).le.10.0) then
               v(ix)=dv(ix,ic,ih)/(r(ix)*alog(10.))
            else
               v(ix)=0.
            end if
         end do
C
         call gerin(r,v,erg,1,nx2(ih),ier)
C
         vsum(ic,ih)=erg
         smas(ic,ih)=vsum(ic,ih)*dens(ic,ih)*10.**(-6)
         vges(ih)=vges(ih)+vsum(ic,ih) !total volume in æmü/mü
         smag10(ih)=smag10(ih)+smas(ic,ih) !total mass (<10.0 æm) in æg/æmü
C
         do ix=1,nx2(ih)
            r(ix)=xr2(ix,ih)
            if (r(ix).le.2.5) then
               v(ix)=dv(ix,ic,ih)/(r(ix)*alog(10.))
            else
               v(ix)=0.
            end if
         end do
C
         call gerin(r,v,erg,1,nx2(ih),ier)
C
         vsum(ic,ih)=erg
         smas(ic,ih)=vsum(ic,ih)*dens(ic,ih)*10.**(-6)
         vges(ih)=vges(ih)+vsum(ic,ih) !total volume in æmü/mü
         smag2_5(ih)=smag2_5(ih)+smas(ic,ih) !total mass (<2.5 æm) in æg/æmü
         smag10m2_5(ih) = smag10(ih) - smag2_5(ih)
C
 1101    continue
      end do               ! Component loop end
      end do               ! Humidity loop end
C
C     Calculation of Volume- and Mass mixture ratios
C
      do ih=1,nih
         do ic=1,mcomp
            if (ncomp(ic).gt.10) goto 1102! No mass calculation for clouds
               vsum(ic,ih)=vsum(ic,ih)/vges(ih)
               smas(ic,ih)=smas(ic,ih)/smag(ih)
 1102       continue            
         end do
      end do
C
C     header of output file (but not when RAWOPT has been called)
C
C      write(11,99)
      write(11,100) typnam,mcomp,nih,numden
      do ihu=1,nih
         do ic=1,mcomp
           if (ic.eq.1) then
              write(11,101) ahum(ihu),comnam(ncomp(ic)),
     &                      sigma(ic),rmod(ic,ihu),
     &                      rmin(ic,ihu),rmax(ic,ihu),
     &                      mixrat(ic),vsum(ic,ihu)
           else
              write(11,102) comnam(ncomp(ic)),sigma(ic),
     &                      rmod(ic,ihu),rmin(ic,ihu),
     &                      rmax(ic,ihu),mixrat(ic),
     &                      vsum(ic,ihu)
           end if
         end do
      end do
      write (11,103) deltar
C
C     Output of calculated value couples for the aerosol components
C
      do ih=1,nih
         write (11,104) ahum(ih),vges(ih),smag10(ih),smag2_5(ih),
     &                  smag1_0(ih),smag0_1(ih),smag10m2_5(ih)
         do ic=1,mcomp
            write (11,105) comnam(ncomp(ic)),nxc(ic,ih)
            do ix=1,nx2(ih)
               if (dn(ix,ic,ih).ne.0.) then
                  write (11,106) xr2(ix,ih),
     &                           dn(ix,ic,ih),
     &                           dv(ix,ic,ih),
     &                           dm(ix,ic,ih)
               end if
            end do
         end do
C
C     Output of the calculated value couples for the aerosol sum
C
         do ix=1,nx2(ih)
            if (dnsum(ix,ih).ne.0.) then
                nxs(ih)=nxs(ih)+1
            end if
         end do
         comnam(ncomp(ic)) = 'comp_sum'
         write (11,105) comnam(ncomp(ic)),nxs(ih)
         do ix=1,nx2(ih)
            if (dnsum(ix,ih).ne.0.) then
               write (11,106) xr2(ix,ih),dnsum(ix,ih),
     &                        dvsum(ix,ih),dmsum(ix,ih)
            end if
         end do
      end do
      close (11)
C
C     Format statments
  100 format(' ',12x,
     &'Distribution Properties of Aerosols and Clouds (OPAC) 3.16'/
     &' ',11x,
     &' ----------------------------------------------------------'/
     &' Aerosol type          :',a30,/
     &' Number of components  :',i3,/
     &' Relative humidities   :',i3,/
     &' Aerosol Nr density    :',f10.0,' per cm3',/,
     &' Output of subroutine RAWMIC',
     &' with the following microphysical parameters:'/
     &'====================================================',
     &'============================',/,
     &'  RH  ','COMP   SIGMA     RMOD      RMIN      RMAX',
     &'      MIXRAT','    VMIXRAT',/,
     &'====================================================',
     &'============================')
  101 format (f5.0,' ',a4,2x,6e10.3,i5)
  102 format (5x  ,' ',a4,2x,6e10.3,i5)
  103 format('====================================================',
     &       '============================',/,
     &       ' Particle radii are given in um',/,
     &       ' dN/dlogr in particles/cm3',/,
     &       ' dV/dlogr in um3/m3',/,
     &       ' dM/dlogr in ug/m3',/,
     &       ' The radius interval dlogr = ',f7.5)
  104 format('========================================',/,
     &       ' Rel. Humid  :   ',f3.0,'%',/,
     &       ' PM tot vol  :   ',1pe10.3,' [um3/m3]',/,
     &       ' PM <=10um   :   ',1pe10.3,' [ug/m3]',/,
     &       ' PM <=2.5um  :   ',1pe10.3,' [ug/m3]',/,
     &       ' PM <=1.0um  :   ',1pe10.3,' [ug/m3]',/,
     &       ' PM <=0.1um  :   ',1pe10.3,' [ug/m3]',/,
     &       ' PM 10-2.5um :   ',1pe10.3,' [ug/m3]')
  105 format('========================================',/,
     &       ' component ',a4,' ',i6,' values',/,
     &       '=========','===========','=========','===========',/,
     &       '  radius ','   dN/dlogr',' dV/dlogr',' dM/dlogr',/,
     &       '=========','===========','=========','===========')
  106 format(4(e10.3))
C
C     End of the humidity loop                                          C
C
      return
      end ! of RAWMIC subroutine
C
C
      FUNCTION RLOGN(SIG,RO,VN,D,E,X)
CCCCC -----------------------------------------------------------------C
C     LOG-NORMAL-DISTRIBUTION  (DN/DLOGR IN particles/cm3)             C
C     THE RADII X AND RO HAVE TO BE EXPRESSED IN æm,                   C
C     N in particles/cm-3                                              C
CCCCC -----------------------------------------------------------------C
C
      PI=4.*ATAN(1.)
      A=VN/(SQRT(2.*PI)*ALOG10(SIG))
      B=ALOG10(RO)
      C=-2.*(ALOG10(SIG))**2
      RLOGN=A*EXP((ALOG10(X)-B)**2/C)
C
      return
      end !of RLOGN subroutine
C
C
      FUNCTION VLOGN(SIG,RO,VN,PI,E,X)
CCCCC -----------------------------------------------------------------C
C     LOG-NORMAL VOLUME DISTRIBUTION (DV/DLOGR IN um3/cm3)             C
C     THE RADII X AND RO HAVE TO BE EXPRESSED IN um,                   C
C     VN in cm-3  ?                                                    C
CCCCC -----------------------------------------------------------------C
C
      A=VN/(SQRT(2.*PI)*ALOG10(SIG))
      B=ALOG10(RO)
      C=-2.*(ALOG10(SIG))**2
      RLOGN=A*EXP((ALOG10(X)-B)**2/C)
      VLOGN=4./3.*PI*X**3.*RLOGN
C
      return
      end !of VLOGN subroutine
C
C
      subroutine optraw (ihum)
CCCCC -----------------------------------------------------------------C
C     New input routine for aerosol types (RAWOPT), not equal          C
C     any more to the version OPTCOM for ATLOPT.                       C
C                                                                      C
C     04.05.94 Exclusion of swelling for soot (component 3)            C
C     15.05.94 Reading of cirrus cdata                                 C
C     04.12.95 Reading of breaking indices                             C
C     07.12.95 New names for component data                            C
C                                                                      C
C     23.09.97 small changes for OPAC 3.0                              C
C     21.10.97 read new data format in ../optdat/ for OPAC 3.1         C
C                                                                      C
C     Version: 21.10.97                                       M. Hess  C
CCCCC -----------------------------------------------------------------C
C     Variables definition
      logical ende
      character*1  dum
      character*2  chum
      character*3  atn,pat
      character*4  comnam
      character*8  inp
      character*7  res
      character*8  file
      character*10 opt,dum2
      character*16 tap
      character*20 infile
      character*20 rname
      character*30 typnam
      integer prnr,acnr,njc,rht
      real n,numden
      real ex(61,6),sc(61,6),ab(61,6),si(61,6),as(61,6),ba(61,6)
      real ph(61,6,167),br(61,6),bi(61,6)
C     COMMON Statements
      common /file/   lfile,file,rname,infile,inp,res,opt
      common /atyp/   natyp,mcomp,ncomp(10),numden,
     &                typnam,comnam(20)
      common /wavel/  mlamb,alamb(61),niw,wlamb(61),
     &                il035,il05,il055,il08,alambp(61),niwp
      common /hum/    khum(8),ahum(8),nih,nhum(8),mhum,chum(8)
      common /mipoi/  latx,lonx,nl,prnr,rht(2),n(2),
     &                njc(2),acnr(5,2),acmr(5,2),nh(2),atn(2),pat(2)
      common /oppoi/  ext(1,6),sca(1,6),abs(1,6),sis(1,6),asy(1,6),
     &                bac(1,6),pha(1,6,167),ext05,bre(1,6),bim(1,6)
      common /angle/  jnangle(167),angle(167),nia,ntheta
C
      save
C
C     Loop for all selected components
      do il=1,nl
         if (nih.eq.0) then
            do ihu=1,mhum
               if (nh(il).eq.ihu) then
                  khum(ihum)=ihu
               end if
            end do
         end if
         ext05=0.
         do ic=1,njc(il)
            jc=acnr(ic,il)
C
C     Exclusion of swelling for insoluble, soot and
C     mineral components and clouds
            if (jc.eq.1.or.jc.eq.3.or.(jc.ge.6.and.jc.le.9).or.
     &          jc.gt.10 ) then
               iht=1
            else
               iht=khum(ihum)
            end if
C
C     Composition of the file name of the component saught
C     based on component number and humidity class
            tap(1:4)=comnam(jc)
            tap(5:6)=chum(iht)
C
C     print*,'tap=',tap
C
            ntap=70
            open (ntap,file=tap,iostat=ios)
            if (ios.ne.0) then
               print*,' error while opening file ',tap
               print*,'ios=',ios
               stop
            end if
            do iline=1,100
               read (ntap,220) dum2
C     print*,'dummy: ', dum2
               if (dum2.eq.'# optical ') then
                  goto 2002
               end if
            end do
 2002       continue
            do iline=1,5
               read (ntap,200) dum
            end do                  
            do ilam=1,mlamb
               read (ntap,500) rlamb,extco,scaco,absco,sisca,asymf,
     &                         exn,refr,refi
               ex(ilam,ic)=extco
               sc(ilam,ic)=scaco
               ab(ilam,ic)=absco
               if (rlamb.eq.0.55) then
                  ext05=ext05+ex(ilam,ic)*acmr(ic,1)*numden
               end if
               si(ilam,ic)=sisca
               as(ilam,ic)=asymf
               br(ilam,ic)=refr
               bi(ilam,ic)=refi
               if (rlamb.ne.wlamb(ilam)) then
                  print*,' '
                  print*,'        !! ATTENTION !!'
                  print*,' '
                  print*,' you have selected the wrong wavelength table'
                  print*,' Please change selection!'
                  print*,' '
                  print*,' Press <ENTER> to return to Input Menu'
                  read (*,'(a1)') dum
                  stop
               end if
            end do
            read (ntap,'(7(/))')
            it=1
            ende=.false.
            do while (.not.ende)
               read (ntap,510,end=511)
     &         angle(it),(ph(ilam,ic,it),ilam=1,mlamb)
               it=it+1
            end do
  511       ntheta=it-1
            do ilam=1,mlamb
               do it=1,ntheta
                  ph(ilam,ic,it)=ph(ilam,ic,it)
               end do
               ba(ilam,ic)=ph(ilam,ic,ntheta)
            end do
            close (ntap)
         end do
      end do
C
C     Format statements
C  100 format(8e10.3)
  200 format(a1)
  220 format(a10)
  500 format(2x,7e10.3,2e11.3)
  510 format(e11.3,1x,70e10.3)
C 1010 format(70X,e10.3)
C
      return
C
      entry optdat (ilamb)
C
C     Selection of optical data from those which have been input       C
C
      do ic=1,njc(1)
         do il=1,mlamb
            if (alamb(ilamb).eq.wlamb(il)) then
               ext(1,ic)=ex(il,ic)
               sca(1,ic)=sc(il,ic)
               abs(1,ic)=ab(il,ic)
               sis(1,ic)=si(il,ic)
               asy(1,ic)=as(il,ic)
               bac(1,ic)=ba(il,ic)
               bre(1,ic)=br(il,ic)
               bim(1,ic)=bi(il,ic)
               do it=1,ntheta
                  pha(1,ic,it)=ph(il,ic,it)
               end do
            end if
         end do
      end do
C
      return
      end
C
C
      SUBROUTINE optpar (ilamb,ihum)
CCCCC -----------------------------------------------------------------C
C     Calculation and output of selected optical parameters            C
C                                                                      C
C     04.11.93 Parameters SCARA, ABSRA, OMERA have been input          C
C     09.11.93 Version for OPAC (call for OUT4 has been deleted)       C
C     13.05.94 Optical depth for all wavelengths                       C
C     14.05.94 Indexing for optical parameters corrected for output    C
C     28.07.94 Normalized extinction coefficients introduced           C
C     04.12.95 Breaking indices introduced                             C
C     02.01.96 Calculation of optical depth over all height layers     C
C     10.01.96 New calculation for optical depth (without Heff)        C
C     22.01.96 EXTRA instead of SCARA                                  C
C     26.09.97 Normalized extinction corrected.                        C
C                                                                      C
C     Version: 26.09.97                                        M. Hess C
CCCCC -----------------------------------------------------------------C
C     Variables definition
      character*3  atn,pat
      character*8  optnam,optun,opanam
      character*4  comnam
      character*30 typnam
      integer prnr,acnr,rht
      real n,numden
      real EXTN(2),ABSN(2),SCAN(2),PF18N(2),supf(167),phafu(167,2)
      real EXTA(2),ABSA(2),SCAA(2),SSA(2),ASF(2),PF18A(2)
      real scar(2),absr(2),omer(2)
C     COMMON statements
      common /atyp/   natyp,mcomp,ncomp(10),numden,
     &                typnam,comnam(20)
      common /layer/  mlay,nltyp(10),parlay(10,5),boundl(10),
     &                boundu(10)
      common /norm/   norm,mixnor
      common /opar/   mopar,jnopar(20),nop,opanam(20),optnam(20),
     &                optun(20)
      common /wavel/  mlamb,alamb(61),niw,wlamb(61),
     &                il035,il05,il055,il08,alambp(61),niwp
      common /mipoi/  latx,lonx,nl,prnr,rht(2),n(2),
     &                njc(2),acnr(5,2),acmr(5,2),nh(2),atn(2),pat(2)
      common /oppoi/  ext(1,6),sca(1,6),abs(1,6),sis(1,6),asy(1,6),
     &                bac(1,6),pha(1,6,167),ext05,bre(1,6),bim(1,6)
      common /FTASTR/ EXTFTA(61),EXTSTR(61),extmit(61)
      common /out/    oparam(16,2),phaf(167,2),indop(16)
      common /sotin/  exi(61),ssi(61),asi(61)
      common /prog/   nprog
      common /angle/  jnangle(167),angle(167),nia,ntheta
      common /masse/  smas(10,8),smag(8),vsum(10,8),vges(8),xabmax
C
CCCCC ------------------------------------------------------------------C
C     Mixing of aerosol types                                           C
C     SUMM(E,A,S) : SUM OF EXTINCTION, ABSORPTION, SCATTERING           C
C     SUPF18      : SUM OF BACSKSCATTER COEFFICIENTS                    C
C     SUMASF      : INTERMEDIATE SUM OF ASSYMMETRy FACTORS (ASF)        C
C     SUMASF      : INTERMEDIATE SUM OF SINGLE SCATTERING ALBEDO (SSA)  C
CCCCC ------------------------------------------------------------------C
C
      do 10 L=1,NL
         SUMME  = 0.
         SUMMA  = 0.
         SUMMS  = 0.
         SUMSSA = 0.
         SUMASF = 0.
         SUPF18 = 0.
         if (jnopar(10).eq.1) then
            do it=1,ntheta
               supf(it)=0.
            end do
         end if
         do 20 JC=1,NJC(L)
            SUMME =  SUMME + ACMR(JC,L)*EXT(l,jc)
            SUMMA =  SUMMA + ACMR(JC,L)*ABS(l,jc)
            SUMMS =  SUMMS + ACMR(JC,L)*SCA(l,jc)
            SUMSSA = SUMSSA + ACMR(JC,L)*sis(l,jc)
     &               *EXT(l,jc)
            SUMASF = SUMASF + ACMR(JC,L)*asy(l,jc)
     &               *SCA(l,jc)
            SUPF18 = SUPF18 + ACMR(JC,L)*bac(l,jc)
            if (jnopar(10).eq.1) then
               do it = 1,ntheta
                  supf(it) = supf(it)+acmr(jc,l)*pha(l,jc,it)
               end do
            end if
   20    CONTINUE
C
C     Normalised optical parameters                                    C
C
         EXTN(L) = SUMME
         ABSN(L) = SUMMA
         SCAN(L) = SUMMS
         PF18N(L) = SUPF18
         if (jnopar(10).eq.1) then
            do it=1,ntheta
               phafu(it,l) = supf(it)
            end do
         end if
         SSA(L) = SUMSSA/SUMME
         ASF(L) = SUMASF/SUMMS
C
C     Absolute optical parameters                                      C
C
         EXTA(L)= EXTN(L) * N(L)
         ABSA(L)= ABSN(L) * N(L)
         SCAA(L)= SCAN(L) * N(L)
         PF18A(L) = PF18N(L)* N(L)
         if (jnopar(10).eq.1.and.norm.eq.1) then
            do it = 1,ntheta
               phafu(it,l) = phafu(it,l)*n(l)
            end do
         end if
         if (norm.eq.1) then
            EXTN(L)= EXTA(L)
            ABSN(L)= ABSA(L)
            SCAN(L)= SCAA(L)
            PF18N(L) = PF18A(L)
         end if
         if (jnopar(15).eq.1.or.jnopar(16).eq.1) then
            exi(ilamb) = extn(1)
            ssi(ilamb) = ssa(1)
            asi(ilamb) = asf(1)
         end if
         if (jnopar(10).eq.1) then
            itp = 1
            do it = 1,ntheta
               if (jnangle(it).eq.1) then
                  phaf(itp,l) = phafu(it,l)
                  itp = itp+1
               end if
            end do
         end if
         if (jnopar(11).eq.1) then
            scar(l) = exta(l)/smag(ihum)*1000. ! Units: m2/g
         end if
         if (jnopar(12).eq.1) then
            absr(l) = absa(l)/smag(ihum)*1000.
         end if
         if (jnopar(13).eq.1) then
            kc = 0
            do jc = 1,njc(l)
               if (ncomp(jc).eq.3) kc = jc
            end do
            if (kc.ne.0) then
               omer(l) = smas(kc,ihum)/ssa(l)
            else
               omer(l) = 99.
            end if
         end if
C
C     Data output
C
         iop = 0
         kop = 0
         if (jnopar(1).eq.1) then
            iop = iop+1
            indop(iop) = 1
            oparam(iop,l) = extn(l)
         end if
         if (jnopar(2).eq.1) then
            iop = iop+1
            indop(iop) = 2
            oparam(iop,l) = scan(l)
         end if
         if (jnopar(3).eq.1) then
            iop = iop+1
            indop(iop) = 3
            oparam(iop,l) = absn(l)
         end if
         if (jnopar(4).eq.1) then
            iop = iop+1
            indop(iop) = 4
            oparam(iop,l) = ssa(l)
         end if
         if (jnopar(5).eq.1) then
            iop = iop+1
            indop(iop) = 5
            oparam(iop,l) = asf(l)
         end if
         if (jnopar(9).eq.1) then
            iop = iop+1
            indop(iop) = 9
            oparam(iop,l) = exta(l)/pf18a(l)
         end if
         if (jnopar(11).eq.1) then
            iop = iop+1
            indop(iop) = 11
            oparam(iop,l) = scar(l)
         end if
         if (jnopar(12).eq.1) then
            iop = iop+1
            indop(iop) = 12
            oparam(iop,l) = absr(l)
         end if
         if (jnopar(13).eq.1) then
            iop = iop+1
            indop(iop) = 13
            oparam(iop,l) = omer(l)
         end if
         if (jnopar(14).eq.1) then
            iop = iop+1
            indop(iop) = 14
            oparam(iop,l) = extn(l)/ext05
            if (norm.eq.0) oparam(iop,l) = oparam(iop,l)*n(l)
         end if
         if (jnopar(18).eq.1) then    ! Breaking indices
            iop = iop+1
            indop(iop) = 18
            oparam(iop,l) = bre(1,1)
            iop = iop+1
            indop(iop) = 19
            oparam(iop,l) = bim(1,1)
         end if
   10 CONTINUE
C
C     OPTICAL DEPTH (only with absolute values)
      if (jnopar(6).eq.1.or.jnopar(7).eq.1.or.jnopar(8).eq.1) then
         if (norm.eq.1) then ! only for absolute values
C
C     Determination of HM, HFTA, HSTR, EXTFTA, EXTSTR from the         C
C     dat input in /layer/ for RAWOPT                                  C
         odepth = 0.
            do il = 1,mlay
               if (nltyp(il).eq.1) then           ! mixing layer
                  heff = parlay(il,1)*
     &                  (exp(-boundl(il)/parlay(il,1))-
     &                   exp(-boundu(il)/parlay(il,1)))
                  odepth = odepth+extn(1)*heff
C     print*,'OD(1)= ',odepth,' Heff= ',heff,' ext= ',extn(1)
               else if (nltyp(il).eq.2) then ! mineral transported
                  heff = (boundu(il)-boundl(il))
                  odepth = odepth+extmit(ilamb)*parlay(il,1)*heff
C     print*,'OD(2)= ',odepth,' Heff= ',heff,' ext= ',extmit(ilamb)
               else if (nltyp(il).eq.3) then               ! free troposphere
                  heff = parlay(il,1)*
     &                 (exp(-boundl(il)/parlay(il,1))-
     &                  exp(-boundu(il)/parlay(il,1)) )
                  odepth = odepth+extfta(ilamb)*heff
C     print*,'OD(3)= ',odepth,' Heff= ',heff,' ext= ',extfta(ilamb)
               else if (nltyp(il).eq.4) then               ! stratosphere
                  heff = (boundu(il)-boundl(il))
                  odepth = odepth+extstr(ilamb)*heff
C
C     print*,'OD(4)= ',odepth,' heff= ',heff,' ext= ',extstr(ilamb)
C
               else if (nltyp(il).eq.5) then               ! cloud
                  heff = (boundu(il)-boundl(il))
                  odepth = odepth + extn(1) * heff
C     print*,'OD(5)= ',odepth,' heff= ',heff,' ext=',extn(1)
         end if
      end do
      odeptha = odepth/alog(10.)
      turbr = 0.008569*alamb(ilamb)**(-4)*(1.+0.0113*
     &        alamb(ilamb)**(-2)+0.00013*
     &        alamb(ilamb)**(-4))
      turbf = (odepth+turbr)/turbr
      if (jnopar(6).eq.1) then
         iop = iop+1
         indop(iop) = 6
         oparam(iop,1) = odepth
      end if
      if (jnopar(7).eq.1) then
         iop = iop+1
         indop(iop) = 7
         oparam(iop,1) = odeptha
      end if
      if (jnopar(8).eq.1) then
         iop = iop+1
         indop(iop) = 8
         oparam(iop,1) = turbf
      end if
      else
         print*,' '
         print*, '          !! ATTENTION !!'
         print*,' '
         print*, ' optical depths may not be calculated together'
         print*, ' with normalized coefficients!'
         print*, ' Please change setting to absolute! '
         print*,' '
         print*, ' Press <ENTER> to return to Input Menu'
         read (*,'(a1)') dum
         stop
      end if
      end if
      if (jnopar(15).eq.1) then
         do ilp = 1,niwp
            if (alambp(ilp).eq.alamb(ilamb))
     &      call out2(ilp,ilamb,iop)
         end do
      else
C
         call out2(ilamb,ilamb,iop)
C
      end if
C
      return
      end
C
C
      subroutine intco
CCCCC ------------------------------------------------------------------C
C     Integration of calculated optical quantities with weighting       C
C     by the Solar constant in the solar spectral region and weighting  C
C     with Planck(300 K) for terrestrial radiation.                     C
C                                                                       C
C     19.04.94: Taken from SOLINT.FOR                                   C
C     25.01.95: Supplemented with the calculation of Angstrom           C
C               coefficients at 0.35/0.5 and 0.5/0.8 æm.                C
C     21.01.96: Calculation of horizontal visibility                    C
C                                                                       C
C     Version: 21.01.96                                         M. Hess C
CCCCC ------------------------------------------------------------------C
C     Variable definitions
      character*8 optnam,optun,opanam
      real welsk(87),exis(87),ssis(87),asis(87)
      real sgrenz(100)
      real sol(100),terr(100),wter(100),fluss(100)
      real oint1(100),oint2(100),gint1(100),gint2(100),aint1(100)
      real atint1(100),atint2(100),aint2(100),eint1(100)
      real angstr
C     COMMON Statements
      common /opar/   mopar,jnopar(20),nop,opanam(20),optnam(20),
     &                optun(20)
      common /wavel/  mlamb,alamb(61),niw,wlamb(61),
     &                il035,il05,il055,il08,alambp(61),niwp
      common /oppoi/  ext(1,6),sca(1,6),abs(1,6),sis(1,6),asy(1,6),
     &                bac(1,6),pha(1,6,167),ext05,bre(1,6),bim(1,6)
      common /out/    oparam(16,2),phaf(167,2),indop(16)
      common /sotin/  exi(61),ssi(61),asi(61)
C
C     Start intco
C     solar and terrestrial integrated quantities
      if(jnopar(15).eq.0.and.jnopar(16).eq.0.
     &   and.jnopar(17).eq.0) return
         open (5,file='wel.dat')
         read (5,*) nwelsk
         do iw=1,nwelsk
            read(5,*) welsk(iw)
         end do
         close(5)
         if (jnopar(15).eq.1) then
            call trapez(alamb,exi,1,mlamb,welsk,
     &                  exis,11,nwelsk-5,ier)
            call trapez(alamb,ssi,1,mlamb,welsk,
     &                  ssis,11,nwelsk-5,ier)
            call trapez(alamb,asi,1,mlamb,welsk,
     &                  asis,11,nwelsk-5,ier)
            do il = 1,nwelsk
               if (welsk(il).eq.0.3)    i03 = il
               if (welsk(il).eq.3.28)   i3  = il
               if (welsk(il).eq.7.992)  i8  = il
               if (welsk(il).eq.15.331) i15 = il
            end do
C        Input of the solar constant ifo wavelength
            open (5,file = 'solar.dat')
            read (5,'(a1)') dum
            read (5,*) (sgrenz(ig),ig = 1,38)
            read (5,'(a1)') dum
            read (5,*) (fluss(iv), iv = 1,37)
            do il=1,37
               sol(il) = fluss(il)/(sgrenz(il+1)-
     &                   sgrenz(il))/4.
            end do
            close (5)
C        Reading of terrestrial Radiation at T=300 K
            open (5,file = 'terr.dat')
            do il = 1,20
               read (5,*) wter(il),terr(il)
            end do
            close (5)
C        Integration
            do il = 1,37
               oint1(il) = ssis(il)*exis(il)*sol(il)
               oint2(il) = exis(il)*sol(il)
               gint1(il) = asis(il)*exis(il)*ssis(il)*sol(il)
               gint2(il) = exis(il)*ssis(il)*sol(il)
               aint1(il) = exis(il)*(1.-ssis(il))*sol(il)
               aint2(il) = sol(il)
               eint1(il) = exis(il)*sol(il)
            end do
            do il=1,20
               atint1(il) = exis(il+i8-1)*(1.-ssis(il+i8-1))*
     &                      terr(il)
               atint2(il) = terr(il)
            end do
C
            call gerin (welsk,oint1,o1erg,11,37,ier)
            call gerin (welsk,oint2,o2erg,11,37,ier)
            call gerin (welsk,gint1,g1erg,11,37,ier)
            call gerin (welsk,gint2,g2erg,11,37,ier)
            call gerin (welsk,aint1,a1erg,11,37,ier)
            call gerin (welsk,aint2,a2erg,11,37,ier)
            call gerin (welsk,eint1,eerg,11,37,ier)
            call gerin (wter,atint1,at1erg,1,20,ier)
            call gerin (wter,atint2,at2erg,1,20,ier)
C
            oint = o1erg/o2erg
            gint = g1erg/g2erg
            aint = a1erg/a2erg
            eint = eerg/a2erg
            atint = at1erg/at2erg
         end if
C     Angstrom Coefficients
         if (jnopar(16).eq.1) then
            al1 = (alog10(exi(il055))-alog10(exi(il035)))/
     &            (alog10(alamb(il035))-alog10(alamb(il055)))
            al2 = (alog10(exi(il08))-alog10(exi(il055)))/
     &            (alog10(alamb(il055))-alog10(alamb(il08)))
            be1 = exi(il035)*alamb(il035)**(al1)
            be2 = exi(il055)*alamb(il055)**(al2)
C     Angstrom coefficient according to AATSR ATBD.
C     Angstrom0.550,0.865 = log(AOD800/AOD550)/log(0.800/0.550)
            angstr = (alog10(exi(il08)/exi(il055))/
     &                alog10(0.800/0.550))
         end if
C     Horizontal visibility
         if (jnopar(17).eq.1) then
            visib = 3.0/(ext05+0.01159)
         end if
C     Write to output file
         write (10,107)
         if (jnopar(17).eq.1) then
            write (10,110) visib
         end if
         if (jnopar(15).eq.1) then
            write (10,120) eint,oint,gint,aint,atint
         end if
         if (jnopar(16).eq.1) then
            write (10,130) alamb(il035),alamb(il055),al1,be1,
     &                     alamb(il055),alamb(il08),al2,be2,
     &                     alamb(il055),alamb(il08),angstr
         end if
C
C     Format statements
  107 format(' ---------------------------------------------------',
     &       '-------------------------')
  110    format(' Visibility: ',f6.2,' [km]')
  120    format(' values weighted with solar spectrum',
     &          ' (0.3-3.3 micron):'/
     &          e10.3,' : extinction coefficient'/
     &          e10.3,' : single scattering albedo'/
     &          e10.3,' : asymmetry parameter'/
     &          e10.3,' : absorption coefficient'/
     &          ' values weighted with terrestrial spectrum at 300K',
     &          ' (8-15 micron):'/
     &          e10.3,' : absorption coefficient')
  130    format(' Angstrom coefficient calculated at: ',
     &          f6.4,' [um] and ',f6.4,' [um]'/
     &          ' ',e10.4,' : alpha'/
     &          ' ',e10.4,' : beta'/
     &          ' Angstrom coefficient calculated at: ',
     &          f6.4,' [um] and ',f6.4,' [um]'/
     &          ' ',e10.4,' : alpha'/
     &          ' ',e10.4,' : beta'/
     &          ' Angstrom coefficient (Globaerosol ATBD) for: ',
     &          f6.4,' [um] and ',f6.4,' [um] is: '/
     &          ' ',e10.4)
C
      return
      end
C
C
      subroutine LANG2(STEXT,mtext,LTEXT)
CCCCC -----------------------------------------------------------------C
C     Length of text is calculated (The end is reached when two        C
C     sequential blanks are met)                                       C
C                                                                      C
C     19.06.94: The maximal length is transferred as input             C
C     03.03.95: Error corrected, when ltext=mtext-1                    C
C     16.04.95: Totally new version                                    C
C                                                                      C
C     Version: 16.04.95                                      M. Hess   C
CCCCC -----------------------------------------------------------------C
C     Variable definition
      character*(*) STEXT
C     Start Subroutine lang2
C
      if (stext(mtext:mtext).ne.' ') then
         ltext = mtext
      else if (stext(mtext-1:mtext-1).eq.' ') then
         do J = mtext,2,-1
            if(STEXT(J:J).EQ.' '.AND.STEXT(J-1:J-1).EQ.' ') then
               LTEXT = J-2
            end if
         end do
      else
         LTEXT = mtext-1
      end if
C
      return
      end
C
C
      subroutine sort1(werte,nx,wsort,index)
CCCCC -----------------------------------------------------------------C
C     Sorting of a one dimensional field by using MINMAX1              C
C                                                                      C
C     wsort : According to the magnitude of the field to sort          C
C     index : Adjoining field of the original indices                  C
C     nx can be maximally 1000 !                                       C
C                                                                      C
C     Version: 17.09.93                                        M. Hess C
CCCCC -----------------------------------------------------------------C
C     Variable definition
      logical ende
      integer index(nx)
      real werte(nx),wsort(nx),whilf(1000)
C
C     Start Sort1
C
      do ix = 1,nx
         whilf(ix) = werte(ix)
      end do
      do ix = 1,nx-1
C
         call minmax1(whilf,nx,wmin,wmax)
C
      if (ix.eq.1) then  ! Determination of maximal value
         wsort(nx) = wmax
         ende = .false.
         i = 0
         do while (.not.ende)
            i = i+1
            if (whilf(i).eq.wmax) then
               index(nx) = i
               ende = .true.
            end if
         end do
      end if
      wsort(ix) = wmin ! Sort the rest of the field
      ende = .false.
      i = 0
      do while (.not.ende)
      i = i+1
         if (whilf(i).eq. wmin) then
            whilf(i) = wmax
            index(ix) = i
            ende = .true.
         end if
      end do
      end do
C
      return
      end
C
C
      subroutine minmax1(werte,nx,wmin,wmax)
CCCCC -----------------------------------------------------------------C
C     Determination of the maximal and minimal value in a              C
C     1-dimensional REAL field                                         C
C                                                                      C
C     Version: 14.02.92                                        M. Hess C
CCCCC -----------------------------------------------------------------C
C     Variable definition
      real werte(nx)
C
C     Start minmax1
      wmin = werte(1)
      wmax = werte(1)
      do ix = 1,nx
         if (werte(ix).ge.wmax) wmax = werte(ix)
         if (werte(ix).le.wmin) wmin = werte(ix)
      end do
      return
      end
C
C
      subroutine TRAPEZ(X,Y,IANFA,IENDA,U,V,IANFN,IENDN,IERROR)
CCCCC -----------------------------------------------------------------C
C     Linear interpolation of an ordinate field V(I), for a            C
C     abscis field U(I) for I=IANFN,IENDN from an outputfield          C
C     (Reference field) Y(I),X(I), FUER I=IANFA,IENDA                  C
C     The U and X fields must be ordered                               C
C                                                                      C
C     IERROR=0 In the nomeral case                                     C
C     IERROR=2 when the outputfield is smaller than the field to be    C
C              interpolated                                            C
C     Version: 15.07.1975                                              C
CCCCC -----------------------------------------------------------------C
C     Variable definition
      DIMENSION X(IENDA),Y(IENDA),U(IENDN),V(IENDN)
C
C     Start TRAPEZ
      IERROR = 0
      IDREHX = 0
      IF (X(IANFA).LT.X(IENDA)) GOTO 2 !The X field is ordered in
         IORDX = (IENDA-IANFA+1)/2     !increasing values.
         DO 14 I = IANFA,IORDX         !The Y field accordingly
            XUMORD = X(I)
            X(I) = X(IENDA+1-I)
            X(IENDA+1-I) = XUMORD
            YUMORD = Y(I)
            Y(I) = Y(IENDA+1-I)
   14       Y(IENDA+1-I) = YUMORD
            IDREHX = 1
    2       IDREHU = 0
            IF (U(IANFN).LT.U(IENDN)) GOTO 12 !The U field is ordered in
               IORDU = (IENDN-IANFN+1)/2      !increasing values.
                  DO 16 I = IANFN,IORDU
                     UUMORD = U(I)
                     U(I) = U(IENDN+1-I)
   16                U(IENDN+1-I) = UUMORD
                     IDREHU = 1
   12                IEXPO = 0
                     IANFNN = IANFN
    3                IF (U(IANFNN).GE.X(IANFA)) GOTO 13 !Prohibit extrapolation
                        IEXPO = 1                       !with X(IANFA).
                        IANFNN = IANFNN+1
                        GOTO 3
   13                   IENDNN = IENDN
    4                   IF (U(IENDNN).LE.X(IENDA)) GOTO 5 !prohibit extrapolation
                          IEXPO = 1                       !with X(IENDA)
                          IENDNN = IENDNN-1
                          GOTO 4
    5                       IF(IEXPO.EQ.1) WRITE(6,98)
     &                      X(IANFA),X(IENDA),U(IANFN),
     &                      U(IENDN),U(IANFNN),U(IENDNN)
                            IF (IEXPO.EQ.1) IERROR = 2
                               IANF = IANFA
                               DO 20 J = IANFNN,IENDNN
                                  DO 10 I = IANF,IENDA
                                  IF (X(I)-U(J)) 10,7,8
    7                                V(J) = Y(I)
                                     GOTO 22
    8                                V(J) = Y(I-1)+(Y(I)-Y(I-1))/
     &                                     (X(I)-X(I-1))*
     &                                     (U(J)-X(I-1))
                                     GOTO 22
   10                             CONTINUE
                                  GOTO 20
   22                             IANF = I
   20                          CONTINUE
                               IF (IDREHX.EQ.0) GOTO 21 !The X and Y fields are
                                  DO 18 I = IANFA,IORDX !put back in their
                                     XUMORD = X(I)      !starting positions
                                     X(I) = X(IENDA+1-I)
                                     X(IENDA+1-I) = XUMORD
                                     YUMORD = Y(I)
                                     Y(I) = Y(IENDA+1-I)
   18                                Y(IENDA+1-I) = YUMORD
   21                                IF (IDREHU.EQ.0) RETURN
                                        DO 19 I = IANFN,IORDU
                                           UUMORD = U(I)
                                           U(I) = U(IENDN+1-I)
                                           U(IENDN+1-I) = UUMORD
                                           VUMORD = V(I)
                                           V(I) = V(IENDN+1-I)
   19                                      V(IENDN+1-I) = VUMORD
C
C     Format Statements
   98 format(1H0,5X,'The reference field goes from X(IANFA) =
     &       ',1PE10.3,1X,'to X(IENDA) =',E10.3/1H ,
     &       'Interpolation should be from U(IANFN) =',E10.3,' to
     &       U(IENDN) =',E10.3/1H ,5X,'values except',E10.3,' and',
     &       E10.3,' which are calculated afterwards.'/1H )
C
      return
      end
C
C
      SUBROUTINE GERIN (H,F,ERG,IA,IE,IERROR)
CCCCC -----------------------------------------------------------------C
C     Trapeze integration for a non equidistant table of values.       C
C     The number of abscissa points can be equal or unequal            C
C     H=Abscissa, F=ordinate, ERG=Integral                             C
C     Integration is from H(IA) till H(IE)                             C
C     When only a STUETZWERT is available IA=IE), the the integral     C
C     receives the value of the STUETZWERTES.                          C
C     IERROR =0 in the normal case                                     C
C     IERROR=1, when IA > IE                                           C
C     IERROR=2, when IA = IE                                           C
C     IERROR=3, when the abscissa differences become too small         C
C     Version: 20.02.1974                                              C
CCCCC -----------------------------------------------------------------C
C     Variable definitions
      DIMENSION H(IE),F(IE)
C
C     Start Subroutine GERIN
      IERROR = 0
      ERG = 0.0
      IF (IE-IA.GE.0) GOTO 6
         WRITE(6,5) IA,IE
         IERROR = 1
      RETURN
    6 IF (IE-IA.NE.0) GOTO 3
         WRITE (6,15) IA
         ERG = F(IA)
         IERROR = 2
      RETURN
    3 IAP = IA+1
      DO 1 I = IAP,IE
         DIFH = H(I)-H(I-1)
         IF (ABS(DIFH/H(I)).GT.1.E-4) GOTO 4 !This warning indicates
         WRITE (6,25) I                      !an error of maximally 1%
         IERROR = 3
    4    ERG = ERG+(F(I)+F(I-1))*DIFH
    1 CONTINUE
      ERG = ERG/2.
C
C     Format statements
    5 FORMAT('0 In the subroutine GERIN IA=',I3,'is larger than IE=',I3)
   15 FORMAT('0 In the subroutine GERIN IA= IE=',I3,' and ERG=F(IA)')
   25 FORMAT('0 In the subroutine GERIN DIFH becomes smaller than 1.E-4
     &        at I=',I4)
C
      RETURN
      END
CCCCCC----------------------------------------------------------------------
CCCCCC End of Subroutines
CCCCCC End of the OPAC Program
CCCCCC----------------------------------------------------------------------

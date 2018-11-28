      parameter (nper=13,iper=2**nper,nmax=2**14)
      real alpha(nmax),z(nmax),pi,t(nmax)
	real alpha1(iper),z1(iper)
	real pout(nmax),zl(nmax),zt(nmax)
	complex*16 wrk1(iper),wrk2(iper)

         character*80 femgeo
         Character*80 xstring


	
      rho=1000
        pi=3.141592653589793

        lam=1d0
c
        b=0.2d0 !0.05d0
        s=0.05d0
        e=0.05d0

!	open(10,file='t.in',status='old')
!	read(10,*)
!	read(10,*) ts,te,dt
!        read(*,*) xstring
        write(*,*) "Input: tstart,tend,dt"
!        read(*,*) ts,te,dt
        ts=0.0
        te=25.0
        dt=0.5
        write(*,*) ts,te,dt
!        dt=8e-3       
!	close(10)

!        write(*,*) xstring
!        PAUSE
!         femgeo='/home/peng/Desktop/ns-ener4/fort/'//xstring//'out'
!         open(28,file=femgeo,status='unknown')

         write(28,*) ts,te

	open(1,file='fort.1',status='old')
      chord=2d0*(1+s+e)+1d0/(1d0+s)+1d0/(1d0+2d0*e+s)
      u=1
	ntstart=ts/dt
	ntend=te/dt
      do 10 it=1,ntstart
10		read(1,*)
      do 20 it=1,ntend-ntstart+1
20		read(1,*) t(it),alpha(it),z(it)
	close(1)
c	dt=t(2)-t(1)

	ttotal=te-ts

	itest=ttotal/dt

	dttest=ttotal/iper


	fq=1/ttotal
	do 30 it=1,iper
		t1=t(1)+(it-1)*dttest
		do 40 it1=1,ntend-ntstart
			if(t1.ge.t(it1).and.t1.le.t(it1+1)) then
				fac=(t1-t(it1))/dt
				alpha1(it)=(1-fac)*alpha(it1)+fac*alpha(it1+1)
				z1(it)=(1-fac)*z(it1)+fac*z(it1+1)
				goto 30
			endif
40		continue
30	continue
	do 50 it=1,iper
		t1=t(1)+(it-1)*dttest
		wrk1(it)=alpha1(it)
		wrk2(it)=z1(it)	
50		write(202,*) t1,alpha1(it),z1(it)

	call fast(wrk1,iper,nper,-1.)
	call fast(wrk2,iper,nper,-1.)
	do 60 i=2,iper/16
                write(28,*) (i-1)*fq,2*abs(wrk1(i)),2*abs(wrk2(i))
60		write(203,*) (i-1)*fq,2*abs(wrk1(i)),2*abs(wrk2(i))


	nn=100
      do 70 it=1,ntstart-nn
		read(1,*)
70		read(3,*)
      do 80 it=1,ntend-ntstart+1+2*nn
		read(1,*) t(it),dum,z(it)
80		read(3,*) dum,zl(it),zt(it)

      zlmax=zl(1+nn)
      zlmin=zl(1+nn)
      ztmax=zt(1+nn)
      ztmin=zt(1+nn)
      pm=0

	do 90 it=1+nn,ntend-ntstart+1+nn
		vel=(z(it+nn)-z(it-nn))/(t(it+nn)-t(it-nn))
		write(22,*) t(it),vel,pi*vel*vel
		pout(it-nn)=pi*vel*vel
          zlmax=max(zlmax,zl(it))
          zlmin=min(zlmin,zl(it))
          ztmax=max(ztmax,zt(it))
          ztmin=min(ztmin,zt(it))
90        pm=pm+pout(it-nn)

      hm=0.5*max(zlmax-zlmin,ztmax-ztmin)
      pm=pm/(ntend-ntstart)
      write(28,*) 'pout=',pm
      write(28,*) 'swept area = ',2*hm
      write(28,*) 'eff=',pm/(hm)

      write(*,*) 'power out=',pm
      write(*,*) 'swept area = ',2*hm
      write(*,*) 'energy extract efficiency=',pm/(hm)

      close(28)
      end


********************************
*         next: FFT            *
********************************
      SUBROUTINE FAST(X,NDIM,NPOW,ONE)
C
C IMPLEMENT THE FAST FOURIER TRANSFORM
C
      IMPLICIT REAL(A-H,O-Z)
      COMPLEX  T,U,W,X(NDIM),ci
      DATA PI,ci/3.1415926535897932384,(0.,1.)/
      N1=2**NPOW
      N2=N1/2
      N3=N1-1
      J= 1
      DO 300 I=1,N3
      IF(I.GE.J) GO TO 100
      T=   X(J)
      X(J)=X(I)
      X(I)=T
100   K=N2
200   IF(K.GE.J) GO TO 300
      J=J-K
      K=K/2
      GO TO 200
300   J=J+K
      DO 500 L=1,NPOW
      LE1=2**L
      LE2=LE1/2
      ANG=PI/LE2
      U=1.
      W=COS(ANG)+ci*ONE*SIN(ANG)
      DO 500 J=1,LE2
      DO 400 I=J,N1,LE1
      IP=I+LE2
      T=    X(IP)*U
      X(IP)=X(I)-T
400   X(I)= X(I)+T
500   U=U*W
      IF(ONE.EQ.1.)RETURN
      SCL= 1./real(N1)
      DO 600 I=1,N1
600   X(I)=X(I)*SCL
      RETURN
      END






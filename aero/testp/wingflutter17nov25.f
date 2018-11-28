	
c       Finite Difference Method Simulation of the Wing Flutter by Zhangli Peng (University of Notre Dame)
C       and Qiang Zhu and Rodrigo Soto, Dec 1, 2016

        implicit real*8(A-H,O-Z)
	include 'parameter.h'
	real*8 w(Nzmax,Nsmax),vr(Nzmax,Nsmax),z(Nzmax)
	real*8 wr(Nsmax),ws(Nsmax)
	real*8 wx(Nsmax),wy(Nsmax)
	real*8 pupx(Nsmax),pupy(Nsmax),pvpx(Nsmax),pvpy(Nsmax)
	real*8 u(Nzmax,Nsmax),v(Nzmax,Nsmax),pupr(Nsmax),pvpr(Nsmax)
	real*8 pups(Nsmax),pvps(Nsmax)
	real*8 px(Nsmax),Py(Nsmax),p(Nsmax)
	real*8 xi(Nzmax,Nsmax),eta(Nzmax,Nsmax)
	real*8 X(Nzmax,Nsmax),Y(Nzmax,Nsmax)
	real*8 sita(Nsmax),wrk(Nsmax)
	real*8 pxpx(Nzmax,Nsmax),pxpe(Nzmax,Nsmax)
	real*8 pypx(Nzmax,Nsmax),pype(Nzmax,Nsmax)
	real*8 r(Nzmax),Jcb(Nzmax,Nsmax)
	real*8 la1(Nzmax),la2(Nzmax),la3(Nzmax),ra(Nzmax)
	real*8 lb1(Nsmax),lb2(Nsmax),lb3(Nsmax),rb(Nsmax)
	real*8 pi,dz,b,lam,s,e,U0,chord,nu,ro
	real*8 vs(Nzmax,Nsmax),omega,amass
	real*8 vr0(2,Nsmax),vs0(2,Nsmax)
	real*8 w0(Nzmax,Nsmax),w1(Nzmax,Nsmax)
	real*8 psi(Nzmax,Nsmax),psis(Nzmax,Nsmax),psis2(Nzmax,Nsmax)
        real*8 pxpr(Nzmax,Nsmax),pxps(Nzmax,Nsmax)
	real*8 pypr(Nzmax,Nsmax),pyps(Nzmax,Nsmax)
	real*8 prpx(Nzmax,Nsmax),prpy(Nzmax,Nsmax)
	real*8 pspx(Nzmax,Nsmax),pspy(Nzmax,Nsmax)
	real*8 zn(Nsmax),ps(Nsmax),w0save(Nzmax,Nsmax)
	real*8 fx,fy,mz,psisave(Nzmax,Nsmax),wsave(Nzmax,Nsmax)
	complex*16 boda1(Nsmax),boda2(Nsmax)
	complex*16 bodb1(Nsmax),bodb2(Nsmax)
	complex*16 fn(Nzmax,Nsmax),ci
	complex*16 wrkc(Nsmax),psin(Nzmax,Nsmax),psins(Nzmax,Nsmax)
	complex*16 rc(Nzmax),wrk1(Nzmax)
	data pi,ci/3.141592653589793d0,(0d0,1d0)/

c 	Due to the normalization, the values of U0 and lam are fixed (since they are used as 
c     	speed and length scales)
c	ro=1d0			!Density
	nu=1d0			!Kinetic viscosity
	U0=1d0			!Inlet velocity
	lam=0.2486!1d0		!Controls the length of the airfoil

c	---------------------------Airfoil----------------------------------------------------
c			     ----	
c			  --       --------
c 	               -       SC    CG	    ----
c			  --       --------
c			     ----
c
c			|-xp*a-|--d--|
c		       	|---------a------------|
c
c	SC: Shear Center, i.e. pitching axis motion
c	CG:CG of the foil
c	a:length of the airfoil
c	If you want to understand better lam, b, d, e, s please use tha matlab code "Joukowski.m"
c	---------------------------------------------------------------------------------------

		


	s=0.0124! 100.0d0			!Controls roundness and thickness of the foil
	e=0.0124! 0.0d0			!Controls thickness of the foil
	
	r0=lam+e+s		!radius for mapping the airfoil

	iout=0
	iout1=0
	chord=2d0*(lam+s+e)+lam*lam/(lam+s)+lam*lam/(lam+2d0*e+s)	!Length of the foil
        xm=chord*pi						!Damping in heaving motion
	amass=pi*chord*chord/4.*200				!Added mass
        phi=90d0						!Phase between pitching and heaving
        phi=phi*pi/180d0					!radians
	apmass=pi*chord*chord/4.*1				!Added inertia
c	xkheave=0.2d0						!Stiffness of heaving motion


	open(1,file='input.dat',status='old')			!To more information read "input.dat"
		read(1,*)
		read(1,*) xp					
		read(1,*)
		read(1,*) xkheave,xkpitch		!Stiffness of pitching and heaving motion 
		read(1,*) 
		read(1,*) ttotal		!Total iteration time
		read(1,*) 
		read(1,*) dtdt			!Time step
      		read(1,*)
       		read(1,*) timestart		!Time of start to recording
       		read(1,*)
       		read(1,*) timeend		!Time of end to recording
       		read(1,*)
       		read(1,*) timeout		!Time step for recording
	close(1)


	xlscale=chord				!To normalize length dimensions
	xtscale=chord/U0			!To normalize time dimension
	xkpitch=xkpitch*xlscale**5/xtscale**2	!Normalize pitch stiffness
	xkheave=xkheave*xlscale**5/xtscale**2	!Normalize heave stiffness
	ttotal=ttotal*xtscale			!Normalize time
	dt=dtdt*xtscale				!Normalize time step
	xmpitch=0.0d0				!Damping in pitching motion
	amp_pitch=1e-3				!Disturbance amplitude

	d=(lam+2d0*e+s)+lam*lam/(lam+2d0*e+s)-chord*xp	!See airfoil description above

	tstart=0.1d0*ttotal
	omega=2*pi/tstart
	itotal=ttotal/dt			!Quantity of time steps 
	b=0.0d0 
	z0=dlog(r0+b) 

	zm=2.0d0				!Limit of the radial direction
	dz=(zm-z0)/(Nzmax-1d0)			!Step of radial direction  z=f(r)
	ds=2d0*pi/Nsmax				!Step of thetha direction
	Rec=1000				!Reynolds number, characteristic dimension: length of the airfoil
	Re=Rec/chord
c	xmu=1/Re
	do 10 i=1,Nzmax					!initializing all vectors
		z(i)=z0+(i-1d0)/(Nzmax-1d0)*(zm-z0)	! i is the counter for r direction
		r(i)=exp(z(i))-b				
		do 12 j=1,Nsmax
			sita(j)=2d0*pi*(j-1d0)/Nsmax

			xi(i,j)=r(i)*cos(sita(j))

			eta(i,j)=r(i)*sin(sita(j))

			X(i,j)=xi(i,j)+lam*lam*(xi(i,j)-e)/((xi(i,j)-e)**2+
     *			eta(i,j)**2)+d-e

			Y(i,j)=eta(i,j)+lam*lam*(-eta(i,j))/((xi(i,j)-e)**2+
     *			eta(i,j)**2)

			pxpx(i,j)=1d0+lam*lam*(eta(i,j)**2-(xi(i,j)-e)**2)/
     *			((xi(i,j)-e)**2+eta(i,j)**2)**2

			pxpe(i,j)=-2d0*lam*lam*eta(i,j)*(xi(i,j)-e)/
     *			((xi(i,j)-e)**2+eta(i,j)**2)**2

			pypx(i,j)=2d0*lam*lam*eta(i,j)*(xi(i,j)-e)/
     *			((xi(i,j)-e)**2+eta(i,j)**2)**2

			pype(i,j)=1d0+lam*lam*(eta(i,j)**2-(xi(i,j)-e)**2)/
     *			((xi(i,j)-e)**2+eta(i,j)**2)**2

			vr(i,j)=0d0
			vs(i,j)=0d0
			w(i,j)=0d0
			w0(i,j)=0d0
			w1(i,j)=0d0
			psi(i,j)=0d0		
			psis(i,j)=0d0

			Jcb(i,j)=pxpx(i,j)*pype(i,j)-pxpe(i,j)*pypx(i,j)

			prpx(i,j)=(pype(i,j)*cos(sita(j))-pypx(i,j)*sin(sita(j)))
     *			/Jcb(i,j)

			prpy(i,j)=(-pxpe(i,j)*cos(sita(j))+pxpx(i,j)*sin(sita(j)))
     *			/Jcb(i,j)

			pspx(i,j)=(-pype(i,j)*sin(sita(j))-pypx(i,j)*cos(sita(j)))
     *			/(r(i)*Jcb(i,j))

			pspy(i,j)=(pxpe(i,j)*sin(sita(j))+pxpx(i,j)*cos(sita(j)))
     *			/(r(i)*Jcb(i,j))

			pxpr(i,j)=pxpx(i,j)*cos(sita(j))+pxpe(i,j)*sin(sita(j))

			pxps(i,j)=-r(i)*pxpx(i,j)*sin(sita(j))+r(i)*pxpe(i,j)*
     *			cos(sita(j))

			pypr(i,j)=pypx(i,j)*cos(sita(j))+pype(i,j)*sin(sita(j))

			pyps(i,j)=-r(i)*pypx(i,j)*sin(sita(j))+r(i)*pype(i,j)*
     *			cos(sita(j))
12		continue
10	continue
	dr=r(2)-r(1)
c2002	continue
	t=0
	it=0
	heave=0
	dadt=0
	dadt2=0
	alpha=0
	alpha0=0
	alpham1=0.
	dalphadt=0
	dalphadt2=0
	alpham2=0


	heave0=0.
	dadt0=0.
	heavem1=0.
	heavem2=0.
	dadtm1=0.
	dadt20=0.
c start
c	HERE START ITERATIONS
1	t=t+dt
	it=it+1
	if(t.le.tstart) then
		fac=1.
	else
		fac=1-tanh((t-tstart)/(0.2*tstart))
	endif
	pitchm=amp_pitch*sin(omega*t+phi)*fac

	do 500 i=1,Nzmax
		do 500 j=1,Nsmax
500			w0save(i,j)=w0(i,j)
c500			psisave(i,j)=psi(i,j)	

	heaves=heave0
	alphas=alpha0
	dadt2s=dadt20
	dalphadt2s=dalphadt20

	iter=0
501	iter=iter+1
	do 505 i=1,Nzmax
		do 505 j=1,Nsmax
505			w0(i,j)=w0save(i,j)
c505			psi(i,j)=psisave(i,j)	
	alpha=(pitchm+mz+apmass*dalphadt2+
     *	xmpitch*(4*alpha0-alpham1)/(2*dt)+
     *	apmass*(5*alpha0-
     *	4*alpham1+alpham2)/(dt*dt))/(2*apmass/(dt*dt)+
     *	3*xmpitch/(2*dt)+xkpitch)
	
	dalphadt=(3*alpha-4*alpha0+alpham1)/(2*dt)
	dalphadt2=(2*alpha-5*alpha0+4*alpham1-alpham2)/(dt*dt)
	
	heave=(fl+amass*dadt2+4*xm*heave0/(2*dt)-xm*heavem1/(2*dt)+
     *	amass*(5*heave0-
     *	4*heavem1+heavem2)/(dt*dt))/
     *	(2*amass/(dt*dt)+3*xm/(2*dt)+xkheave)

	dadt=(3*heave-4*heave0+heavem1)/(2*dt)
	dadt2=(2*heave-5*heave0+4*heavem1-heavem2)/(dt*dt)

	if(iter.ge.2) then
		err=abs(heaves-heave)
		if(err.le.1e-6) goto 502		
	endif
	if(iter.ge.100) stop 'too many iterations'
	heaves=heave
	dadt2s=dadt2
	alphas=alpha
	dalphadt2s=dalphadt2
		
	call cont(w0,z,r,Jcb,la1,la2,la3,rc,wrk1,wrkc,boda1,boda2,
     *	bodb1,bodb2,psin,psins,psi,psis,psis2,fn,pxpx,pxpe,pypx,pype,
     *	sita,X,Y,U0,dadt,dalphadt,alpha,dz,ds,b)	

	call vrvs(t,tstart,amp,omega,heave,dadt,dadt2,alpm,phi,
     *	alpha,dalphadt,dalphadt2,Jcb,r,psis,psi,sita,X,Y,z,pxpx,pxpe,
     *	pypx,pype,dz,vr,vs)

c	VORTICITY EQUATION 
c	STEP 1
	do 15 j=1,Nsmax		!here is checking all grid in thetha direction
	if(j.eq.1) then
		jp1=j+1
		jp2=j+2
		jm1=Nsmax
		jm2=Nsmax-1
	elseif(j.eq.Nsmax) then
		jp1=1
		jp2=2	
		jm1=j-1
		jm2=j-2
	elseif(j.eq.2) then
		jp1=j+1
		jp2=j+2
		jm1=j-1
		jm2=Nsmax
	elseif(j.eq.Nsmax-1) then
		jp1=Nsmax
		jp2=1	
		jm1=j-1
		jm2=j-2
	else
		jp1=j+1
		jm1=j-1
		jp2=j+2
		jm2=j-2
	endif

	do 20 i=2,Nzmax-1			!review equation (6) Laura G. Paolo B.
		if(vs(i,j).eq.0d0) then
		ra(i)=w0(i,j)+dt/2d0*(-vs(i,j)/(dsqrt(Jcb(i,j))*r(i))*
     *		(w0(i,jp1)-w0(i,jm1))/(2d0*ds)+1d0/(Re*Jcb(i,j)*r(i)**2)*
     *		(w0(i,jp1)-2d0*w0(i,j)+w0(i,jm1))/(ds*ds))
    		elseif(vs(i,j).lt.0d0) then
		ra(i)=w0(i,j)+dt/2d0*(-vs(i,j)/(dsqrt(Jcb(i,j))*r(i))*
     *		(-w0(i,jp2)+4*w0(i,jp1)-3*w0(i,j))/(2*ds)+
     *     1d0/(Re*Jcb(i,j)*r(i)**2)*
     *		(w0(i,jp1)-2d0*w0(i,j)+w0(i,jm1))/(ds*ds))
		else
		ra(i)=w0(i,j)+dt/2d0*(-vs(i,j)/(dsqrt(Jcb(i,j))*r(i))*
     *		(3*w0(i,j)-4*w0(i,jm1)+w0(i,jm2))/(2*ds)+
     *     1d0/(Re*Jcb(i,j)*r(i)**2)*
     *		(w0(i,jp1)-2d0*w0(i,j)+w0(i,jm1))/(ds*ds))
		endif    								
		la2(i)=1d0+dt/(Re*Jcb(i,j))*dexp(-2d0*z(i))/(dz*dz)
		la3(i)=-dt/2d0*(-1d0/dsqrt(Jcb(i,j))*vr(i,j)*dexp(-z(i))
     *		/(2*dz)+1d0/(Re*Jcb(i,j))*dexp(-2d0*z(i))*(1d0/(dz*dz)-
     *		1d0/(2d0*dz))+1d0/(Re*Jcb(i,j)*r(i))*dexp(-z(i))/
     *		(2d0*dz))					     	
		la1(i)=-dt/2d0*(-1d0/dsqrt(Jcb(i,j))*vr(i,j)*dexp(-z(i))*
     *		(-1d0)/(2d0*dz)+1d0/(Re*Jcb(i,j))*dexp(-2d0*z(i))*(
     *		1d0/(dz*dz)+1d0/(2d0*dz))+1d0/(Re*Jcb(i,j)*r(i))*
     *		dexp(-z(i))*(-1d0)/(2d0*dz))	
		 			
20	continue
c	BOUNDARY CONDITIONS AT THE FOIL SURFACE
	la2(1)=1d0
	la3(1)=0d0
	la1(1)=0d0

	fr=-1d0*(dadt*sin(alpha)-dalphadt*Y(1,j))*
     *		(-pxpx(1,j)*sin(sita(j))+pxpe(1,j)*cos(sita(j)))-
     *		1d0*(dadt*cos(alpha)+dalphadt*X(1,j))*
     *		(-pypx(1,j)*sin(sita(j))+pype(1,j)*cos(sita(j)))

     	ra(1)=(-2*(psi(2,j)-psi(1,j)-dr*fr)/(dr*dr)-1/(r(1))*fr-
     *  1/(r(1)*r(1))*psis2(1,j))/Jcb(1,j)


c	FAR FIELD 	

	if(vr(Nzmax,j).le.0) then	     				
	la2(Nzmax)=1d0
	la3(Nzmax)=0d0
	la1(Nzmax)=0d0
	ra(Nzmax)=0d0
	else
	la2(Nzmax)=0d0
	la3(Nzmax)=1d0
	la1(Nzmax)=-1d0
	ra(Nzmax)=0d0
	endif	
	     				
	call tridag(la1,la2,la3,ra,w1(1,j))

15	continue
	call cont(w1,z,r,Jcb,la1,la2,la3,rc,wrk1,wrkc,boda1,boda2,
     *	bodb1,bodb2,psin,psins,psi,psis,psis2,fn,pxpx,pxpe,pypx,pype,
     *	sita,X,Y,U0,dadt,dalphadt,alpha,dz,ds,b)	

	call vrvs(t,tstart,amp,omega,heave,dadt,dadt2,alpm,phi,
     *	alpha,dalphadt,dalphadt2,Jcb,r,psis,psi,sita,X,Y,z,pxpx,pxpe,
     *	pypx,pype,dz,vr,vs)


c	STEP 2
	do 55 i=2,Nzmax-1
	do 60 j=1,Nsmax
		lb2(j)=1d0+dt/(Re*Jcb(i,j)*r(i)**2)/(ds*ds)
60	continue
	e1=-dt/2d0*(vs(i,1)/(dsqrt(Jcb(i,1))*r(i))/(2d0*ds)+1d0/(Re*
     *	Jcb(i,1)*r(i)**2)/(ds*ds))

	e2=-dt/2d0*(-vs(i,Nsmax)/(dsqrt(Jcb(i,Nsmax))*r(i))/(2d0*ds)+
     *	1d0/(Re*Jcb(i,Nsmax)*r(i)**2)/(ds*ds))

	do 70 j=2,Nsmax
		lb1(j)=-dt/2d0*(vs(i,j)/(dsqrt(Jcb(i,j))*r(i))/(2d0*ds)+
     *		1d0/(Re*Jcb(i,j)*r(i)**2)/(ds*ds))

70	continue
	lb1(1)=0d0	

	do 80 j=1,Nsmax-1
		lb3(j)=-dt/2d0*(-vs(i,j)/(dsqrt(Jcb(i,j))*r(i))/(2d0*ds)+
     *		1d0/(Re*Jcb(i,j)*r(i)**2)/(ds*ds))

80	continue
	lb3(Nsmax)=0d0

	do 90 j=1,Nsmax
		rb(j)=w1(i,j)+dt/2d0*(-1d0/dsqrt(Jcb(i,j))*vr(i,j)*
     *		dexp(-z(i))*(w1(i+1,j)-w1(i-1,j))/(2d0*dz)+
     *		1d0/(Re*Jcb(i,j))*dexp(-2d0*z(i))*((w1(i+1,j)-2d0*w1(i,j)
     *		+w1(i-1,j))/(dz*dz)-(w1(i+1,j)-w1(i-1,j))/(2d0*dz))+
     *		1d0/(Re*Jcb(i,j)*r(i))*dexp(-z(i))*(w1(i+1,j)-w1(i-1,j))/
     *		(2d0*dz))

						
90	continue
	
        lb1(1)=e1
        lb3(Nsmax)=e2
      call thomas_pr(Nsmax,lb2,lb3,lb1,rb,wrk)

	do 100 j=1,Nsmax
100		w(i,j)=wrk(j)
55	continue     

	do 56 j=1,Nsmax
	fr=-1d0*(dadt*sin(alpha)-dalphadt*Y(1,j))*
     *		(-pxpx(1,j)*sin(sita(j))+pxpe(1,j)*cos(sita(j)))-
     *		1d0*(dadt*cos(alpha)+dalphadt*X(1,j))*
     *		(-pypx(1,j)*sin(sita(j))+pype(1,j)*cos(sita(j)))

      w(1,j)=(-2*(psi(2,j)-psi(1,j)-dr*fr)/(dr*dr)-1/(r(1))*fr-
     *  1/(r(1)*r(1))*psis2(1,j))/Jcb(1,j)

	w(Nzmax,j)=w1(Nzmax,j)
56	continue


c CALCULUS OF HYDRODYNAMIC FORCE AND MOMENT

	do 400 j=1,Nsmax
		wr(j)=(w(2,j)-w(1,j))/dr
		zn(j)=wr(j)/sqrt(Jcb(1,j))
400	continue
	do 410 j=2,Nsmax-1
410		ws(j)=(w(1,j+1)-w(1,j-1))/(2d0*ds)
		ws(1)=(w(1,2)-w(1,Nsmax))/(2d0*ds)
		ws(Nsmax)=(w(1,1)-w(1,Nsmax-1))/(2d0*ds)

	do 420 j=1,Nsmax
		wx(j)=prpx(1,j)*wr(j)+pspx(1,j)*ws(j)
		wy(j)=prpy(1,j)*wr(j)+pspy(1,j)*ws(j)		
		px(j)=X(1,j)*dalphadt**2+Y(1,j)*dalphadt2-sin(alpha)*dadt2
     *		-1d0/Re*wy(j)

		py(j)=-X(1,j)*dalphadt2+Y(1,j)*dalphadt**2-cos(alpha)*dadt2
     *		+1d0/Re*wx(j)

		ps(j)=zn(j)/Re
420	continue

	p(1)=0d0
	do 430 j=2,Nsmax/2+1
		p(j)=p(j-1)+0.5d0*(px(j-1)*pxps(1,j-1)+py(j-1)*pyps(1,j-1)+
     *		px(j)*pxps(1,j)+py(j)*pyps(1,j))*ds

430	continue
	psave=p(Nsmax/2+1)
	p(Nsmax)=p(1)-0.5d0*(px(1)*pxps(1,1)+py(1)*pyps(1,1)+
     *		px(Nsmax)*pxps(1,Nsmax)+py(Nsmax)*pyps(1,Nsmax))*ds

	do 432 j=Nsmax-1,Nsmax/2+1,-1
		p(j)=p(j+1)-0.5d0*(px(j+1)*pxps(1,j+1)+py(j+1)*pyps(1,j+1)+
     *		px(j)*pxps(1,j)+py(j)*pyps(1,j))*ds

432	continue
	p(Nsmax/2+1)=0.5*(p(Nsmax/2+1)+psave)

	do 440 i=1,Nzmax
		do 440 j=1,Nsmax
			U(i,j)=1d0/dsqrt(Jcb(i,j))*((pype(i,j)*cos(sita(j))-
     *			pypx(i,j)*sin(sita(j)))*vr(i,j)+
     *			(-pype(i,j)*sin(sita(j))-pypx(i,j)*cos(sita(j)))*
     *			vs(i,j))
			V(i,j)=1d0/dsqrt(Jcb(i,j))*((-pxpe(i,j)*cos(sita(j))+
     *			pxpx(i,j)*sin(sita(j)))*vr(i,j)+
     *			(pxpe(i,j)*sin(sita(j))+pxpx(i,j)*cos(sita(j)))*
     *			vs(i,j))
440	continue
	

	do 450 j=1,Nsmax
		pupr(j)=(U(2,j)-U(1,j))/dr
450		pvpr(j)=(V(2,j)-V(1,j))/dr
	do 455 j=2,Nsmax-1
		pups(j)=(U(1,j+1)-U(1,j-1))/(2*ds)
455		pvps(j)=(V(1,j+1)-V(1,j-1))/(2*ds)
	pups(1)=(U(1,2)-U(1,Nsmax))/(2*ds)
	pups(Nsmax)=(U(1,1)-U(1,Nsmax-1))/(2*ds)
	pvps(1)=(V(1,2)-V(1,Nsmax))/(2*ds)
	pvps(Nsmax)=(V(1,1)-V(1,Nsmax-1))/(2*ds)				
	do 460 j=1,Nsmax
		pupx(j)=prpx(1,j)*pupr(j)+pspx(1,j)*pups(j)
		pupy(j)=prpy(1,j)*pupr(j)+pspy(1,j)*pups(j)
		pvpx(j)=prpx(1,j)*pvpr(j)+pspx(1,j)*pvps(j)
460		pvpy(j)=prpy(1,j)*pvpr(j)+pspy(1,j)*pvps(j)	
	fx=0d0
	fy=0d0
	mz=0d0
	fyx=0
c465		write(*,*) j,pvpx(j)-pupy(j),w(1,j)

	do 470 j=2,Nsmax
		dfx=0.5d0*(-p(j-1)*pyps(1,j-1)+2d0/Re*(pyps(1,j-1)*
     *		pupx(j-1)
     *		-0.5d0*pxps(1,j-1)*(pupy(j-1)+pvpx(j-1)))-p(j)*pyps(1,j)+
     *		2d0/Re*(pyps(1,j)*pupx(j)-0.5d0*pxps(1,j)*(pupy(j)+
     *		pvpx(j))))*ds

		fx=fx+dfx

		dfy=0.5d0*(p(j-1)*pxps(1,j-1)+2d0/Re*(0.5d0*pyps(1,j-1)*
     *		(pupy(j-1)+pvpx(j-1))-pxps(1,j-1)*pvpy(j-1))+
     *		p(j)*pxps(1,j)+2d0/Re*(0.5d0*
     *		pyps(1,j)*(pupy(j)+pvpx(j))-pxps(1,j)*pvpy(j)))*ds

		fy=fy+dfy

		fyx=fyx+dfy*0.5*(x(1,j-1)+x(1,j))

		mz=mz+0.5d0*(p(j-1)*(X(1,j-1)*pxps(1,j-1)+Y(1,j-1)*
     *		pyps(1,j-1))+2d0/Re*(-X(1,j-1)*pxps(1,j-1)*pvpy(j-1)+
     *		(X(1,j-1)*pyps(1,j-1)+Y(1,j-1)*pxps(1,j-1))*0.5d0*
     *		(pupy(j-1)+pvpx(j-1))-Y(1,j-1)*pyps(1,j-1)*pupx(j-1))+
     *		p(j)*(X(1,j)*pxps(1,j)+Y(1,j)*pyps(1,j))+2d0/Re*
     *		(-X(1,j)*pxps(1,j)*pvpy(j)+(X(1,j)*pyps(1,j)+
     *		Y(1,j)*pxps(1,j))*0.5d0*(pupy(j)+pvpx(j))-Y(1,j)*
     *		pyps(1,j)*pupx(j)))*ds

470	continue

	dfx=0.5d0*(-p(Nsmax)*pyps(1,Nsmax)+2d0/Re*
     *	(pyps(1,Nsmax)*pupx(Nsmax)
     *	-0.5d0*pxps(1,Nsmax)*(pupy(Nsmax)+pvpx(Nsmax)))-
     *	p(1)*pyps(1,1)+
     *		2d0/Re*(pyps(1,1)*pupx(1)-0.5d0*pxps(1,1)*(pupy(1)+
     *		pvpx(1))))*ds
	
	fx=fx+dfx

	dfy=0.5d0*(p(Nsmax)*pxps(1,Nsmax)+2d0/Re*(0.5d0*pyps(1,Nsmax)*
     *		(pupy(Nsmax)+pvpx(Nsmax))-pxps(1,Nsmax)*pvpy(Nsmax))+
     *		p(1)*pxps(1,1)+2d0/Re*(0.5d0*
     *		pyps(1,1)*(pupy(1)+pvpx(1))-pxps(1,1)*pvpy(1)))*ds

	fy=fy+dfy

	fyx=fyx+fdy*0.5*(x(1,1)+x(1,Nsmax))

	mz=mz+0.5d0*(p(Nsmax)*(X(1,Nsmax)*pxps(1,Nsmax)+Y(1,Nsmax)*
     *		pyps(1,Nsmax))+2d0/Re*(-X(1,Nsmax)*pxps(1,Nsmax)*
     *		pvpy(Nsmax)+
     *		(X(1,Nsmax)*pyps(1,Nsmax)+Y(1,Nsmax)*pxps(1,Nsmax))*0.5d0*
     *		(pupy(Nsmax)+pvpx(Nsmax))-Y(1,Nsmax)*pyps(1,Nsmax)*
     *		pupx(Nsmax))+
     *		p(1)*(X(1,1)*pxps(1,1)+Y(1,1)*pyps(1,1))+2d0/Re*
     *		(-X(1,1)*pxps(1,1)*pvpy(1)+(X(1,1)*pyps(1,1)+
     *		Y(1,1)*pxps(1,1))*0.5d0*(pupy(1)+pvpx(1))-Y(1,1)*
     *		pyps(1,1)*pupx(1)))*ds
	fd=fx*cos(alpha)-fy*sin(alpha)
	fl=fx*sin(alpha)+fy*cos(alpha)
	power=dadt*fl-dalphadt*mz
	
	xpres=xp+(fyx/fy)/chord
	goto 501

502	continue
	heavem2=heavem1
	heavem1=heave0
	heave0=heave
	dadt0=dadt
	dadt20=dadt2
	alpham2=alpham1
	alpham1=alpha0
	alpha0=alpha
	dalphadt20=dalphadt2
	do 110 i=1,Nzmax
		do 110 j=1,Nsmax
110			w0(i,j)=w(i,j)	
	
	pin=pitchm*dalphadt
	pout=xm*dadt*dadt



C      OUTPUT TIME HISTORY OF PITCHING AND HEAVING MOTIONS

	if(it/20*20.eq.it) then
         write(*,*) "time",t/xtscale
	write(2,*) t/xtscale,pout/xlscale
	write(1,*) t/xtscale,alpha*180./pi,heave/xlscale
	yyt=X(1,1)*sin(alpha)+Y(1,1)*cos(alpha)+heave
	yyl=X(1,nsmax/2+1)*sin(alpha)+Y(1,nsmax/2+1)*cos(alpha)+heave
	write(3,*) t/xtscale,yyl/xlscale,yyt/xlscale


c	write(1,999) t/period,fx,fy
c	write(3,999) t/period,fd,fl,mz
c	write(4,999) t/period,power

	call flush(1)
	call flush(2)
	endif
	
	

C      OUTPUT DATAFILES FOR VORTICITY AND PRESSURE FIELDS
       Nt=timeout/dtdt
       istart=timestart/dtdt
       iend=timeend/dtdt
      
       if(it.ge.istart.and.it.le.iend.and.
     *	 (it-1)/(Nt)*(Nt).eq.it-1) then
c             write(10+iout,*) 'title'
c                   write(10+iout,*) 'VARIABLES  = X, Y, Z,"pressure"'
               write(10+iout,99) iout,Nzmax*(Nsmax+1),(Nzmax-1)*Nsmax
 99   format(1x,'ZONE T=',i7,',N=',i7,',E=',i7,',ET=QUADRILATERAL,
     *  F=FEPOINT')

	do 333 i=1,Nzmax
		do 334 j=1,Nsmax
			xx=X(i,j)*cos(alpha)-Y(i,j)*sin(alpha)
			yy=X(i,j)*sin(alpha)+Y(i,j)*cos(alpha)+heave
334			write(10+iout,999) xx/chord,yy/chord, 0.0, w(i,j),
     *			psi(i,j)
			xx=X(i,1)*cos(alpha)-Y(i,1)*sin(alpha)
			yy=X(i,1)*sin(alpha)+Y(i,1)*cos(alpha)+heave
333		write(10+iout,999) xx/chord,yy/chord,0.0,w(i,1),psi(i,1)
909	format(1x,3(e14.7,1x))
999	format(1x,5(f14.7,1x))
      do 31 j=1,Nsmax
         do 31 i=1,Nzmax-1
 31         write(10+iout,*) j+(i-1)*(Nsmax+1),j+1+(i-1)*(Nsmax+1),
     *           j+1+i*(Nsmax+1),j+i*(Nsmax+1)
	call flush(10+iout)
	iout=iout+1
	endif

	if(it.eq.itotal)	stop	


	goto 1
	end


C         Main program ends here
C         Main program ends here
C         Main program ends here
C         Main program ends here










C        Subroutines





c UPDATE Vr and Vs
	subroutine vrvs(t,tstart,amp,omega,heave,dadt,dadt2,alpm,phi,
     *	alpha,dalphadt,dalphadt2,Jcb,r,psis,psi,sita,X,Y,z,pxpx,pxpe,
     *	pypx,pype,dz,vr,vs)
       implicit real*8(A-H,O-Z)
	include 'parameter.h'	
	real*8 z(Nzmax),vr(Nzmax,Nsmax),vs(Nzmax,Nsmax)
	real*8 r(Nzmax),Jcb(Nzmax,Nsmax)
	real*8 psi(Nzmax,Nsmax),psis(Nzmax,Nsmax)
	real*8 sita(Nsmax)
	real*8 pxpx(Nzmax,Nsmax),pxpe(Nzmax,Nsmax)
	real*8 pypx(Nzmax,Nsmax),pype(Nzmax,Nsmax)
	real*8 X(Nzmax,Nsmax),Y(Nzmax,Nsmax)


c UPDATE Vr and Vs	
	do 230 j=1,Nsmax
		do 240 i=1,Nzmax
			vr(i,j)=1d0/dsqrt(Jcb(i,j))*
     *			(1d0/r(i)*psis(i,j)-
     *	(dadt*sin(alpha)-dalphadt*Y(i,j))*(pxpx(i,j)*cos(sita(j))
     *		+pxpe(i,j)*sin(sita(j)))-(dadt*cos(alpha)+
     *	 dalphadt*X(i,j))*(pypx(i,j)*cos(sita(j))+
     *	pype(i,j)*sin(sita(j))))					
 
240		continue
	i=1
	vs(i,j)=1d0/dsqrt(Jcb(i,j))*(-exp(-z(i))*
     *			(psi(i+1,j)-psi(i,j))/dz-
     *			(dadt*sin(alpha)-dalphadt*Y(i,j))*
     *			(-pxpx(i,j)*sin(sita(j))+pxpe(i,j)*cos(sita(j)))-
     *			(dadt*cos(alpha)+dalphadt*X(i,j))*
     *			(-pypx(i,j)*sin(sita(j))+pype(i,j)*cos(sita(j))))

		vs(1,j)=0d0
		do 250 i=2,Nzmax-1
			vs(i,j)=1d0/dsqrt(Jcb(i,j))*
     *			(-dexp(-z(i))*
     *			(psi(i+1,j)-psi(i-1,j))/(2d0*dz)-
     *			(dadt*sin(alpha)-dalphadt*Y(i,j))*
     *			(-pxpx(i,j)*sin(sita(j))+pxpe(i,j)*cos(sita(j)))-
     *			(dadt*cos(alpha)+dalphadt*X(i,j))*
     *			(-pypx(i,j)*sin(sita(j))+pype(i,j)*cos(sita(j))))
250	continue
		i=Nzmax	
		vs(i,j)=1d0/dsqrt(Jcb(i,j))*
     *		(-dexp(-z(i))*
     *			(psi(i,j)-psi(i-1,j))/dz-
     *			(dadt*sin(alpha)-dalphadt*Y(i,j))*
     *			(-pxpx(i,j)*sin(sita(j))+pxpe(i,j)*cos(sita(j)))-
     *			(dadt*cos(alpha)+dalphadt*X(i,j))*
     *			(-pypx(i,j)*sin(sita(j))+pype(i,j)*cos(sita(j))))
230	continue   
	end

c       Solve the continuity equation
	subroutine cont(w,z,r,Jcb,la1,la2,la3,rc,wrk1,wrkc,boda1,boda2,
     *	bodb1,bodb2,psin,psins,psi,psis,psis2,fn,pxpx,pxpe,pypx,pype,
     *	sita,X,Y,U0,dadt,dalphadt,alpha,dz,ds,b)

	implicit real*8(A-H,O-Z)
   	include 'parameter.h'

c	FLOW FUNCTION EQUATION (INCOMPRESSIBILITY) 

	real*8 w(Nzmax,Nsmax),z(Nzmax)
	real*8 r(Nzmax),Jcb(Nzmax,Nsmax)
	real*8 la1(Nzmax),la2(Nzmax),la3(Nzmax)
	real*8 psi(Nzmax,Nsmax),psis(Nzmax,Nsmax),psis2(Nzmax,Nsmax)
	real*8 sita(Nsmax)
	real*8 pxpx(Nzmax,Nsmax),pxpe(Nzmax,Nsmax)
	real*8 pypx(Nzmax,Nsmax),pype(Nzmax,Nsmax)
	real*8 X(Nzmax,Nsmax),Y(Nzmax,Nsmax)
	complex*16 boda1(Nsmax),boda2(Nsmax)
	complex*16 bodb1(Nsmax),bodb2(Nsmax)
	complex*16 fn(Nzmax,Nsmax),ci
	complex*16 wrkc(Nsmax),psin(Nzmax,Nsmax),psins(Nzmax,Nsmax)
	complex*16 psins2(Nzmax,Nsmax),rc(Nzmax),wrk1(Nzmax)
	data pi,ci/3.141592653589793d0,(0d0,1d0)/

c 	SPECIFY BOUNDARY CONDITIONS
	do 120 j=1,Nsmax
		boda1(j)=r(1)*(dadt*sin(alpha)-dalphadt*Y(1,j))*
     *		(pxpx(1,j)*cos(sita(j))+pxpe(1,j)*sin(sita(j)))+
     *		r(1)*(dadt*cos(alpha)+dalphadt*X(1,j))*
     *		(pypx(1,j)*cos(sita(j))+pype(1,j)*sin(sita(j)))

		boda2(j)=-1d0*(dadt*sin(alpha)-dalphadt*Y(1,j))*
     *		(-pxpx(1,j)*sin(sita(j))+pxpe(1,j)*cos(sita(j)))-
     *		1d0*(dadt*cos(alpha)+dalphadt*X(1,j))*
     *		(-pypx(1,j)*sin(sita(j))+pype(1,j)*cos(sita(j)))

		bodb1(j)=r(Nzmax)*U0*(cos(alpha)*(-pypx(Nzmax,j)*
     *		sin(sita(j))+
     *		pype(Nzmax,j)*cos(sita(j)))-sin(alpha)*(pxpx(Nzmax,j)*
     *		sin(sita(j))-pxpe(Nzmax,j)*cos(sita(j))))

		bodb2(j)=U0*(cos(alpha)*(pypx(Nzmax,j)*cos(sita(j))+
     *		pype(Nzmax,j)*sin(sita(j)))-sin(alpha)*
     *		(-pxpx(Nzmax,j)*
     *		cos(sita(j))+pxpe(Nzmax,j)*sin(sita(j))))

120	continue

	call fast(boda1,Nsmax,-1)
	call fast(boda2,Nsmax,-1)
	call fast(bodb1,Nsmax,-1)
	call fast(bodb2,Nsmax,-1)

	do 130 i=1,Nzmax
		do 140 j=1,Nsmax
140			wrkc(j)=Jcb(i,j)*w(i,j) 
		call fast(wrkc,Nsmax,-1)
		do 150 j=1,Nsmax
150			Fn(i,j)=wrkc(j)
130	continue
	do 160 j=1,Nsmax
		if(j.le.Nsmax/2+1) then
			n=j-1
		else
			n=-(Nsmax-j)-1
		endif			

		do 170 i=2,Nzmax-1
170			la2(i)=-2d0-dz*dz*n*n*dexp(2d0*z(i))/(dexp(z(i))-b)**2
		do 180 i=2,Nzmax-1
180			la1(i)=1d0-dz*b*dexp(-z(i))/(1d0-b*dexp(-z(i)))/2d0
		do 190 i=2,Nzmax-1
190			la3(i)=1d0+dz*b*dexp(-z(i))/(1d0-b*dexp(-z(i)))/2d0
		do 200 i=2,Nzmax-1
200			rc(i)=-dz*dz*dexp(2d0*z(i))*Fn(i,j)
		la2(1)=1d0
		la1(1)=0d0
		la3(1)=0d0
		if(n.eq.0) then
			rc(1)=0d0
		else		
			rc(1)=boda1(j)/(ci*n)
		endif

		la2(Nzmax)=1d0
		la1(Nzmax)=0d0
		la3(Nzmax)=0d0	
		if(n.eq.0) then
			rc(Nzmax)=0d0
		else
			rc(Nzmax)=bodb1(j)/(ci*n)
		endif
		call tridagc(la1,la2,la3,rc,wrk1)

		do 210 i=1,Nzmax
			psin(i,j)=wrk1(i)
			psins(i,j)=ci*n*wrk1(i)		
210			psins2(i,j)=-n*n*wrk1(i)
160	continue
	do 305 i=1,Nzmax
		do 320 j=1,Nsmax
320			wrkc(j)=psin(i,j)	
		call fast(wrkc,Nsmax,1)
		do 330 j=1,Nsmax
330			psi(i,j)=dreal(wrkc(j))
		do 340 j=1,Nsmax
340			wrkc(j)=psins(i,j)
		call fast(wrkc,Nsmax,1)
		do 350 j=1,Nsmax
350			psis(i,j)=dreal(wrkc(j))
		do 360 j=1,Nsmax
360			wrkc(j)=psins2(i,j)
		call fast(wrkc,Nsmax,1)
		do 370 j=1,Nsmax
370			psis2(i,j)=dreal(wrkc(j))	
305	continue			
	end	



	subroutine tridag(a,b,c,r,u)
	implicit real*8(A-H,O-Z)
	include 'parameter.h'
	parameter (n=NZMAX)
	real*8 a(n),b(n),c(n),r(n),u(n)

	integer j
	real*8 bet,gam(n)

	if(b(1).eq.0d0) stop 'tridag: rewrite equations'
	bet=b(1)
	u(1)=r(1)/bet
	do 1 j=2,n
		gam(j)=c(j-1)/bet
		bet=b(j)-a(j)*gam(j)
		if(bet.eq.0d0) stop 'tridag failed'
		u(j)=(r(j)-a(j)*u(j-1))/bet
1	continue
	do 2 j=n-1,1,-1
		u(j)=u(j)-gam(j+1)*u(j+1)			
2	continue
	return
	end

	subroutine tridagc(a,b,c,r,u)
	implicit real*8(A-H,O-Z)
	include 'parameter.h'
	parameter (n=NZMAX)
	real*8 cabs
	real*8 a(n),b(n),c(n),bet,gam(n)
	integer j
	complex*16 r(n),u(n)
	external cabs
	if(b(1).eq.0) stop 'tridagc: rewrite equations'
	bet=b(1)
	u(1)=r(1)/bet
	do 1 j=2,n
		gam(j)=c(j-1)/bet
		bet=b(j)-a(j)*gam(j)
		if(bet.eq.0.d0) stop 'tridagc failed'
		u(j)=(r(j)-a(j)*u(j-1))/bet
1	continue
	do 2 j=n-1,1,-1
		u(j)=u(j)-gam(j+1)*u(j+1)			
2	continue
	return
	end

	function cabs(c)
	real*8 cabs
	complex*16 c
	cabs=dsqrt(dreal(c)**2+dimag(c)**2)
	end

	subroutine sparse(a,b,c,e1,e2,x,y)

c This subroutine solve the equation Ax=y, where A is a sparse matrix, including a tridiagonal 
c matrix with b as the diagonal terms, a the terms to the left of b and c the terms to the right,
c and two additional terms, e1 at (1,n) and e2 at (n,1)
	implicit real*8(A-H,O-Z)
	include 'parameter.h'
	parameter (n=NSMAX)
	real*8 a(n),b(n),c(n)
	real*8 x(n),y(n),e1,e2
	real*8 Ti(n,n)
	call sparse_inv(a,b,c,e1,e2,Ti)
	do 10 i=1,n
		x(i)=0d0
		do 10 j=1,n
10			x(i)=x(i)+Ti(i,j)*y(j)
	end

	subroutine sparse_inv(a,b,c,e1,e2,Ti)
c This subroutine yields the inverse of a sparse matrix, including a tridiagonal matrix 
c with b as the diagonal terms, a the terms to the left of b and c the terms to the right,
c and two additional terms, e1 at (1,n) and e2 at (n,1)
	implicit real*8(A-H,O-Z)
	include 'parameter.h'
	parameter (n=NSMAX)
	real*8 a(n),b(n),c(n)
	real*8 wrk(n,n),Ti(n,n),lambda
	b(1)=b(1)-e2
	b(n)=b(n)-e1
	call trid_inv(a,b,c,wrk)
	lambda=e2*wrk(1,1)+e1*wrk(n,1)+e2*wrk(1,n)+e1*wrk(n,n)
	do 35 i=1,n
		do 35 j=1,n
35			Ti(i,j)=wrk(i,j)-(wrk(i,1)+wrk(i,n))*
     *			(e2*wrk(1,j)+e1*wrk(n,j))/(1d0+lambda)
	b(1)=b(1)+e2
	b(n)=b(n)+e1
	end

	subroutine trid_inv(a,b,c,Ti)
c This subroutine yields the inverse of a tridiagonal matrix with b as 
c the diagonal terms, a the terms to the left of b and c the terms to the right
	implicit real*8(A-H,O-Z)
	include 'parameter.h'
	parameter (n=NSMAX)
	real*8 a(n),b(n),c(n)
	real*8 Ti(n,n),s(0:n),f(n+1)
	real*8 wrk
	s(0)=1d0
	s(1)=b(1)
	do 10 i=2,n
10		s(i)=b(i)*s(i-1)-c(i-1)*a(i)*s(i-2)
c
	f(n+1)=1d0
	f(n)=b(n)
	do 20 i=n-1,1,-1
20		f(i)=b(i)*f(i+1)-c(i)*a(i+1)*f(i+2)
	do 30 i=1,n	
		j=i		
		wrk=1d0
		Ti(i,j)=wrk*s(i-1)*f(j+1)/s(n)	
c		write(*,*) Ti(i,j)		
		do 40 j=i+1,n
			wrk=-wrk*c(j-1)
40			Ti(i,j)=wrk*s(i-1)*f(j+1)/s(n)
		wrk=1d0
		do 50 j=i-1,1,-1
			wrk=-wrk*a(j+1)
50			Ti(i,j)=wrk*s(j-1)*f(i+1)/s(n)
30	continue
	end
		
********************************
*         next: FFT            *
********************************
      SUBROUTINE fast(X,NDIM,IONE)
C
C IMPLEMENT THE FAST FOURIER TRANSFORM
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16  T,U,W,X(NDIM),ci
      DATA PI,ci/3.1415926535897932384d0,(0.d0,1.d0)/
      NPOW=int(dlog(NDIM+1.d0)/dlog(2.d0))
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
      U=1.d0
      W=COS(ANG)+ci*IONE*sin(ANG)
      DO 500 J=1,LE2
      DO 400 I=J,N1,LE1
      IP=I+LE2
      T=    X(IP)*U
      X(IP)=X(I)-T
400   X(I)= X(I)+T
500   U=U*W
      IF(IONE.EQ.1)RETURN
      SCL= 1.d0/(N1)
      DO 600 I=1,N1
600   X(I)=X(I)*SCL
      RETURN
      END
										

C****************************************************************************


      subroutine thomas_pr
     +
     +  (N   ! matrix size
     +  ,a   ! diagonal 
     +  ,b   ! super-diagonal row
     +  ,c   ! sub-diagonal row
     +  ,s   ! rhs
     +  ,x   ! solution 
     +  )

c-----------------------------------------
c Copyright by C. Pozrikidis, 1999
c All rights reserved.
c
c This program is to be used only under the
c stipulations of the licensing agreement.
c----------------------------------------

c------------------------------------------------
c This program accompanies the book:
c
c           C. Pozrikidis
c ``Numerical Computation in Science and Engineering''
c      Oxford University Press, 1998
c------------------------------------------------

c------------------------------------------
c Thomas algorithm for nearly tridiagonal systems
c with wrapped first and last elements,
c corresponding to a periodic boundary condition
c
c  T . x = s
c 
c Coefficient matrix:
c
c      | a1 b1  0   0  ...  0   0    c1   |
c      | c2 a2  b2  0  ...  0   0    0    |
c      | 0  c3  a3  b3 ...  0   0    0    |
c T =  | ..............................   |
c      | 0  0   0   0  ... cn-1 an-1 bn-1 |
c      | bn 0   0   0  ...  0   cn   an   |
c
c
c The solution is found using a
c modification of Algorithm 3.4.1, p. 143
c
c------------------------------------------

      Implicit Double Precision (a-h,o-z)

      Dimension a(1026),b(1026),c(1026),s(1026),x(1026)
      Dimension d(1026),y(1026)

      Dimension x0(1026)

      Parameter (tol=0.00000001D0)

c--------
c prepare
c--------

      Na = N-1
      Nb = N-2

      save1  = s(1)
      saveNa = s(Na)

c------------------------------
c First assume that x(N) = 0
c and solve the first N-1 equations
c neglecting the last column
c and the last row
c------------------------------

      x(N) = 0.0D0

c-- REGULAR THOMAS

c-----
c reduction to upper bidiagonal
c-----

      d(1) = b(1)/a(1)
      y(1) = s(1)/a(1)

      Do i=1,Nb
       i1 = i+1
       Den   = a(i1)-c(i1)*d(i)
       d(i1) = b(i1)/Den
       y(i1) = (s(i1)-c(i1)*y(i))/Den
      End Do

c----
c back substitution
c----

      x(Na) = y(Na)

      Do i=Nb,1,-1
        x(i)= y(i)-d(i)*x(i+1)
      End Do

c-----
c compute the first residual, defined in terms of the
c last equation
c-----

      R0 = a(N)*x(N) + b(N)*x(1) + c(N)*x(Na) - s(N)

c-----
c save the solution
c-----

      Do i=1,N
        x0(i) = x(i)
      End Do

c------------------------------
c Second, assume that x(N) = 1
c and solve the first N-1 equations
c with a modified RHS
c------------------------------

      x(N) = 1.0D0

      s(1)  = s(1)  - c(1)  * x(N)
      s(Na) = s(Na) - b(Na) * x(N)


c-- REGULAR THOMAS

c-----
c reduction to upper bidiagonal
c-----

      d(1) = b(1)/a(1)
      y(1) = s(1)/a(1)

      Do i=1,Nb
       i1 = i+1
       Den   = a(i1)-c(i1)*d(i)
       d(i1) = b(i1)/Den
       y(i1) = (s(i1)-c(i1)*y(i))/Den
      End Do

c-----
c Back substitution
c-----

      x(Na) = y(Na)

      Do i=Nb,1,-1
        x(i)= y(i)-d(i)*x(i+1)
      End Do

c------
c compute the second residual:
c-----

      R1 = a(N)*x(N) + b(N)*x(1) + c(N)*x(Na) - s(N)

c-----
c rectify the rhs
c-----

      s(1)  = save1
      s(Na) = saveNa

c---------------------------------
c The residual takes the form:
c
c R = (R1-R0) x(N) + R0
c
c compute the correct value of x(N)
c to make R=0
c---------------------------------

      x(N) = R0/(R0-R1)

c----------------------------
c compose the solution vector
c using
c
c x = (x1-x0) x(N) + x0
c----------------------------

      Do i=1,Na
       x(i) = (x(i)-x0(i)) * x(N) + x0(i)
      End Do

c-----------------------
c Verification and alarm
c-----------------------

      Res = s(1) - a(1)*x(1) -b(1)*x(2)-c(1)*x(N)

      If(abs(Res).gt.tol) write (6,*) " thomas_pr: alarm, 1",Res

      Do i=2,Na
        Res = s(i)-c(i)*x(i-1)-a(i)*x(i)-b(i)*x(i+1)
        If(abs(Res).gt.tol) write (6,*) " thomas_pr: alarm ",i,Res
      End Do

      Res = s(N)-c(N)*x(Na)-a(N)*x(N)-b(N)*x(1)

      If(abs(Res).gt.tol) write (6,*) " thomas_pr: alarm ",N,Res

c-----
c Done
c-----

 100  Format (1x,f15.10)

      Return
      End

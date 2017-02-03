c     file genray.f
c     Program to generate random rays within the specified acceptance
      implicit none
      real x,y,th,ph,del,xmax,ymax,thmax,phmax,delmax,r
      real zi,vzi,random,rand
      integer nrays,i,iseed
      print *,'How many rays to generate (max=950)'
      read(*,*)nrays
      print *,'x will lie in range [-xmax, xmax], etc.'
      print *,'Enter xmax, thmax, ymax, phmax, delmax'
      read(*,*)xmax,thmax,ymax,phmax,delmax
      print *,'Enter seed for random number generator'
      read(*,*)iseed
      r=rand(iseed)
      if(nrays.gt.950)nrays=950
      zi=0.
      vzi=0.
c
      open(unit=7,type='new',file='genray.dat')
      do i=1,nrays
c       pick random values of x in range (-xmax,+xmax)
        x=random(-xmax,xmax)
        th=random(-thmax,thmax)
        y=random(-ymax,ymax)
        ph=random(-phmax,phmax)
        del=random(-delmax,delmax)
        write(7,100)x,th,y,ph,zi,vzi,del
 100    format(7f10.4)
      enddo
      print *,nrays,' rays written to file genray.dat'
      stop
      end
c
c
        real*4 function random(xmin,xmax)
        implicit none
        real x,xmin,xmax,xrange,rand
        xrange=xmax-xmin
        x=xmin+rand()*xrange
        random=x
        return
        end

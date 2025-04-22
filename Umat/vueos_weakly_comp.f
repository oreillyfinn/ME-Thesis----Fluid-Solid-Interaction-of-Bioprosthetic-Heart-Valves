      subroutine vueos (
C Read only (unmodifiable) variables –
     1     nblock,
     2     jElem, kIntPt, kLayer, kSecPt, 
     3     stepTime, totalTime, dt, cmname,
     4     nstatev, nfieldv, nprops,
     5     props, tempOld, tempNew, fieldOld, fieldNew,
     6     stateOld, charLength, coordMp, 
     7     densityMean, refDensity, densityNew,
     8     dkk, Em,
C Write only (modifiable) variables –
     8     press, dPdRho, dPdEm, 
     9     stateNew )
C
      include 'vaba_param_dp.inc'
C
      dimension props(nprops), 
     1  tempOld(nblock),
     2  fieldOld(nblock,nfieldv), 
     3  stateOld(nblock,nstatev), 
     4  tempNew(nblock),
     5  fieldNew(nblock,nfieldv),
     6  charLength(nblock), coordMp(nblock,*), 
     7  densityMean(nblock), refDensity(nblock),
     8  densityNew(nblock),
     9  dkk(nblock), Em(nblock),
     1  press(nblock),dPdRho(nblock),dPdEm(nblock),
     2  stateNew(nblock)
C
      character*80 cmname
C
      parameter ( zero = 0.d0, one = 1.d0, half = 0.5d0 )
C
      c0 = props(1) 
	  gamma = props(2)
C
      do k=1, nblock
  
       rho0 = refDensity(k)
	   rho1 = densityNew(k)
	   
	   ek0 = (c0**2)*rho0/gamma
	   
       press(k) = ek0*( ((rho1/rho0)**gamma) - 1)
	   
C dP/dEm	   
	   dPdEm(k) = 0.0
C dP/dRho	   
	   dPdRho(k) = gamma*ek0*(rho1**(gamma-1))/(rho0**gamma)

      end do
C
      return
      end
PROGRAM exerB
  IMPLICIT NONE
  REAL(8) :: m, p, rho, t, a, c

  c = 1.0/2
  m = 70.0
  p = 400.0
  rho = 1.2
  !t 3hrs

  DO i = 0
    v = v + dt*p/(m*v) - dt*c*rho*a*v**2/m
  END DO

END PROGRAM exerB

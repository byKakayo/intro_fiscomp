PROGRAM exerC1
  IMPLICIT NONE
  REAL(8) :: m, l, dt, t, theta, omega

  !Definir o valor de g

  DO i = 0,
    omega = omega - (g/l)*theta*dt
    theta = theta + omega*dt
  END DO
END PROGRAM exerC1

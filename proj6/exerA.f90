PROGRAM exerA
  IMPLICIT NONE
  REAL(8) :: t, dt, x, y, vx, vy, vx1, vy1, x1, y1, r, aux
  REAL(8), PARAMETER :: pi=4.0d0*atan(1.0d0)
  INTEGER(8) :: i

  OPEN(UNIT=2, FILE="trajA1_out.dat")

  !i = 0
  READ(*,*)x
  READ(*,*)vy
  READ(*,*)dt

  y = 0
  vx = 0

  !i = 1
  x1 = x + vx*dt
  y1 = y + vy*dt

  r = sqrt(x1**2 + y1**2)

  vx = vx - 4*(pi**2)*x1/(r**3)
  vy = vy - 4*(pi**2)*y1/(r**3)

  i = 2
  DO WHILE(.TRUE.)
    t = i*dt

    aux = 2*x1 - x - 4*(pi**2)*x1*(dt**2)/(r**3)
    x = x1
    x1 = aux

    aux = 2*y1 - y - 4*(pi**2)*y1*(dt**2)/(r**3)
    y = y1
    y1 = aux

    r = sqrt(x1**2 + y1**2)

    vx1 = vx - 4*(pi**2)*x1/(r**3)
    vy1 = vy - 4*(pi**2)*y1/(r**3)


    WRITE(2,*)x1, y1

    IF(y1*y <= 0 .AND. vy1*vy > 0)THEN
      EXIT
    END IF

    i = i + 1

    vx = vx1
    vy = vy1
  END DO

  WRITE(*,*) !Escrever sobre a escolha de dt p/ obter orbitas estaveis

END PROGRAM exerA

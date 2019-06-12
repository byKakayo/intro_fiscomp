PROGRAM exer1
  IMPLICIT NONE
  REAL(8) :: r, d, x0, x, e, lambda, lamb, aux
  INTEGER(8) :: i, j
  INTEGER(8), PARAMETER :: iMax = 1000 !Valor maximo de iterações

  !Arquivo de saída
  OPEN(UNIT=2,FILE="dist_out.dat")

  READ(*,*) r
  READ(*,*) x0
  READ(*,*) e

  lambda = 0d0
  lamb = 0d0
  lambda = lambda + log(abs(r*(1d0-2d0*x0)))/iMax
  x = x0 + e

  WRITE(2, *)0, x0, e

  DO i = 1, iMax
    x0 = x0*r*(1d0-x0)
    x = x*r*(1d0-x)

    lambda = lambda + log(abs(r*(1d0-2d0*x0)))

    d = abs(x-x0)
    !Escreve a população e distancia para cada t
    WRITE(2, *)i, x0, d
  END DO

  !Linearizando os dados da distãncia de 10 a 40

  DO j = 1, 40
    READ(2, *)i, x0, d
    IF(j > 10)THEN
      lamb = lamb + (j - 25.d0)*log(d)
      aux = aux + (j - 25.d0)**2
    END IF
  END DO

  PRINT *, lamb/aux
  PRINT *, lambda/iMax

  !Fecha arquivo de saída
  CLOSE(2)
END PROGRAM exer1

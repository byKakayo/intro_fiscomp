PROGRAM exer1
  IMPLICIT NONE
  REAL(8) :: r, t, d, lambda
  INTEGER(8) :: x0, x, e, i
  INTEGER(8), PARAMETER :: iMax = 1000 !Valor maximo de iterações

  !Arquivo de saída
  OPEN(UNIT=2,FILE="dist_out.dat")

  !Ler do terminal os valores de:
  !Constante
  !Valor inicial de individuos
  !
  READ(*,*)r
  READ(*,*)x0
  READ(*,*)e

  DO i = 1,
    !Escreve a população e distancia para cada t
    WRITE(2, *)t, x, d
  END DO

  PRINT *, !lambda pela exponencial, lambda

  !Fecha arquivo de saída
  CLOSE(2)
END PROGRAM exer1

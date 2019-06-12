PROGRAM mapa_logistico
  IMPLICIT NONE
  REAL(8) :: r, x1, x2, x3
  INTEGER(8) :: i
  INTEGER(8), PARAMETER :: iMax = 50 !Valor maximo de iterações

  !Arquivo de saída
  OPEN(UNIT=2,FILE="mapr3.dat")

  r = 3.0
  x1 = 0.2
  x2 = 0.4
  x3 = 0.6

  DO i = 0, iMax
    !Escreve o número de individuos em cada iteração
    WRITE(2, *)i, x1, x2, x3
    x1 = r*x1*(1-x1)
    x2 = r*x2*(1-x2)
    x3 = r*x3*(1-x3)
  END DO

  !Fecha arquivo de saída
  CLOSE(2)
END PROGRAM mapa_logistico

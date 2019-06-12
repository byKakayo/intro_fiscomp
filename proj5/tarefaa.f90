PROGRAM tarefaa
  IMPLICIT NONE
  REAL(8) :: x, dx, fx, g1, g2, g3
  INTEGER(8) :: i, n

  !Arquivo de saída
  OPEN(UNIT=10,FILE="plotaf.dat")
  OPEN(UNIT=11,FILE="plotag1.dat")
  OPEN(UNIT=12,FILE="plotag2.dat")
  OPEN(UNIT=13,FILE="plotag3.dat")

  dx = 1d-2
  n = 1/dx -1
  x = 0
  !Escreve primeira linha no arquivo de saída
  WRITE(10, *)x, 0
  WRITE(11, *)x, 0
  WRITE(12, *)x, 0
  WRITE(13, *)x, 0

  DO i = 0, n
    x = x + dx

    fx = x
    g1 = 1*x*(1-x)
    g2 = 2*x*(1-x)
    g3 = 3*x*(1-x)

    WRITE(10, *)x, fx
    WRITE(11, *)x, g1
    WRITE(12, *)x, g2
    WRITE(13, *)x, g3
  END DO

  !Fecha arquivo de saída
  CLOSE(10)
  CLOSE(11)
  CLOSE(12)
  CLOSE(13)
END PROGRAM tarefaa

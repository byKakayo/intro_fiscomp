PROGRAM exer2
  IMPLICIT NONE
  REAL(8) :: t
  REAL(8) :: dt = 0.01 !0.2         !Intervalo de tempo
  REAL(8) :: lambda = 0.7           !Constante de decaimento
  INTEGER(8), PARAMETER :: n = 5000 !Número de átomos da amostra inicial


  !Arquivo de saida
  OPEN(UNIT=2,FILE="decai_out")


  CLOSE(2)
END PROGRAM exer2

PROGRAM exer1
  IMPLICIT NONE
  REAL(8) :: dt, lambda, t
  INTEGER(8) :: n, i

  !Arquivo de saida
  OPEN(UNIT=2,FILE="decai_out")

  !Ler do terminal os valores de:
  !Número de átomos da amostra inicial
  !Constante de decaimento
  !Intervalo de tempo
  READ(*,*)n
  READ(*,*)lambda
  READ(*,*)dt

  DO i = 0,
    n = -lambda*n*dt
  END DO
  !rand()

  CLOSE(2)
END PROGRAM exer1

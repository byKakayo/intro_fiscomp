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

WRITE(2, *)0, n

  DO i = 1,
    t = i*dt
    n = -lambda*n*dt
    WRITE(2, *)t, n
  END DO
  !rand()

  CLOSE(2)
END PROGRAM exer1

PROGRAM exer2
  IMPLICIT NONE
  REAL(8) :: t, tm
  REAL(8) :: dt = 0.01  !0.2         !Intervalo de tempo
  REAL(8) :: lambda = 0.7            !Constante de decaimento
  INTEGER(8), PARAMETER :: n0 = 5000 !Número de átomos da amostra inicial
  INTEGER(8) :: n, i, x, a, j

  !Arquivo de saida
  OPEN(UNIT=2,FILE="decai_out")

  n = n0

  !Escreve primeira linha no arquivo de saída
  WRITE(2, *)0, n

  !Define o número de iterações
  x = INT(10.0d0/dt)

  DO i = 1, x
    a = 0
    tm = tm + t*lambda*n/n0*dt
    DO j = 1, n
      !Probabilidade de decair
      IF(rand() < lambda*dt)THEN
        a = a + 1
      END IF
    END DO
    t = i*dt
    n = n - a
    !Escreve a quantidade de átomos restantes para cada t
    WRITE(2, *)t, n
  END DO

  PRINT *, tm, 1/lambda

  !Fecha arquivo de saída
  CLOSE(2)
END PROGRAM exer2

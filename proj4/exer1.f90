PROGRAM exer1
  IMPLICIT NONE
  REAL(8) :: dt, lambda, t
  INTEGER(8) :: n, i, x, a, j

  !Arquivo de saida
  OPEN(UNIT=2,FILE="decai_out")

  !Ler do terminal os valores de:
  !Número de átomos da amostra inicial
  !Constante de decaimento
  !Intervalo de tempo
  READ(*,*)n
  READ(*,*)lambda
  READ(*,*)dt

  !Escreve primeira linha no arquivo de saída
  WRITE(2, *)0, n

  !Define o número de iterações
  x = INT(10.0d0/dt)

  DO i = 1, x
    a = 0
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

  !Fecha arquivo de saída
  CLOSE(2)
END PROGRAM exer1

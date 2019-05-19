PROGRAM exer1
  IMPLICIT NONE
  REAL(8) :: dt, lambda, t
  INTEGER(8) :: n, i, x

  !Arquivo de entrada
  OPEN(UNIT=1,FILE="decai_in")

  !Arquivo de saida
  OPEN(UNIT=2,FILE="decai_out")

  !Ler do arquivo de entrada os valores de:
  !Número de átomos da amostra inicial
  !Constante de decaimento
  !Intervalo de integração
  !Tempo total
  READ(1,*)n
  READ(1,*)lambda
  READ(1,*)dt
  READ(1,*)t

  !Fecha o arquivo de entrada
  CLOSE(1)

  !Escreve a primeira linha no arquivo de saída
  WRITE(2, *)0, n

  !Define o número de iterações
  x = INT(t/dt)

  DO i = 1, x
    n = n - n*lambda*dt
    t = i*dt
    !Escreve o valor de N para cada t no arquivo de saída
    WRITE(2, *)t, n
  END DO

  !Fecha o arquivo de saída
  CLOSE(2)
END PROGRAM exer1

PROGRAM exerA1
  IMPLICIT NONE
  REAL(8), PARAMETER :: m = 70.0 !Massa kg
  REAL(8), PARAMETER :: p = 400.0 !Potência W
  REAL(8) :: v, t, dt
  INTEGER :: i, n !Contador e número de iterações

  !Arquivo de saida
  OPEN(UNIT=2,FILE="velA_out.dat")

  !Ler número de valores de V(0), DeltaT, T
  READ(*,*)v
  READ(*,*)dt
  READ(*,*)t

  !Escreve primeira linha no arquivo de saída
  WRITE(2,*)0, v

  n = INT(t/dt)

  !Cálculo iterativo da velocidade
  DO i = 1, n
    !Calcula a nova velocidade
    v = v + p/(m*v)*dt
    t = i*dt
    !Escreve o valor de velocidade para cada t 
    WRITE(2,*)t, v
  END DO
END PROGRAM exerA

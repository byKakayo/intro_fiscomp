PROGRAM exerA
  IMPLICIT NONE
  REAL(8) :: m, p, v, i, t, dt

  !Definindo os valores de massa e potência
  m = 70.0;
  p = 400.0;

  !Arquivo de saida
  OPEN(UNIT=2,FILE="velA_out.dat")

  !Ler número de valores de V(0), DeltaT, T
  READ(*,*)v
  READ(*,*)dt
  READ(*,*)t

  !Cálculo iterativo da velocidade
  DO i = 0,
    v = v + p/(m*v)*dt
    WRITE(2,*)t, v
  END DO
END PROGRAM exerA

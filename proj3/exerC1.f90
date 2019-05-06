PROGRAM exerC1
  IMPLICIT NONE
  REAL(8), PARAMETER :: pi=4.0d0*atan(1.0d0) !pi com precisão dupla
  REAL(8), PARAMETER :: g = 10.0 !Aceleração da gravidade
  REAL(8) :: m, l, dt, t, theta, omega, aux
  INTEGER :: i, n !Contador e número de iterações

  REAL(8) :: h, K, U !Variáveis para calculo da energia

  !Arquivo de saida p/ geração do gráfico
  OPEN(UNIT=2,FILE="exerC1_out.dat")
  OPEN(UNIT=3,FILE="exerC1E_out.dat")

  !Ler do terminal os valores de:
  !Tempo total T
  !DeltaT
  !Massa m
  !Comprimento da aste l
  !Ângulo inicial theta
  READ(*,*)t
  READ(*,*)dt
  READ(*,*)m
  READ(*,*)l
  READ(*,*)theta

  !Converte graus para radianos
  theta = theta*(pi/180)

  !Escreve primeira linha no arquivo de saída
  WRITE(2,*)0, theta

  !Setando velocidade angular inicial
  omega = 0

  !Define o número de iterações
  n = INT(t/dt)

  DO i = 1, n
    aux = omega - (g/l)*theta*dt
    theta = theta + omega*dt
    omega = aux
    t = i*dt
    IF(theta < -pi)THEN
      theta = theta + 2*pi
    ELSE IF(theta > pi)THEN
      theta = theta - 2*pi
    END IF
    !Escreve o valor do ângulo para cada t
    WRITE(2,*)t, theta

    h = l*(1 - cos(theta))
    U = m*g*h
    K = 0.5*m*omega**2

    WRITE(3,*)t, U+K, U, K
  END DO

  !Fecha arquivo de saída
  CLOSE(2)
  CLOSE(3)
END PROGRAM exerC1

PROGRAM exerB
  IMPLICIT NONE
  REAL(8), PARAMETER :: p = 400.0 !Potência W
  REAL(8), PARAMETER :: m = 70.0 !Massa kg
  REAL(8), PARAMETER :: c = 1.0/2 !Coeficiente de arrasto
  REAL(8), PARAMETER :: rho = 1.2 !Densidade do ar kg/m^3
  REAL(8) :: v, t, dt, a, vi, da, tf
  INTEGER :: i, x, n !Contadores e número de iterações

  !Arquivo de saida p/ geração do gráfico
  OPEN(UNIT=2,FILE="grafB.dat")

  !Ler do terminal os valores de:
  !Velocidade inicial
  !DeltaT
  !Tempo total T
  !Área de contato A
  READ(*,*)v
  READ(*,*)dt
  READ(*,*)t !t 3hrs
  READ(*,*)a

  !Escreve primeira linha no arquivo de saída
  WRITE(2,*)0, v

  !Define o número de iterações
  n = INT(t/dt)

  DO i = 1, n
    vi = v + dt*p/(m*v) - dt*c*rho*a*(v**2)/m
    da = da + vi*dt/2
    !Verifica se chegou na velocidade terminal
    IF(abs(vi - v)<= epsilon(vi))THEN
      IF (tf == 0) THEN
        tf = i*dt
      END IF
      da = da + v*dt
    END IF
    v = vi
    !Escreve o valor de velocidade para cada t
    WRITE(2,*)i*dt, v
  END DO

  !Fecha arquivo de saída
  CLOSE(2)

  !Escrever as respostas das questões
  !Questão 1
  PRINT *, "O ciclista se curva para que a área de seu corpo em"
  PRINT *, "contato com o ar seja reduzida, sendo assim, diminui"
  PRINT *, "o efeito resistivo do ar sobre ele. Os ciclistas correm"
  PRINT *, "em grupo para que uma parte deles se beneficiem do vácuo"
  PRINT *, "gerado pelos ciclistas que ficam à frente do grupo, podendo"
  PRINT *, "assim se revezarem. A vantagem do corredor manter-se atrás"
  PRINT *, "de outro é justamente o descrito para um grupo de ciclistas,"
  PRINT *, "o corredor poupa energia para depois fazer a ultrapassagem"
  !Questão 2
  PRINT *, "Instante em que alcança velocidade terminal: ", tf
  !Questão 3
  PRINT *, "Espaço total percorrido pelo ciclista: ", da
  !Questão 4
  PRINT *, "Velocidade final do ciclista: ", v
  !Questão 5
  PRINT *, "Velocidade média do ciclista: ", da/t
END PROGRAM exerB

PROGRAM exerB
  IMPLICIT NONE
  INTEGER :: n, i, num, j
  REAL(8) :: h, x, x1, a, b
  REAL(8) :: resExato, desvT, desvS, desvB

  !Arquivo de entrada
  OPEN(UNIT=1,FILE="tabB_in.dat")
  !Arquivo de saida
  OPEN(UNIT=2,FILE="tabB_out.dat")

  !Definindo os extremos de integração
  a = 0.0
  b = 1.0

  !Ler número de valores de N
  READ(1, *) num

  !Calcula o valor "exato" da integral
  resExato = cos(1.0/2) - 2.0/3 - cos(3.0/2)/3

  !Escrevedo descrição no arquivo tabB_out
  WRITE(2, *)"N ", &
  & " h ", &
  & " Regra do trapézio ", &
  & " Regra do Simpson ", &
  & " Regra de Bode "

  DO i = 1, num
    !Lê o valor de n do arquivo
    READ(1, *) n

    !Calcula o valor de h
    h = (b-a)/n

    desvT = 0.0
    desvS = 0.0
    desvB = 0.0

    DO j=1,n-1,2
      x = a + j*h

      !Regra do trapézio
      desvT = desvT + f(x-h) + 2.0*f(x) + f(x+h)

      !Regra de Simpson
      desvS = desvS + f(x-h) + 4.0*f(x) + f(x+h)

      !Regra do Bode
      IF (MOD(j+3, 4).eq.0) THEN
          desvB = desvB + 7.0*f(x-h) + 32.0*f(x) + 12.0*f(x+h)
          desvB = desvB + 32.0*f(x+2.0*h) + 7.0*f(x+3.0*h)
      END IF
    END DO

    desvT = (h*desvT)/2.0
    desvS = (h*desvS)/3.0
    desvB = (2.0*h*desvB)/45.0

    desvT = abs(desvT - resExato)
    desvS = abs(desvS - resExato)
    desvB = abs(desvB - resExato)

    !Escrevendo o valor de n e h e seus respectivos desvios
    WRITE(2, *) n, h, desvT, desvS, desvB
  END DO

  !Escrevendo no terminal quais os melhores valores de N para cada método
  !e sua justificativa
  PRINT *, "Regra do trapézio: N = 1024"
  PRINT *, "Regra de Simpson: N = 64"
  PRINT *, "Regra de Bode: N = 16"
  PRINT *, "Os melhores valores de N foram escolhidos correlacionando"
  PRINT *, "o mínimo de iterações para um desvio pequeno"

  CONTAINS
    !Função matemática a ser integrada
    FUNCTION f(y)
        REAL(8) :: y, f
        f = cos(y)*sin(y/2)
    END FUNCTION
END PROGRAM exerB

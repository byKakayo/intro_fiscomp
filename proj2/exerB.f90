PROGRAM exerB
  IMPLICIT NONE
  INTEGER :: n, i, num
  REAL(8) :: h
  REAL(8) :: resExato, desvT, desvS, desvB
  REAL(8) :: f0, f1, f2, f3, f4, f_1

  !Arquivo de entrada
  OPEN(UNIT=1,FILE="tabB_in.dat")
  !Arquivo de saida
  OPEN(UNIT=2,FILE="tabB_out.dat")

  !Ler número de valores de N
  READ(1, *)num

  resExato = cos(1.0/2) - 2.0/3 - cos(3.0/2)/3

  !Escrevedo descrição no arquivo tabB_out
  WRITE(2, *)"N ", &
  & " h ", &
  & " Regra do trapézio ", &
  & " Regra do Simpson ", &
  & " Regra de Bode ", &
  & " Derivada segunda simétrica 5 pontos ", &
  & " Derivada terceira anti-simétrica 5 pontos"

  DO i = 1, num
    READ(1, *)n
    h = 1.0/n

    f0 = 0.0
    f1 = cos(h)*sin(h/2)
    f2 = cos(2*h)*sin(h)
    f3 = cos(3*h)*sin(3*h/2)
    f4 = cos(4*h)*sin(2*h)
    f_1 = cos(-h)*sin((-h)/2)

    !Regra do trapézio
    desvT = (h/2)*(f1 + 2*f0 + f_1)
    desvT = abs(desvT - resExato)

    !Regra de Simpson
    desvS = (h/3)*(f1 + 4*f0 + f_1)
    desvS = abs(desvS - resExato)

    !Regra de Bode
    desvB = (2*h/45)*(7*f0 + 32*f1 +  12*f2 + 32*f3 + 7*f4)
    desvB = abs(desvB - resExato)

    WRITE(2, *)n, h, desvT, desvS, desvB
    PRINT *, n, h, desvT, desvS, desvB, desvT-desvS
  END DO
END PROGRAM exerB

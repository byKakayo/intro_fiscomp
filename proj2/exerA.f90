PROGRAM exerA
  IMPLICIT NONE
  INTEGER :: n, i
  REAL(8) :: h, resExatoD1, resExatoD2, resExatoD3
  REAL(8) :: f0, f1, f_1, f2, f_2
  REAL(8) :: desv1, desv2, desv3, desv4, desv5, desv6
  REAL(8), PARAMETER :: x = 1.0/5 !Definindo o valor de x

  !Arquivo de entrada
  OPEN(UNIT=1,FILE="tabA_in.dat")
  !Arquivo de saida
  OPEN(UNIT=2,FILE="tabA_out.dat")

  !Ler número de valores de h
  READ(1, *)n

  !Escrevedo descrição no arquivo tabA_out
  WRITE(2, *)"h", &
  & " Derivada simétrica 3 pontos", &
  & " Derivada para frente 2 pontos", &
  & " Derivada para trás 2 pontos", &
  & " Derivada simétrica 5 pontos", &
  & " Derivada segunda simétrica 5 pontos", &
  & " Derivada terceira anti-simétrica 5 pontos"

  !Definindo os valores "exatos" para as derivadas de primeira, segunda e terceira ordem
  resExatoD1 = (exp(x)*cos(x/2)**2) - (exp(x)*sin(x)/2)
  resExatoD2 = (-2*exp(x)*sin(x) - exp(x)*cos(x) + 2*exp(x)*cos(x/2)**2)/2
  resExatoD3 = (-2*exp(x)*sin(x) - 3*exp(x)*cos(x) + 2*exp(x)*cos(x/2)**2)/2

  !Cálculo do valor da função em x
  f0 = exp(x)*cos(x/2)**2

  DO i = 1, n
    !Lê o valor de h
    READ(1, *)h

    !Cálculo dos valores das funções de f(1), f(-1), f(2) e f(-2)
    f1 = exp(x+h)*cos((x+h)/2)**2
    f_1 = exp(x-h)*cos((x-h)/2)**2
    f2 = exp(x+2*h)*cos((x+2*h)/2)**2
    f_2 = exp(x-2*h)*cos((x-2*h)/2)**2

    !Derivada simétrica de 3 pontos
    desv1 = (f1 - f_1)/(2*h)
    desv1 = abs(resExatoD1 - desv1)

    !Derivada para frente de 2 pontos
    desv2 = (f1 - f0)/h
    desv2 = abs(resExatoD1 - desv2)

    !Derivada para trás de 2 pontos
    desv3 = (f0 - f_1)/h
    desv3 = abs(resExatoD1 - desv3)

    !Derivada simétrica de 5 pontos
    desv4 = (-f2 + 8*f1 - 8*f_1 + f_2)/(12*h)
    desv4 = abs(resExatoD1 - desv4)

    !Derivada segunda simétric de 5 pontos
    desv5 = (-f2 + 16*f1 - 30*f0 + 16*f_1 - f_2)/(12*h**2)
    desv5 = abs(resExatoD2 - desv5)

    !Derivada terceira anti-simétrica de 5 pontos
    desv6 = (f2 - 2*f1 + 2*f_1 - f_2)/(2*h**3)
    desv6 = abs(resExatoD3 - desv6)

    !Escrevendo o valor de h e seus respectivos desvios
    WRITE(2, *)h, desv1, desv2, desv3, desv4, desv5,desv6
  END DO

  !Escrevendo no terminal quais os melhores valores de h para cada método
  !e sua justificativa
  PRINT *, "Derivada simétrica de 3 pontos: h = 10^-7"
  PRINT *, "Derivada para frente de 2 pontos: h = 10^-5"
  PRINT *, "Derivada para trás de 2 pontos: h = 10^-7"
  PRINT *, "Derivada simétrica de 5 pontos: h = 5.10^-4"
  PRINT *, "Derivada segunda simétric de 5 pontos: h = 10^-2"
  PRINT *, "Derivada terceira anti-simétrica de 5 pontos: h = 10^-3"
  PRINT *, "Os melhores valores de h foram escolhidos analisando o comportamento do erro em função de h,"
  PRINT *, "em determinado valor, o erro deixa de seguir o comportamento esperado"
END PROGRAM exerA

PROGRAM exerC
  IMPLICIT NONE
  INTEGER :: n, i
  REAL(8), DIMENSION(3) :: xn, xs, xd, xi, hs, hd, sinal

  !Arquivo de saida
  OPEN(UNIT=2,FILE="tabC_out.dat")

  !Lê o número de iterações
  READ(*,*)n

  !Escrevedo descrição no arquivo tabC_out
  WRITE(2, *)"iter ", &
  & " dir1 ", &
  & " dir2 ", &
  & " dir3 ", &
  & " NR1 ", &
  & " NR2 ", &
  & " NR3 ", &
  & " sec1 ", &
  & " sec2 ", &
  & " sec3"

  !Definindo os chutes iniciais para as raízes
  xn = (/-1.0, 0.5, 1.6/)
  xs = xn
  xd = xn
  !Definindo o passo para o método da secante
  hs = 1d-8
  xi = xs+hs
  !Definindo o passo para o método direto
  hd = 5d-2

  DO i = 1, n
    !Método de Newton-Raphson
    xn = xn - f(xn)/df(xn)

    !Método da secante
    xs = xs - (f(xs)*(xs - xi))/(f(xs) - f(xi))

    !Método Direto
    !Acresce um passo(hd) a x e verifica a mudança de sinal da função
    !se trocar de sinal, passou pela raíz, então o valor próximo da
    !raíz é dado pelo penúltimo valor de x testado
    sinal = f(xd+hd)*f(xd)
    sinal = sinal/abs(sinal)
    sinal = (sinal+1)/2
    xd = xd + sinal*hd

    !Escrevendo a iteração e os valores encontrados para as raízes
    WRITE(2, *) i, xd, xn, xs
  END DO

CONTAINS
  !Função matemática cujas raízes serão encontradas
  FUNCTION f(x)
    IMPLICIT NONE
    REAL(8), DIMENSION(3) :: x, f
    f = 2*x**3 - 4*x**2 - x + 2
  END FUNCTION f
  !Função correspondente a derivada de f(x)
  FUNCTION df(x)
    IMPLICIT NONE
    REAL(8), DIMENSION(3) :: x, df
    df = 6*x**2 - 8*x - 1
  END FUNCTION df
END PROGRAM exerC

PROGRAM exer02
  IMPLICIT NONE
  INTEGER(8) :: n, i, Nlim, naux, x !Definindo variáveis para cálculo do fatorial e aproximação de stirling
  REAL(8) :: lnFat
  REAL(8), parameter :: pi=4.0d0*atan(1.0d0) !Definindo uma constante com o valor de pi com precisão dupla

  n = 1 !Valor inicial de n
  i = 1 !Valor inicial do contador

  !Encontra valor inteiro máximo de n
  DO WHILE (n > 0)
    i = i + 1
    n = n * 2
  END DO
  Nlim = 2**(i-1)-1

  i = 0 !Valor inicial do termo
  n = 1 !Valor inicial do fatorial
  naux = 1 !Armazena valor anterior do fatorial calculado
  !Calcula o fatorial máximo que variável n armazena
  DO WHILE ((n < Nlim) .AND. (n > 0))
    i = i + 1
    naux = n
    n = n*i
  END DO
  !Valor de i antes da parada do while
  i = (i-1)
  !Escreve no terminal o valor máximo de n, fatorial máximo de n e o número do fatorial
  PRINT *, Nlim, naux, i
  !Valor inicial do fatorial
  n = 1
  !Calculo a aproximação de stirling e o fatorial de cada termo
  DO x = 1, i
    lnFat = x*log(real(x,8)) - x + 0.5*log(2.0*pi*x) !Aproximação de Stirling (ln(fatorial(n)))
    n = n*x !Cálculo do fatorial
    !Escreve no terminal n, n!, ln(n!) e aproximação de stirling(n)
    PRINT *, x, n, log(real(n,8)), lnFat
  END DO
  !fim do programa
END PROGRAM exer02

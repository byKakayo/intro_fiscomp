PROGRAM exer03
  IMPLICIT NONE
  INTEGER(8) :: fat, i, x
  REAL(4) :: x1, aprox1, aux1 !Variáveis utilizadas p/ aproximação de seno com precisão simples
  REAL(8) :: x2, aprox2, aux2 !Variáveis utilizadas p/ aproximação de seno com precisão dupla

  DO x=1, 4 !Loop que varia o valor de X de 1 a 4
    x1 = x*0.1e0 !Multiplica X por 0.1 p/ obter os valores pedidos no exercício
    aprox1 = 2*epsilon(aprox1) !Aproximação do erro inicial
    i = 1 !Contador
    !Gera um novo termo da série de Taylor enquanto o último termo for maior que o menor valor que pode ser representado com precisão simples
    DO WHILE (ABS(aprox1) > epsilon(aprox1))
      aux1 = aprox1 !Guarda o valor do termo anterior
      aprox1 = (x1**i)/fat(i) !Calcula o modulo do novo termo da série
      i = i+2 !Atualiza contador
    END DO

    x2 = x*0.1d0
    aprox2 = 2*epsilon(aprox2)
    i = 1
    !Gera um novo termo da série de Taylor enquanto o último termo for maior que o menor valor que pode ser representado com precisão dupla
    DO WHILE (ABS(aprox2) > epsilon(aprox2))
      aux2 = aprox2 !Guarda o valor do termo anterior
      aprox2 = (x2**i)/fat(i) !Calcula o modulo o novo termo da série
      i = i+2 !Atualiza contador
    END DO

    PRINT *, x1, aux1, aux2 !Escreve no terminal o valor de X, precisão simples, precisão dupla
  END DO
  PRINT *, ''
  PRINT *, ''
  PRINT *, 'É uma boa aproximação dado que em poucas iterações obtevesse um '
  PRINT *, 'erro muito próximo da precisão da mantissa'
END PROGRAM exer03

INTEGER(8) FUNCTION fat(x) !Função que calcula o fatorial do valor dado
  IMPLICIT NONE
  INTEGER(8) :: x,j
  fat = 1
  DO j=1,x
    fat = fat*j
  END DO
END FUNCTION fat

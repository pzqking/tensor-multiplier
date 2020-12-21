PROGRAM MAIN


Double precision,allocatable::A(:,:),B(:,:),C(:,:)
INTEGER::MT1,MT2,NT3,MT3,NT2,I,N1,N2,NT1,M1,M2,MC,NC
CHARACTER::ASW

100 PRINT*,"¸Ã³ÌÐòÎª¾ØÕó×Ô¶¯Ïà³ËÆ÷£¬¿É½â¾öB*AÐÍÎÊÌâ£¬ÇëÊäÈëAµÄ³¤Óë¿í£º"!Òýµ¼
PRINT*,"³¤"
READ*,N1
PRINT*,"¿í"
READ*,M1
ALLOCATE(A(N1,M1))
!¶¨ÒåA¾ØÕó
NT1=1
MT1=1
DO MT1=1,M1
  DO NT1=1,N1
    PRINT*,"ÇëÊäÈëa",MT1,"",NT1
    READ(*,*)A(NT1,MT1)
    
  END DO
  NT1=1

END DO 
NT1=1
MT1=1
PRINT*,"¾ØÕóAµÄÏîÓÐ£º"
DO WHILE(MT1<=M1)
  DO WHILE(NT1<=N1)
   PRINT*,"a",MT1,"",NT1,"=",A(MT1,NT1)
   NT1=NT1+1
  END DO
  NT1=1
  MT1=MT1+1
END DO
PRINT*,"ÇëÊäÈëBµÄ³¤Óë¿í£º"
PRINT*,"³¤"
READ*,N2
PRINT*,"¿í"
READ*,M2
ALLOCATE(B(N2,M2))
NT2=1
MT2=1
DO WHILE(MT2<=M2)
  DO WHILE(NT2<=N2)
    PRINT*,"ÇëÊäÈëb",MT2,"",NT2
    READ*,B(MT2,NT2)
    NT2=NT2+1
  END DO
  NT2=1
  MT2=MT2+1
END DO 
NT2=1
MT2=1
PRINT*,"¾ØÕóBµÄÏîÓÐ£º"
DO WHILE(MT2<=M2)
  DO WHILE(NT2<=N2)
   PRINT*,"b",MT2,"",NT2,"=",B(MT2,NT2)
   NT2=NT2+1
  END DO
  NT2=1
  MT2=MT2+1
END DO
NC=N2
MC=M1
NT3=1
MT3=1
MT1=1
MT2=1
NT1=1
NT2=1
I=1
ALLOCATE(C(NC,MC))
DO WHILE(MT3<=MC)
  DO WHILE(NT3<=NC)
    IF(NC<MC) THEN
      DO WHILE(I<=NC)
        C(NT3,MT3)=A(NT3,I)*B(I,NT3)+C(NT3,MT3)
        I=I+1
      END DO
      I=1
    END IF
    IF (NC>=MC) THEN
      DO WHILE(I<=MC)
        C(NT3,MT3)=A(NT3,I)*B(I,NT3)+C(NT3,MT3)
        I=I+1
      END DO
      I=1
    END IF
    NT3=NT3+1
  END DO
  NT3=1
  MT3=MT3+1
END DO
NT3=1
MT3=1
PRINT*,"¾ØÕóCµÄÏîÓÐ£º"
DO WHILE(MT3<=MC)
  DO WHILE(NT3<=NC)
   PRINT*,"c",MT3,"",NT3,"=",C(MT3,NT3)
   NT3=NT3+1
  END DO
  NT3=1
  MT3=MT3+1
END DO

DEALLOCATE(A,B,C)
PRINT *,"»¹À´£(Y/N"
300 READ(*,*),ASW
 IF(ASW=="Y")THEN
  GOTO 100
  ELSE IF(ASW=="N")THEN
  GOTO 200
  ELSE
  PRINT*,"ÐÖµÜ£¬ÇëÊäÈëY»òÕßNÖÐµÄÒ»¸ö£¬ÔÙÀ´Ò»±éÊäY£¬·´Ö®ÊäÈëN£¬Ð»Ð»¡£"
  GOTO 300
  END IF
200 PRINT*,"±¾³ÌÐòÓÉpzqking¿ª·¢£¬Ð»Ð»Ê¹ÓÃ"
READ(*,*)
END PROGRAM MAIN
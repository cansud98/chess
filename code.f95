!==============================================
! This chess code is written by Cansu Duzgun 
! as a final project for BIL112E, Spring 2018.
!       @Istanbul Technical University
! 
! Updates (by Feb 2024):
! - Variable names are adapted to English.
! - Knight codes are simplified.
!
! TODO:
! - Minor coding mistakes need to be fixed.
! - Special chess movements can be adapted
!   (e.g., castling).
!==============================================

program chess_cansu
parameter (m=8)
integer a(m,m)

print*, "********************************************************"
print*, "Hello! Note that the game starts with positive numbers."
print*, "Please check the instructions before playing the game."
print*, "          Practice daily, the best never rest!"
print*, "********************************************************"

open(11,file="board.dat")

do i=1,m

read(11,*) (a(i,j), j=1,m)
write(*,25) (a(i,j), j=1,m)

25 format(8(2x,i2))

enddo

     do i=1,50
     icount=mod(i,2)

if(icount.eq.1)  print*, "White's turn..."
if(icount.eq.0)  print*, "Black's turn..."

 
print*, "which piece do you want to move?"
read*, i1,j1

print*, "where do you want to move it?"
read*, i2,j2

if(a(i1,j1).eq.1.and.icount.eq.1) then
call pawn(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.-1.and.icount.eq.0) then
call pawn2(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.2.and.icount.eq.1) then
call rook(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.-2.and.icount.eq.0) then
call rook2(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.3.and.icount.eq.1) then
call knight(a,m,i1,i2,j1,j2) 

elseif(a(i1,j1).eq.-3.and.icount.eq.0) then
call knight2(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.4.and.icount.eq.1) then
call bishop(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.-4.and.icount.eq.0) then
call bishop2(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.6.and.icount.eq.1) then
call king(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.-6.and.icount.eq.0) then
call king2(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.5.and.icount.eq.1) then
call queen(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.-5.and.icount.eq.0) then
call queen2(a,m,i1,i2,j1,j2)

endif
enddo
end



!WHITE (POSITIVE) PAWN CODES

subroutine pawn(a,m,i1,i2,j1,j2)
integer a(m,m)

11 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.idiff.eq.-1.and.jdiff.eq.0) then
a(i1,j1)=0
a(i2,j2)=1
elseif(a(i2,j2).eq.0.and.idiff.eq.-2.and.jdiff.eq.0.and.i1.eq.7) then
a(i1,j1)=0
a(i2,j2)=1
elseif(a(i1,j1).eq.1.and.a(i2,j2).lt.0.and.idiff.eq.-1.and.abs(jdiff).eq.1) then
a(i1,j1)=0
a(i2,j2)=1
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 11
endif

do i=1,m
write(*,25) (a(i,j), j=1,m)
25 format(8(2x,i2))
enddo 
end



!BLACK (NEGATIVE) PAWN CODES

subroutine pawn2(a,m,i1,i2,j1,j2)
integer a(m,m)

12 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.idiff.eq.1.and.jdiff.eq.0) then
a(i1,j1)=0
a(i2,j2)=-1
elseif(a(i2,j2).eq.0.and.idiff.eq.2.and.jdiff.eq.0.and.i1.eq.2) then
a(i1,j1)=0
a(i2,j2)=-1
elseif(a(i1,j1).eq.-1.and.a(i2,j2).gt.0.and.idiff.eq.1.and.abs(jdiff).eq.1) then
a(i1,j1)=0
a(i2,j2)=-1
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 12
endif


do i=1,m
write (*,30) (a(i,j), j=1,m)
30 format(8(2x,i2))
enddo
end



!WHITE (POSITIVE) ROOK CODES

subroutine rook(a,m,i1,i2,j1,j2)
integer a(m,m)

21 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.abs(idiff).gt.0.and.jdiff.eq.0) then
a(i1,j1)=0
a(i2,j2)=2
elseif(a(i1,j1).eq.2.and.a(i2,j2).lt.0.and.abs(idiff).gt.0.and.jdiff.eq.0) then
a(i1,j1)=0
a(i2,j2)=2
elseif(a(i2,j2).eq.0.and.idiff.eq.0.and.abs(jdiff).gt.0) then
a(i1,j1)=0
a(i2,j2)=2
elseif(a(i1,j1).eq.2.and.a(i2,j2).lt.0.and.idiff.eq.0.and.abs(jdiff).gt.0) then
a(i1,j1)=0
a(i2,j2)=2
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 21
endif

do i=1,m
write (*,35) (a(i,j), j=1,m)
35 format(8(2x,i2))
enddo
end



!BLACK (NEGATIVE) ROOK CODES

subroutine rook2(a,m,i1,i2,j1,j2)
integer a(m,m)

22 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.abs(idiff).gt.0.and.jdiff.eq.0) then
a(i1,j1)=0
a(i2,j2)=-2
elseif(a(i1,j1).eq.-2.and.a(i2,j2).gt.0.and.abs(idiff).gt.0.and.jdiff.eq.0) then
a(i1,j1)=0
a(i2,j2)=-2
elseif(a(i2,j2).eq.0.and.idiff.eq.0.and.abs(jdiff).gt.0) then
a(i1,j1)=0
a(i2,j2)=-2
elseif(a(i1,j1).eq.-2.and.a(i2,j2).gt.0.and.idiff.eq.0.and.abs(jdiff).gt.0) then
a(i1,j1)=0
a(i2,j2)=-2
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 22
endif

do i=1,m
write (*,40) (a(i,j), j=1,m)
40 format(8(2x,i2))
enddo
end




!WHITE (POSITIVE) BISHOP CODES

subroutine bishop(a,m,i1,i2,j1,j2)
integer a(m,m)

31 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.abs(idiff).eq.abs(jdiff)) then
a(i1,j1)=0
a(i2,j2)=4
elseif(a(i1,j1).eq.4.and.a(i2,j2).lt.0.and.abs(idiff).eq.abs(jdiff)) then
a(i1,j1)=0
a(i2,j2)=4
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 31
endif

do i=1,m
write (*,45) (a(i,j), j=1,m)
45 format(8(2x,i2))
enddo
end



!BLACK (NEGATIVE) BISHOP CODES

subroutine bishop2(a,m,i1,i2,j1,j2)
integer a(m,m)

32 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.abs(idiff).eq.abs(jdiff)) then
a(i1,j1)=0
a(i2,j2)=-4
elseif(a(i1,j1).eq.-4.and.a(i2,j2).gt.0.and.abs(idiff).eq.abs(jdiff)) then
a(i1,j1)=0
a(i2,j2)=-4
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 32
endif

do i=1,m
write (*,50) (a(i,j), j=1,m)
50 format(8(2x,i2))
enddo
end



!WHITE (POSITIVE) KNIGHT CODES

subroutine knight(a,m,i1,i2,j1,j2)
integer a(m,m)

41 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.abs(idiff).eq.2.and.abs(jdiff).eq.1)then
a(i1,j1)=0
a(i2,j2)=3
elseif(a(i1,j1).eq.3.and.a(i2,j2).lt.0.and.abs(idiff).eq.2.and.abs(jdiff).eq.1) then
a(i1,j1)=0
a(i2,j2)=3
elseif(a(i2,j2).eq.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.2)then
a(i1,j1)=0
a(i2,j2)=3
elseif(a(i1,j1).eq.3.and.a(i2,j2).lt.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.2) then
a(i1,j1)=0
a(i2,j2)=3
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 41
endif

do i=1,m
write (*,50) (a(i,j), j=1,m)
50 format(8(2x,i2))
enddo
end



! BLACK (NEGATIVE) KNIGHT CODES

subroutine knight2(a,m,i1,i2,j1,j2)
integer a(m,m)

42 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.abs(idiff).eq.2.and.abs(jdiff).eq.1)then
a(i1,j1)=0
a(i2,j2)=-3
elseif(a(i1,j1).eq.-3.and.a(i2,j2).gt.0.and.abs(idiff).eq.2.and.abs(jdiff).eq.1) then
a(i1,j1)=0
a(i2,j2)=-3
elseif(a(i2,j2).eq.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.2)then
a(i1,j1)=0
a(i2,j2)=-3
elseif(a(i1,j1).eq.-3.and.a(i2,j2).gt.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.2) then
a(i1,j1)=0
a(i2,j2)=-3
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 42
endif

do i=1,m
write (*,55) (a(i,j), j=1,m)
55 format(8(2x,i2))
enddo
end



! WHITE (POSITIVE) KING CODES 

subroutine king(a,m,i1,i2,j1,j2)
integer a(m,m)

51 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.0) then
a(i1,j1)=0
a(i2,j2)=6
elseif(a(i1,j1).eq.6.and.a(i2,j2).lt.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.0) then
a(i1,j1)=0
a(i2,j2)=6
elseif(a(i2,j2).eq.0.and.abs(idiff).eq.0.and.abs(jdiff).eq.1) then
a(i1,j1)=0
a(i2,j2)=6
elseif(a(i1,j1).eq.6.and.a(i2,j2).lt.0.and.abs(idiff).eq.0.and.abs(jdiff).eq.1) then
a(i1,j1)=0
a(i2,j2)=6
elseif(a(i2,j2).eq.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.1) then
a(i1,j1)=0
a(i2,j2)=6
elseif(a(i1,j1).eq.6.and.a(i2,j2).lt.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.1)then
a(i1,j1)=0
a(i2,j2)=6
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 51
endif

do i=1,m
write(*,60) (a(i,j), j=1,m)
60 format(8(2x,i2))
enddo
end



!BLACK (NEGATIVE) KING CODES

subroutine king2(a,m,i1,i2,j1,j2)
integer a(m,m)

52 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.0) then
a(i1,j1)=0
a(i2,j2)=-6
elseif(a(i1,j1).eq.-6.and.a(i2,j2).gt.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.0) then
a(i1,j1)=0
a(i2,j2)=-6
elseif(a(i2,j2).eq.0.and.abs(idiff).eq.0.and.abs(jdiff).eq.1) then
a(i1,j1)=0
a(i2,j2)=-6
elseif(a(i1,j1).eq.-6.and.a(i2,j2).gt.0.and.idiff.eq.0.and.abs(jdiff).eq.1) then
a(i1,j1)=0
a(i2,j2)=-6
elseif(a(i2,j2).eq.0.and.abs(idiff).eq.abs(jdiff)) then
a(i1,j1)=0
a(i2,j2)=-6
elseif(a(i1,j1).eq.-6.and.a(i2,j2).gt.0.and.abs(idiff).eq.1.and.abs(jdiff).eq.1)then
a(i1,j1)=0
a(i2,j2)=-6
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 52
endif

do i=1,m
write(*,65) (a(i,j), j=1,m)
65 format(8(2x,i2))
enddo
end



!WHITE (POSITIVE) QUEEN CODES

subroutine queen(a,m,i1,i2,j1,j2)
integer a(m,m)

61 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.abs(idiff).gt.0.and.jdiff.eq.0) then
a(i1,j1)=0
a(i2,j2)=5
elseif(a(i1,j1).eq.5.and.a(i2,j2).lt.0.and.abs(idiff).gt.0.and.jdiff.eq.0) then
a(i1,j1)=0
a(i2,j2)=5
elseif(a(i2,j2).eq.0.and.idiff.eq.0.and.abs(jdiff).gt.0) then
a(i1,j1)=0
a(i2,j2)=5
elseif(a(i1,j1).eq.5.and.a(i2,j2).lt.0.and.idiff.eq.0.and.abs(jdiff).gt.0) then
a(i1,j1)=0
a(i2,j2)=5
elseif(a(i2,j2).eq.0.and.abs(idiff).eq.abs(jdiff)) then
a(i1,j1)=0
a(i2,j2)=5
elseif(a(i1,j1).eq.5.and.a(i2,j2).lt.0.and.abs(idiff).eq.abs(jdiff)) then
a(i1,j1)=0
a(i2,j2)=5
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 61
endif

do i=1,m
write (*,70) (a(i,j), j=1,m)
70 format(8(2x,i2))
enddo
end



!BLACK (NEGATIVE) QUEEN CODES

subroutine queen2(a,m,i1,i2,j1,j2)
integer a(m,m)

62 idiff=i2-i1
jdiff=j2-j1

if(a(i2,j2).eq.0.and.abs(idiff).gt.0.and.jdiff.eq.0) then
a(i1,j1)=0
a(i2,j2)=-5
elseif(a(i1,j1).eq.-5.and.a(i2,j2).gt.0.and.abs(idiff).gt.0.and.jdiff.eq.0) then
a(i1,j1)=0
a(i2,j2)=-5
elseif(a(i2,j2).eq.0.and.idiff.eq.0.and.abs(jdiff).gt.0) then
a(i1,j1)=0
a(i2,j2)=-5
elseif(a(i1,j1).eq.-5.and.a(i2,j2).gt.0.and.idiff.eq.0.and.abs(jdiff).gt.0) then
a(i1,j1)=0
a(i2,j2)=-5
elseif(a(i2,j2).eq.0.and.abs(idiff).eq.abs(jdiff)) then
a(i1,j1)=0
a(i2,j2)=-5
elseif(a(i1,j1).eq.-5.and.a(i2,j2).gt..0.and.abs(idiff).eq.abs(jdiff)) then
a(i1,j1)=0
a(i2,j2)=-5
else
print*, "you made an incorrect move, please try again."
print*, "which piece do you want to move?"
read*, i1,j1
print*, "where do you want to move it?"
read*, i2,j2
goto 62
endif

do i=1,m
write (*,70) (a(i,j), j=1,m)
70 format(8(2x,i2))
enddo
end 

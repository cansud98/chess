program satranc_cansu
parameter (m=8)
integer a(m,m)

open(11,file="tahta.dat")

do i=1,m

read(11,*) (a(i,j), j=1,m)
write(*,25) (a(i,j), j=1,m)

25 format(8(2x,i2))

enddo

     do i=1,50
     isayi=mod(i,2)

if(isayi.eq.1) print*, "sira beyaz taslarda"
if(isayi.eq.0) print*, "sira siyah taslarda"


print*, "hangi tasi hareket ettirmek istiyorsunuz?"
read*, i1,j1

print*, "nereye?"
read*, i2,j2

if(a(i1,j1).eq.1.and.isayi.eq.1) then
call piyon(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.-1.and.isayi.eq.0) then
call piyon2(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.2.and.isayi.eq.1) then
call kale(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.-2.and.isayi.eq.0) then
call kale2(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.3.and.isayi.eq.1) then
call at(a,m,i1,i2,j1,j2) 

elseif(a(i1,j1).eq.-3.and.isayi.eq.0) then
call at2(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.4.and.isayi.eq.1) then
call fil(a,m,i1,i2,j1,j2)

elseif(a(i1,j1).eq.-4.and.isayi.eq.0) then
call fil2(a,m,i1,i2,j1,j2)

!elseif(a(i1,j1).eq.5.and.isayi.eq.1) then
!call vezir(a,m,i1,i2,j1,j2)

!elseif(a(i1,j1).eq.-5.and.isayi.eq.0) then
!call vezir2(a,m,i1,i2,j1,j2)

!elseif(a(i1,j1).eq.6.and.isayi.eq.1) then
!call sah(a,m,i1,i2,j1,j2)

!elseif(a(i1,j1).eq.-6.and.isayi.eq.0) then
!call sah2(a,m,i1,i2,j1,j2)


endif

enddo


end

!BEYAZ PİYON KODLARİ

subroutine piyon(a,m,i1,i2,j1,j2)
integer a(m,m)


ifark=i2-i1
jfark=j2-j1

if(a(i2,j2).eq.0.and.ifark.eq.1.and.jfark.eq.0) then
a(i1,j1)=0
a(i2,j2)=1
elseif(a(i1,j1).eq.1.and.a(i2,j2).lt.0.and.ifark.eq.1.and.abs(jfark).eq.1) then
a(i1,j1)=0
a(i2,j2)=1
endif


do i=1,m

write(*,25) (a(i,j), j=1,m)

25 format(8(2x,i2))

enddo 

end

!SİYAH PİYON KODLARİ

subroutine piyon2(a,m,i1,i2,j1,j2)
integer a(m,m)


ifark=i2-i1
jfark=j2-j1

if(a(i2,j2).eq.0.and.ifark.eq.-1.and.jfark.eq.0) then
a(i1,j1)=0
a(i2,j2)=-1
elseif(a(i1,j1).eq.-1.and.a(i2,j2).gt.0.and.abs(jfark).eq.1) then
a(i1,j1)=0
a(i2,j2)=-1

endif


do i=1,m

write (*,30) (a(i,j), j=1,m)

30 format(8(2x,i2))

enddo
end



!BEYAZ KALE KODLARİ
!beyaz kaleye tas yeme kodu ekledim ama calismiyor, neden?

subroutine kale(a,m,i1,i2,j1,j2)
integer a(m,m)

ifark=i2-i1
jfark=j2-j1

if(a(i2,j2).eq.0.and.abs(ifark).gt.0.and.jfark.eq.0) then
a(i1,j1)=0
a(i2,j2)=2
!elseif(a(i1,j1).eq.2.and.a(i2,j2).lt.0.and.abs(ifark).gt.0.and.jfark.eq.0) then
!a(i1,j1)=0
!a(i2,j2)=2
endif

do i=1,m

write (*,35) (a(i,j), j=1,m)

35 format(8(2x,i2))

enddo
end

!SİYAH KALE KODLARİ

subroutine kale2(a,m,i1,i2,j1,j2)
integer a(m,m)

ifark=i2-i1
jfark=j2-j1

if(a(i2,j2).eq.0.and.ifark.lt.0.and.jfark.eq.0) then
a(i1,j1)=0
a(i2,j2)=-2
endif

do i=1,m

write (*,40) (a(i,j), j=1,m)

40 format(8(2x,i2))

enddo
end




!BEYAZ FİL KODLARİ

subroutine fil(a,m,i1,i2,j1,j2)
integer a(m,m)

ifark=i2-i1
jfark=j2-j1

if(a(i2,j2).eq.0.and.abs(ifark).eq.abs(jfark)) then
a(i1,j1)=0
a(i2,j2)=4
endif

do i=1,m

write (*,45) (a(i,j), j=1,m)

45 format(8(2x,i2))

enddo
end

!SİYAH FİL KODLARİ

subroutine fil2(a,m,i1,i2,j1,j2)
integer a(m,m)

ifark=i2-i1
jfark=j2-j1

if(a(i2,j2).eq.0.and.abs(ifark).eq.abs(jfark)) then
a(i1,j1)=0
a(i2,j2)=-4
endif

do i=1,m

write (*,50) (a(i,j), j=1,m)

50 format(8(2x,i2))

enddo
end




!BEYAZ AT KODLARİ

subroutine at(a,m,i1,i2,j1,j2)
integer a(m,m)

ifark=i2-i1
jfark=j2-j1

if(a(i2,j2).eq.0.and.ifark.eq.2.and.jfark.eq.1)then
a(i1,j1)=0
a(i2,j2)=3
endif


if(a(i2,j2).eq.0.and.ifark.eq.2.and.jfark.eq.-1)then
a(i1,j1)=0
a(i2,j2)=3
endif

if(a(i2,j2).eq.0.and.ifark.eq.1.and.jfark.eq.2)then
a(i1,j1)=0
a(i2,j2)=3
endif


if(a(i2,j2).eq.0.and.ifark.eq.-1.and.jfark.eq.2)then
a(i1,j1)=0
a(i2,j2)=3
endif


if(a(i2,j2).eq.0.and.ifark.eq.-2.and.jfark.eq.1)then
a(i1,j1)=0
a(i2,j2)=3
endif


if(a(i2,j2).eq.0.and.ifark.eq.-2.and.jfark.eq.-1)then
a(i1,j1)=0
a(i2,j2)=3
endif

if(a(i2,j2).eq.0.and.ifark.eq.1.and.jfark.eq.-2)then
a(i1,j1)=0
a(i2,j2)=3
endif


if(a(i2,j2).eq.0.and.ifark.eq.-1.and.jfark.eq.-2)then
a(i1,j1)=0
a(i2,j2)=3
endif


do i=1,m

write (*,50) (a(i,j), j=1,m)

50 format(8(2x,i2))

enddo
end


!SİYAH AT KODLARİ
!siyah at hareket etmiyor :((((

subroutine at2(a,m,i1,i2,j1,j2)
integer a(m,m)

ifark=i2-i1
jfark=j2-j1

if(a(i2,j2).eq.0.and.ifark.eq.2.and.jfark.eq.1)then
a(i1,j1)=0
a(i2,j2)=-3
endif


if(a(i2,j2).eq.0.and.ifark.eq.2.and.jfark.eq.-1)then
a(i1,j1)=0
a(i2,j2)=-3
endif

if(a(i2,j2).eq.0.and.ifark.eq.1.and.jfark.eq.2)then
a(i1,j1)=0
a(i2,j2)=-3
endif


if(a(i2,j2).eq.0.and.ifark.eq.-1.and.jfark.eq.2)then
a(i1,j1)=0
a(i2,j2)=-3
endif


if(a(i2,j2).eq.0.and.ifark.eq.-2.and.jfark.eq.1)then
a(i1,j1)=0
a(i2,j2)=-3
endif


if(a(i2,j2).eq.0.and.ifark.eq.-2.and.jfark.eq.-1)then
a(i1,j1)=0
a(i2,j2)=-3
endif

if(a(i2,j2).eq.0.and.ifark.eq.1.and.jfark.eq.-2)then
a(i1,j1)=0
a(i2,j2)=-3
endif


if(a(i2,j2).eq.0.and.ifark.eq.-1.and.jfark.eq.-2)then
a(i1,j1)=0
a(i2,j2)=-3
endif

do i=1,m

write (*,55) (a(i,j), j=1,m)

55 format(8(2x,i2))

enddo
end


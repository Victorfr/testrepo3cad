Dim tmpObr
Dim index
Dim xamb: Set xamb=amb
dim minXl
dim maxxdl
'msgbox xamb.box(0).varregola("_krepeg")
Public Function jobmain(var)
aa="010,013,012,018,015,026,042,046,047,044,057,058,059,060,061,063 \n" ' список типологий пересечения 1-основная - остальные с чем пересекается, с новой строки начинается новое выполнение скрипта
aa=aa & "011,013,012,018,015,026,043,046,047,044,057,058,059,060,061,063 \n"
aa=aa & "013,011,010,014,018 \n"
aa=aa & "012,011,010,014,018,047,027 \n"
aa=aa & "014,012,013,026,015,042,043,057 \n"
aa=aa & "015,010,011,014 \n"
aa=aa & "026,010,011,014 \n"
aa=aa & "046,010 \n"
aa=aa & "046,011 \n"
aa=aa & "106,107,108 \n"
aa=aa & "107,106,108 \n"
aa=aa & "108,106,107 \n"
aa=aa & "027,047,012,102 \n"
aa=aa & "102,027,047 \n"
aa=aa & "100,047,044 \n"
aa=aa & "301,059 \n"
aa=aa & "303,059 \n"
aa=aa & "600,603 \n"
aa=aa & "605,603,601,602 \n"
aa=aa & "603,605,600\n"
aa=aa & "601,605\n"
aa=aa & "602,605\n"
'aa=aa & "005,002,003,007\n"

JobMain=replace(aa,"\n",vbcrlf)
index=0
minXl=999999
maxxdl=0
End Function

Public Sub jobini(p,var)
tmpObr=""
if (p.tipo=600 or p.tipo=605 or p.tipo=603 or p.tipo=601 or p.tipo=602) then
p.parametro(6)=""

end if 
End Sub

Public Sub jobpannello(p,p1,var,lati,pl,pa,pp,ax,ay,az)
'msgbox p.da&"x" & p.dl & "x" & p.dp & "|" & p1.da & "x" & p1.dl & "x" & p1.dp
'msgbox p.pl-p1.pl & "x" & p.pa-p1.pa & "x" & p.pp-p1.pp
'msgbox p1.pl-p.pl & "x" & p1.pa-p.pa & "x" & p1.pp-p.pp & "t" & p.tipo
'msgbox lati & " " & p.tipo &" " & p1.tipo
'вычислим координаты типологии, с которой пересекается текущая, относительно текущей (без учета толщин типологий)
 
xl=p1.pl-p.pl ' разница смещений второстепенной типологии относительно главной по ширине
xa=p1.pa-p.pa ' разница смещений второстепенной типологии относительно главной по высоте
xp=p1.pp-p.pp ' разница смещений второстепенной типологии относительно главной по глубине

'dim strL :set strl=lati
'если типологии из одного бокса
'p1.dl, p1.da, p1.dp - размеры второстепенной типологии
if p.tipo=014 and p1.tipo=057 then

'msgbox lati
 
'msgbox xa
end if
if (p.tipo=600 or p.tipo=605 or p.tipo=603 or p.tipo=601 or p.tipo=602) then

p.parametro(6)=p.parametro(6) & p1.parametro(8) &"x" &lati & "x"& p1.tipo & ";"

p.flJob=1
end if 

''if (p1.tipo=18) and (p.tipo=10 or p.tipo=11) then 
 'if minXl>xl then minxl=xl
 'if maxxdl<p1.pl+p1.dl then maxxdl=p1.pl+p1.dl
 'p.parametro(9)=minxL & "X" & maxxdl
'end if 

if ( p1.tipo=063) and (p1.xbox.id<>p.xbox.id) then
'msgbox p1.dimoa & p1.dimol & p1.dimop 
strParamSkaf = p.xbox.varregola("varParamSkaf")
'msgbox 1
tmpobr=tmpobr&"|"&p1.tipo & "x" & xl & "x" & xa & "x" & xp &"x"& p1.dl &"x" & p1.da & "x" & p1.dp & "x" & lati & "x" & p1.parametro(8) & "x" & p.tipo & "x" & strParamSkaf
p.xbox.varregola("provobr")="X"
index=index+1'количество обработок

end if
if (p.xbox.id=p1.xbox.id) then 
strParamSkaf = p.xbox.varregola("varParamSkaf")
if (p.tipo)=301 then
'msgbox p.parametro(8)
end if 
tmpobr=tmpobr&"|"&p1.tipo & "x" & xl & "x" & xa & "x" & xp &"x"& p1.dl &"x" & p1.da & "x" & p1.dp & "x" & lati & "x" & p1.parametro(8) & "x" & p.tipo & "x" & strParamSkaf
p.xbox.varregola("provobr")="X"
index=index+1'количество обработок
 
end if 

End Sub

Public Sub jobfin(p,var)

'msgbox index
'msgbox tmpObr
'msgbox p.tipo & " " & tmpObr
 
tmpObr=replace(tmpObr,",",".")

p.parametro(7)=index&tmpObr
'msgbox p.parametro(7)
index=0
p.flJob=1
p.xbox.flJob=2
End Sub
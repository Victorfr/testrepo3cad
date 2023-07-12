Dim tmpObr
Dim index
Dim xamb: Set xamb=amb
'msgbox xamb.box(0).varregola("_krepeg")
Public Function jobmain(var)
aa="030,034 \n" ' список типологий пересечения 1-основная - остальные с чем пересекается, с новой строки начинается новое выполнение скрипта
aa="034,030 \n"
'aa=aa & "005,002,003,007\n"

JobMain=replace(aa,"\n",vbcrlf)
index=0

End Function

Public Sub jobini(p,var)
tmpObr=""

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
if (p.xbox.id=p1.xbox.id) then 

tmpobr=tmpobr&"|"&p1.tipo & "x" & xl & "x" & xa & "x" & xp &"x"& p1.dl &"x" & p1.da & "x" & p1.dp & "x" & lati & "x" & p1.parametro(8) & "x" & p.tipo
p.xbox.varregola("provobr")="X"
end if 
index=index+1'количество обработок


End Sub

Public Sub jobfin(p,var)

'msgbox index
'msgbox tmpObr
'msgbox p.tipo & " " & tmpObr
'tmpObr=replace(tmpObr,",",".")
'p.parametro(7)=index&tmpObr

'msgbox p.parametro(7)
index=0
p.flJob=1
p.xbox.flJob=2
End Sub
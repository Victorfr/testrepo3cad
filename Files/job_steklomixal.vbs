option explicit
dim xamb

public function jobmain(var)

dim i 
dim x 
dim testbox
dim AllDopFur ,idsection,idsteklosection
dim curL,curA,maxL,maxA
set xamb=amb
Dim typeShkaf
Dim size
Dim stekloSize
'------новые данные-------

Dim count
Dim param_h(100)
Dim param_l(100)
Dim min_sizes(100)
Dim max_size
dim max_size_index
dim minimum
dim maximum
dim index
dim strVarData
dim strStekloData
dim strDatapolka
dim testPos
dim polkCount
dim testCount
dim lastPolkNumber
dim allLongPolkProf
strVarData=""
strStekloData=""
idsteklosection=999
lastPolkNumber=999
polkCount=0
testCount=0
allLongPolkProf=0

'------------зполнение массива данными из вариантов---------

for i=0 to xamb.nbox-1

   set x=xamb.box(i)
   'проверка есть ли вариант размер стекла
   'msgbox x.EsisteVarianteRegola("stekloSize")
   if x.varREgola("tipshkaf")<>"" then
   typeShkaf = Int(x.varREgola("tipshkaf"))
   end if 

	   if   x.EsisteVarianteRegola("stekloSize") and x.EsisteVarianteRegola("tipshkaf")=false  then
			'для полок стекла
			
			strDatapolka=x.varregola("stekloSize")
            testpos=InStr(strDatapolka,":")
			if testpos>0 then
				stekloSize=Split(strDatapolka,":")
				curL=Int(stekloSize(0))
				curA=Int(stekloSize(1))
				strStekloData =CurL & "X" & CurA & "|" & strStekloData
				
				if x.varregola("stekloCount")="" then
				testCount=0
				else 
				testCount=int(x.varregola("stekloCount"))
				 
				end if 
				polkCount=testCount+polkCount
				idsteklosection=i
			end if 
		end if 
		'если полка с вариантом длины профиля
		if   x.EsisteVarianteRegola("polkprofSizeMx") then 
		 x.varregola("polkprofSizeMx")="X"
		 lastPolkNumber=i
		 allLongPolkProf=allLongPolkProf+30+x.dl
		end if 
next

for i=0 to xamb.nbox-1
 set x=xamb.box(i)
 'if x.esisteVarianteRegola("largMixBox") then
' x.rivaluta
 'end if
 if not(x.EsisteVarianteRegola("tipshkaf")) then
 
 if i=idsteklosection  then
        x.varREgola("steklosizemax")=strStekloData
		x.varregola("stekloCountScene")=polkCount
		 
		else
		if   x.EsisteVarianteRegola("stekloSize")   then
		x.varREgola("steklosizemax")="X"
		'msgbox i
		end if 
		'x.fljob=1
 end if
 end if 
 next
if lastPolkNumber<>999 then
	set x=xamb.box(lastPolkNumber)
	x.varregola("polkprofSizeMx")=allLongPolkProf+50+50
end if 
 'тест создания нового box
 if (polkCount>0) then
	'set testbox=xamb.BoxNuovo
	 
end if 
end function

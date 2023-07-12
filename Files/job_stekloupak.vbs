option explicit
dim xamb

public function jobmain(var)

dim i 
dim x 
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
dim testbox
strVarData=""
strStekloData=""
idsteklosection=999
polkCount=0
testCount=0
dim naborkras
dim naborpat
dim curkrasid
dim curpatid
naborkras=""
naborpat=""
curkrasid=""
curpatid=""
'------------зполнение массива данными из вариантов---------

for i=0 to xamb.nbox-1

   set x=xamb.box(i)
   if x.varREgola("tipshkaf")<>"" then
   typeShkaf = Int(x.varREgola("tipshkaf"))
   end if 
   
   'добавление ремкомплектов к крашенным фасадам
   if x.EsisteVarianteRegola("cvetvstav_door") and x.EsisteVarianteRegola("_ModDoorRasp") then
   
    if x.varregola("_ModDoorRasp")="S26"  or x.varregola("_ModDoorRasp")="S01"then
		 curkrasid=x.varregola("curkrasid")
		 'naborkras=x.varregola("naborkras")
		 
			if instr(naborkras,curkrasid)<=0 then
				naborkras= curkrasid & ";" & naborkras
				x.varregola("naborkras")=curkrasid
				else
				x.varregola("naborkras")="X"
			end if
		else
			x.varregola("naborkras")="X"
	end if 
	if x.EsisteVarianteRegola("_patina") then
		if x.varregola("_patina")<>"999" then
			curpatid=x.varregola("curpatid")
			if instr(naborpat,curpatid)<=0 then
				naborpat = curpatid & ";" & naborpat
				x.varregola("naborpat")=curpatid
				else
				x.varregola("naborpat")="X"
			end if 
		else
			x.varregola("naborpat")="X"
		end if 
	end if 

   end if 
if x.EsisteVarianteRegola("tipshkaf") then
	   if   typeShkaf=9 or typeShkaf=12 then
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
				'msgbox i
			end if 
		end if 
		end if 
		'if x.EsisteVarianteRegola("shprpar") then
		'Значит в это правило нужно передавать номер шкафа (для дверей)
				'if x.isChild    then
				'если правило плейсер
				'msgbox x.blato      
				'end if
		'end if 
next
 

for i=0 to xamb.nbox-1
 set x=xamb.box(i)
 if  x.EsisteVarianteRegola("tipshkaf")  then 
 if i=idsteklosection then
        x.varREgola("steklosizemax")=strStekloData
		x.varregola("stekloCountScene")=polkCount
		  

		else
		 
		x.varREgola("steklosizemax")="X"
		'x.fljob=1
 end if
 end if
 next

end function

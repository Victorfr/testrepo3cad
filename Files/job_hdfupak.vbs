option explicit
dim xamb

public function jobmain(var)

dim i 
dim x 
dim box
dim AllDopFur ,idsection,idsteklosection,idsectionHDF,idsectionSc
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
dim strVarDataHdf
dim strStekloData
dim strDatapolka
dim testPos
dim hdfOnlyFlag
dim hasMantero
hdfOnlyFlag=""
strVarData=""
strVarDataHdf=""
strStekloData=""
idsection=999
idsectionHDF=999
idsectionSc=999
Dim matWall
'------------зполнение массива данными из вариантов---------
hasmantero=false
for i=0 to xamb.nbox-1
   set x=xamb.box(i)
   'msgbox x.varREgola("tipshkaf")&" "&i
   typeShkaf=0
   if x.varREgola("tipshkaf")<>"" then
   typeShkaf = Int(x.varREgola("tipshkaf"))
   'msgbox typeShkaf &" "&i
   end if 
   if x.varRegola("onlyHDF")<>"" then
   hdfOnlyFlag=x.varRegola("onlyHDF")
   end if
   matWall=x.varregola("mat_backwall")
   
   if hdfOnlyFLAG="X" and matWall="01"  Then
   		'для хдф
 		'Size=Split(x.varregola("hdfsize"),":")
		if x.varregola("hdfsize")<>"" then
 		'curL=Int(size(0))
		'curA=Int(size(1))
		'strVarDataHDF =CurL & "X" & CurA & "|" & strVarDataHDF
		strVarData =x.varregola("hdfsize") & "|" & strVarData
		end if 
        'idsectionHDF=i
		 idsection=i
   end if 
   
   
   if x.esistevarianteregola("shMantera") and typeShkaf=12 Then
     if x.varregola("shMantera")=1 then 
	 hasmantero=true
	 end if 
   end if
   
   if    x.EsisteVarianteRegola("hdfsize") or typeShkaf=9 or typeShkaf=10 then
 		'Size=Split(x.varregola("hdfsize"),"X")
		if x.varregola("hdfsize")<>"" and x.varREgola("mat_backwall")="01" then
		 
 		'curL=Int(size(0))
		'curA=Int(size(1))
		'strVarData =CurL & "X" & CurA & "|" & strVarData
		if x.varregola("hdfsize")<>""   then
				strVarData =x.varregola("hdfsize") & "|" & strVarData
		end if
		idsection=i
		 
		end if 
		if  x.EsisteVarianteRegola("mat_backwall") then
		
		
        end if

    end if
	
	if (typeShkaf=1 or typeShkaf=2) then
	 x.varRegola("allScUp")=0
		if (idsectionSc=999) then
		idsectionSc=i
		x.varRegola("allScUp")=1
		end if
	 if (idsectionSc<i) then
		set box =xamb.box(idsectionSc)
		box.varRegola("allScUp")=0
		x.varRegola("allScUp")=1
		idsectionSc=i
	 end if
	end if 
	
	
next


'msgbox idsection
for i=0 to xamb.nbox-1
 set x=xamb.box(i)
 if i=idsection then
		x.varREgola("hdfsizemax")=strVarData
		else
		x.varREgola("hdfsizemax")="test"
 end if
 if i=idsectionHDF and i<>idsection then
		'x.varREgola("hdfsizemax")=strVarDataHDF
				else
		'x.varREgola("hdfsizemax")="test"
 end if 

 
   if x.esistevarianteregola("shMantera") and not(x.esistevarianteregola("tipshkaf")) Then 
		if  hasmantero=true Then
		x.varregola("shmantera")=1
		Else
	 
		x.varregola("shmantera")=0
		end if 
   end if 

 

next 

end function

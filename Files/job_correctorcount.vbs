option explicit
dim xamb

public function jobmain(var)

dim i 
dim x 
dim AllDopFur ,idsection,idsteklosection,idsectionHDF
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
hdfOnlyFlag=""
strVarData=""
strVarDataHdf=""
strStekloData=""
idsection=999
Dim matWall
Dim correctorCount
Dim correctorraspzerk
Dim testVar
Dim idzerk
'------------зполнение массива данными из вариантов---------
idsection=9999
idzerk=9999
for i=0 to xamb.nbox-1
   set x=xamb.box(i)
   'msgbox x.varREgola("tipshkaf")&" "&i
   typeShkaf=0
    if x.varREgola("_modDoorRasp")<>"" then
	 
	'если значение варианта численное. то суммируем
	testVar=x.varRegola("correctorCheck")
	  
		if  IsNumeric(testVar) then 
		'if x.varRegola("correctorCheck")="1" then 
		    if x.varREgola("_modDoorRasp")<>"S30" then
			correctorCount=correctorCount+CInt(x.varRegola("correctorCheck"))
			'msgbox x.varRegola("correctorCheck")
			idsection=i
			else 
			correctorraspzerk=correctorraspzerk+CInt(x.varRegola("correctorCheck"))
			idzerk=i
			end if 
		'end if
		end if 
   end if 
  
next


'msgbox idsection

for i=0 to xamb.nbox-1
 set x=xamb.box(i)
 x.varREgola("correctorCount")="X"
 next


for i=0 to xamb.nbox-1
 set x=xamb.box(i)
 
 if i=idsection  then
		x.varREgola("correctorCount")=correctorCount
		else
		'x.varREgola("correctorCount")="X"
 end if
  if i=idzerk  then
		x.varREgola("correctorCount")=correctorraspzerk
		else
		'x.varREgola("correctorCount")="X"
 end if
next 
end function


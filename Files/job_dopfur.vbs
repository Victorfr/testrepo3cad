option explicit
dim xamb
dim typeShkaf	
public function jobmain(var)
dim i 
dim x 
dim AllDopFur ,idsection 
dim furnabor
set xamb=amb
dim ColkarkasUniq : Set ColkarkasUniq = CreateObject("Scripting.Dictionary")
Dim ColkarkasUniqArray

AllDopFur=0
for i=0 to xamb.nbox-1
   set x=xamb.box(i)

   if x.varREgola("dopfur")>"0" then
      idsection=i
      x.varREgola("AllDopFur")=0
      AllDopFur=AllDopFur+x.varREgola("dopfur")
   end if
   
   if x.EsisteVarianteRegola("tipshkaf")=true then 
   typeShkaf = x.varREgola("tipshkaf")  
   
   '<<<<<<<<<<<<<<<<****************************************'
   '2 4 6 8 9 10 11 12'  отдельностоящие
   if typeShkaf=2 or typeShkaf=4 or typeShkaf=6  or typeShkaf=8 or typeShkaf=9 or typeShkaf=10 or typeShkaf=11 or typeShkaf=12 Then
      if not(ColkarkasUniq.Exists(CStr(x.varREgola("_colkarkas")))) then ColkarkasUniq.Add  CStr(x.varREgola("_colkarkas")),0  
      if not(ColkarkasUniq.Exists(CStr(x.varREgola("_colkarkas_vn")))) then ColkarkasUniq.Add  CStr(x.varREgola("_colkarkas_vn")),0     
   end if
   if i=xamb.nbox-1 then
      ColkarkasUniqArray = ColkarkasUniq.Keys
      x.varREgola("ColkarkasUniq")=join(ColkarkasUniqArray,";")
   Else
      x.varREgola("ColkarkasUniq")="NotLastBox"
   end if
   '****************************************>>>>>>>>>>>>>>>>'
   
   if typeShkaf=10 then typeShkaf=9
   if typeShkaf=3 or typeShkaf=5 or typeShkaf=7 then typeShkaf=1
   if typeShkaf=4 or typeShkaf=6 or typeShkaf=8 then typeShkaf=2
   			if instr(furnabor,typeShkaf)<=0 then
				furnabor= typeShkaf & ";" & furnabor
				x.varregola("furnabor")=typeShkaf
				else
				x.varregola("furnabor")="X"
			end if 
   end if 

next




      if idsection<>"" then        
        set x=xamb.box(idsection)
        x.varREgola("AllDopFur")=AllDopFur
      end if
end function

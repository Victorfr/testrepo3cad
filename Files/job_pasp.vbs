option explicit
dim xamb

public function jobmain(var)
dim i 
dim x 
dim AllCount_pasp ,idsection

set xamb=amb

AllCount_pasp=0
for i=0 to xamb.nbox-1
   set x=xamb.box(i)
   if x.varREgola("pasp_count")>"0" then
      idsection=i
      x.varREgola("AllCount_pasp")=0
      AllCount_pasp=AllCount_pasp+x.varREgola("pasp_count")
   end if
next
      if idsection<>"" then        
        set x=xamb.box(idsection)
        x.varREgola("AllCount_pasp")=AllCount_pasp
      end if
end function

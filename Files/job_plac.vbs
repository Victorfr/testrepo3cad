option explicit

Public Function JobMain(var)
  dim aa
  aa="800,801 \n" ' Tipologia simboli impianti idrici su laterali
  JobMain=replace(aa,"\n",vbcrlf)
end function

Public sub JobIni(p,var)   

end sub 

Public Sub JobPannello(p,p1,var,lati,pl,pa,pp,ax,ay,az)
  dim latti  
  latti=mid(lati,2,1)
  if latti=0 and pl<11then
    p1.varRegola("_matFia")=p.varRegola("_matFiaSx")
  elseif latti=1 and abs(pl)<11 then
    p1.varRegola("_matFia")=p.varRegola("_matFiaDx")
  end if
end sub

Public sub JobFin(p,var)

End sub 
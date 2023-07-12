option explicit

Public Function JobMain(var)
  dim aa
  aa="802,803 \n" ' Tipologia simboli impianti idrici su laterali
  JobMain=replace(aa,"\n",vbcrlf)
end function

Public sub JobIni(p,var)   

end sub 

Public Sub JobPannello(p,p1,var,lati,pl,pa,pp,ax,ay,az)

  p1.varRegola("_prist1")=1

end sub

Public sub JobFin(p,var)

End sub 
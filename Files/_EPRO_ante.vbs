Dim x1,x2,pTerra

Sub Kromfas(dl,da,dp,dlf,daf,kromjpg,ck,sa_anta,c2)
   dlf=dl-2
   daf=da-2
   kromjpg=ex(zparametro(6),"/",1)
   kromrgb=ex(zparametro(6),"/",2)
   tip_anta=ex(zparametro(4),"/",1)
   Vkrg=SPLIT(kromrgb,"|")
   ReDim Preserve Vkrg(6)
   If len(vkrg(0)) Then coloreRal=GetColore(vkrg(0)) Else coloreRal=-1
   ck=wcolore(4,-1,percorso & "\foto\struttura\" & kromjpg)
   If coloreral<>-1 Then
      ck=wColore(4,ColoreRal,percorso & "\foto\struttura\ral.jpg")
   End If
   If left(ucase(tip_anta),1)="S" Or left(ucase(tip_anta),1)="D" Or left(ucase(tip_anta),1)="E" Then
      ck=c2
   End If
   sag="0,0,"&dl&",0,"&dl&","&da&",0,"&da&",FO,1,1,"&dl-1&",1,"&dl-1&","&da-1&",1,"&da-1
   SAG=WSAGOMA(MID(sag,2),1).NOME
   prof_k=dp
   wrtrasforma 0,sa_anta,0
   westrudi 2,sag,prof_k,ck,ck,0
   wrchiudi
End Sub
Sub Ramka(dl,da,dlf,daf,dp,px,py,c1,c2,side)
   cf1=c1
   cf2=c1
   colors=0
   If side=1 Then colors=9
   sv1="0,0,"&px&",0,"&px&","&daf&",0,"&daf
   ' контур горизонтальных планок развёрнут на 90 градусов что бы не крутить текстуру
   sg1="0,0,"&py&",0,"&py&","&dlf-2*px&",0,"&dlf-2*px
   ' ********************************************************************************
   sv1=WSAGOMA(MID(sv1,2),1).NOME
   sg1=WSAGOMA(MID(sg1,2),1).NOME
   wrtrasforma (dl-dlf)/2,(da-daf)/2,0
   '9 - white
   westrudi 2,sv1,dp,cf1,cf2,0,colors
   wrchiudi
   wrtrasforma dl-(dl-dlf)/2-px,(da-daf)/2,0
   westrudi 2,sv1,dp,cf1,cf2,0,colors
   wrchiudi
   wrtrasforma (dl-dlf)/2+px,(da-daf)/2+py,0,0,0,90
   westrudi 2,sg1,dp,cf1,cf2,0,colors
   wrchiudi
   wrtrasforma (dl-dlf)/2+px,da-(da-daf)/2,0,0,0,90
   westrudi 2,sg1,dp,cf1,cf2,0,colors
   wrchiudi
End Sub
Sub colorematerialefoto (c1,c2)
   listafoto=ex(zparametro(6),"/",1)
   listargb=ex(zparametro(6),"/",2)
   Vjpg=SPLIT(listafoto,";")
   ReDim Preserve Vjpg(6)
   nomejpg=vjpg(0)
   nomejpg1=vjpg(1)
   nomejpg2=vjpg(2)
   nomejpg3=vjpg(3)
   nomejpg4=vjpg(4)
   nomejpg5=vjpg(5)
   Vrgb=SPLIT(listaRgb,"|")
   ReDim Preserve Vrgb(6)
   If len(vrgb(0)) Then coloreRal=GetColore(vrgb(0)) Else coloreRal=-1
   If len(vrgb(1)) Then coloreRal1=GetColore(vrgb(1)) Else coloreRal1=-1
   If len(vrgb(2)) Then coloreRal2=GetColore(vrgb(2)) Else coloreRal2=-1
   If len(vrgb(3)) Then coloreRal3=GetColore(vrgb(3)) Else coloreRal3=-1
   If len(vrgb(4)) Then coloreRal4=GetColore(vrgb(4)) Else coloreRal4=-1
   If len(vrgb(5)) Then coloreRal5=GetColore(vrgb(5)) Else coloreRal5=-1
   If coloreral<>-1 Then
      c1=wColore(4,ColoreRal,percorso & "\foto\struttura\ral.jpg")
   Else
      c1=wColore(4,ColoreRal,percorso & "\foto\struttura\" & nomejpg)
   End If
End Sub
Sub Anta2(dl,da,dp,c1,c2,pTerra,lato,px,py,pa,pAltezza,pLarghezza)
   v=split(ex(zparametro(5),"/",1),"\")
   ReDim Preserve V(4)
   px=calcola(v(0))
   py=calcola(v(1))
   dlf=dl-2
   daf=da-2
   side=ex(zparametro(6),"/",3)
   Ramka dl,da,dlf,daf,dp,px,py,c1,c2,side
   Kromfas dl,da,dp,dlf,daf,kromjpg,ck,sa_anta,c2
   ' ********************* стекло ***************************
   vetrojpg=ex(zparametro(5),"/",2)
   vetromat=ex(zparametro(5),"/",4)
   
   ColoreVetRal=-1
   If left(ucase(vetrojpg),3)="RAL" Then
      ColoreVetRal=GetColore(ex(zparametro(5),"/",3))
   End If
   c9=wColore(vetromat,ColoreVetRal,percorso & "\foto\struttura\" & vetrojpg)
   zpannello px-5,py-5,dp/2-2,dl+10-px*2,da+10-py*2,4,c9
   ' *****************************************************
End Sub
'=======двусторонний фасад типа Vectot
Sub Anta3(dl,da,dp,c1,c2,pTerra,lato,px,py,pa,pAltezza,pLarghezza)
   v=split(ex(zparametro(5),"/",1),"\")
   ReDim Preserve V(4)
   nome3ds="box_fas.3ds"
   side=ex(zparametro(6),"/",3)
   
   'Ramka dl,da,dlf,daf,dp,px,py,c1,c2
   ' Kromfas dl,da,dp,dlf,daf,kromjpg,ck,sa_anta,c2
   ' ********************* стекло ***************************
   'vetrojpg=ex(zparametro(5),"/",2)
   'vetromat=ex(zparametro(5),"/",4)
   ' ColoreVetRal=-1
   'If left(ucase(vetrojpg),3)="RAL" Then
   ''   ColoreVetRal=GetColore(ex(zparametro(5),"/",3))
   'End If
   'c9=wColore(vetromat,ColoreVetRal,percorso & "\foto\struttura\" & vetrojpg)
   wrTrasforma 0,0,dp,90,0,0
   If side =1 Then
      color3ds=c1 & "," & c2 & "," & c1
   Else
      color3ds=c1 & "," & c2 & "," & c3
   End If
   wrX2 percorso & "\3ds\" & nome3ds,0,color3ds,dp,da,dl,1,param_rast
   wrchiudi
   
   ' *****************************************************
End Sub


Sub Anta1(dl,da,dp,c1,c2,pTerra,lato,px,py,pa,pAltezza,pLarghezza)
   tip_anta=ex(zparametro(4),"/",1)
   'для фасадов, которые фрезеруются из мдф
   If tip_anta="S08" Or tip_anta="S26" Or tip_anta="S27"  Or tip_anta="S01" Or tip_anta="S29"    Then
      nome3ds=ex(zparametro(5),"/",1)
      vid_vis=ex(zparametro(5),"/",2)
      
      larg=dl
      alt=da
      If nome3ds="DEKCORVS.3DS" Then
         larg=da
         alt=dl
      End If
      colPat=ex(zparametro(4),"/",10)
      If left(ucase(nome3ds),9)<>"CLASSICSD" Then param_rast="170;170;170;170;0;0"
      If left(ucase(nome3ds),5)<>"CLASS" Then param_rast="35;35;35;35;0;0"
      If left(ucase(nome3ds),3)="DEK" Then param_rast="20;20;20;20;0;0"
      If left(ucase(nome3ds),3)="ICE" Then param_rast="100;100;100;100;0;0"
      If left(ucase(nome3ds),6)="DEKKOL" Then param_rast="5;5;50;50;0;0"
      If left(ucase(nome3ds),9)="CLASSIC00" Then param_rast="20;20;20;20;0;0"
      If left(ucase(nome3ds),9)="CLASSIC02" Then param_rast="106;106;84;84;0;0"
      If left(ucase(nome3ds),7)="NICOLLE" Then param_rast="100;100;100;100;0;0"
      If left(ucase(nome3ds),6)="VECTOR" Then param_rast="100;100;100;100;0;0"
      
      If nome3ds="CLASSICS.3DS" Then param_rast="100;100;100;100;0;0"
      c2=wColore(4,-1,percorso & "\foto\struttura\" & colPat)
      If left(ucase(nome3ds),4)="SU_L" Then param_rast="274;6;100;100;0;0"
      If left(ucase(nome3ds),4)="SU_R" Then param_rast="6;274;100;100;0;0"
      If left(ucase(nome3ds),4)="SH_V" Then param_rast="100:100:100;100;10;294"
      If left(ucase(nome3ds),4)="SH_N" Then param_rast="100:100:100;100;294;10"
      
      If vid_vis="Y"   Then
         c8=wColore(12,-1,0)
         c2=c8
         c3=c8
         c4=c8
         c5=c8
         c6=c8
         c7=c8
      End If
      If right(ucase(colPat),4)<>".JPG" Then c2=c1
      
      If tip_anta="S26"  Then
         c8=c1
      End If
      If tip_anta="S29" Then
         'c1=c3
         c2=c1
         c3=c1
         
         
      End If
      If tip_anta="S08" Then
         'c3=c1
      End If
      If tip_anta="S26" Then
         c3=c1
      End If
      If tip_anta="S01" Then
         c3=c1
      End If
      'msgbox param_rast
      color3ds=c1 & "," & c2 & "," & c3 & "," & c4 & "," & c5 & "," & c6 & "," & c7 & "," & c8
      wrX2 percorso & "\3ds\" & nome3ds,0,color3ds,larg,alt,dp,1,param_rast
   Elseif tip_anta="S17" Then
      tex="ral_t1.jpg"
      If ex(zparametro(6),"/",3)=1 Then tex="ral_t1_.jpg"
      c1=wColore(4,GetColore(ex(zparametro(6),"/",2)),percorso & "\foto\struttura\" & tex)
      wrtrasforma 0,0,0
      zpannello 0,0,0,dl,da,dp,c1
      wrchiudi
   Elseif tip_anta="S129" Then
      nome3ds=ex(zparametro(5),"/",1)
      vid_vis=ex(zparametro(5),"/",2)
      
      
      
      
   Else
      
      
      If left(ucase(tip_anta),1)<>"S" And left(ucase(tip_anta),1)<>"D" And left(ucase(tip_anta),1)<>"E" And left(ucase(tip_anta),1)<>"L" Then
         colorematerialefoto c1,c2
      End If
      dlf=dl-2
      daf=da-2
      Kromfas dl,da,dp,dlf,daf,kromjpg,ck,sa_anta,c2
      sag="0,0,"&dlf&",0,"&dlf&","&daf&",0,"&daf
      SAG=WSAGOMA(MID(sag,2),1).NOME
      wrtrasforma (dl-dlf)/2,(da-daf)/2,0
      westrudi 2,sag,dp,c1,c1
      wrchiudi
   End If
End Sub
Sub Anta99(dl,da,dp,c1,c2,pTerra,lato,px,py,pa,pAltezza,pLarghezza)
   ' ********************* стекло ***************************
   vetrojpg=ex(zparametro(6),"/",1)
   vetromat=ex(zparametro(6),"/",3)
   ColoreVetRal=-1
   If left(ucase(vetrojpg),3)="RAL" Then
      ColoreVetRal=GetColore(ex(zparametro(6),"/",2))
   End If
   c9=wColore(vetromat,ColoreVetRal,percorso & "\foto\struttura\" & vetrojpg)
   zpannello -20,-20,0,dl+40,da+40,4,c9
   ' *****************************************************
End Sub
Sub Anta100(dl,da,dp,c1,c2,pTerra,lato,px,py,pa,pAltezza,pLarghezza)
   nomeman=ex(zParametro(4),"/",1)
   
   color_3d=c1 & "," & c2 & "," & c3
   wrX2 percorso & "\3ds\" & nomeman,0,color_3d,dl,da,dp,3,"30;30;0;0;0;0"
End Sub

Sub Maniglia1 (dl,da,dp,c1,c2,pTerra,lato,px,py,pa,pAltezza,pLarghezza)
   nomeman=ex(zParametro(9),"/",6)
   l_man=calcola(ex(zParametro(9),"/",1))
   a_man=calcola(ex(zParametro(9),"/",2))
   p_man=calcola(ex(zParametro(9),"/",3))
   ll=calcola(ex(zParametro(9),"/",1))
   aa=calcola(ex(zParametro(9),"/",2))
   man_pos=calcola(ex(zParametro(9),"/",4))
   man_orient=calcola(ex(zParametro(9),"/",5))
   man_vert=calcola(ex(zParametro(9),"/",7))
   l_man10=calcola(ex(zParametro(9),"/",8))
   textur=ex(zParametro(9),"/",9) 's-o
   
   If man_orient=1 Or man_orient=3 Then
      ll=calcola(ex(zParametro(9),"/",2))
      aa=calcola(ex(zParametro(9),"/",1))
   End If
   s_dp=dp
   If man_pos=1 Or man_pos=4 Or man_pos=7 Then trx=30
   If man_pos=2 Or man_pos=5 Or man_pos=8 Or man_pos=11 Or man_pos=12 Then trx=dl/2-ll/2
   If man_pos=3 Or man_pos=6 Or man_pos=9 Then trx=dl-30-ll
   If man_pos=1 Or man_pos=2 Or man_pos=3 Then try=da-30-aa
   If man_pos=4 Or man_pos=5 Or man_pos=6 Then try=da/2-aa/2
   If man_pos=7 Or man_pos=8 Or man_pos=9 Then try=30
   If man_pos=11 Then try=da-aa+2 : s_dp=-2'-30  на торце сверху
   If man_pos=12 Then try=aa-2 : s_dp=-2'da-aa-30   на торце снизу
   
   
   If man_orient=1 Or man_orient=3 Then
      wrTrasforma trx,try+l_man,s_dp,ang_man,0,90
   Elseif man_orient=0 Or man_orient=2 Then
      If man_pos=7 And left(ucase(nomeman),6)="ART200" And man_vert=1 Then
         wrTrasforma trx+l_man,try+a_man,dp,0,0,180
      Elseif man_pos=9 And left(ucase(nomeman),6)="ART200" And man_vert=1 Then
         wrTrasforma trx+l_man,try+a_man,dp,0,0,180
      Elseif man_pos=7 And left(ucase(nomeman),6)="ART201" And man_vert=1 Then
         wrTrasforma trx+l_man,try+a_man,dp,0,0,180
      Elseif man_pos=9 And left(ucase(nomeman),6)="ART201" And man_vert=1 Then
         wrTrasforma trx+l_man,try+a_man,dp,0,0,180
      Elseif man_pos=12 Then
         wrTrasforma trx+l_man,try,s_dp,ang_man,0,180
      Else
         wrTrasforma trx,try,s_dp,ang_man,0,0
      End If
   Else
      xm=l_man*cos(abs(man_ug)*0.01745329251994)
      ym=a_man*sin(abs(man_ug)*0.01745329251994)
      If man_pos=1 Then trx=30+ym
      If man_pos=1 Then try=da-30-xm-ym
      If man_pos=3 Then trx=dl-30-xm-ym
      If man_pos=3 Then try=da-30-ym
      If man_pos=7 Then try=30+xm
      If man_pos=9 Then trx=dl-30-xm
      wrTrasforma trx,try,dp,0,0,man_ug
   End If
   If dl=146 And dp=146 And da=716 Then
      wrTrasforma 0,0,-30,0,0,0
   End If
   colman=0
   If left(ucase(nomeman),6)="ART214" Or left(ucase(nomeman),6)="ART215" Then
      c1=wColore(15,-1,percorso & "\3DS\Maniglie\nerzav.jpg")
      colman=c1 & ","
   End If
   rast=0
   If left(ucase(nomeman),6)="ART287" Or left(ucase(nomeman),6)="ART289" Or left(ucase(nomeman),6)="ART290" Or left(ucase(nomeman),6)="ART267" Or left(ucase(nomeman),6)="ART268" Or left(ucase(nomeman),6)="ART269" Or left(ucase(nomeman),6)="ART354" Or left(ucase(nomeman),6)="ART350" Or left(ucase(nomeman),6)="ART352" Or left(ucase(nomeman),6)="ART342" Or left(ucase(nomeman),6)="ART343" Or left(ucase(nomeman),6)="ART335" Or left(ucase(nomeman),6)="ART336" Then
      c1=wColore(16,-1,percorso & "\3DS\Maniglie\nerzav.jpg")
      colman=c1 & ","
   End If
   If left(ucase(nomeman),6)="ART288" Then
      c1=wColore(17,-1,percorso & "\3DS\Maniglie\Nickel.jpg")
      colman=c1 & ","
   End If
   If left(ucase(nomeman),6)="ART270" Then
      c1=wColore(18,-1,percorso & "\3DS\Maniglie\black.jpg")
      colman=c1 & ","
   End If
   If left(ucase(nomeman),6)="ART271"  Then
      c1=wColore(18,-1,percorso & "\3DS\Maniglie\belii.jpg")
      colman=c1 & ","
   End If
   If textur<>"" Then 's-o
   c1=wColore(18,-1,percorso & "\3DS\Maniglie\" & textur) 's-o
   colman=c1 & "," 's-o
End If
wrX2 percorso & "\3ds\Maniglie\" & nomeman,0,colman,l_man,a_man,p_man,3,rast
If dl=146 And dp=146 And da=716 Then
   wrchiudi
End If
wrchiudi
If man_vert=1 Then
   If man_pos=1 Then
      wrTrasforma trx+a_man,try+a_man-l_man10,dp,0,0,-90
   End If
   If man_pos=3 Then
      wrTrasforma trx+l_man-a_man,try+a_man,dp,0,0,90
   End If
   If man_pos=7 Then
      wrTrasforma trx+a_man,try,dp,0,0,-90
   End If
   If man_pos=9 Then
      wrTrasforma trx+l_man-a_man,try+l_man10,dp,0,0,90
   End If
   wrX2 percorso & "\3ds\Maniglie\" & nomeman,0,0,l_man10,a_man,p_man,3,"50;50;0;0;0;0"
   wrchiudi
End If
wrchiudi
End Sub
(* ::Package:: *)

(*******************************************************************************
  sgn-sat
  
  sgn-sat is a Mathematica notebook to do derivation based on the relations
  "y=sgn(x-y) <=> y=sat(x)" and "y=dio(x+y) <=> y=max(x,0)"
   
  For further information on the functions sgn, sat, and dio, see the paper 
  Xiong, Kikuuwe & Yamamoto 2013, Journal of Applied Mathematics, 
  http://dx.doi.org/10.1155/2013/320276
   
  Author: Ryo Kikuuwe
  
  Copyright (c) 2014-2015 Ryo Kikuuwe
  
  The "sgn-sat" is a free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
  
  Contact: Ryo Kikuuwe, kikuuwe@ieee.org
*******************************************************************************)
rSgnSatSolve[AA_, CC_, BB_, xx_, rVerbose_] := Module[{A0,A1,B0,B1,S},
   S = Simplify;
   Print["==========================="];
   Print["Let us solve"]; 
   Print["   ", AA, " = ", CC, " Sgn[ ", BB, " ]"]; 
   Print["with respect to ", xx];
   B1 = - Coefficient[BB, xx];
   B0 = BB + B1 * xx;
   A1 = Coefficient[AA, xx]/CC; 
   A0 = AA - CC*A1*xx; 
   Print["---------------------------"];
   If[rVerbose,
     Print["derivation..."]; 
     Print["   ", A0/CC + A1*xx //S, " = Sgn[ ", B0, " ", - B1*xx//S, " ]"]; 
     Print["   ", A0/CC + A1*xx //S, " = Sgn[ ", A1*B0/B1//S," "         ,        -A1*xx//S, " ]"]; 
     Print["   ", A0/CC + A1*xx //S, " = Sgn[(", A1*B0/B1 + A0/CC//S, ")+(", -(A0/CC+A1*xx)//S,")]"]; 
     Print["   ", A0/CC + A1*xx //S, " = Sat[ ", A1*B0/B1 + A0/CC//S, " ]"]; 
     Print["The answer is:"];
     Print["   ", A1*xx+A0/CC//S," = Sat[ "                                    ,A0/CC + A1*B0/B1 //S, " ]"]; 
     Print["   ", A1*xx      //S," = ", -A0/CC/A1 //S,               " + Sat[ ",A0/CC + A1*B0/B1 //S, " ]"]; 
     Print["   ", xx         //S," = ", -A0/CC/A1 //S," + ", 1/A1 //S, " Sat[ ",A0/CC + A1*B0/B1 //S, " ]"]; 
     Print["==========================="];
   ];
   (xx//S)==(-A0/CC/A1 //S)  + (1/A1 //S) Sat[ A0/CC + A1*B0/B1 //S]  //Return;
 ];
rDioMaxSolve[AA_, CC_, BB_, xx_, rVerbose_] := Module[{A0,A1,B0,B1,S},
   S = Simplify;
   Print["==========================="];
   Print["Let us solve"]; 
   Print["   ", AA, " = ", CC, " Dio[ ", BB, " ]"]; 
   Print["with respect to ", xx];
   B1 = Coefficient[BB, xx];
   B0 = BB + B1 * xx;
   A1 = Coefficient[AA, xx]/CC; 
   A0 = AA - CC*A1*xx; 
   Print["---------------------------"];
   If[rVerbose,
     Print["derivation..."]; 
     Print["   ", A0/CC + A1*xx //S, " = Dio[ ", B0, "+",  B1*xx//S, " ]"]; 
     Print["   ", A0/CC + A1*xx //S, " = Dio[ ", A1*B0/B1//S," "         ,        -A1*xx//S, " ]"]; 
     Print["   ", A0/CC + A1*xx //S, " = Dio[(", A1*B0/B1 + A0/CC//S, ")+(", -(A0/CC+A1*xx)//S,")]"]; 
     Print["   ", A0/CC + A1*xx //S, " = Dio[ ", A1*B0/B1 + A0/CC//S, " ]"]; 
     Print["The answer is:"];
     Print["   ", A1*xx+A0/CC//S," = Max[0, "                                    ,A0/CC + A1*B0/B1 //S, " ]"]; 
     Print["   ", A1*xx      //S," = ", -A0/CC/A1 //S,               " + Max[0, ",A0/CC + A1*B0/B1 //S, " ]"]; 
     Print["   ", xx         //S," = ", -A0/CC/A1 //S," + ", 1/A1 //S, " Max[0, ",A0/CC + A1*B0/B1 //S, " ]"]; 
     Print["==========================="];
   ];
   (xx//S)==(-A0/CC/A1 //S)  + (1/A1 //S) Max[0, A0/CC + A1*B0/B1 //S]  //Return;
 ];



AA = X y - z; 
CC = Y; 
BB = x + Z y; 
xx = y; 
rVerbose = True;
rDioMaxSolve[AA, CC, BB, xx, rVerbose]


AA = X y - z; 
CC = Y; 
BB = x - Z y; 
xx = y; 
rVerbose = False;
rSgnSatSolve[AA, CC, BB, xx, rVerbose]

AA = M (vk-vk1)/T-fk; 
CC = F; 
BB = -vk; 
xx = vk; 
rVerbose = False;
rSgnSatSolve[AA, CC, BB, xx, rVerbose]


rSgnSatSolve[a x + b, c, e - f x , x, rVerbose]




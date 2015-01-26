(* ::Package:: *)

(*******************************************************************************
  sgn-sat
  
  sgn-sat is a Mathematica notebook to do derivation based on the relation
  "y=sgn(x-y) <=> y=sat(x)"
  
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

rSgnSatSolve[AA_, CC_, BB_, xx_, rVerbose_] := Module[{A0,A1,B0,B1},
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
     Print["   ", A0/CC + A1*xx , " = Sgn[ ", B0, " ", - B1*xx, " ]"]; 
     Print["   ", A0/CC + A1*xx , " = Sgn[ ", A1*B0/B1," "         ,        -A1*xx, " ]"]; 
     Print["   ", A0/CC + A1*xx , " = Sgn[ ", A1*B0/B1 + A0/CC, " ", -(A0/CC+A1*xx)," ]"]; 
     Print["   ", A0/CC + A1*xx , " = Sat[ ", A1*B0/B1 + A0/CC, " ]"]; 
   ];
   Print["The answer is:"];
   Print["   ", A1*xx+A0/CC," = Sat[ "                           , A0/CC + A1*B0/B1, " ]"]; 
   Print["   ", A1*xx      ," = ", -A0/CC   ,           " + Sat[ ",A0/CC + A1*B0/B1, " ]"]; 
   Print["   ", xx         ," = ", -A0/CC/A1," + ", 1/A1, " Sat[ ",A0/CC + A1*B0/B1, " ]"]; 
   Print["==========================="];
 ];

rSgnSatSolve[y, F, x-y, y, True];

AA = X y - z; 
CC = Y; 
BB = x - Z y; 
xx = y; 
rVerbose = True;
rSgnSatSolve[AA, CC, BB, xx, rVerbose];

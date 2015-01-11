(* ::Package:: *)

BeginPackage["ScatterPlot`"]
Unprotect[DotDash, FrequencyDiagram, DDAxesLabels, DDPlotLabel, DDMedian]

DotDash::usage = "DotDash[data,lpopts___Rule] produces a dot-dash scatter plot, as defined by Edward Tufte in `The Visual Display of Quantitative Information'. Data should be entered as a list of numeric {x,y} pairs. The optional parameter lpopts can be any collection of ListPlot options. There are three additional options: DDAxesLabels, DDPlotLabel,and DDMedian with default values {{\"xmin\",\"xmax\"},{\"ymin\",\"ymax\"}}, Text[\"\",{0,0}], and True respectively.  The maximum, minimum, and median values are printed for both x and y.

Examples:
DotDash[{{2,5},{2.1,6},{2,5.5},{3,6}}],

DotDash[Table[{Random[],Random[]},{i,25}],DDPlotLabel->Text[FontForm[\"Title\",{\"Courier-Bold\",14}],{.5,1.1}]],
DotDash[Table[{Random[],Random[]},{i,25}],

DDAxesLabels->{{\"very small\",\"quite big\"},{\"very low\",\"high\"}},
DDMedian->False,
DDPlotLabel->
 Text[FontForm[\"A Nice Title\",{\"Courier-Bold\",14}],{.5,1.1}],
 PlotStyle->PointSize[.018]]"

DDAxesLabels::usage="An option for DotDash, with default value DDAxesLabels->{{\"xmin\",\"xmax\"},{\"ymin\",\"ymax\"}}"

DDPlotLabel::usage="An option for DotDash, with default value DDPlotLabel->Text[\"\",{0,0}]."

DDMedian::usage="An option for DotDash, with default value DDMedian->True. Any other value suppresses the display of the medians along the two axes."

FrequencyDiagram::usage="FrequencyDiagram[data,lpopts___Rule] produces a one variable scatter plot. The data is represented as dots above a number line. If the data contains seven copies of the value 3.14, the plot will have seven dots stacked above 3.14. Data may be entered as either a vector of numeric values, or as a list of {value,frequency} pairs. The optional parameter lpopts can be any collection of ListPlot or DotDash options.

Examples: FrequencyDiagram[{2,5,4,2,7,4,2,3}],
FrequencyDiagram[{{3.14,7},{3,4},{2.71828,3}}],
FrequencyDiagram[Table[Round[100 Random[]],{i,1,125}]]"

(* Version 1.4
   
     Copyright February 1994, February 2001, January 2015

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    See http://www.gnu.org/licenses/gpl-3.0-standalone.html for a copy of the license.
           
                   Dr. Eric Gossett
                   Bethel University
                   3900 Bethel Drive
                   St. Paul, MN 55112
                   gossett@bethel.edu
           
   Version 1.06: FrequencyDiagram added, change in Graphics, for Mma 2.0
   Version 1.2:  DDAxesLabels, DDPlotLabel, DDMedian added, both functions
                 now return a graphics object
   Version 1.3:  Fixed some errorsa with DDMedian
   Version 1.4:  Switched from copyright to GPLv3 copyleft.

*)

Begin["`Private`"]
Unprotect[DotDash, FrequencyDiagram]
Clear[DotDash, FrequencyDiagram, DDAxesLabels, DDPlotLabel, DDMedian]
Off[General::spell1]

(* Roman Maeder's Filter Options *)
FilterOptions[command_Symbol, opts___] := 
   Module[{keywords = First /@ Options[command]},
          Sequence @@ Select[ {opts}, MemberQ[keywords, First[#]]&]
         ]

ValidDotDashData[x_] := MatrixQ[x, NumberQ] && Dimensions[x][[2]] == 2;

ValidFrequencyData[x_] := VectorQ[x, NumberQ] ||
                      (MatrixQ[x, NumberQ] && Dimensions[x][[2]] == 2);

xMargin[xvals_,ymin_,yinc_] := Module[{dashes={},i},
  For[i=1,i<=Length[xvals],i++,
      dashes = Append[dashes,
        Line[{{xvals[[i]],ymin-4 yinc},
              {xvals[[i]],ymin-2 yinc}}]]];
   Return[dashes]
]

yMargin[yvals_,xmin_,xinc_] := Module[{dashes={},i},
  For[i=1,i<=Length[yvals],i++,
      dashes = Append[dashes,
        Line[{{xmin-4 xinc,yvals[[i]]},
              {xmin-2 xinc,yvals[[i]]}}]]];
   Return[dashes]
]

Options[DotDash] = {DDAxesLabels->{{"xmin","xmax"},{"ymin","ymax"}},
                    DDPlotLabel->Text["",{0,0}], DDMedian->True}

Median[L_?VectorQ] := Module[{len=Length[L],S=Sort[L]},If[OddQ[len],
                            Return[S[[(len+1)/2]]],
                Return[(S[[len/2]]+S[[len/2+1]])/2.]]]

DotDash[data_,lpopts___Rule] := Module[{xvals, yvals, xt, yt, xrange,
      yrange, xmin, xmax, ymin, ymax, xmargin, ymargin, labels, graph,
      axlbls, xminlbl, xmaxlbl, yminlbl, ymaxlbl,pltlbl, prnmed, xmed, ymed, medlbl},
 
    {xvals,yvals} = Transpose[data];
    xmin=Min[xvals];
    xmax=Max[xvals];
    ymin=Min[yvals];
    ymax=Max[yvals];
    xmed = Median[xvals];
    ymed = Median[yvals];
    xrange=xmax-xmin;
    yrange=ymax-ymin;
    xt = .025/GoldenRatio xrange;
    yt = .025 yrange;
    xmargin = xMargin[xvals,ymin,yt];
    ymargin = yMargin[yvals,xmin,xt]; 

    axlbls = DDAxesLabels /. {FilterOptions[DotDash,lpopts]} /. Options[DotDash];
    pltlbl = DDPlotLabel  /. {FilterOptions[DotDash,lpopts]} /. Options[DotDash];
    prnmed = DDMedian     /. {FilterOptions[DotDash,lpopts]} /. Options[DotDash];
    If[prnmed,
       medlbl = Graphics[{Text["median",{xmed,ymin-6yt},{-1,0}],    
               Text[ToString[xmed],{xmed,ymin-9yt},{-1,0}],
               Text[ToString[ymed],{xmin-6xt,ymed},{1,0}],
               Text["median",{xmin-6xt,ymed+3yt},{1,0}]
            }],
       medlbl=Graphics[{}]];

    labels=Graphics[{pltlbl,
              Text[ToString[xmin],{xmin,ymin-9yt},{-1,0}],
              Text[axlbls[[1,1]],{xmin,ymin-6yt},{-1,0}],
              Text[ToString[xmax],{xmax,ymin-9yt},{1,0}],
              Text[axlbls[[1,2]],{xmax,ymin-6yt},{1,0}],
              Text[ToString[ymin],{xmin-6xt,ymin},{1,0}],
              Text[axlbls[[2,1]],{xmin-6xt,ymin+3yt},{1,0}],
              Text[ToString[ymax],{xmin-6xt,ymax},{1,0}],
              Text[axlbls[[2,2]],{xmin-6xt,ymax+3yt},{1,0}]
            }];
    graph = Show[ListPlot[data,FilterOptions[ListPlot,lpopts],
           Axes->False,PlotStyle->PointSize[.01],
           PlotRange->{{xmin-xt,xmax+xt},{ymin-yt,ymax+yt}},
           AspectRatio->1,DisplayFunction->Identity],labels,medlbl,
         Graphics[{Thickness[.001],xmargin,ymargin}],
         PlotRange->All, 
         DisplayFunction->$DisplayFunction];
      Return[graph]
  ] /; ValidDotDashData[data]
   
FrequencyDiagram[data_,lpopts___Rule] := Module[{FreqPairs, SortedList, 
     xmin, vals, step, xmax, graph, 
     ymin, ymax, xmargin, FullData,i,j, numx, pointsize},

(* collect data into {value,frequency} pairs *)

If[MatrixQ[data],
  FreqPairs = data,
  (* else *)
  SortedList = Sort[data];
  FreqPairs = {};
  While[(Length[SortedList] > 0),
    AppendTo[FreqPairs,{SortedList[[1]],Count[SortedList,SortedList[[1]]]}];
    SortedList = DeleteCases[SortedList,SortedList[[1]]];
  ]
];

FullData = {};
For[i=1,i<=Length[FreqPairs],i++,
  FullData=Join[FullData,
      Table[{FreqPairs[[i,1]],j},{j,1,FreqPairs[[i,2]]}]];
];

(* the list of values, min and max, etc *)

If[VectorQ[data],
  vals = data,
  (* else *)
  vals = Transpose[FreqPairs][[1]]
];

numx = Length[FreqPairs];

xmin = Min[vals];
xmax = Max[vals];
ymin = 0;
ymax = Max[Transpose[FreqPairs][[2]]];

step = Which[
             ymax ==  1, 6,
         ymax <= 10, 5,
         ymax <= 20, 4,
         ymax <= 30, 3,
         ymax <= 40, 2,
         ymax >  40, 1
        ];

pointsize = Which[
             ymax ==  1, 6,
         (ymax <= 10) && (numx <= 25),  5,
         (ymax <= 20) && (numx <= 50),  4,
         (ymax <= 30) && (numx <= 75),  3,
         (ymax <= 40) && (numx <= 125), 2,
         (ymax >  40) || (numx >  175), 1
        ];


(* produce the graph *)

graph = Show[ListPlot[FullData,lpopts,Axes->{True,False},
           AxesOrigin->{xmin-.05(xmax-xmin),0},
       PlotStyle->AbsolutePointSize[pointsize],
       AspectRatio->1/step,
       PlotRange->{{xmin-.05(xmax-xmin),xmax+.05(xmax-xmin)},
                   {ymin-.05(ymax-ymin),ymax+.05(ymax-ymin)}},
           DisplayFunction->Identity],
        (* PlotRange->All,*)
     DisplayFunction->$DisplayFunction];
  Return[graph]

] /; ValidFrequencyData[data]


On[General::spell1];
End[]
Protect[DotDash, FrequencyDiagram, DDAxesLabels, DDPlotLabel, DDMedian]
EndPackage[]

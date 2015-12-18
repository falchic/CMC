BeginPackage["CMC`"];

CMC::usage="CMC is a package that contains functions useful to calculate different mesaures of circularity. With these functions it is possible to detect a highlighted region in an image, calculate its area, centroids in order to obtain several mesaures of circularity. It is also possible to export the mesaurses to an XLS spreadsheet.";
CMCPalette::usage="CMCPalette[] shows the palette";
ImgRegion::usage="ImgRegion[im] is a function that transform a binarize image to a region (polygon)";
GetSection::usage="GetSection[im] detect the highlighted region we want to analyze from image";
InPolyQ::usage="";
FindCentr::usage="";
FindSC::usage="";
BinaryImageToRegion::usage="";
Inters::usage="";
CalcC::usage="";
CalcM::usage="";
CalcT::usage="";
CalcV::usage="";

Begin["`Private`"];

CMCPalette[]:=With[{logo=Import["CMC\\cmc.png"],buttonOpts=Sequence[ImageSize->{160,Automatic}]},
  CreatePalette[DynamicModule[{ready=False,ready2=False,z,imgName="none",circularityC="",circularityV="",circularityT="",circularityM="Calculating...",show,compare,dataSaved={}},
    Dynamic@Column[{logo,
      Style[StringJoin["Image loaded: ",imgName],14],
      Button["Import image",importButton[],
        Method->"Queued",buttonOpts],
      Button["Calculate circularity",calcButton[],
        Enabled->ready,Method->"Queued",buttonOpts],
      Row[{Checkbox[Dynamic[show],Enabled->ready]," Show section"}],
      Row[{Checkbox[Dynamic[compare],Enabled->ready], " Compare with circle"}],
      Button["Export data",exportButton[],
        Enabled->ready2,Method->"Queued",buttonOpts],
      Button["Clear history",clearButton[],
        Enabled->ready2,Method->"Queued",buttonOpts]
  },Center,ItemSize->16,Spacings->{Automatic,{0,1,Automatic}}],

(* init start *)
Initialization:>(

importButton[]:= Module[{x},
  x=SystemDialogInput["FileOpen",{$UserBaseDirectory,{"Images"->{"*.jpeg","*.png","*.jpg"},"All files"->{"*.*"}}},WindowTitle->"Select image"];
  imgName=If[x=!=$Canceled,FileNameTake[x],"none"];
  z=If[x=!=$Canceled,Import[x]];
  ready=If[x==$Canceled,False,True];
  If[x=!=$Canceled,CreateDialog[Column[{Row[{z},ImageSize->Dynamic[CurrentValue["WindowSize"]/.{a_,_}:>a-15]]}],WindowSize->{500,All},WindowTitle->StringJoin["Image - ",imgName],WindowMargins->{{0,Automatic},{Automatic,0}}]]
];

calcButton[]:=Module[{values,bimg,cg,sols2},
  values=FindSC[z,show,imgName];
  bimg="bimg" /. values;
  cg="cg" /. values;
  ready2=True;
  CreateDialog[Dynamic@Grid[{
    {Style["Mesaure",18],Style["Value",18]},
    {Style[4*Pi*("A"/"P"^2),26],Style[circularityC,22]},
    {Style[HoldForm@Sum["var" {Subscript["d","i","j"]}/("max"{Subscript["d","i","j"]}),{"i",1,"L"}],26],Style[circularityM,22]},
    {Style[HoldForm@Sum[Subscript["dist", "X"]("p")/"h"^3,Element["p","X"]],26],Style[circularityV,22]},
    {Style["A"/"h"^2,26],Style[circularityT,22]}},Alignment->{Center,Center},Frame->All,Spacings->{2,2}
    ], WindowSize->{All,All},WindowTitle->StringJoin["Circularity - ",imgName],WindowMargins->{{0,Automatic},{0,Automatic}}];
  circularityC=CalcC[bimg];
  circularityV =CalcV[bimg];
  circularityT=CalcT[bimg];
  circularityM=CalcM[cg,bimg];
  If[compare==True,CreateDialog[Dynamic@Grid[{
    {Style["Mesaure",18],Style["Value",18]},
    {Style[4*Pi*("A"/"P"^2),26],Style["1",22]},
    {Style[HoldForm@Sum["var" {Subscript["d","i","j"]}/("max"{Subscript["d","i","j"]}),{"i",1,"L"}],26],Style["0",22]},
    {Style[HoldForm@Sum[Subscript["dist", "X"]("p")/"h"^3,Element["p","X"]],26],Style["1",22]},
    {Style["A"/"h"^2,26],Style["1",22]}},Alignment->{Center,Center},Frame->All,Spacings->{2,2}],WindowSize->{All,All},WindowTitle->"Circularity - Circle",WindowMargins->{{Automatic,0},{0,Automatic}}]];
  If[dataSaved=={},AppendTo[dataSaved,{"File","C","M","T","V"}]];
  AppendTo[dataSaved,{imgName,circularityC,circularityM,circularityT,circularityV}]
];

exportButton[]:=Module[{},
  Export[SystemDialogInput["FileSave",{"test",{"Spreadsheet Formats (*.xls)"->{"*.xls"},"Plain Text Document (*.txt)"->{"*.txt"}}}],dataSaved];
  dataSaved={};
  ready2=False
];

clearButton[]:=Module[{},
  dataSaved={};
  ready2=False
];

)

],WindowTitle->"cMc",Background->White]]

ImgRegion[im_]:=Polygon[Part[#,Last@FindShortestTour[#]]&@PixelValuePositions[MorphologicalPerimeter[Erosion[FillingTransform@ColorNegate@Binarize[im,0.91],2],CornerNeighbors->False],1]];

GetSection[im_]:=Module[{m,m2,h2,s,b,bimg},
  {h2,s,b}=ColorSeparate[im,"HSB"];
  m=ImageMultiply[im,Binarize[Abs[Mod[ImageData[h2]-0.332,1,-1/2]]//Image,0.1]//ColorNegate];
  m2=Dilation[MorphologicalTransform[DeleteSmallComponents[Binarize[m]],{"Min","Max"}],1]//ColorNegate;
  bimg = DeleteBorderComponents[MorphologicalComponents[m2,0.9]]//Image//Binarize;
  Return[bimg]
];

(* check if a point is member of a region*)
InPolyQ[poly_,pt_]:=Graphics`PolygonUtils`PointWindingNumber[poly,pt]=!=0;

(* find centroids *)
FindCentr[bimg_]:=Module[{fdistance,cgravity1,outside,cgravity,region},
  fdistance =DistanceTransform[bimg]//MaxDetect;
  region=ImgRegion[ColorNegate[bimg]];
  cgravity1= DeleteDuplicates[Last /@ComponentMeasurements[fdistance,"Centroid"], EuclideanDistance[#1, #2] <12&];
  outside=Select[cgravity1,InPolyQ[region,#]==False&];
  cgravity=DeleteCases[cgravity1,Alternatives@@outside];
  Return[cgravity]
];

FindSC[z_,show_,imgName_]:=Module[{rp,pp,centr,region,bimg,cg},
  bimg=GetSection[z];
  region=ImgRegion[ColorNegate[bimg]];
  rp=RegionPlot[region, AspectRatio->Automatic,ImageSize->Full];
  cg=FindCentr[bimg];
  pp=ListPlot[cg[[All]],AspectRatio->Automatic,PlotStyle->{Red,PointSize[Small]},ImageSize->Full];
  centr=Show[rp,pp, Frame->None];
  If[show==True,
    CreateDialog[Column[{Row[{centr},ImageSize->Dynamic[CurrentValue["WindowSize"]/.{a_,_}:>a-15]],Style[StringJoin["Number of centroids: ",ToString@Length[cg]],15]}],WindowSize->{500,All},WindowTitle->StringJoin["Section and Centroids - ",imgName],WindowMargins->{{Automatic,Automatic},{Automatic,0}}]];
  Return[{"bimg"-> bimg,"cg"-> cg}]
];

BinaryImageToRegion[bimg_]:=With[{idata=ImageData[bimg],xmax=First@ImageDimensions[bimg],ymax=Last@ImageDimensions[bimg]},BoundaryDiscretizeGraphics@First@RegionPlot[idata[[IntegerPart@(ymax-y),IntegerPart@x]]==1,{x,1,xmax},{y,1,ymax}]];

(* C mesaure *)
CalcC[bimg_]:=Module[{region,area,peri,circ},
  region=BinaryImageToRegion[bimg];
  area=Area[region];
  peri=ArcLength[RegionBoundary@region];
  circ=4*Pi*(area/peri^2);
  Return[circ]
];

(* M mesaure *)
CalcM[cg_,bimg_]:=Module[{dist,var,max,circ,sols2},
  (* find intersection with region boundary *)
  Inters[{c1_,c2_}]:=Module[{lines,sol,boundary,region},
    region=ImgRegion[ColorNegate[bimg]];
    boundary=RegionBoundary[region];
    lines=Table[InfiniteLine[{c1,c2},{Cos[i],Sin[i]}],{i,0, \[Pi],\[Pi]/4}];
    sol=Flatten[Values@Quiet[Solve[{x,y}\[Element]#&&{x,y}\[Element]boundary,{x,y}]&/@lines],1]//DeleteDuplicates;
    If[Count[sol,_ConditionalExpression,Infinity]==0,sols2=AppendTo[sols2,{c1,c2}->sol]]
  ];
  Clear[sols2];
  SetSharedVariable[sols2];
  sols2=List[];
  ParallelMap[(Inters[#])&,cg];
  dist=Flatten@Outer[EuclideanDistance,{Keys[#]},Values[#],1]&/@sols2;
  var=Variance[#]&/@dist;
  max=Max[#]&/@dist;
  circ=Total[var/max];
  Return[circ]
];

(* V mesaure *)
CalcV[bimg_]:=Module[{distransform,circ},
  distransform=Flatten@ImageData[DistanceTransform[bimg]];
  circ=Total[distransform]/(Max[distransform]^3);
  Return[circ]
];

(* T mesaure *)
CalcT[bimg_]:=Module[{region,area,dist,circ},
  region=BinaryImageToRegion[bimg];
  area=Area[region];
  dist=Max[Flatten@ImageData[DistanceTransform[bimg]]];
  circ=area/(dist^2);
  Return[circ]
];

End[];
EndPackage[];

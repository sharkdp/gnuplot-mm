(* ::Package:: *)

gnuplotPackageDirectory = "~/.Mathematica/gnuplot-mm";

reloadGnuPlot := Get[FileNameJoin[{gnuplotPackageDirectory, "gnuplot.m"}]];

Clear[NRange, gnuListPlot, gnuData, gnuPlot];

NRange[a_, b_, n_] := Range[a, b, (b-a)/(n-1)];

Options[gnuListPlot] = {
    DataRange -> {},
    Root -> "",
    Title -> "",
    PlotSize -> "5,3",
    Font -> "Verdana,20",
    LineWidth -> 3,
    PlotStyle -> "",
    PrintScript -> False,
    Latex -> False,
    LatexStyle -> "",
    PNG -> False,
    PdfCrop -> True,
    NumberFormat -> "$%g$",
    PdfFontConversion -> False,
    ImportAsImage -> True
};

gnuListPlot[data_, cmds_ : "", OptionsPattern[]] :=
Module[{rootdir, plotdir, title, latex, png, delete, scriptfile, outfile, infile, convfile, datafile, datarange, ndata, datadim, xdata, plotrange, terminal, colornames, lout, texlist, texlines, sout, cmdsr, plot},
    rootdir = OptionValue[Root];
    title = OptionValue[Title];
    latex = OptionValue[Latex];
    png = OptionValue[PNG];
    importAsImage = OptionValue[Latex] && OptionValue[ImportAsImage];

    If[latex && png, Print["Can not use latex and png terminal at the same time"]; Exit[]];

    (* Create plot directory *)
    delete = False;
    If[rootdir == "", directory = $TemporaryDirectory];
    If[title == "",
        (* No title given, use temp directory *)
        plotdir = Check[CreateDirectory[], Return[]];
        title = Last[FileNameSplit[plotdir]];
        delete = True;
        ,
        plotdir = FileNameJoin[{rootdir, title}];
        If[!FileExistsQ[plotdir],
            Check[CreateDirectory[plotdir], Return[]];
        ];
    ];

    (* File names *)
    scriptfile = FileNameJoin[{plotdir, "plot.gp"}];
    infile = FileNameJoin[{plotdir, title<>".pdf"}];
    convfile = FileNameJoin[{plotdir, title<>"_conv.pdf"}];
    outfile = FileNameJoin[{plotdir, title<>".pdf"}];
    If[latex, outfile = FileNameJoin[{plotdir, title<>".tex"}];];             
    If[png, infile = outfile = FileNameJoin[{plotdir, title<>".png"}];];

    datafile[nr_] := FileNameJoin[{plotdir, title<>"_"<>ToString[nr]<>".csv"}];

    (* Adjust data dimension, if simple list is given *)
    ndata = If[Depth@data == 2, {data}, data];

    (* Add data range *)
    datarange = OptionValue[DataRange];
    If[datarange != {},
        datadim = Dimensions[data];
        xdata[length_] := Transpose@{N[NRange[datarange[[1]], datarange[[2]], length]]};
        ndata = Map[
            If[Length[Dimensions[#]] == 1, Join[xdata[Length@#], Transpose[{#}], 2], #] &
        , ndata];
    ];

    (* Export the data *)
    MapIndexed[
        Export[datafile[First[#2]], #1, "Table", TextDelimiters -> " "] &
        , N[ndata]
    ];

    (* Add blank lines after each data block for 3D plots *)
	Table[
        If[Last@Dimensions@(ndata[[i]]) == 3,
            Run["gawk -f", FileNameJoin[{gnuplotPackageDirectory, "addblanks.awk"}], datafile[i], "> "<>datafile[i]<>"_2"];
            Run["mv ", datafile[i]<>"_2", datafile[i]];
        ];
    , {i, 1, Length@ndata}];

    (* Construct default plot command *)
    If[cmds == "",
        cmdsr = OptionValue[PlotStyle]<>"\n\n";

        cmdsr = cmdsr<>If[datarange != {}, "set xrange ["<>ToString[datarange[[1]]//N]<>":"<>ToString[datarange[[2]]//N]<>"];\n\n", ""];

        colornames = {"BLUE", "RED", "GREEN", "MAGENTA", "ORANGE", "TURQUOISE", "BLACK", "GRAY"};

        cmdsr = cmdsr<>"plot "<>StringJoin[Table["#"<>ToString[k]<>" w l @"<>colornames[[Mod[k, Length@colornames, 1]]]<>" not, ", {k, 1, Length[ndata]}]];
        cmdsr = StringDrop[cmdsr, -2];
        ,
        cmdsr = OptionValue[PlotStyle]<>"\n\n"<>cmds;
    ];

    (* Write the gnuplot script *)
    terminal = "pdfcairo enhanced color";
    If[latex, terminal = "cairolatex pdf standalone color"];
    If[png, terminal = "pngcairo enhanced"];
    sout = OpenWrite[scriptfile];
    WriteString[sout, "#!/usr/bin/gnuplot\n\n"];
    WriteString[sout, "set terminal "<>terminal<>" size "<>OptionValue["PlotSize"]<>" font '"<>OptionValue["Font"]<>"' linewidth "<>ToString[OptionValue["LineWidth"]]<>"\n"];
    WriteString[sout, "set termoption dashed\n\n"];
    WriteString[sout, "load '"<>FileNameJoin[{gnuplotPackageDirectory, "header.gp"}]<>"'\n\n"];

    If[latex, WriteString[sout, "set format '"<>OptionValue[NumberFormat]<>"'\n\n"]];

    cmdsr = StringReplace[cmdsr,
        Table["#"<>ToString[k]<>" " -> "'"<>datafile[k]<>"' ", {k, 1, Length[ndata]}]
    ];
    WriteString[sout, "set output '"<>outfile<>"'\n\n"];
    WriteString[sout, cmdsr<>"\n\n"];
    Close[sout];

    If[OptionValue[PrintScript],
        FilePrint[scriptfile];
    ];

    (* Call gnuplot *)
    If[Run["gnuplot ", scriptfile, "2> /tmp/gnuploterr.log"] != 0,
        Print["There was an error executing gnuplot"];
        FilePrint["/tmp/gnuploterr.log"];
        FilePrint[scriptfile];
    ];

    (* Call pdflatex for cairolatex terminal *)
    If[latex,
        (* Add additional tex commands *)
        texlist = ReadList[outfile, "String"];
        texlines = "\\usepackage{amsmath,amssymb}\n\n"<>OptionValue[LatexStyle];
        texlines = StringJoin@Map[#<>"\n"&,Take[texlist, 2]~Join~{texlines}~Join~Drop[texlist, 2]];
        lout = OpenWrite[outfile];
        WriteString[lout, texlines];
        Close[lout];

        If[Run["pdflatex", "-output-directory="<>plotdir, outfile, "> /tmp/pdflatexerr.log 2>&1"] != 0,
            Print["There was an error executing pdflatex"];
            FilePrint["/tmp/pdflatexerr.log"];
        ];
        If[OptionValue[PdfFontConversion],
            (* do some magic :-) *)
            Run["gs -sDEVICE=pswrite -dNOCACHE -sOutputFile=- -q -dbatch -dNOPAUSE -dQUIET '"<>infile<>"' -c quit | ps2pdf - '"<>convfile<>"'"];
            Run["mv '"<>infile<>"_2' '"<>infile<>"'"];
            ,
            Run["cp '"<>infile<>"' '"<>convfile<>"'"];
        ];
        ,
        Run["cp '"<>infile<>"' '"<>convfile<>"'"];
    ];

    If[importAsImage,
        oldconvfile = convfile;
        convfile = convfile<>".png";
        Run["convert '"<>oldconvfile<>"' '"<>convfile<>"'"];
        ,
        (* Remove margins from pdf file *)
        If[!png && OptionValue[PdfCrop],
            Run["pdfcrop --pdfversion 1.4 ", convfile, convfile,"> test.out"];

            If[OptionValue[PdfFontConversion],
                Run["gs --without-x -sDEVICE=pswrite -dNOCACHE -sOutputFile=- -q -dbatch -dNOPAUSE -dQUIET '"<>convfile<>"' -c quit | ps2pdf - '"<>convfile<>"_2'"];
                Run["mv '"<>convfile<>"_2' '"<>convfile<>"'"];
            ];
        ];
    ];

    (* Import plot *)
    plot = If[png || importAsImage, ImageCrop[Import[convfile]], First[Import[convfile]]];

    (* Remove temp directories *)
    If[delete, DeleteDirectory[plotdir, DeleteContents -> True]];

    Return[plot];
];

gnuListPlot[data_, opts : OptionsPattern[]] := gnuListPlot[data, "", opts];

gnuData[funcs_, range_, npoints_] := Module[{nfuncs, xdata},
    nfuncs = If[Head[funcs] === List, funcs, {funcs}];
    xdata = N[NRange[range[[2]], range[[3]], npoints]];
    Return[Map[Table[{x, # /. range[[1]] -> x /. ComplexInfinity -> 0}, {x, xdata}]&, nfuncs]];
];

Options[gnuPlot] = Options[gnuListPlot];

gnuPlot[funcs_, range_ : {x, -5, 5}, cmds_ : "", opts : OptionsPattern[]] := Module[{npoint},
    npoints = 1000; (* TODO *)
    Return[gnuListPlot[gnuData[funcs, range, npoints], cmds, DataRange -> {range[[2]], range[[3]]}, opts]];
];

gnuPlot[funcs_, range_ : {x, -5, 5}, opts : OptionsPattern[]] := gnuPlot[funcs, range, "", opts];

gnuPlot[funcs_, opts : OptionsPattern[]] := gnuPlot[funcs, {x, -5, 5}, "", opts];




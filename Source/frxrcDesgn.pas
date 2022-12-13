{******************************************}
{                                          }
{             FastReport v6.0              }
{          Language resource file          }
{                                          }
{         Copyright (c) 1998-2022          }
{         by Alexander Tzyganenko,         }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit frxrcDesgn;

interface

implementation

uses frxRes;

const resXML =
'<?xml version="1.1" encoding="utf-8"?><Resources CodePage="1252"><StrRes Name="2000" Text="Object Inspector"/><StrRes Name="oiProp" Text="Properties"/' + 
'><StrRes Name="oiEvent" Text="Events"/><StrRes Name="oiFavorites" Text="Favorites"/><StrRes Name="oiFavIns" Text="Hold Ctrl key and click on selection' + 
' box to add property to Favorites."/><StrRes Name="2100" Text="Data Tree"/><StrRes Name="2101" Text="Data"/><StrRes Name="2102" Text="Variables"/><Str' + 
'Res Name="2103" Text="Functions"/><StrRes Name="2104" Text="Create field"/><StrRes Name="2105" Text="Create caption"/><StrRes Name="2106" Text="Classe' + 
's"/><StrRes Name="dtNoData" Text="No data available."/><StrRes Name="dtNoData1" Text="Go to the &#38;#34;Report|Data...&#38;#34; menu to add existing ' + 
'datasets to your report, or switch to the &#38;#34;Data&#38;#34; tab and create new datasets."/><StrRes Name="dtData" Text="Data"/><StrRes Name="dtSys' + 
'Var" Text="System variables"/><StrRes Name="dtVar" Text="Variables"/><StrRes Name="dtFunc" Text="Functions"/><StrRes Name="2200" Text="Report Tree"/><' + 
'StrRes Name="2300" Text="Open Script File"/><StrRes Name="2301" Text="Save Script to File"/><StrRes Name="2302" Text="Run Script"/><StrRes Name="2303"' + 
' Text="Trace Into"/><StrRes Name="2304" Text="Terminate Script"/><StrRes Name="2305" Text="Evaluate"/><StrRes Name="2306" Text="Language:"/><StrRes Na' + 
'me="2307" Text="Align"/><StrRes Name="2308" Text="Align Left"/><StrRes Name="2309" Text="Align Middle"/><StrRes Name="2310" Text="Align Right"/><StrRe' + 
's Name="2311" Text="Align Top"/><StrRes Name="2312" Text="Align Center"/><StrRes Name="2313" Text="Align Bottom"/><StrRes Name="2314" Text="Space Hori' + 
'zontally"/><StrRes Name="2315" Text="Space Vertically"/><StrRes Name="2316" Text="Center Horizontally in Band"/><StrRes Name="2317" Text="Center Verti' + 
'cally in Band"/><StrRes Name="2318" Text="Same Width"/><StrRes Name="2319" Text="Same Height"/><StrRes Name="2320" Text="Text"/><StrRes Name="2321" Te' + 
'xt="Style"/><StrRes Name="2322" Text="Font Name"/><StrRes Name="2323" Text="Font Size"/><StrRes Name="2324" Text="Bold"/><StrRes Name="2325" Text="Ita' + 
'lic"/><StrRes Name="2326" Text="Underline"/><StrRes Name="2327" Text="Font Color"/><StrRes Name="2328" Text="Highlight"/><StrRes Name="2329" Text="Tex' + 
't Rotation"/><StrRes Name="2330" Text="Align Left"/><StrRes Name="2331" Text="Align Center"/><StrRes Name="2332" Text="Align Right"/><StrRes Name="233' + 
'3" Text="Justify"/><StrRes Name="2334" Text="Align Top"/><StrRes Name="2335" Text="Align Middle"/><StrRes Name="2336" Text="Align Bottom"/><StrRes Nam' + 
'e="2337" Text="Frame"/><StrRes Name="2338" Text="Top Line"/><StrRes Name="2339" Text="Bottom Line"/><StrRes Name="2340" Text="Left Line"/><StrRes Name' + 
'="2341" Text="Right Line"/><StrRes Name="2342" Text="All Frame Lines"/><StrRes Name="2343" Text="No Frame"/><StrRes Name="2344" Text="Edit Frame"/><St' + 
'rRes Name="2345" Text="Background Color"/><StrRes Name="2346" Text="Frame Color"/><StrRes Name="2347" Text="Frame Style"/><StrRes Name="2348" Text="Fr' + 
'ame Width"/><StrRes Name="2349" Text="Standard"/><StrRes Name="2350" Text="New Report"/><StrRes Name="2351" Text="Open Report"/><StrRes Name="2352" Te' + 
'xt="Save Report"/><StrRes Name="2353" Text="Preview"/><StrRes Name="2354" Text="New Report Page"/><StrRes Name="2355" Text="New Dialog Page"/><StrRes ' + 
'Name="2356" Text="Delete Page"/><StrRes Name="2357" Text="Page Settings"/><StrRes Name="2358" Text="Variables"/><StrRes Name="2359" Text="Cut"/><StrRe' + 
's Name="2360" Text="Copy"/><StrRes Name="2361" Text="Paste"/><StrRes Name="2362" Text="Copy Formatting"/><StrRes Name="2363" Text="Undo"/><StrRes Name' + 
'="2364" Text="Redo"/><StrRes Name="2365" Text="Group"/><StrRes Name="2366" Text="Ungroup"/><StrRes Name="2367" Text="Show Grid"/><StrRes Name="2368" T' + 
'ext="Align to Grid"/><StrRes Name="2369" Text="Fit to Grid"/><StrRes Name="2370" Text="Zoom"/><StrRes Name="2371" Text="Extra Tools"/><StrRes Name="23' + 
'72" Text="Select Tool"/><StrRes Name="2373" Text="Hand Tool"/><StrRes Name="2374" Text="Zoom Tool"/><StrRes Name="2375" Text="Edit Text Tool"/><StrRes' + 
' Name="2376" Text="Copy Format Tool"/><StrRes Name="2377" Text="Insert Band"/><StrRes Name="2378" Text="&amp;File"/><StrRes Name="2379" Text="&amp;Edi' + 
't"/><StrRes Name="2380" Text="Find..."/><StrRes Name="2381" Text="Find Next"/><StrRes Name="2382" Text="Replace..."/><StrRes Name="2383" Text="&amp;Re' + 
'port"/><StrRes Name="2384" Text="Data..."/><StrRes Name="2385" Text="Options..."/><StrRes Name="2386" Text="Styles..."/><StrRes Name="2387" Text="&amp' + 
';View"/><StrRes Name="2388" Text="Toolbars"/><StrRes Name="2389" Text="Standard"/><StrRes Name="2390" Text="Text"/><StrRes Name="2391" Text="Frame"/><' + 
'StrRes Name="2392" Text="Alignment Palette"/><StrRes Name="2393" Text="Extra Tools"/><StrRes Name="2394" Text="Object Inspector"/><StrRes Name="2395" ' + 
'Text="Data Tree"/><StrRes Name="2396" Text="Report Tree"/><StrRes Name="2397" Text="Rulers"/><StrRes Name="2398" Text="Guides"/><StrRes Name="2399" Te' + 
'xt="Delete Guides"/><StrRes Name="2400" Text="Options..."/><StrRes Name="2401" Text="&amp;Help"/><StrRes Name="2402" Text="Help Contents..."/><StrRes ' + 
'Name="2403" Text="About FastReport..."/><StrRes Name="2404" Text="Tab Order..."/><StrRes Name="2405" Text="Undo"/><StrRes Name="2406" Text="Redo"/><St' + 
'rRes Name="2407" Text="Cut"/><StrRes Name="2408" Text="Copy"/><StrRes Name="2409" Text="Paste"/><StrRes Name="2410" Text="Group"/><StrRes Name="2411" ' + 
'Text="Ungroup"/><StrRes Name="2412" Text="Delete"/><StrRes Name="2413" Text="Delete Page"/><StrRes Name="2414" Text="Select All"/><StrRes Name="2415" ' + 
'Text="Edit..."/><StrRes Name="2416" Text="Bring to Front"/><StrRes Name="2417" Text="Send to Back"/><StrRes Name="2418" Text="New..."/><StrRes Name="2' + 
'419" Text="New Report"/><StrRes Name="2420" Text="New Page"/><StrRes Name="2421" Text="New Dialog"/><StrRes Name="2422" Text="Open..."/><StrRes Name="' + 
'2423" Text="Save"/><StrRes Name="2424" Text="Save As..."/><StrRes Name="2425" Text="Variables..."/><StrRes Name="2426" Text="Page Settings..."/><StrRe' + 
's Name="2427" Text="Preview"/><StrRes Name="2428" Text="Exit"/><StrRes Name="2429" Text="Report Title"/><StrRes Name="2430" Text="Report Summary"/><St' + 
'rRes Name="2431" Text="Page Header"/><StrRes Name="2432" Text="Page Footer"/><StrRes Name="2433" Text="Header"/><StrRes Name="2434" Text="Footer"/><St' + 
'rRes Name="2435" Text="Master Data"/><StrRes Name="2436" Text="Detail Data"/><StrRes Name="2437" Text="Subdetail Data"/><StrRes Name="2438" Text="Data' + 
' 4th level"/><StrRes Name="2439" Text="Data 5th level"/><StrRes Name="2440" Text="Data 6th level"/><StrRes Name="2441" Text="Group Header"/><StrRes Na' + 
'me="2442" Text="Group Footer"/><StrRes Name="2443" Text="Child"/><StrRes Name="2444" Text="Column Header"/><StrRes Name="2445" Text="Column Footer"/><' + 
'StrRes Name="2446" Text="Overlay"/><StrRes Name="2447" Text="Vertical bands"/><StrRes Name="2448" Text="Header"/><StrRes Name="2449" Text="Footer"/><S' + 
'trRes Name="2450" Text="Master Data"/><StrRes Name="2451" Text="Detail Data"/><StrRes Name="2452" Text="Subdetail Data"/><StrRes Name="2453" Text="Gro' + 
'up Header"/><StrRes Name="2454" Text="Group Footer"/><StrRes Name="2455" Text="Child"/><StrRes Name="2456" Text="0°"/><StrRes Name="2457" Text="45°"' + 
'/><StrRes Name="2458" Text="90°"/><StrRes Name="2459" Text="180°"/><StrRes Name="2460" Text="270°"/><StrRes Name="2461" Text="Font Settings"/><StrR' + 
'es Name="2462" Text="Bold"/><StrRes Name="2463" Text="Italic"/><StrRes Name="2464" Text="Underline"/><StrRes Name="2465" Text="SuperScript"/><StrRes N' + 
'ame="2466" Text="SubScript"/><StrRes Name="2467" Text="Condensed"/><StrRes Name="2468" Text="Wide"/><StrRes Name="2469" Text="12 cpi"/><StrRes Name="2' + 
'470" Text="15 cpi"/><StrRes Name="2471" Text="Report (*.fr3)|*.fr3"/><StrRes Name="2472" Text="Pascal files (*.pas)|*.pas|C++ files (*.cpp)|*.cpp|Java' + 
'Script files (*.js)|*.js|Basic files (*.vb)|*.vb|All files|*.*"/><StrRes Name="2473" Text="Pascal files (*.pas)|*.pas|C++ files (*.cpp)|*.cpp|JavaScri' + 
'pt files (*.js)|*.js|Basic files (*.vb)|*.vb|All files|*.*"/><StrRes Name="2474" Text="Connections..."/><StrRes Name="2475" Text="Language"/><StrRes N' + 
'ame="2476" Text="Toggle breakpoint"/><StrRes Name="2477" Text="Run to cursor"/><StrRes Name="2478" Text="!Add child band"/><StrRes Name="2479" Text="E' + 
'dit Fill"/><StrRes Name="2480" Text="Default charset"/><StrRes Name="2481" Text="Watch List"/><StrRes Name="2482" Text="Break Points List"/><StrRes Na' + 
'me="2483" Text="Auto Guides"/><StrRes Name="2484" Text="Delete Horizontal Guides"/><StrRes Name="2485" Text="Delete Vertical Guides"/><StrRes Name="24' + 
'86" Text="Use Horizontal Guides"/><StrRes Name="2487" Text="Use Vertical Guides"/><StrRes Name="2488" Text="Step Over"/><StrRes Name="2489" Text="Run ' + 
'Until Return"/><StrRes Name="dsCm" Text="Centimeters"/><StrRes Name="dsInch" Text="Inches"/><StrRes Name="dsPix" Text="Pixels"/><StrRes Name="dsChars"' + 
' Text="Characters"/><StrRes Name="dsCode" Text="Code"/><StrRes Name="dsData" Text="Data"/><StrRes Name="dsPage" Text="Page"/><StrRes Name="dsRepFilter' + 
'" Text="Report (*.fr3)|*.fr3"/><StrRes Name="dsComprRepFilter" Text="Compressed report (*.fr3)|*.fr3"/><StrRes Name="dsSavePreviewChanges" Text="Save ' + 
'changes to preview page?"/><StrRes Name="dsSaveChangesTo" Text="Save changes to "/><StrRes Name="dsCantLoad" Text="Couldn''t load file"/><StrRes Name="' + 
'dsStyleFile" Text="Style"/><StrRes Name="dsCantFindProc" Text="Could not locate the main proc"/><StrRes Name="dsClearScript" Text="This will clear all' + 
' code. Do you want to continue?"/><StrRes Name="dsNoStyle" Text="No style"/><StrRes Name="dsStyleSample" Text="Style sample"/><StrRes Name="dsTextNotF' + 
'ound" Text="Text ''%s'' not found"/><StrRes Name="dsContinueSearch" Text="Search from beginning ?"/><StrRes Name="dsReplace" Text="Replace this occurenc' + 
'e of ''%s''?"/><StrRes Name="2600" Text="About FastReport"/><StrRes Name="2601" Text="Visit our webpage for more info:"/><StrRes Name="2602" Text="Sales' + 
':"/><StrRes Name="2603" Text="Support:"/><StrRes Name="2700" Text="Page Options"/><StrRes Name="2701" Text="Paper"/><StrRes Name="2702" Text="Width"/>' + 
'<StrRes Name="2703" Text="Height"/><StrRes Name="2704" Text="Size"/><StrRes Name="2705" Text="Orientation"/><StrRes Name="2706" Text="Left"/><StrRes N' + 
'ame="2707" Text="Top"/><StrRes Name="2708" Text="Right"/><StrRes Name="2709" Text="Bottom"/><StrRes Name="2710" Text="Margins"/><StrRes Name="2711" Te' + 
'xt="Paper Source"/><StrRes Name="2712" Text="First page"/><StrRes Name="2713" Text="Other pages"/><StrRes Name="2714" Text="Portrait"/><StrRes Name="2' + 
'715" Text="Landscape"/><StrRes Name="2716" Text="Other options"/><StrRes Name="2717" Text="Columns"/><StrRes Name="2718" Text="Number"/><StrRes Name="' + 
'2719" Text="Width"/><StrRes Name="2720" Text="Positions"/><StrRes Name="2721" Text="Other"/><StrRes Name="2722" Text="Duplex"/><StrRes Name="2723" Tex' + 
't="Print to previous page"/><StrRes Name="2724" Text="Mirror margins"/><StrRes Name="2725" Text="Large height in design mode"/><StrRes Name="2726" Tex' + 
't="Endless page width"/><StrRes Name="2727" Text="Endless page height"/><StrRes Name="2800" Text="Select Report Datasets"/><StrRes Name="2900" Text="E' + 
'dit Variables"/><StrRes Name="2901" Text="Category"/><StrRes Name="2902" Text="Variable"/><StrRes Name="2903" Text="Edit"/><StrRes Name="2904" Text="D' + 
'elete"/><StrRes Name="2905" Text="List"/><StrRes Name="2906" Text="Load"/><StrRes Name="2907" Text="Save"/><StrRes Name="2908" Text="Expression:"/><St' + 
'rRes Name="2909" Text="Dictionary (*.fd3)|*.fd3"/><StrRes Name="2910" Text="Dictionary (*.fd3)|*.fd3"/><StrRes Name="vaNoVar" Text="(no variables defi' + 
'ned)"/><StrRes Name="vaVar" Text="Variables"/><StrRes Name="vaDupName" Text="Duplicate name"/><StrRes Name="3000" Text="Designer Options"/><StrRes Nam' + 
'e="3001" Text="Grid"/><StrRes Name="3002" Text="Type"/><StrRes Name="3003" Text="Size"/><StrRes Name="3004" Text="Dialog form:"/><StrRes Name="3005" T' + 
'ext="Other"/><StrRes Name="3006" Text="Fonts"/><StrRes Name="3007" Text="Code window"/><StrRes Name="3008" Text="Memo editor"/><StrRes Name="3009" Tex' + 
't="Size"/><StrRes Name="3010" Text="Size"/><StrRes Name="3011" Text="Colors"/><StrRes Name="3012" Text="Gap between bands:"/><StrRes Name="3013" Text=' + 
'"cm"/><StrRes Name="3014" Text="in"/><StrRes Name="3015" Text="pt"/><StrRes Name="3016" Text="pt"/><StrRes Name="3017" Text="pt"/><StrRes Name="3018" ' + 
'Text="Centimeters:"/><StrRes Name="3019" Text="Inches:"/><StrRes Name="3020" Text="Pixels:"/><StrRes Name="3021" Text="Show grid"/><StrRes Name="3022"' + 
' Text="Align to Grid"/><StrRes Name="3023" Text="Show editor after insert"/><StrRes Name="3024" Text="Use object''s font settings"/><StrRes Name="3025"' + 
' Text="Workspace"/><StrRes Name="3026" Text="Tool windows"/><StrRes Name="3027" Text="LCD grid color"/><StrRes Name="3028" Text="Free bands placement"' + 
'/><StrRes Name="3029" Text="Show drop-down fields list"/><StrRes Name="3030" Text="Show startup screen"/><StrRes Name="3031" Text="Restore defaults"/>' + 
'<StrRes Name="3032" Text="Show band captions"/><StrRes Name="3100" Text="Select DataSet"/><StrRes Name="3101" Text="Number of records:"/><StrRes Name=' + 
'"3102" Text="Dataset"/><StrRes Name="3103" Text="Filter"/><StrRes Name="3104" Text="All"/><StrRes Name="3105" Text="Selected"/><StrRes Name="3106" Tex' + 
't="Unselected"/><StrRes Name="3107" Text="Intersections"/><StrRes Name="dbNotAssigned" Text="[not assigned]"/><StrRes Name="3200" Text="Group"/><StrRe' + 
's Name="3201" Text="Break on"/><StrRes Name="3202" Text="Options"/><StrRes Name="3203" Text="Data field"/><StrRes Name="3204" Text="Expression"/><StrR' + 
'es Name="3205" Text="Keep group together"/><StrRes Name="3206" Text="Start new page"/><StrRes Name="3207" Text="Show in outline"/><StrRes Name="3300" ' + 
'Text="System Memo"/><StrRes Name="3301" Text="Data band"/><StrRes Name="3302" Text="DataSet"/><StrRes Name="3303" Text="DataField"/><StrRes Name="3304' + 
'" Text="Function"/><StrRes Name="3305" Text="Expression"/><StrRes Name="3306" Text="Aggregate value"/><StrRes Name="3307" Text="System variable"/><Str' + 
'Res Name="3308" Text="Count invisible bands"/><StrRes Name="3309" Text="Text"/><StrRes Name="3310" Text="Running total"/><StrRes Name="agAggregate" Te' + 
'xt="Insert Aggregate"/><StrRes Name="vt1" Text="[DATE]"/><StrRes Name="vt2" Text="[TIME]"/><StrRes Name="vt3" Text="[PAGE#]"/><StrRes Name="vt4" Text=' + 
'"[TOTALPAGES#]"/><StrRes Name="vt5" Text="[PAGE#] of [TOTALPAGES#]"/><StrRes Name="vt6" Text="[LINE#]"/><StrRes Name="vt7" Text="[PAGE]"/><StrRes Name' + 
'="vt8" Text="[TOTALPAGES]"/><StrRes Name="vt9" Text="[PAGE] of [TOTALPAGES]"/><StrRes Name="vt10" Text="[LINE]"/><StrRes Name="vt11" Text="[COPYNAME#]' + 
'"/><StrRes Name="vt12" Text="[TABLEROW]"/><StrRes Name="vt13" Text="[TABLECOLUMN]"/><StrRes Name="3400" Text="OLE object"/><StrRes Name="3401" Text="I' + 
'nsert..."/><StrRes Name="3402" Text="Edit..."/><StrRes Name="3403" Text="Close"/><StrRes Name="olStretched" Text="Stretched"/><StrRes Name="3500" Text' + 
'="Barcode Editor"/><StrRes Name="3501" Text="Code"/><StrRes Name="3502" Text="Type of Bar"/><StrRes Name="3503" Text="Zoom:"/><StrRes Name="3504" Text' + 
'="Options"/><StrRes Name="3505" Text="Rotation"/><StrRes Name="3506" Text="Calc Checksum"/><StrRes Name="3507" Text="Text"/><StrRes Name="3508" Text="' + 
'0°"/><StrRes Name="3509" Text="90°"/><StrRes Name="3510" Text="180°"/><StrRes Name="3511" Text="270°"/><StrRes Name="bcCalcChecksum" Text="Calc Ch' + 
'ecksum"/><StrRes Name="bcShowText" Text="Show Text"/><StrRes Name="3600" Text="Edit Aliases"/><StrRes Name="3601" Text="Press Enter to edit item"/><St' + 
'rRes Name="3602" Text="Dataset alias"/><StrRes Name="3603" Text="Field aliases"/><StrRes Name="3604" Text="Reset"/><StrRes Name="3605" Text="Update"/>' + 
'<StrRes Name="alUserName" Text="User name"/><StrRes Name="alOriginal" Text="Original name"/><StrRes Name="3700" Text="Parameters Editor"/><StrRes Name' + 
'="qpName" Text="Name"/><StrRes Name="qpDataType" Text="Data Type"/><StrRes Name="qpValue" Text="Value"/><StrRes Name="3800" Text="Master-Detail Link"/' + 
'><StrRes Name="3801" Text="Detail fields"/><StrRes Name="3802" Text="Master fields"/><StrRes Name="3803" Text="Linked fields"/><StrRes Name="3804" Tex' + 
't="Add"/><StrRes Name="3805" Text="Clear"/><StrRes Name="3850" Text="HTML"/><StrRes Name="3851" Text="Load"/><StrRes Name="3852" Text="Copy"/><StrRes ' + 
'Name="3853" Text="Paste"/><StrRes Name="3860" Text="Source Code"/><StrRes Name="3861" Text="Insert Expression"/><StrRes Name="3862" Text="Insert Aggre' + 
'gate"/><StrRes Name="3864" Text="Word Wrap"/><StrRes Name="3870" Text="HTML"/><StrRes Name="3871" Text="Format to the Control Width"/><StrRes Name="38' + 
'72" Text="Show Control Size"/><StrRes Name="3880" Text="Default"/><StrRes Name="3881" Text="Background Color:"/><StrRes Name="3882" Text="Font Color:"' + 
'/><StrRes Name="3883" Text="Font Name:"/><StrRes Name="3884" Text="Font Size:"/><StrRes Name="3885" Text="Link Color:"/><StrRes Name="3886" Text="Pre ' + 
'Font Name:"/><StrRes Name="3887" Text="Margin Width:"/><StrRes Name="3888" Text="Margin Height:"/><StrRes Name="3900" Text="Memo"/><StrRes Name="3901"' + 
' Text="Insert Expression"/><StrRes Name="3902" Text="Insert Aggregate"/><StrRes Name="3903" Text="Insert Formatting"/><StrRes Name="3904" Text="Word W' + 
'rap"/><StrRes Name="3905" Text="Text"/><StrRes Name="3906" Text="Format"/><StrRes Name="3907" Text="Highlight"/><StrRes Name="3940" Text="ListBox edit' + 
'or"/><StrRes Name="3941" Text="ComboBox editor"/><StrRes Name="3951" Text="Load"/><StrRes Name="3961" Text="Insert Expression"/><StrRes Name="3962" Te' + 
'xt="Insert Aggregate"/><StrRes Name="4000" Text="Picture"/><StrRes Name="4001" Text="Load"/><StrRes Name="4002" Text="Copy"/><StrRes Name="4003" Text=' + 
'"Paste"/><StrRes Name="4004" Text="Clear"/><StrRes Name="4005" Text="PDF"/><StrRes Name="piEmpty" Text="Empty"/><StrRes Name="4100" Text="Chart Editor' + 
'"/><StrRes Name="4101" Text="Add Series"/><StrRes Name="4102" Text="Delete Series"/><StrRes Name="4103" Text="Edit Chart"/><StrRes Name="4104" Text="B' + 
'and source"/><StrRes Name="4105" Text="Fixed data"/><StrRes Name="4106" Text="DataSet"/><StrRes Name="4107" Text="Data Source"/><StrRes Name="4108" Te' + 
'xt="Values"/><StrRes Name="4109" Text="Select the chart series or add a new one."/><StrRes Name="4110" Text="Move Series Up"/><StrRes Name="4111" Text' + 
'="Move Series Down"/><StrRes Name="4114" Text="Other options"/><StrRes Name="4115" Text="TopN values"/><StrRes Name="4116" Text="TopN caption"/><StrRe' + 
's Name="4117" Text="Sort order"/><StrRes Name="4126" Text="X Axis"/><StrRes Name="ch3D" Text="3D View"/><StrRes Name="chAxis" Text="Show Axis"/><StrRe' + 
's Name="chsoNone" Text="None"/><StrRes Name="chsoAscending" Text="Ascending"/><StrRes Name="chsoDescending" Text="Descending"/><StrRes Name="chxtText"' + 
' Text="Text"/><StrRes Name="chxtNumber" Text="Numeric"/><StrRes Name="chxtDate" Text="Date"/><StrRes Name="4200" Text="Rich Editor"/><StrRes Name="420' + 
'1" Text="Open File"/><StrRes Name="4202" Text="Save File"/><StrRes Name="4203" Text="Undo"/><StrRes Name="4204" Text="Font"/><StrRes Name="4205" Text=' + 
'"Insert Expression"/><StrRes Name="4206" Text="Bold"/><StrRes Name="4207" Text="Italic"/><StrRes Name="4208" Text="Underline"/><StrRes Name="4209" Tex' + 
't="Left Align"/><StrRes Name="4210" Text="Center Align"/><StrRes Name="4211" Text="Right Align"/><StrRes Name="4212" Text="Justify"/><StrRes Name="421' + 
'3" Text="Bullets"/><StrRes Name="4300" Text="Cross-tab Editor"/><StrRes Name="4301" Text="Source data"/><StrRes Name="4302" Text="Dimensions"/><StrRes' + 
' Name="4303" Text="Rows"/><StrRes Name="4304" Text="Columns"/><StrRes Name="4305" Text="Cells"/><StrRes Name="4306" Text="Cross-tab structure"/><StrRe' + 
's Name="4307" Text="Row header"/><StrRes Name="4308" Text="Column header"/><StrRes Name="4309" Text="Row grand total"/><StrRes Name="4310" Text="Colum' + 
'n grand total"/><StrRes Name="4311" Text="Swap rows/columns"/><StrRes Name="4312" Text="!Select style"/><StrRes Name="4313" Text="!Save current style.' + 
'.."/><StrRes Name="4314" Text="!Show title"/><StrRes Name="4315" Text="!Show corner"/><StrRes Name="4316" Text="!Reprint headers on new page"/><StrRes' + 
' Name="4317" Text="!Auto size"/><StrRes Name="4318" Text="!Border around cells"/><StrRes Name="4319" Text="!Print down then across"/><StrRes Name="432' + 
'0" Text="!Side-by-side cells"/><StrRes Name="4321" Text="!Join equal cells"/><StrRes Name="4322" Text="None"/><StrRes Name="4323" Text="Sum"/><StrRes ' + 
'Name="4324" Text="Min"/><StrRes Name="4325" Text="Max"/><StrRes Name="4326" Text="Average"/><StrRes Name="4327" Text="Count"/><StrRes Name="4328" Text' + 
'="Ascending (A-Z)"/><StrRes Name="4329" Text="Descending (Z-A)"/><StrRes Name="4330" Text="No Sort"/><StrRes Name="crStName" Text="!Enter the style na' + 
'me:"/><StrRes Name="crResize" Text="!To resize a cross-tab, set its &#38;#34;AutoSize&#38;#34; property to False."/><StrRes Name="crSubtotal" Text="Su' + 
'btotal"/><StrRes Name="crNone" Text="None"/><StrRes Name="crSum" Text="Sum"/><StrRes Name="crMin" Text="Min"/><StrRes Name="crMax" Text="Max"/><StrRes' + 
' Name="crAvg" Text="Avg"/><StrRes Name="crCount" Text="Count"/><StrRes Name="crAsc" Text="A-Z"/><StrRes Name="crDesc" Text="Z-A"/><StrRes Name="4400" ' + 
'Text="Expression Editor"/><StrRes Name="4401" Text="Expression:"/><StrRes Name="4500" Text="Display Format"/><StrRes Name="4501" Text="Category"/><Str' + 
'Res Name="4502" Text="Format"/><StrRes Name="4503" Text="Format string:"/><StrRes Name="4504" Text="Decimal separator:"/><StrRes Name="fkText" Text="T' + 
'ext (no formatting)"/><StrRes Name="fkNumber" Text="Number"/><StrRes Name="fkDateTime" Text="Date/Time"/><StrRes Name="fkBoolean" Text="Boolean"/><Str' + 
'Res Name="fkNumber1" Text="1234.5;%g"/><StrRes Name="fkNumber2" Text="1234.50;%2.2f"/><StrRes Name="fkNumber3" Text="1,234.50;%2.2n"/><StrRes Name="fk' + 
'Number4" Text="$1,234.50;%2.2m"/><StrRes Name="fkDateTime1" Text="11.28.2002;mm.dd.yyyy"/><StrRes Name="fkDateTime2" Text="28 nov 2002;dd mmm yyyy"/><' + 
'StrRes Name="fkDateTime3" Text="November 28, 2002;mmmm dd, yyyy"/><StrRes Name="fkDateTime4" Text="02:14;hh:mm"/><StrRes Name="fkDateTime5" Text="02:1' + 
'4am;hh:mm am/pm"/><StrRes Name="fkDateTime6" Text="02:14:00;hh:mm:ss"/><StrRes Name="fkDateTime7" Text="02:14am, November 28, 2002;hh:mm am/pm, mmmm d' + 
'd, yyyy"/><StrRes Name="fkBoolean1" Text="0,1;0,1"/><StrRes Name="fkBoolean2" Text="No,Yes;No,Yes"/><StrRes Name="fkBoolean3" Text="_,x;_,x"/><StrRes ' + 
'Name="fkBoolean4" Text="False,True;False,True"/><StrRes Name="4600" Text="Highlight Editor"/><StrRes Name="4601" Text="Conditions"/><StrRes Name="4602' + 
'" Text="Add"/><StrRes Name="4603" Text="Delete"/><StrRes Name="4604" Text="Edit"/><StrRes Name="4605" Text="Style"/><StrRes Name="4606" Text="Frame"/>' + 
'<StrRes Name="4607" Text="Fill"/><StrRes Name="4608" Text="Font"/><StrRes Name="4609" Text="Visible"/><StrRes Name="4700" Text="Report Settings"/><Str' + 
'Res Name="4701" Text="General"/><StrRes Name="4702" Text="Printer settings"/><StrRes Name="4703" Text="Copies"/><StrRes Name="4704" Text="General"/><S' + 
'trRes Name="4705" Text="Password"/><StrRes Name="4706" Text="Collate copies"/><StrRes Name="4707" Text="Double pass"/><StrRes Name="4708" Text="Print ' + 
'if empty"/><StrRes Name="4709" Text="Description"/><StrRes Name="4710" Text="Name"/><StrRes Name="4711" Text="Description"/><StrRes Name="4712" Text="' + 
'Picture"/><StrRes Name="4713" Text="Author"/><StrRes Name="4714" Text="Major"/><StrRes Name="4715" Text="Minor"/><StrRes Name="4716" Text="Release"/><' + 
'StrRes Name="4717" Text="Build"/><StrRes Name="4718" Text="Created"/><StrRes Name="4719" Text="Modified"/><StrRes Name="4720" Text="Description"/><Str' + 
'Res Name="4721" Text="Version"/><StrRes Name="4722" Text="Browse..."/><StrRes Name="4723" Text="Inheritance settings"/><StrRes Name="4724" Text="Selec' + 
't the option:"/><StrRes Name="4725" Text="Don''t change"/><StrRes Name="4726" Text="Detach the base report"/><StrRes Name="4727" Text="Inherit from bas' + 
'e report:"/><StrRes Name="4728" Text="Inheritance"/><StrRes Name="4729" Text="Templates path :"/><StrRes Name="rePrnOnPort" Text="on"/><StrRes Name="r' + 
'iNotInherited" Text="This report is not inherited."/><StrRes Name="riInherited" Text="This report is inherited from base report: %s"/><StrRes Name="48' + 
'00" Text="Lines"/><StrRes Name="4900" Text="SQL"/><StrRes Name="4901" Text="Query Builder"/><StrRes Name="5000" Text="Password"/><StrRes Name="5001" T' + 
'ext="Enter the password:"/><StrRes Name="5100" Text="Style Editor"/><StrRes Name="5101" Text="Color..."/><StrRes Name="5102" Text="Font..."/><StrRes N' + 
'ame="5103" Text="Frame..."/><StrRes Name="5104" Text="Add"/><StrRes Name="5105" Text="Delete"/><StrRes Name="5106" Text="Edit"/><StrRes Name="5107" Te' + 
'xt="Load"/><StrRes Name="5108" Text="Save"/><StrRes Name="5200" Text="Frame Editor"/><StrRes Name="5201" Text="Frame"/><StrRes Name="5202" Text="Line"' + 
'/><StrRes Name="5203" Text="Shadow"/><StrRes Name="5211" Text="Style:"/><StrRes Name="5214" Text="Color:"/><StrRes Name="5215" Text="Width:"/><StrRes ' + 
'Name="5216" Text="Choose the line style, then choose the line to apply the style."/><StrRes Name="5300" Text="New Item"/><StrRes Name="5301" Text="Ite' + 
'ms"/><StrRes Name="5302" Text="Templates"/><StrRes Name="5303" Text="Inherit the report"/><StrRes Name="5400" Text="Tab Order"/><StrRes Name="5401" Te' + 
'xt="Controls listed in tab order:"/><StrRes Name="5402" Text="Move Up"/><StrRes Name="5403" Text="Move Down"/><StrRes Name="5500" Text="Evaluate"/><St' + 
'rRes Name="5501" Text="Expression"/><StrRes Name="5502" Text="Result"/><StrRes Name="5600" Text="Report Wizard"/><StrRes Name="5601" Text="Data"/><Str' + 
'Res Name="5602" Text="Fields"/><StrRes Name="5603" Text="Groups"/><StrRes Name="5604" Text="Layout"/><StrRes Name="5605" Text="Style"/><StrRes Name="5' + 
'606" Text="Step 1. Select the dataset."/><StrRes Name="5607" Text="Step 2. Select the fields to display."/><StrRes Name="5608" Text="Step 3. Create gr' + 
'oups (optional)."/><StrRes Name="5609" Text="Step 4. Define the page orientation and data layout."/><StrRes Name="5610" Text="Step 5. Choose the repor' + 
't style."/><StrRes Name="5611" Text="Add &#62;"/><StrRes Name="5612" Text="Add all &#62;&#62;"/><StrRes Name="5613" Text="&#60; Remove"/><StrRes Name=' + 
'"5614" Text="&#60;&#60; Remove all"/><StrRes Name="5615" Text="Add &#62;"/><StrRes Name="5616" Text="&#60; Remove"/><StrRes Name="5617" Text="Selected' + 
' fields:"/><StrRes Name="5618" Text="Available fields:"/><StrRes Name="5619" Text="Groups:"/><StrRes Name="5620" Text="Orientation"/><StrRes Name="562' + 
'1" Text="Layout"/><StrRes Name="5622" Text="Portrait"/><StrRes Name="5623" Text="Landscape"/><StrRes Name="5624" Text="Tabular"/><StrRes Name="5625" T' + 
'ext="Columnar"/><StrRes Name="5626" Text="Fit fields to page width"/><StrRes Name="5627" Text="&#60;&#60; Back"/><StrRes Name="5628" Text="Next &#62;&' + 
'#62;"/><StrRes Name="5629" Text="Finish"/><StrRes Name="5630" Text="New table..."/><StrRes Name="5631" Text="New query..."/><StrRes Name="5632" Text="' + 
'Select database connection:"/><StrRes Name="5633" Text="Select a table:"/><StrRes Name="5634" Text="or"/><StrRes Name="5635" Text="Create a query..."/' + 
'><StrRes Name="5636" Text="Configure connections"/><StrRes Name="wzStd" Text="Standard Report Wizard"/><StrRes Name="wzDMP" Text="Dot-Matrix Report Wi' + 
'zard"/><StrRes Name="wzStdEmpty" Text="Standard Report"/><StrRes Name="wzDMPEmpty" Text="Dot-Matrix Report"/><StrRes Name="5700" Text="Connection Wiza' + 
'rd"/><StrRes Name="5701" Text="Connection"/><StrRes Name="5702" Text="Choose the connection type:"/><StrRes Name="5703" Text="Choose the database:"/><' + 
'StrRes Name="5704" Text="Login"/><StrRes Name="5705" Text="Password"/><StrRes Name="5706" Text="Prompt login"/><StrRes Name="5707" Text="Use login/pas' + 
'sword:"/><StrRes Name="5708" Text="Table"/><StrRes Name="5709" Text="Choose the table name:"/><StrRes Name="5710" Text="Filter records:"/><StrRes Name' + 
'="5711" Text="Query"/><StrRes Name="5712" Text="SQL statement:"/><StrRes Name="5713" Text="Query Builder"/><StrRes Name="5714" Text="Edit Query Parame' + 
'ters"/><StrRes Name="5715" Text="Move top"/><StrRes Name="5716" Text="Swap side"/><StrRes Name="ftAllFiles" Text="All Files"/><StrRes Name="ftPictures' + 
'" Text="Pictures"/><StrRes Name="ftPDF" Text="PDF file"/><StrRes Name="ftDB" Text="Databases"/><StrRes Name="ftRichFile" Text="RichText file"/><StrRes' + 
' Name="ftTextFile" Text="Text file"/><StrRes Name="prNotAssigned" Text="(Not assigned)"/><StrRes Name="prInvProp" Text="Invalid property value"/><StrR' + 
'es Name="prDupl" Text="Duplicate name"/><StrRes Name="prPict" Text="(Picture)"/><StrRes Name="mvExpr" Text="Allow Expressions"/><StrRes Name="mvStretc' + 
'h" Text="Stretch"/><StrRes Name="mvStretchToMax" Text="Stretch to Max Height"/><StrRes Name="mvShift" Text="Shift"/><StrRes Name="mvShiftOver" Text="S' + 
'hift When Overlapped"/><StrRes Name="mvVisible" Text="Visible"/><StrRes Name="mvPrintable" Text="Printable"/><StrRes Name="mvFont" Text="Font..."/><St' + 
'rRes Name="mvFormat" Text="Display Format..."/><StrRes Name="mvClear" Text="Clear Contents"/><StrRes Name="mvAutoWidth" Text="Auto Width"/><StrRes Nam' + 
'e="mvWWrap" Text="Word Wrap"/><StrRes Name="mvSuppress" Text="Suppress Repeated Values"/><StrRes Name="mvHideZ" Text="Hide Zeros"/><StrRes Name="mvHTM' + 
'L" Text="Allow HTML Tags"/><StrRes Name="lvDiagonal" Text="Diagonal"/><StrRes Name="pvAutoSize" Text="Auto Size"/><StrRes Name="pvCenter" Text="Center' + 
'"/><StrRes Name="pvAspect" Text="Keep Aspect Ratio"/><StrRes Name="bvSplit" Text="Allow Split"/><StrRes Name="bvKeepChild" Text="Keep Child Together"/' + 
'><StrRes Name="bvPrintChild" Text="Print Child If Invisible"/><StrRes Name="bvStartPage" Text="Start New Page"/><StrRes Name="bvPrintIfEmpty" Text="Pr' + 
'int If Detail Empty"/><StrRes Name="bvKeepDetail" Text="Keep Detail Together"/><StrRes Name="bvKeepFooter" Text="Keep Footer Together"/><StrRes Name="' + 
'bvReprint" Text="Reprint On New Page"/><StrRes Name="bvOnFirst" Text="Print On First Page"/><StrRes Name="bvOnLast" Text="Print On Last Page"/><StrRes' + 
' Name="bvKeepGroup" Text="Keep Together"/><StrRes Name="bvFooterAfterEach" Text="Footer After Each Row"/><StrRes Name="bvDrillDown" Text="Drill-Down"/' + 
'><StrRes Name="bvResetPageNo" Text="Reset Page Numbers"/><StrRes Name="srParent" Text="Print On Parent"/><StrRes Name="bvKeepHeader" Text="Keep Header' + 
' Together"/><StrRes Name="obCatDraw" Text="Draw"/><StrRes Name="obCatOther" Text="Other objects"/><StrRes Name="obCatOtherControls" Text="Other contro' + 
'ls"/><StrRes Name="obDiagLine" Text="Diagonal line"/><StrRes Name="obRect" Text="Rectangle"/><StrRes Name="obRoundRect" Text="Rounded rectangle"/><Str' + 
'Res Name="obEllipse" Text="Ellipse"/><StrRes Name="obTrian" Text="Triangle"/><StrRes Name="obDiamond" Text="Diamond"/><StrRes Name="obLabel" Text="Lab' + 
'el control"/><StrRes Name="obEdit" Text="Edit control"/><StrRes Name="obMemoC" Text="Memo control"/><StrRes Name="obButton" Text="Button control"/><St' + 
'rRes Name="obChBoxC" Text="CheckBox control"/><StrRes Name="obRButton" Text="RadioButton control"/><StrRes Name="obLBox" Text="ListBox control"/><StrR' + 
'es Name="obCBox" Text="ComboBox control"/><StrRes Name="obDateEdit" Text="DateEdit control"/><StrRes Name="obImageC" Text="Image control"/><StrRes Nam' + 
'e="obPanel" Text="Panel control"/><StrRes Name="obGrBox" Text="GroupBox control"/><StrRes Name="obBBtn" Text="BitBtn control"/><StrRes Name="obSBtn" T' + 
'ext="SpeedButton control"/><StrRes Name="obMEdit" Text="MaskEdit control"/><StrRes Name="obChLB" Text="CheckListBox control"/><StrRes Name="obDBLookup' + 
'" Text="DBLookupComboBox control"/><StrRes Name="obBevel" Text="Bevel object"/><StrRes Name="obShape" Text="Shape object"/><StrRes Name="obText" Text=' + 
'"Text object"/><StrRes Name="obSysText" Text="System text"/><StrRes Name="obLine" Text="Line object"/><StrRes Name="obPicture" Text="Picture object"/>' + 
'<StrRes Name="obBand" Text="Band object"/><StrRes Name="obDataBand" Text="Data band"/><StrRes Name="obSubRep" Text="Subreport object"/><StrRes Name="o' + 
'bDlgPage" Text="Dialog form"/><StrRes Name="obRepPage" Text="Report page"/><StrRes Name="obReport" Text="Report object"/><StrRes Name="obRich" Text="R' + 
'ichText object"/><StrRes Name="obPDF" Text="PDF object"/><StrRes Name="obOLE" Text="OLE object"/><StrRes Name="obChBox" Text="CheckBox object"/><StrRe' + 
's Name="obChart" Text="Chart object"/><StrRes Name="obBarC" Text="Barcode object"/><StrRes Name="obCross" Text="Cross-tab object"/><StrRes Name="obDBC' + 
'ross" Text="DB Cross-tab object"/><StrRes Name="obGrad" Text="Gradient object"/><StrRes Name="obDMPText" Text="Dot-matrix Text object"/><StrRes Name="' + 
'obDMPLine" Text="Dot-matrix Line object"/><StrRes Name="obDMPCmd" Text="Dot-matrix Command object"/><StrRes Name="obBDEDB" Text="BDE Database"/><StrRe' + 
's Name="obBDETb" Text="BDE Table"/><StrRes Name="obBDEQ" Text="BDE Query"/><StrRes Name="obBDEComps" Text="BDE components"/><StrRes Name="obIBXDB" Tex' + 
't="IBX Database"/><StrRes Name="obIBXTb" Text="IBX Table"/><StrRes Name="obIBXQ" Text="IBX Query"/><StrRes Name="obIBXComps" Text="IBX Components"/><S' + 
'trRes Name="obADODB" Text="ADO Database"/><StrRes Name="obADOTb" Text="ADO Table"/><StrRes Name="obADOQ" Text="ADO Query"/><StrRes Name="obADOComps" T' + 
'ext="ADO Components"/><StrRes Name="obDBXDB" Text="DBX Database"/><StrRes Name="obDBXTb" Text="DBX Table"/><StrRes Name="obDBXQ" Text="DBX Query"/><St' + 
'rRes Name="obDBXComps" Text="DBX Components"/><StrRes Name="obFIBDB" Text="FIB Database"/><StrRes Name="obFIBTb" Text="FIB Table"/><StrRes Name="obFIB' + 
'Q" Text="FIB Query"/><StrRes Name="obFIBComps" Text="FIB Components"/><StrRes Name="obDataBases" Text="DataBases"/><StrRes Name="obTables" Text="Table' + 
's"/><StrRes Name="obQueries" Text="Queries"/><StrRes Name="ctString" Text="String"/><StrRes Name="ctDate" Text="Date and Time"/><StrRes Name="ctConv" ' + 
'Text="Conversions"/><StrRes Name="ctFormat" Text="Formatting"/><StrRes Name="ctMath" Text="Mathematical"/><StrRes Name="ctOther" Text="Other"/><StrRes' + 
' Name="IntToStr" Text="Converts an integer value to a string"/><StrRes Name="FloatToStr" Text="Converts a float value to a string"/><StrRes Name="Date' + 
'ToStr" Text="Converts a date to a string"/><StrRes Name="TimeToStr" Text="Converts a time to a string"/><StrRes Name="DateTimeToStr" Text="Converts a ' + 
'date-and-time value to a string"/><StrRes Name="VarToStr" Text="Converts a variant value to a string"/><StrRes Name="StrToInt" Text="Converts a string' + 
' to an integer value"/><StrRes Name="StrToInt64" Text="Converts a string to an Int64 value"/><StrRes Name="StrToFloat" Text="Converts a string to a fl' + 
'oating-point value"/><StrRes Name="StrToDate" Text="Converts a string to a date format"/><StrRes Name="StrToTime" Text="Converts a string to a time fo' + 
'rmat"/><StrRes Name="StrToDateTime" Text="Converts a string to a date-and-time format"/><StrRes Name="Format" Text="Returns formatted string assembled' + 
' from a series of array arguments"/><StrRes Name="FormatFloat" Text="Formats a floating-point value"/><StrRes Name="FormatDateTime" Text="Formats a da' + 
'te-and-time value"/><StrRes Name="FormatMaskText" Text="Returns a string formatted using an edit mask"/><StrRes Name="EncodeDate" Text="Returns a TDat' + 
'eTime type for a specified Year, Month, and Day"/><StrRes Name="DecodeDate" Text="Breaks TDateTime into Year, Month, and Day values"/><StrRes Name="En' + 
'codeTime" Text="Returns a TDateTime type for a specified Hour, Min, Sec, and MSec"/><StrRes Name="DecodeTime" Text="Breaks TDateTime into hours, minut' + 
'es, seconds, and milliseconds"/><StrRes Name="Date" Text="Returns current date"/><StrRes Name="Time" Text="Returns current time"/><StrRes Name="Now" T' + 
'ext="Return current date and time"/><StrRes Name="DayOfWeek" Text="Returns the day of the week for a specified date"/><StrRes Name="IsLeapYear" Text="' + 
'Indicates whether a specified year is a leap year"/><StrRes Name="DaysInMonth" Text="Returns a number of days in a specified month"/><StrRes Name="Len' + 
'gth" Text="Returns a length of a string"/><StrRes Name="Copy" Text="Returns a substring of a string"/><StrRes Name="Pos" Text="Returns a position of a' + 
' substring in a string"/><StrRes Name="Delete" Text="Removes a substring from a string"/><StrRes Name="Insert" Text="Inserts a substring into a string' + 
'"/><StrRes Name="Uppercase" Text="Converts all character in a string to upper case"/><StrRes Name="Lowercase" Text="Converts all character in a string' + 
' to lower case"/><StrRes Name="Trim" Text="Trims all trailing and leading spaces in a string"/><StrRes Name="NameCase" Text="Converts first character ' + 
'of a word to upper case"/><StrRes Name="CompareText" Text="Compares two strings"/><StrRes Name="Chr" Text="Converts an integer value to a char"/><StrR' + 
'es Name="Ord" Text="Converts a character value to an integer"/><StrRes Name="SetLength" Text="Sets the length of a string"/><StrRes Name="Round" Text=' + 
'"Rounds a floating-point value to the nearest whole number"/><StrRes Name="Trunc" Text="Truncates a floating-point value to an integer"/><StrRes Name=' + 
'"Int" Text="Returns the integer part of a real number"/><StrRes Name="Frac" Text="Returns the fractional part of a real number"/><StrRes Name="Sqrt" T' + 
'ext="Returns the square root of a specified number"/><StrRes Name="Abs" Text="Returns an absolute value"/><StrRes Name="Sin" Text="Returns the sine of' + 
' an angle (in radians)"/><StrRes Name="Cos" Text="Returns the cosine of an angle (in radians)"/><StrRes Name="ArcTan" Text="Returns the arctangent"/><' + 
'StrRes Name="Tan" Text="Returns the tangent"/><StrRes Name="Exp" Text="Returns the exponential"/><StrRes Name="Ln" Text="Returns the natural log of a ' + 
'real expression"/><StrRes Name="Pi" Text="Returns the 3.1415926... number"/><StrRes Name="Inc" Text="Increments a value"/><StrRes Name="Dec" Text="Dec' + 
'rements a value"/><StrRes Name="RaiseException" Text="Raises an exception"/><StrRes Name="ShowMessage" Text="Shows a message box"/><StrRes Name="Rando' + 
'mize" Text="Starts the random numbers generator"/><StrRes Name="Random" Text="Returns a random number"/><StrRes Name="ValidInt" Text="Returns True if ' + 
'specified string contains a valid integer"/><StrRes Name="ValidFloat" Text="Returns True if specified string contains a valid float"/><StrRes Name="Va' + 
'lidDate" Text="Returns True if specified string contains a valid date"/><StrRes Name="IIF" Text="Returns TrueValue if specified Expr is True, otherwis' + 
'e returns FalseValue"/><StrRes Name="Get" Text="For internal use only"/><StrRes Name="Set" Text="For internal use only"/><StrRes Name="InputBox" Text=' + 
'"Displays an input dialog box that enables the user to enter a string"/><StrRes Name="InputQuery" Text="Displays an input dialog box that enables the ' + 
'user to enter a string"/><StrRes Name="MessageDlg" Text="Shows a message box"/><StrRes Name="CreateOleObject" Text="Creates an OLE object"/><StrRes Na' + 
'me="VarArrayCreate" Text="Creates a variant array"/><StrRes Name="VarType" Text="Return a type of the variant value"/><StrRes Name="DayOf" Text="Retur' + 
'ns day number (1..31) of given Date"/><StrRes Name="MonthOf" Text="Returns month number (1..12) of given Date"/><StrRes Name="YearOf" Text="Returns ye' + 
'ar of given Date"/><StrRes Name="ctAggregate" Text="Aggregate"/><StrRes Name="Sum" Text="Calculates the sum of the Expr for the Band datarow"/><StrRes' + 
' Name="Avg" Text="Calculates the average of the Expr for the Band datarow"/><StrRes Name="Min" Text="Calculates the minimum of the Expr for the Band d' + 
'atarow"/><StrRes Name="Max" Text="Calculates the maximum of the Expr for the Band datarow"/><StrRes Name="Count" Text="Calculates the number of dataro' + 
'ws"/><StrRes Name="wzDBConn" Text="New Connection Wizard"/><StrRes Name="wzDBTable" Text="New Table Wizard"/><StrRes Name="wzDBQuery" Text="New Query ' + 
'Wizard"/><StrRes Name="5800" Text="Connections"/><StrRes Name="5801" Text="New"/><StrRes Name="5802" Text="Delete"/><StrRes Name="cpName" Text="Name"/' + 
'><StrRes Name="cpConnStr" Text="Connection string"/><StrRes Name="startCreateNew" Text="Create new report"/><StrRes Name="startCreateBlank" Text="Crea' + 
'te blank report"/><StrRes Name="startOpenReport" Text="Open report"/><StrRes Name="startOpenLast" Text="Open last report"/><StrRes Name="startEditAlia' + 
'ses" Text="Edit connection aliases"/><StrRes Name="startHelp" Text="Help"/><StrRes Name="5900" Text="Watches"/><StrRes Name="5901" Text="Add Watch"/><' + 
'StrRes Name="5902" Text="Delete Watch"/><StrRes Name="5903" Text="Edit Watch"/><StrRes Name="6000" Text="Inherit error"/><StrRes Name="6001" Text="Bas' + 
'e and inherited reports have duplicate objects. What should we do?"/><StrRes Name="6002" Text="Delete duplicates"/><StrRes Name="6003" Text="Rename du' + 
'plicates"/><StrRes Name="6004" Text="Sort by Name"/><StrRes Name="crGroup" Text="Group"/><StrRes Name="4331" Text="Grouping"/><StrRes Name="dsColorOth' + 
'" Text="Other..."/><StrRes Name="6100" Text="Fill Editor"/><StrRes Name="6101" Text="Brush"/><StrRes Name="6102" Text="Gradient"/><StrRes Name="6103" ' + 
'Text="Glass"/><StrRes Name="6104" Text="Brush style:"/><StrRes Name="6105" Text="Background color:"/><StrRes Name="6106" Text="Foreground color:"/><St' + 
'rRes Name="6107" Text="Gradient style:"/><StrRes Name="6108" Text="Start color:"/><StrRes Name="6109" Text="End color:"/><StrRes Name="6110" Text="Ori' + 
'entation:"/><StrRes Name="6111" Text="Color:"/><StrRes Name="6112" Text="Blend:"/><StrRes Name="6113" Text="Show hatch"/><StrRes Name="bsSolid" Text="' + 
'Solid"/><StrRes Name="bsClear" Text="Clear"/><StrRes Name="bsHorizontal" Text="Horizontal"/><StrRes Name="bsVertical" Text="Vertical"/><StrRes Name="b' + 
'sFDiagonal" Text="Forward Diagonal"/><StrRes Name="bsBDiagonal" Text="Backward Diagonal"/><StrRes Name="bsCross" Text="Cross"/><StrRes Name="bsDiagCro' + 
'ss" Text="Diagonal cross"/><StrRes Name="gsHorizontal" Text="Horizontal"/><StrRes Name="gsVertical" Text="Vertical"/><StrRes Name="gsElliptic" Text="E' + 
'lliptic"/><StrRes Name="gsRectangle" Text="Rectangle"/><StrRes Name="gsVertCenter" Text="Vertical Center"/><StrRes Name="gsHorizCenter" Text="Horizont' + 
'al Center"/><StrRes Name="foVertical" Text="Vertical"/><StrRes Name="foHorizontal" Text="Horizontal"/><StrRes Name="foVerticalMirror" Text="Vertical M' + 
'irror"/><StrRes Name="foHorizontalMirror" Text="Horizontal Mirror"/><StrRes Name="6200" Text="Hyperlink Editor"/><StrRes Name="6201" Text="Hyperlink k' + 
'ind"/><StrRes Name="6202" Text="URL"/><StrRes Name="6203" Text="Page number"/><StrRes Name="6204" Text="Anchor"/><StrRes Name="6205" Text="Report"/><S' + 
'trRes Name="6206" Text="Report page"/><StrRes Name="6207" Text="Custom"/><StrRes Name="6208" Text="Properties"/><StrRes Name="6209" Text="Specify an U' + 
'RL (for example: http://www.url.com):"/><StrRes Name="6210" Text="or enter the expression that returns an URL:"/><StrRes Name="6211" Text="Specify a p' + 
'age number:"/><StrRes Name="6212" Text="or enter the expression that returns a page number:"/><StrRes Name="6213" Text="Specify an anchor name:"/><Str' + 
'Res Name="6214" Text="or enter the expression that returns an anchor name:"/><StrRes Name="6215" Text="Report name:"/><StrRes Name="6216" Text="Report' + 
' variable:"/><StrRes Name="6217" Text="Specify a variable value:"/><StrRes Name="6218" Text="or enter the expression that returns a variable value:"/>' + 
'<StrRes Name="6219" Text="Report page:"/><StrRes Name="6220" Text="Specify a hyperlink value:"/><StrRes Name="6221" Text="or enter the expression that' + 
' returns a hyperlink value:"/><StrRes Name="6222" Text="What will happen if you click this object in the preview window:"/><StrRes Name="6223" Text="S' + 
'pecified URL will be opened."/><StrRes Name="6224" Text="You will go to the specified page."/><StrRes Name="6225" Text="You will go to the object that' + 
' contains specified anchor."/><StrRes Name="6226" Text="Specified report will be generated and opened in a separate preview tab."/><StrRes Name="6227"' + 
' Text="Specified page will be generated and opened in a separate preview tab."/><StrRes Name="6228" Text="You should create OnClick event handler to d' + 
'efine a custom action."/><StrRes Name="6229" Text="Modify the object''s appearance so it will look like a clickable link"/><StrRes Name="6300" Text="Se' + 
'lect same type on Parent"/><StrRes Name="6301" Text="Select same type on Page"/><StrRes Name="6302" Text="Reset to Parent object"/><StrRes Name="6303"' + 
' Text="Reset to Parent object with childs"/><StrRes Name="mvHyperlink" Text="Hyperlink..."/><StrRes Name="obMap" Text="Object &#38;#34;Map&#38;#34;"/>' + 
'<StrRes Name="6350" Text="Color Range Collection Editor"/><StrRes Name="6351" Text="Auto"/><StrRes Name="Auto" Text="Auto"/><StrRes Name="6352" Text="' + 
'Color"/><StrRes Name="6353" Text="Start"/><StrRes Name="6354" Text="End"/><StrRes Name="6355" Text="Add"/><StrRes Name="6356" Text="Delete"/><StrRes N' + 
'ame="6360" Text="Size Range Collection Editor"/><StrRes Name="6361" Text="Size"/><StrRes Name="6362" Text="Shape Tags Editor"/><StrRes Name="6363" Tex' + 
't="Map file (*.shp, *.osm, *.gpx)"/><StrRes Name="6364" Text="Embed the file in the report"/><StrRes Name="6365" Text="Select source:"/><StrRes Name="' + 
'6366" Text="Empty layer with geodata provided by an application"/><StrRes Name="6367" Text="Add Layer"/><StrRes Name="6368" Text="Interactive layer"/>' + 
'<StrRes Name="6369" Text="Layer with geodata provided by a database"/><StrRes Name="AddLayer" Text="Add Layer..."/><StrRes Name="DrawInteractiveLayer"' + 
' Text="Draw..."/><StrRes Name="6370" Text="Map Editor"/><StrRes Name="6371" Text="Map"/><StrRes Name="6372" Text="Add..."/><StrRes Name="6380" Text="G' + 
'eneral"/><StrRes Name="6381" Text="Keep Aspect Ratio"/><StrRes Name="6382" Text="Mercator Projection"/><StrRes Name="6383" Text="Fill..."/><StrRes Nam' + 
'e="6384" Text="Frame..."/><StrRes Name="6390" Text="Color Scale"/><StrRes Name="6391" Text="Visible"/><StrRes Name="6392" Text="Border Color:"/><StrRe' + 
's Name="6393" Text="Border Width:"/><StrRes Name="6394" Text="Dock:"/><StrRes Name="6395" Text="Title:"/><StrRes Name="6396" Text="Text:"/><StrRes Nam' + 
'e="6397" Text="Font..."/><StrRes Name="6398" Text="Values"/><StrRes Name="6399" Text="Format"/><StrRes Name="6400" Text="Size Scale"/><StrRes Name="64' + 
'10" Text="Data"/><StrRes Name="6411" Text="Data source:"/><StrRes Name="6412" Text="Filter:"/><StrRes Name="6413" Text="Zoom the polygon:"/><StrRes Na' + 
'me="6414" Text="Analytical data"/><StrRes Name="6415" Text="Value:"/><StrRes Name="6416" Text="Function:"/><StrRes Name="6417" Text="Spatial data"/><S' + 
'trRes Name="6418" Text="Column:"/><StrRes Name="6419" Text="Latitude:"/><StrRes Name="6420" Text="Longitude:"/><StrRes Name="6421" Text="Label:"/><Str' + 
'Res Name="6422" Text="Geodata:"/><StrRes Name="6423" Text="DataSet"/><StrRes Name="6424" Text="Data Kind:"/><StrRes Name="opSum" Text="Sum"/><StrRes N' + 
'ame="opMin" Text="Min"/><StrRes Name="opMax" Text="Max"/><StrRes Name="opAverage" Text="Average"/><StrRes Name="opCount" Text="Count"/><StrRes Name="6' + 
'430" Text="Appearance"/><StrRes Name="6431" Text="Visible"/><StrRes Name="6432" Text="Border style:"/><StrRes Name="6433" Text="Fill color:"/><StrRes ' + 
'Name="6434" Text="Palette:"/><StrRes Name="6435" Text="Point size:"/><StrRes Name="6436" Text="Highlight color:"/><StrRes Name="psSolid" Text="Solid"/' + 
'><StrRes Name="psDash" Text="Dash"/><StrRes Name="psDot" Text="Dot"/><StrRes Name="psDashDot" Text="Dash Dot"/><StrRes Name="psDashDotDot" Text="Dash ' + 
'Dot Dot"/><StrRes Name="psClear" Text="Clear"/><StrRes Name="psInsideFrame" Text="Inside Frame"/><StrRes Name="psUserStyle" Text="User Style"/><StrRes' + 
' Name="psAlternate" Text="Alternate"/><StrRes Name="mpNone" Text="None"/><StrRes Name="mpLight" Text="Light"/><StrRes Name="mpPastel" Text="Pastel"/><' + 
'StrRes Name="mpGrayScale" Text="Gray Scale"/><StrRes Name="mpEarth" Text="Earth"/><StrRes Name="mpSea" Text="Sea"/><StrRes Name="mpBrightPastel" Text=' + 
'"Bright Pastel"/><StrRes Name="6440" Text="Color Ranges"/><StrRes Name="6441" Text="Start Color:"/><StrRes Name="6442" Text="Middle Color:"/><StrRes N' + 
'ame="6443" Text="End Color:"/><StrRes Name="6444" Text="Number of ranges:"/><StrRes Name="6445" Text="Ranking Factor:"/><StrRes Name="rfValue" Text="V' + 
'alue"/><StrRes Name="rfPercentile" Text="Percentile"/><StrRes Name="rfCluster" Text="Cluster"/><StrRes Name="rfAutoCluster" Text="AutoCluster"/><StrRe' + 
's Name="6450" Text="Size Ranges"/><StrRes Name="6451" Text="Start Size:"/><StrRes Name="6452" Text="End Size:"/><StrRes Name="6460" Text="Labels"/><St' + 
'rRes Name="6461" Text="Label Kind:"/><StrRes Name="6462" Text="Label Column:"/><StrRes Name="mlNone" Text="None"/><StrRes Name="mlName" Text="Name"/><' + 
'StrRes Name="mlValue" Text="Value"/><StrRes Name="mlNameAndValue" Text="Name and Value"/><StrRes Name="IOSaveTO" Text="Save As"/><StrRes Name="IOSelec' + 
'tDir" Text="Select directory"/><StrRes Name="IOOpenFile" Text="Select file"/><StrRes Name="IOTransportUploading" Text="Uploading to"/><StrRes Name="IO' + 
'TransportDownloading" Text="Downloading from"/><StrRes Name="FTPIOTransport" Text="FTP..."/><StrRes Name="6470" Text="Host Name:"/><StrRes Name="6471"' + 
' Text="Required fields are not filled"/><StrRes Name="6472" Text="Remember Properties"/><StrRes Name="6473" Text="Port:"/><StrRes Name="6474" Text="Us' + 
'er Name:"/><StrRes Name="6475" Text="Password:"/><StrRes Name="6476" Text="Use passive mode for transfers"/><StrRes Name="6477" Text="Remote Dir:"/><S' + 
'trRes Name="6478" Text="FTP connection details"/><StrRes Name="6479" Text="General"/><StrRes Name="6480" Text="Proxy"/><StrRes Name="6481" Text="Conne' + 
'ct Method:"/><StrRes Name="6482" Text="File Name:"/><StrRes Name="6483" Text="Create Directory"/><StrRes Name="6484" Text="Delete"/><StrRes Name="Dele' + 
'teDirectory" Text="Do you really want to delete directory"/><StrRes Name="DeleteFile" Text="Do you really want to delete file"/><StrRes Name="DropboxI' + 
'OTransport" Text="Dropbox..."/><StrRes Name="6490" Text="Dropbox connection details"/><StrRes Name="6491" Text="Application Key:"/><StrRes Name="6492"' + 
' Text="Browser"/><StrRes Name="6493" Text="Use Proxy Server"/><StrRes Name="6494" Text="Pass login to browser (not safe)"/><StrRes Name="6495" Text="A' + 
'pplication secret:"/><StrRes Name="6496" Text="Show/Hide"/><StrRes Name="6497" Text="Setup connection"/><StrRes Name="OneDriveIOTransport" Text="OneDr' + 
'ive..."/><StrRes Name="6500" Text="OneDrive connection details"/><StrRes Name="6501" Text="Application ID:"/><StrRes Name="BoxComIOTransport" Text="Bo' + 
'x.com..."/><StrRes Name="6510" Text="Box.com connection details"/><StrRes Name="6511" Text="Client ID"/><StrRes Name="6512" Text="Client Secret"/><Str' + 
'Res Name="6513" Text="ID"/><StrRes Name="GoogleDriveIOTransport" Text="GoogleDrive..."/><StrRes Name="6520" Text="Google Drive connection details"/><S' + 
'trRes Name="YandexDiskIOTransport" Text="YandexDisk..."/><StrRes Name="6521" Text="Yandex Disk connection details"/><StrRes Name="GMailIOTransport" Te' + 
'xt="GoogleMail..."/><StrRes Name="6522" Text="Google Mail connection details"/><StrRes Name="OutlookIOTransport" Text="Outlook..."/><StrRes Name="6523' + 
'" Text="Outlook connection details"/><StrRes Name="obGauge" Text="Object &#38;#34;Gauge&#38;#34;"/><StrRes Name="obIntervalGauge" Text="Object &#38;#3' + 
'4;Interval Gauge&#38;#34;"/><StrRes Name="6530" Text="Gauge Editor"/><StrRes Name="6531" Text="Major Scale"/><StrRes Name="6532" Text="Minor Scale"/><' + 
'StrRes Name="6533" Text="Pointer"/><StrRes Name="6534" Text="Gauge Kind:"/><StrRes Name="GaugeKind0" Text="Horizontal"/><StrRes Name="GaugeKind1" Text' + 
'="Vertical"/><StrRes Name="GaugeKind2" Text="Circle"/><StrRes Name="6535" Text="Pointer Kind:"/><StrRes Name="PointerKind0" Text="Segment"/><StrRes Na' + 
'me="PointerKind1" Text="Triangle"/><StrRes Name="PointerKind2" Text="Diamond"/><StrRes Name="PointerKind3" Text="Pentagon"/><StrRes Name="PointerKind4' + 
'" Text="Band"/><StrRes Name="6536" Text="Minimum:"/><StrRes Name="6537" Text="Maximum:"/><StrRes Name="6538" Text="Value:"/><StrRes Name="6539" Text="' + 
'Major Scale Step:"/><StrRes Name="6540" Text="Minor Scale Step:"/><StrRes Name="6541" Text="Margins"/><StrRes Name="6542" Text="Left:"/><StrRes Name="' + 
'6543" Text="Top:"/><StrRes Name="6544" Text="Right:"/><StrRes Name="6545" Text="Bottom:"/><StrRes Name="6546" Text="Width:"/><StrRes Name="6547" Text=' + 
'"Height:"/><StrRes Name="6548" Text="Visible Digits"/><StrRes Name="6549" Text="Bilateral"/><StrRes Name="6550" Text="Ticks"/><StrRes Name="6551" Text' + 
'="Length"/><StrRes Name="6552" Text="Start Value:"/><StrRes Name="6553" Text="End Value:"/><StrRes Name="6554" Text="Angle:"/><StrRes Name="obZipCode"' + 
' Text="Object &#38;#34;Zip Code&#38;#34;"/><StrRes Name="ltTag" Text="Tag"/><StrRes Name="ltAmount" Text="Amount"/><StrRes Name="6560" Text="Layer Tag' + 
's Editor"/><StrRes Name="6561" Text="File Tags"/><StrRes Name="6562" Text="Layer Tags"/><StrRes Name="6563" Text="Tag Filter:"/><StrRes Name="stPoint"' + 
' Text="Point"/><StrRes Name="stPolyline" Text="Polyline"/><StrRes Name="stPolygon" Text="Polygon"/><StrRes Name="stRect" Text="Rectangle"/><StrRes Nam' + 
'e="stEllipse" Text="Ellipse"/><StrRes Name="stDiamond" Text="Diamond"/><StrRes Name="stPicture" Text="Picture"/><StrRes Name="stLegend" Text="Legend"/' + 
'><StrRes Name="6570" Text="Save"/><StrRes Name="6571" Text="Select"/><StrRes Name="6572" Text="Point"/><StrRes Name="6573" Text="Polyline"/><StrRes Na' + 
'me="6574" Text="Polygon"/><StrRes Name="6575" Text="Tags"/><StrRes Name="6576" Text="Delete Edited Point"/><StrRes Name="6577" Text="Remove from Layer' + 
'"/><StrRes Name="6578" Text="Rectangle"/><StrRes Name="6579" Text="Rounded rectangle"/><StrRes Name="6580" Text="Ellipse"/><StrRes Name="6581" Text="D' + 
'iamond"/><StrRes Name="6582" Text="Picture"/><StrRes Name="6583" Text="Edit Picture"/><StrRes Name="6584" Text="Сonstrain Proportions"/><StrRes Name=' + 
'"6585" Text="Legend"/><StrRes Name="6586" Text="Enabled"/><StrRes Name="6587" Text="Edit condition"/><StrRes Name="6588" Text="Enter breakpoint condit' + 
'ion"/><StrRes Name="6589" Text="disabled"/><StrRes Name="6590" Text="not accessible"/><StrRes Name="6591" Text="Watches"/><StrRes Name="6592" Text="Lo' + 
'cal"/><StrRes Name="6593" Text="Code completion and Syntax memo"/><StrRes Name="6594" Text="Show script variables"/><StrRes Name="6595" Text="Show rep' + 
'ort objects"/><StrRes Name="6596" Text="Show Rtti variables"/><StrRes Name="6597" Text="Tab stops:"/><StrRes Name="6598" Text="Stick to guides"/><StrR' + 
'es Name="6599" Text="Use guides as anchor"/><StrRes Name="6600" Text="Editors configurator .."/><StrRes Name="6601" Text="Save data"/><StrRes Name="66' + 
'02" Text="Clear data"/><StrRes Name="6603" Text="Add row before"/><StrRes Name="6604" Text="Add row after"/><StrRes Name="6605" Text="Add column befor' + 
'e"/><StrRes Name="6606" Text="Add column after"/><StrRes Name="6607" Text="Join cell"/><StrRes Name="6608" Text="Split cell"/><StrRes Name="6609" Text' + 
'="Enable code window hints"/><StrRes Name="6610" Text="Enable workspace hints"/><StrRes Name="obTable" Text="Table object"/><StrRes Name="obCellularTe' + 
'xt" Text="CellularText object"/><StrRes Name="obDigitalSignature" Text="Digital Signature"/><StrRes Name="6700" Text="SQL Syntax:"/><StrRes Name="6701' + 
'" Text="Edit SQL syntax"/><StrRes Name="6702" Text="New syntax preset"/><StrRes Name="6703" Text="New syntax Style"/><StrRes Name="6704" Text="Delete ' + 
'syntax"/><StrRes Name="6705" Text="Load SQL syntax"/><StrRes Name="6706" Text="Save SQL syntax"/><StrRes Name="6707" Text="SQL Syntax presets|*.ini"/>' + 
'<StrRes Name="6710" Text="Guides stick accuracy:"/><StrRes Name="obHTML" Text="HTML object"/><StrRes Name="obListBox" Text="ListBox object"/><StrRes N' + 
'ame="obComboBox" Text="ComboBox object"/><StrRes Name="6711" Text="Highlight intersection of objects"/></Resources>' + 
' ';

initialization
  frxResources.AddXML(resXML);

end.

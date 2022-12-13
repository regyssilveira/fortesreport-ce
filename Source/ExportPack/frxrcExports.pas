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

unit frxrcExports;

interface

implementation

uses frxRes;

const resXML =
'<?xml version="1.1" encoding="utf-8"?><Resources CodePage="1252"><StrRes Name="8000" Text="Export to Excel"/><StrRes Name="8001" Text="Styles"/><StrRe' + 
's Name="8002" Text="Pictures"/><StrRes Name="8003" Text="Merge cells"/><StrRes Name="8004" Text="Fast export"/><StrRes Name="8005" Text="WYSIWYG"/><St' + 
'rRes Name="8006" Text="As text"/><StrRes Name="8007" Text="Background"/><StrRes Name="8008" Text="Open Excel after export"/><StrRes Name="8009" Text="' + 
'Excel file (*.xls)|*.xls"/><StrRes Name="8010" Text=".xls"/><StrRes Name="8100" Text="Export to Excel"/><StrRes Name="8101" Text="Styles"/><StrRes Nam' + 
'e="8102" Text="WYSIWYG"/><StrRes Name="8103" Text="Background"/><StrRes Name="8104" Text="Open Excel after export"/><StrRes Name="8105" Text="XML Exce' + 
'l file (*.xls)|*.xls"/><StrRes Name="8106" Text=".xls"/><StrRes Name="8200" Text="Export to HTML table"/><StrRes Name="8201" Text="Open after export"/' + 
'><StrRes Name="8202" Text="Styles"/><StrRes Name="8203" Text="Pictures"/><StrRes Name="8204" Text="All in one folder"/><StrRes Name="8205" Text="Fixed' + 
' width"/><StrRes Name="8206" Text="Page navigator"/><StrRes Name="8207" Text="Multipage"/><StrRes Name="8208" Text="Mozilla browser"/><StrRes Name="82' + 
'09" Text="Background"/><StrRes Name="8210" Text="HTML file (*.html)|*.html"/><StrRes Name="8211" Text=".html"/><StrRes Name="8300" Text="Export to tex' + 
't (dot-matrix printer)"/><StrRes Name="8301" Text="Preview on/off"/><StrRes Name="8302" Text=" Export properties  "/><StrRes Name="8303" Text="Page br' + 
'eaks"/><StrRes Name="8304" Text="OEM codepage"/><StrRes Name="8305" Text="Empty lines"/><StrRes Name="8306" Text="Lead spaces"/><StrRes Name="8307" Te' + 
'xt="Page numbers"/><StrRes Name="8308" Text="Enter numbers and/or page ranges, separated by commas. For example: 1,3,5-12"/><StrRes Name="8309" Text="' + 
' Scaling "/><StrRes Name="8310" Text="X Scale"/><StrRes Name="8311" Text="Y Scale"/><StrRes Name="8312" Text=" Frames "/><StrRes Name="8313" Text="Non' + 
'e"/><StrRes Name="8314" Text="Simple"/><StrRes Name="8315" Text="Graphic"/><StrRes Name="8316" Text="Only with OEM codepage"/><StrRes Name="8317" Text' + 
'="Print after export"/><StrRes Name="8318" Text="Save settings"/><StrRes Name="8319" Text=" Preview "/><StrRes Name="8320" Text="Width:"/><StrRes Name' + 
'="8321" Text="Height:"/><StrRes Name="8322" Text="Page"/><StrRes Name="8323" Text="Zoom in"/><StrRes Name="8324" Text="Zoom out"/><StrRes Name="8325" ' + 
'Text="Text file (dot-matrix printer)(*.prn)|*.prn"/><StrRes Name="8326" Text=".prn"/><StrRes Name="8400" Text="Print"/><StrRes Name="8401" Text="Print' + 
'er"/><StrRes Name="8402" Text="Name"/><StrRes Name="8403" Text="Setup..."/><StrRes Name="8404" Text="Copies"/><StrRes Name="8405" Text="Number of copi' + 
'es"/><StrRes Name="8406" Text=" Printer init setup "/><StrRes Name="8407" Text="Printer type"/><StrRes Name="8408" Text=".fpi"/><StrRes Name="8409" Te' + 
'xt="Printer init template (*.fpi)|*.fpi"/><StrRes Name="8410" Text=".fpi"/><StrRes Name="8411" Text="Printer init template (*.fpi)|*.fpi"/><StrRes Nam' + 
'e="8500" Text="Export to RTF"/><StrRes Name="8501" Text="Pictures"/><StrRes Name="8502" Text="WYSIWYG"/><StrRes Name="8503" Text="Open after export"/>' + 
'<StrRes Name="8504" Text="RTF file (*.rtf)|*.rtf"/><StrRes Name="8505" Text=".rtf"/><StrRes Name="8600" Text="Export Settings"/><StrRes Name="8601" Te' + 
'xt=" Image settings "/><StrRes Name="8602" Text="JPEG quality"/><StrRes Name="8603" Text="Resolution (dpi)"/><StrRes Name="8604" Text="Separate files"' + 
'/><StrRes Name="8605" Text="Crop pages"/><StrRes Name="8606" Text="Monochrome"/><StrRes Name="8700" Text="Export to PDF"/><StrRes Name="8701" Text="PD' + 
'F/A"/><StrRes Name="8702" Text="Embedded fonts"/><StrRes Name="8703" Text="Print optimized"/><StrRes Name="8704" Text="Outline"/><StrRes Name="8705" T' + 
'ext="Background"/><StrRes Name="8706" Text="Open after export"/><StrRes Name="8707" Text="Adobe PDF file (*.pdf)|*.pdf"/><StrRes Name="8708" Text=".pd' + 
'f"/><StrRes Name="8709" Text="Transparency"/><StrRes Name="8710" Text="PDF Standard:"/><StrRes Name="8711" Text="PDF Version:"/><StrRes Name="8712" Te' + 
'xt="Compressed"/><StrRes Name="8716" Text="Signature"/><StrRes Name="8717" Text="Digital signature"/><StrRes Name="8718" Text="Sign the document"/><St' + 
'rRes Name="8719" Text="Location"/><StrRes Name="8720" Text="Reason"/><StrRes Name="8721" Text="Contact"/><StrRes Name="8722" Text="Certificate"/><StrR' + 
'es Name="8723" Text="File:"/><StrRes Name="8724" Text="Password"/><StrRes Name="8725" Text="Save the password"/><StrRes Name="8726" Text="Attention!!!' + 
' Saving a password may be insecure!"/><StrRes Name="8727" Text="Additional information"/><StrRes Name="8728" Text="Description"/><StrRes Name="8729" T' + 
'ext="Auto fill"/><StrRes Name="DigitalSignature" Text="Digital Signature"/><StrRes Name="Input" Text="Input"/><StrRes Name="SignatureObjectName" Text=' + 
'"Signature object name"/><StrRes Name="8713" Text="Interactive forms"/><StrRes Name="8714" Text="Used glyphs"/><StrRes Name="8715" Text="A simple expr' + 
'ession can be used here.&#13;&#10; Example: &#38;#34;A-Z,a-z&#38;#34; or a glyph number: &#38;#34;#40-#50,#200,#255&#38;#34;."/><StrRes Name="RTFexpor' + 
't" Text="RTF file"/><StrRes Name="BMPexport" Text="BMP image"/><StrRes Name="JPEGexport" Text="JPEG image"/><StrRes Name="TIFFexport" Text="TIFF image' + 
'"/><StrRes Name="PNGexport" Text="PNG image"/><StrRes Name="EMFexport" Text="EMF image"/><StrRes Name="TextExport" Text="Text (matrix printer)"/><StrR' + 
'es Name="XlsOLEexport" Text="Excel table (OLE)"/><StrRes Name="HTMLexport" Text="HTML file"/><StrRes Name="XlsXMLexport" Text="Excel table (XML)"/><St' + 
'rRes Name="PDFexport" Text="PDF file"/><StrRes Name="ProgressWait" Text="Please wait"/><StrRes Name="ProgressRows" Text="Setting up rows"/><StrRes Nam' + 
'e="ProgressColumns" Text="Setting up columns"/><StrRes Name="ProgressStyles" Text="Setting up styles"/><StrRes Name="ProgressObjects" Text="Exporting ' + 
'objects"/><StrRes Name="TIFFexportFilter" Text="Tiff image (*.tif)|*.tif"/><StrRes Name="BMPexportFilter" Text="BMP image (*.bmp)|*.bmp"/><StrRes Name' + 
'="JPEGexportFilter" Text="Jpeg image (*.jpg)|*.jpg"/><StrRes Name="PNGexportFilter" Text="PNG image (*.png)|*.png"/><StrRes Name="EMFexportFilter" Tex' + 
't="EMF image (*.emf)|*.emf"/><StrRes Name="HTMLNavFirst" Text="First"/><StrRes Name="HTMLNavPrev" Text="Prev"/><StrRes Name="HTMLNavNext" Text="Next"/' + 
'><StrRes Name="HTMLNavLast" Text="Last"/><StrRes Name="HTMLNavRefresh" Text="Refresh"/><StrRes Name="HTMLNavPrint" Text="Print"/><StrRes Name="HTMLNav' + 
'Total" Text="Total pages"/><StrRes Name="8800" Text="Export to Text"/><StrRes Name="8801" Text="Text file (*.txt)|*.txt"/><StrRes Name="8802" Text=".t' + 
'xt"/><StrRes Name="SimpleTextExport" Text="Text file"/><StrRes Name="8850" Text="Export to CSV"/><StrRes Name="8851" Text="CSV file (*.csv)|*.csv"/><S' + 
'trRes Name="8852" Text=".csv"/><StrRes Name="8853" Text="Separator"/><StrRes Name="CSVExport" Text="CSV file"/><StrRes Name="8900" Text="Send by E-mai' + 
'l"/><StrRes Name="8901" Text="E-mail"/><StrRes Name="8902" Text="Account"/><StrRes Name="8903" Text="Connection"/><StrRes Name="8904" Text="Address"/>' + 
'<StrRes Name="8905" Text="Attachment"/><StrRes Name="8906" Text="Format"/><StrRes Name="8907" Text="From Address"/><StrRes Name="8908" Text="From Name' + 
'"/><StrRes Name="8909" Text="Host"/><StrRes Name="8910" Text="Login"/><StrRes Name="8911" Text="Mail"/><StrRes Name="8912" Text="Message"/><StrRes Nam' + 
'e="8913" Text="Text"/><StrRes Name="8914" Text="Organization"/><StrRes Name="8915" Text="Password"/><StrRes Name="8916" Text="Port"/><StrRes Name="891' + 
'7" Text="Remember properties"/><StrRes Name="8918" Text="Required fields are Empty"/><StrRes Name="8919" Text="Advanced export settings"/><StrRes Name' + 
'="8920" Text="Signature"/><StrRes Name="8921" Text="Build"/><StrRes Name="8922" Text="Subject"/><StrRes Name="8923" Text="Best regards"/><StrRes Name=' + 
'"8924" Text="Send mail to"/><StrRes Name="8925" Text="Mail transport:"/><StrRes Name="8926" Text="Time out"/><StrRes Name="EmailExport" Text="E-mail"/' + 
'><StrRes Name="FastReportFile" Text="FastReport file"/><StrRes Name="GifexportFilter" Text="Gif file (*.gif)|*.gif"/><StrRes Name="GIFexport" Text="Gi' + 
'f image"/><StrRes Name="8950" Text="Continuous"/><StrRes Name="8951" Text="Page Header/Footer"/><StrRes Name="8952" Text="Text"/><StrRes Name="8953" T' + 
'ext="Header/Footer"/><StrRes Name="8954" Text="None"/><StrRes Name="ODSExportFilter" Text="Open Document Spreadsheet file (*.ods)|*.ods"/><StrRes Name' + 
'="ODSExport" Text="Open Document Spreadsheet"/><StrRes Name="ODTExportFilter" Text="Open Document Text file (*.odt)|*.odt"/><StrRes Name="ODTExport" T' + 
'ext="Open Document Text"/><StrRes Name="8960" Text=".ods"/><StrRes Name="8961" Text=".odt"/><StrRes Name="8962" Text="Security"/><StrRes Name="8963" T' + 
'ext="Security settings"/><StrRes Name="8964" Text="Owner password"/><StrRes Name="8965" Text="User password"/><StrRes Name="8966" Text="Print the docu' + 
'ment"/><StrRes Name="8967" Text="Modify the document"/><StrRes Name="8968" Text="Copy of text and graphics"/><StrRes Name="8969" Text="Add or modify t' + 
'ext annotations"/><StrRes Name="8970" Text="PDF Security"/><StrRes Name="8971" Text="Document information"/><StrRes Name="8972" Text="Information"/><S' + 
'trRes Name="8973" Text="Title"/><StrRes Name="8974" Text="Author"/><StrRes Name="8975" Text="Subject"/><StrRes Name="8976" Text="Keywords"/><StrRes Na' + 
'me="8977" Text="Creator"/><StrRes Name="8978" Text="Producer"/><StrRes Name="8979" Text="Authentification"/><StrRes Name="8980" Text="Permissions"/><S' + 
'trRes Name="8981" Text="Viewer"/><StrRes Name="8982" Text="Viewer preferences"/><StrRes Name="8983" Text="Hide toolbar"/><StrRes Name="8984" Text="Hid' + 
'e menubar"/><StrRes Name="8985" Text="Hide window user interface"/><StrRes Name="8986" Text="Fit window"/><StrRes Name="8987" Text="Center window"/><S' + 
'trRes Name="8988" Text="Print scaling"/><StrRes Name="8989" Text="Confirmation Reading"/><StrRes Name="9000" Text="Rows count:"/><StrRes Name="9001" T' + 
'ext="Split To Sheet"/><StrRes Name="9002" Text="Don''t split"/><StrRes Name="9003" Text="Use report pages"/><StrRes Name="9004" Text="Use print on pare' + 
'nt"/><StrRes Name="9101" Text="Export to DBF"/><StrRes Name="9102" Text="dBase (DBF) export"/><StrRes Name="9103" Text=".dbf"/><StrRes Name="9104" Tex' + 
't="Failed to load the file"/><StrRes Name="9105" Text="Failure"/><StrRes Name="9106" Text="Field names"/><StrRes Name="9107" Text="Automatically"/><St' + 
'rRes Name="9108" Text="Manually"/><StrRes Name="9109" Text="Load from file"/><StrRes Name="9110" Text="Text files (*.txt)|*.txt|All files|*.*"/><StrRe' + 
's Name="9111" Text="DBF files (*.dbf)|*.dbf|All files|*.*"/><StrRes Name="9151" Text="Excel 97/2000/XP file"/><StrRes Name="9152" Text="Auto create fi' + 
'le"/><StrRes Name="9153" Text="Options"/><StrRes Name="9154" Text="Page"/><StrRes Name="9155" Text="Grid Lines"/><StrRes Name="9156" Text="All in one ' + 
'page"/><StrRes Name="9157" Text="Data Grouping"/><StrRes Name="9158" Text="Like the report"/><StrRes Name="9159" Text="Chunks. Each chunk has (rows):"' + 
'/><StrRes Name="9160" Text="Chunk size must be a non negative integer value."/><StrRes Name="9161" Text="SingleSheet must be False when exporting to c' + 
'hunks"/><StrRes Name="9162" Text="Author"/><StrRes Name="9163" Text="Comment"/><StrRes Name="9164" Text="Keywords"/><StrRes Name="9165" Text="Revision' + 
'"/><StrRes Name="9166" Text="Version"/><StrRes Name="9167" Text="Application"/><StrRes Name="9168" Text="Subject"/><StrRes Name="9169" Text="Category"' + 
'/><StrRes Name="9170" Text="Company"/><StrRes Name="9171" Text="Title"/><StrRes Name="9172" Text="Manager"/><StrRes Name="9173" Text="General"/><StrRe' + 
's Name="9174" Text="Information"/><StrRes Name="9175" Text="Protection"/><StrRes Name="9176" Text="Password"/><StrRes Name="9177" Text="If you set a n' + 
'on empty password string, the generated document will be protected with this password. The password is always written in Unicode characters and must b' + 
'e shorter than 256 Unicode characters."/><StrRes Name="9178" Text="Confirmation"/><StrRes Name="9179" Text="Password confirmation does not match the p' + 
'assword. Type the password again."/><StrRes Name="9180" Text="Attempting to set a password of %d characters. The maximum allowed length of a password ' + 
'is %d characters."/><StrRes Name="9181" Text="Adjust page size"/><StrRes Name="9182" Text="Export to Excel 97/2000/XP"/><StrRes Name="9183" Text="Dele' + 
'te empty rows"/><StrRes Name="9184" Text="Export formulas"/><StrRes Name="9185" Text="Data only"/><StrRes Name="BiffCol" Text="Resizing columns"/><Str' + 
'Res Name="BiffRow" Text="Resizing rows"/><StrRes Name="BiffCell" Text="Exporting cells"/><StrRes Name="BiffImg" Text="Exporting pictures"/><StrRes Nam' + 
'e="9200" Text="Microsoft Excel 2007 XML"/><StrRes Name="9201" Text="Microsoft PowerPoint 2007 XML"/><StrRes Name="9203" Text="Microsoft Word 2007 XML"' + 
'/><StrRes Name="9204" Text="Microsoft Excel 2007 XML (*.xlsx)|*.xlsx"/><StrRes Name="9205" Text="Microsoft PowerPoint 2007 XML (*.pptx)|*.pptx"/><StrR' + 
'es Name="9206" Text="Microsoft Word 2007 XML (*.docx)|*.docx"/><StrRes Name="9300" Text="HTML Layered"/><StrRes Name="9301" Text="HTML files (*.html)|' + 
'*.html|All files|*.*"/><StrRes Name="9302" Text="HTML Layered Export"/><StrRes Name="9303" Text="HTML5 Layered"/><StrRes Name="9304" Text="HTML4 Layer' + 
'ed"/><StrRes Name="9305" Text="Export to HTML Layered"/><StrRes Name="9400" Text="Reordering components"/><StrRes Name="9401" Text="Rendering componen' + 
'ts"/><StrRes Name="9402" Text="Adjusting intersecting components"/><StrRes Name="9403" Text="Removing empty rows"/><StrRes Name="9404" Text="Adjusting' + 
' frames"/><StrRes Name="9405" Text="Splitting big cells"/><StrRes Name="9406" Text="Composing BIFF file"/><StrRes Name="xProto" Text="Export Prototype' + 
'"/><StrRes Name="xBlank" Text="Blank Export"/><StrRes Name="expMailAddr" Text="Address or addresses delimited by comma"/><StrRes Name="9500" Text="Exp' + 
'ort to SVG"/><StrRes Name="9512" Text="Unified Pictures"/><StrRes Name="9513" Text="Formatted"/><StrRes Name="9520" Text="Export to ZPL"/><StrRes Name' + 
'="9521" Text="Print as bitmap"/><StrRes Name="9522" Text="Break lines"/><StrRes Name="9523" Text="ZPL file..."/><StrRes Name="9530" Text="Export to PS' + 
'"/><StrRes Name="9531" Text="Has Multiple Files"/><StrRes Name="9532" Text="Include Images"/><StrRes Name="9540" Text="Export to PPML"/><StrRes Name="' + 
'HTMLExtension" Text=".html"/><StrRes Name="SVGDescription" Text="SVG file"/><StrRes Name="SVGFilter"      Text="SVG file (*.svg)|*.svg"/><StrRes Name=' + 
'"SVGExtension"   Text=".svg"/><StrRes Name="ZPLFilter"      Text="ZPL file (*.zpl)|*.zpl"/><StrRes Name="ZPLExtension"   Text=".zpl"/><StrRes Name="PS' + 
'Filter"      Text="PS file (*.ps)|*.ps"/><StrRes Name="PSExtension"   Text=".ps"/><StrRes Name="PPMLFilter"      Text="PPML file (*.ppml)|*.ppml"/><St' + 
'rRes Name="PPMLExtension"   Text=".ppml"/><StrRes Name="PPMLexport"   Text="PPML Export ..."/><StrRes Name="PSexport"   Text="PS Export ..."/><StrRes ' + 
'Name="10000" Text="Layout type"/><StrRes Name="10001" Text="Table - tabular layout of the report"/><StrRes Name="10002" Text="Object - layout with flo' + 
'ating objects position"/><StrRes Name="10003" Text="Text - not implemented"/><StrRes Name="10100" Text="Signature Error"/><StrRes Name="10101" Text="S' + 
'ave Log"/><StrRes Name="10102" Text="Ignore certificate"/><StrRes Name="WrongPassword" Text="Wrong password"/><StrRes Name="UnknownHashAlgorithm" Text' + 
'="Unknown hash algorithm"/><StrRes Name="UnknownChipherAlgorithm" Text="Unknown chipher algorithm"/><StrRes Name="InvalidASN1File" Text="Invalid certi' + 
'ficate file"/><StrRes Name="CantParseCertificate" Text="Can''t parse certificate"/><StrRes Name="PrepareSignError" Text="Prepare signature error"/><Str' + 
'Res Name="SignDigestError" Text="Signature digest error"/><StrRes Name="CertificateNotFound" Text="Certificate file not found"/></Resources>' + 
' ';

initialization
  frxResources.AddXML(resXML);

end.

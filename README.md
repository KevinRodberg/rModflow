# rModflow
<table>
<tr>
<td><b>  chooseBudgetTerms(CBCterms):   </b></td>
<td>     Choose Modflow Cell-by-cell Budget Terms
</td></tr>
<td colspan="2">Argument:<br>&emsp;   CBCterms: &emsp;vector created by listBinHeaders
</td></tr>
<tr>
<td><b>  chooseDataSource(matrixSource):</b></td><td>Choose Source Data
</td></tr>
<td colspan="2">Argument:<br>&emsp; matrixSource:</b></td>
</tr>
<tr>
<td><b>  chooseModel():</b></td><td>Choose Modflow Model
</td></tr>
<tr><td colspan="2">&emsp;Returns model name from vector of available models in MFModels
</td></tr>		 
<tr>
<td><b>  defineMFmodel():</b></td><td>
			Defines SFWMD Modflow Model characteristics
</td></tr>
<tr><td colspan="2">&emsp;
			Returns MFmodel.Params Data.frame providing model characteristics
<p>				Examples:
<br>&emsp;		MFmodel.Params <- defineMFmodel()
<br>&emsp;		model <- chooseModel()
<br>&emsp;		M <- as.data.frame(MFmodel.Params[model,])

</td></tr>		 
<tr>
<td><b>  definePlotOpts():</b></td><td>Define Plot options
</td></tr>
<tr><td colspan="2">&emsp;Returns Plot options as a list: <br>
&emsp;&emsp;list(matrixSource=matrixSrc, printingOn=printingOn, printAnnualOn=printAnnualOn, Animform=Animform)) <br>
</td></tr>
<tr>
<td><b>  exit(msg):</b></td><td>Exit Function
</td></tr>
<tr><td colspan="2">
				Arguments:
<br>&emsp;
						msg 	&emsp;		Message to display on exit<
</tr>
<tr>
<td><b>  listBinHeaders(filPtr):</b></td><td>Creates list of Budget Term Headers available in <br>Modflow binary cell-by-cell file
</td></tr>
<tr><td colspan="2">&emsp;Returns CBCTermSet as list <br>
&emsp;&emsp;&emsp;						c(firstHeader$TEXT,firstHeader$K,firstHeader$NR,firstHeader$NC,CBCterms)
<br>
				Arguments:
<br>&emsp;fileptr:	file pointer</td></tr>
<tr>
<td><b>  readCBCbinByTerm(filPtr, term, SP_rng, lay):</b></td><td>Read budget term by layer
</td></tr>
<tr><td colspan="2">&emsp;Returns bigVector<br>
Arguments:<br>
&emsp;  fileptr: 	&emsp; 							file pointer<br>
&emsp;  term 			&emsp;&emsp; 				Modflw Budget Term to Read<br>
&emsp;  SP_rng 		&emsp; 							List of Stress Periods to read<br>
&emsp;  lay 			&emsp;&emsp;&emsp; 	Model Layer to Read;

</td></tr>
<tr>
<td><b>readCBCHeader(filPtr):</b></td><td>Read cell-by-cell budget header record
</td></tr>
<tr><td colspan="2">&emsp;fileptr:	file pointer</td></tr>
<tr>
<td><b>readgridPoints(Modelgrd.Path, Model.Shape):</b></td><td>Read Model Mesh Points
</td></tr>
<tr><td colspan="2">&emsp;Returns ModelGridCoords Data.frame providing model row, column and coordinates
<br>
				Arguments:
<br>&emsp;  Modelgrd.Path:	  &emsp; 							path to model mesh shapefile
<br>&emsp;  Model.Shape: 			&emsp; 							name of model mesh shapefile
</td></tr>
<tr>
<td><b>readHeadsbin(filPtr, SP_rng):</b></td><td>Read Heads by Stress Period
</td></tr>
<tr><td colspan="2">&emsp;Returns bigVector
<br>
				Arguments:
<br>&emsp;  fileptr:	  &emsp; 							file pointer
<br>&emsp;  SP_rng: 		&emsp; 							List of Stress Periods to read
<tr>
<td><b>readHeadsbinAtPnts(filPtr, SP_rng, PointVector):</b></td><td>Read Heads for a vector of points at specified Stress Periods
</td></tr>
<tr><td colspan="2">&emsp;Returns listOfPnts [with values for stress period range]
<br>
				Arguments:
<br>&emsp;  fileptr:	  &emsp; 							file pointer
<br>&emsp;  SP_rng: 		&emsp; 							List of Stress Periods to read
<br>&emsp;PointVector
</td></tr>
<tr>
<td><b>readHeadsbinByLay(to.Read, selectLayer, maxSP):</b></td><td>Read Heads by Layer
</td></tr>
<tr><td colspan="2">&emsp;Returns bigVector
<br>
				Arguments:
<br>&emsp;  to.Read:	 		 	&emsp;	&emsp; 					File name to be assigned to a file pointer (filPtr)
<br>&emsp;  selectLayer: 		&emsp; 									Model Layer to read
<br>&emsp;  maxSP: 					&emsp;&emsp;&emsp;	 		Maximum Stress Period  to read
<tr>
<td><b>readHeadsHeader(filPtr):</b></td><td>Read Heads header record
</td></tr>
<tr><td colspan="2">&emsp;Returns header as a list c(KSTP,KPER,PERTIM,TOTTIM,TEXT,NC,NR,K)
<br>				Arguments:
<br>&emsp;  fileptr:	  &emsp; 							file pointer
</td></tr>
</table>

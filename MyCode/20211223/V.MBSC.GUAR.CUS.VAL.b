*-----------------------------------------------------------------------------
*  DATE             : 18 NOV 2021
*  CLIENT           : BDC
*  MODULE			: Limit & CUSTOMER
*  AUTHOR			: Yasmine & ROMANI EZZAT
*-----------------------------------------------------------------------------
    SUBROUTINE V.MBSC.GUAR.CUS.VAL
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
	$INSERT I_F.CUSTOMER
	$INSERT I_F.MBSC.GUARANTOR.PARAM
*----------------------------------------------------------------------------
	*DEBUG
*----------------------------------------------------------------------------

	FN.MBSC.GUARANTOR.PARAM = 'F.MBSC.GUARANTOR.PARAM'
	F.MBSC.GUARANTOR.PARAM  = ''
	CALL OPF(FN.MBSC.GUARANTOR.PARAM, F.MBSC.GUARANTOR.PARAM)
	
*----------------------------------------------------------------------------

	CALL GET.LOC.REF(APPLICATION,"GUARANTOR",GUARANTOR.POS)
	GUARANTOR.ID = R.NEW(EB.CUS.LOCAL.REF)<1,GUARANTOR.POS>
	
	IF GUARANTOR.ID NE '' THEN
		CURR.CUS.ID  = ID.NEW
		GUARANTOR.LEN = DCOUNT(GUARANTOR.ID,SM)
		
		EI = 1
		LOOP
			SLCT.GUAR.ID  = FIELD(GUARANTOR.ID,SM,EI)
			
			RG = EI+1
			LOOP
			
				REST.GUAR.ID  = FIELD(GUARANTOR.ID,SM,RG)
				IF SLCT.GUAR.ID EQ REST.GUAR.ID THEN
					AF = EB.CUS.LOCAL.REF
					AV = GUARANTOR.POS
					AS = EI
					TEXT ='MBSC.DUPLICATE.GUARANTOR'
					CALL STORE.OVERRIDE(REST.GUAR.ID)
					REST.GUAR.ID = ''
					*ETEXT = 'LI-MBSC.DUPLICATE.GUARANTOR'
					*CALL STORE.END.ERROR
				END
			UNTIL RG GT GUARANTOR.LEN
			RG++
			REPEAT
			
		EI++
		UNTIL EI GE GUARANTOR.LEN
		REPEAT
		
		I = 1
		LOOP
		WHILE I LE GUARANTOR.LEN
			GUAR.ID = FIELD(GUARANTOR.ID,SM,I)
			CALL F.READ(FN.MBSC.GUARANTOR.PARAM,GUAR.ID,R.GUARANTOR,F.MBSC.GUARANTOR.PARAM,ERR.GUARANTOR)
			IF R.GUARANTOR NE '' THEN
				GUAR.CUS = R.GUARANTOR<GUAR.PARAM.CUSTOMER>
				CUS.NO   = DCOUNT(GUAR.CUS,VM)
				
				IF CUS.NO GT '1' THEN	
					TEXT ='MBSC.GUARANTOR.MULTI'
					CALL STORE.OVERRIDE(CURR.NO)
					CURR.NO = ''
				END 
			END
			IF CURR.CUS.ID EQ GUAR.ID THEN
				AF = EB.CUS.LOCAL.REF
				AV = GUARANTOR.POS
				AS = I
				TEXT ='MBSC.INVALID.GUARANTOR'
				CALL STORE.OVERRIDE(GUAR.ID)
				GUAR.ID = ''
				*ETEXT = 'LI-MBSC.INVALID.GUARANTOR'
				*CALL STORE.END.ERROR
			END
		
		I++
		REPEAT
	END  
 
*------------------------------------------------------------------------------------------------------------------
RETURN
END

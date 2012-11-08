header {
	package it.imolinfo.jbi4cics.commareaparser;

	import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
	import it.imolinfo.jbi4cics.typemapping.cobol.CobolType;
	import it.imolinfo.jbi4cics.typemapping.cobol.CobolTypeDescriptor;
	import it.imolinfo.jbi4cics.exception.FormatException;
	import it.imolinfo.jbi4cics.exception.ParseException;
	import it.imolinfo.jbi4cics.messageformat.commarea.NestingHandler;
}

class CommareaParser extends Parser;

options { k=3;
}

// The grammar here is: NL* COBOLTYPEDESC (NL+ COBOLTYPEDESC|EOF)* EOF
commarea_definition returns[CommareaBeanMappingDescriptor commareaBeanMappingDescriptor] throws FormatException,ParseException
	{commareaBeanMappingDescriptor=new CommareaBeanMappingDescriptor();
	 CobolTypeDescriptor cobolTypeDescriptor;}
	: (NL)*
	  cobolTypeDescriptor=data_description_entry {commareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);}
	  ((NL)+ (cobolTypeDescriptor=data_description_entry {commareaBeanMappingDescriptor.addFieldMapping(cobolTypeDescriptor.getName().replace('-','_'),cobolTypeDescriptor.getName(),cobolTypeDescriptor);} | EOF{commareaBeanMappingDescriptor=NestingHandler.handleNesting(commareaBeanMappingDescriptor);}))*
	  EOF{commareaBeanMappingDescriptor=NestingHandler.handleNesting(commareaBeanMappingDescriptor);}
	;

/*data_description_entry
	: level_number  (((VARIABLE_NAME | "FILLER" )? (redefines_clause)? (blank_when_zero_clause)? (external_clause)? (global_clause)? (group_usage_clause)? (justified_clause)? (occurs_clause)? (picture_clause)? (sign_clause)? (synchronized_clause)? (usage_clause)? (value_clause)? (date_format_clause)?)
	| (VARIABLE_NAME renames_clause)
	| (VARIABLE_NAME value_clause ))
	POINT NL
	;*/

/*data_description_entry
	: LEVEL_NUMBER (VARIABLE_NAME | "FILLER" ) (redefines_clause)? (blank_when_zero_clause)? (external_clause)? (global_clause)? (group_usage_clause)? (justified_clause)? (occurs_clause)? (picture_clause)? (sign_clause)? (synchronized_clause)? (usage_clause)? (value_clause)? (date_format_clause)?	POINT
	;*/

data_description_entry returns[CobolTypeDescriptor cobolTypeDescriptor] throws ParseException
	{ cobolTypeDescriptor = new CobolTypeDescriptor();
	  PictureDefinition pictureDefinition;
	  OccursDefinition occursDefinition;
	  CobolType type = null;}
    : //(levelNumber:LEVEL_NUMBER{cobolTypeDescriptor.setLevel(Integer.valueOf(levelNumber.getText().trim()).intValue());})
	  (levelNumber:INTEGER{cobolTypeDescriptor.setLevel(Integer.parseInt(levelNumber.getText().trim()));})
      (variableName:VARIABLE_NAME {cobolTypeDescriptor.setName(variableName.getText());} | "FILLER" {cobolTypeDescriptor.setName("FILLER");} )
      //(redefines_clause)?
      //(blank_when_zero_clause)?
      //(external_clause)?
      //(global_clause)?
      //(group_usage_clause)?
      //(justified_clause)?
      (occursDefinition=occurs_clause{occursDefinition.populate(cobolTypeDescriptor);})?
      (pictureDefinition=picture_clause{pictureDefinition.populate(cobolTypeDescriptor);})?
      //(sign_clause)?
      //(synchronized_clause)?
      (type=usage_clause{if (type != null) cobolTypeDescriptor.setType(type); if (type == CobolType.INTEGER) cobolTypeDescriptor.setBigEndian(true);})?
      //(value_clause)?
      //(date_format_clause)?
      POINT{if (cobolTypeDescriptor.getType() == null) cobolTypeDescriptor.setType(CobolType.NESTED_COMMAREA);}
	;

/*level_number
	: DIGIT DIGIT
	;*/

blank_when_zero_clause
	: "BLANK" ("WHEN")? ("ZERO"|"ZEROS"|"ZEROES")
	;

date_format_clause
	: "DATE FORMAT" ("IS")? VARIABLE_NAME
	;

/*date_pattern
	: ("X"|"XX"|"XXX"|"XXXX")? ("YY"|"YYYY") ("X"|"XX"|"XXX"|"XXXX")?
	;*/

external_clause
	: "EXTERNAL"
	;

global_clause
	: "GLOBAL"
	;

justified_clause
	: ("JUSTIFIED"|"JUST") ("RIGHT")?
	;

group_usage_clause
	: "GROUP-USAGE" ("IS")? "NATIONAL"
	;

occurs_clause returns[OccursDefinition occursDefinition]
	{occursDefinition=new OccursDefinition();}
	: "OCCURS"
	  (minSize:INTEGER{occursDefinition.setMinSize(Integer.parseInt(minSize.getText().trim()));occursDefinition.setDynamic(true);} "TO")?
	  size:INTEGER{if (occursDefinition.isDynamic()) {occursDefinition.setMaxSize(Integer.parseInt(size.getText().trim()));} else {occursDefinition.setSize(Integer.parseInt(size.getText().trim()));};}
	  ("TIMES")?
	  ("DEPENDING" ("ON")? variableName:VARIABLE_NAME{occursDefinition.setVariableName(variableName.getText());})?
	  (("ASCENDING"|"DESCENDING") ("KEY")? ("IS")? (VARIABLE_NAME)+)*
	  ("INDEXED" ("BY")? (VARIABLE_NAME)+)?
	;

picture_clause returns[PictureDefinition pictureDefinition]
	{PictureDefinition pd;
	  pictureDefinition=null;}
	: ("PICTURE"|"PIC") ("IS")? pd=picture_definition {pictureDefinition=pd;}
	;

picture_definition returns[PictureDefinition pictureDefinition]
	{pictureDefinition=new PictureDefinition();
	  int l1,l2,l3,l4;}
	: {LT(1).getText().startsWith("A")||LT(1).getText().startsWith("a")||LT(1).getText().startsWith("X")||LT(1).getText().startsWith("a")}? v1:VARIABLE_NAME{pictureDefinition.setStringLength(v1.getText().length());pictureDefinition.setType(PictureDefinition.STRING);} (l1=length_definition{pictureDefinition.setStringLength(l1);})? //XXX o AAA o X(9) A(9)
	| {((LT(1).getText().startsWith("S")||LT(1).getText().startsWith("s"))&&(LT(1).getText().contains("V")||LT(1).getText().contains("v")))}? v4:VARIABLE_NAME{pictureDefinition.setType(PictureDefinition.NUMERIC);pictureDefinition.setSigned(true);pictureDefinition.setIntegerLength(((v4.getText().indexOf("V")>0)?v4.getText().indexOf("V"):v4.getText().indexOf("v"))-1);pictureDefinition.setDecimalLength(v4.getText().length()-((v4.getText().indexOf("V")>0)?v4.getText().indexOf("V"):v4.getText().indexOf("v"))-1);} (l4=length_definition{pictureDefinition.setDecimalLength(l4);})? //s999v999 o s999v(9)
	| ((PLUS{pictureDefinition.setSigned(true);pictureDefinition.setSignPlus(true);})? i:INTEGER{pictureDefinition.setIntegerLength(i.getText().length());pictureDefinition.setType(PictureDefinition.NUMERIC);} (l2=length_definition{pictureDefinition.setIntegerLength(l2);})?                 // 999 o 9(9) o +999 o +9(9)
	  |{LT(1).getText().startsWith("S")||LT(1).getText().startsWith("s")}? v2:VARIABLE_NAME{pictureDefinition.setType(PictureDefinition.NUMERIC);pictureDefinition.setSigned(true);pictureDefinition.setIntegerLength(v2.getText().length()-1);} (l2=length_definition{pictureDefinition.setIntegerLength(l2);})?)//  S999 o s9(9)
	  ({LT(1).getText().startsWith("V")||LT(1).getText().startsWith("v")}? v3:VARIABLE_NAME{pictureDefinition.setDecimalLength(v3.getText().length()-1);} (l3=length_definition{pictureDefinition.setDecimalLength(l3);})?)? // V999 o V9(9)
	;


length_definition returns[int length]
	{length=0;}
	: i:LENGTH_DEFINITION {length=Integer.parseInt(i.getText().substring(1,i.getText().length()-1));}
	;

redefines_clause
	: LEVEL_NUMBER (VARIABLE_NAME|"FILLER")? "REDEFINES" VARIABLE_NAME
	;

/*renames_clause
	: "66" VARIABLE_NAME "RENAMES" VARIABLE_NAME(("THROUGH"|"THRU") VARIABLE_NAME)?
	;*/

sign_clause
	: ("SIGN" ("IS")?)? ("LEADING"|"TRAILING") ("SEPARATE" ("CHARACTER")?)?
	;

synchronized_clause
	: ("SYNCHRONIZED"|"SYNC") ("LEFT"|"RIGHT")?
	;

usage_clause returns [CobolType type]
	{ type = null; }
	: ("USAGE" ("IS")?)? ( ("BYNARY" ("NATIVE")?) | ("COMP"{type=CobolType.INTEGER;} ("NATIVE")?) | ("COMP-1" ("NATIVE")?) | ("COMP-2" ("NATIVE")?) | ("COMP-3"{type=CobolType.PACKED_DECIMAL;} ("NATIVE")?) | ("COMP-4"{type=CobolType.INTEGER;} ("NATIVE")?) | ("COMP-5" ("NATIVE")?)
    | ("COMPUTATIONAL"{type=CobolType.INTEGER;} ("NATIVE")?) | ("COMPUTATIONAL-1" ("NATIVE")?) | ("COMPUTATIONAL-2" ("NATIVE")?) | ("COMPUTATIONAL-3"{type=CobolType.PACKED_DECIMAL;} ("NATIVE")?) | ("COMPUTATIONAL-4"{type=CobolType.INTEGER;} ("NATIVE")?) | ("COMPUTATIONAL-5" ("NATIVE")?)
	| ("DISPLAY" ("NATIVE")?) | ("DISPLAY-1" ("NATIVE")?) | ("INDEX") | ("NATIONAL" ("NATIVE")?) | object_phrase | ("PACKED_DECIMAL" ("NATIVE")?) | "PROCEDURE-POINTER" | "FUNCTION-POINTER" )
	;

object_phrase
	: "OBJECT REFERENCE" (VARIABLE_NAME)?
	;

value_clause
	: "VALUE" ("IS")? VARIABLE_NAME |
	  "88" VARIABLE_NAME (("VALUE" ("IS")?)|("VALUES") ("ARE")?) (VARIABLE_NAME ( ("THROUGH"|"THRU") VARIABLE_NAME)?)+
	;

class CommareaLexer extends Lexer;

options { k=3;
}

WS
	: ( ' ' |  '\t'  )
	{$setType(Token.SKIP);}
	;

NL
	: ('\r' '\n' | '\n') {newline();}
	;

// Comments. The rule ('*'|'/') (~('\n'|'\r'))* NL doesn't work: why?
COMMENT
	: ('*'|'/') ('\0'..'\11'|'\13'|'\14'|'\16'..'\377')* NL {$setType(Token.SKIP); newline();}
	;

/*SIGN
	: 'S'
	;*/

/*STRING_DEFINITION_CHAR
	: ('A'|'X')
	;*/

LENGTH_DEFINITION
	: '(' INTEGER ')'
	;

/*NUMERIC_DEFINITION_CHAR
	: '9'
	;*/

//LEVEL_NUMBER
	//:
	//INTEGER
	//DIGIT DIGIT
	//( ' ' |  '\t'  )
	//;

VARIABLE_NAME
	: LETTER (LETTER|DIGIT|'-')*
	;

INTEGER
	: (DIGIT)+
	;

POINT
	: '.'
	;

PLUS
	: '+'
	;

protected
DIGIT
	: '0'..'9'
	;

protected
LETTER
	: ('A'..'Z') | ('a'..'z')
	;

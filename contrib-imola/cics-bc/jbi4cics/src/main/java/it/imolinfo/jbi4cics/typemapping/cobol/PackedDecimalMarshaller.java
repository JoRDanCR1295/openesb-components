/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.typemapping.cobol;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.jbi.Messages;

import java.math.BigDecimal;

public class PackedDecimalMarshaller {
	
  /**
   * The logger for this class and its instances.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(PackedDecimalMarshaller.class);
  
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(PackedDecimalMarshaller.class);
  
  
    
  public final static byte POSITIVE_SIGN = 0x0C;
  public final static byte NEGATIVE_SIGN = 0x0D;
  public final static byte NO_SIGN = 0x0F;
  
  /**
   * void constructor.
   */
    public PackedDecimalMarshaller(){
    }
  
  /*
   * questo formato memorizza due cifre per byte. 
   */
  public static void marshallBigDecimal(BigDecimal value, byte[] buffer, CobolTypeDescriptor cobolType, int startingOffset) throws FormatException{
    LOG.debug("value: "+value);
    //log.debug("virtualDecimalPoint: "+cobolType.getVirtualDecimalPoint());
    value=value.movePointRight(cobolType.getVirtualDecimalPoint());
    //log.debug("value2: "+value);
    value=value.setScale(0);
    //log.debug("value3: "+value);
    boolean isNegative=false;
    if (value.compareTo(new BigDecimal(0))==-1){
      value=value.abs();
      isNegative=true;
    }
    //log.debug("value4: "+value);
    String stringValue=value.toPlainString();
    //log.debug("stringValue: "+stringValue);
    StringBuffer stringBufferValue=new StringBuffer();
    int numberSize=cobolType.getIntegerPartLength()+cobolType.getDecimalPartLength();
//  at this point we don't have scale and comma and sign
    for (int i=0;i<(numberSize-stringValue.length());i++){
      stringBufferValue.append('0');
    }
    // at this point we have the right length
    stringValue=stringBufferValue+stringValue;
    //log.debug("stringValue after padding: "+stringValue.toString());
    byte[] valueArray=stringValue.toString().getBytes();
    int bufferedLength=cobolType.getBufferedLength();
    for (int i=bufferedLength-1;i>=0;i--){
      byte result;
      if (i==bufferedLength-1){
      // gestione del segno
        byte signByte;
        if (cobolType.isSigned()){
          if (isNegative){
            signByte=NEGATIVE_SIGN;
          }
          else {
            signByte=POSITIVE_SIGN;
          }
        }
        else {
          signByte=NO_SIGN;
        }
        result=(byte)((valueArray[i*2+(valueArray.length%2)-1]<<4)|signByte);
      }
      else {
        if (i==0 && (valueArray.length%2)==0){
          result=(byte)(0x0f&valueArray[0]);
        }
        else {
          byte lowSemibyte=(byte)(0x0f&valueArray[i*2+(valueArray.length%2)]);
          byte highSemibyte=(byte)(0x0f&valueArray[i*2+(valueArray.length%2)-1]);
//          log.debug("indice: "+i+Arrays.toString(valueArray));
//          log.debug("indice lowsemibyte:"+(i*2+(valueArray.length%2))+" lowSemibyte:       "+lowSemibyte);
//          log.debug("lowSemibyte:       ["+HexDump.toHex(lowSemibyte)+"]");
//          log.debug("indice highSemibyte: "+(i*2+(valueArray.length%2)-1)+" highSemibyte:       "+highSemibyte);
//          log.debug("highSemibyte:       ["+HexDump.toHex(highSemibyte)+"]");
          result=(byte)( (highSemibyte << 4) | lowSemibyte );
        }
      }
      // a questo punto abbiamo il byte dobbiamo inserirlo al punto giusto
      buffer[startingOffset+i]=result;      
    }    
  }
  
  public static BigDecimal unmarshallBigDecimal(byte[] buffer, CobolTypeDescriptor cobolType,int startingOffset) throws FormatException{
    StringBuffer stringBuffer=new StringBuffer();
    boolean positive=true;
    for (int i=0;i<cobolType.getBufferedLength();i++){      
      byte currentByte=buffer[startingOffset+i];
      //log.debug("current byte: "+currentByte);
      if (i==cobolType.getBufferedLength()-1){
        //gestione del segno
        if (cobolType.isSigned()){
          byte signbyte=(byte)(currentByte&0x0F);
          switch (signbyte) {
            case POSITIVE_SIGN : {positive=true;break;}
            case NEGATIVE_SIGN : {positive=false;break;}
            default: {
              LOG.error("CIC002115_Signed_decimal_int_not_found", signbyte);
              throw new FormatException(MESSAGES.getString("CIC002115_Signed_decimal_int_not_found", signbyte));
            }
          }
        }        
        byte firstChar=(byte)((currentByte&0xF0)>>4);
        stringBuffer.append(String.valueOf(firstChar));
      }
      else {
        byte lowSemiByte=(byte)(currentByte&0x0F);
        byte highSemiByte=(byte)((currentByte&0xF0)>>4);
        //log.debug("lowSemiByte: "+lowSemiByte);
        //log.debug("highSemiByte: "+highSemiByte);
        stringBuffer.append(String.valueOf(highSemiByte));
        stringBuffer.append(String.valueOf(lowSemiByte));
        
        //log.debug("stringbuffer: "+stringBuffer);
      }        
    }
    //log.debug("stringbuffer: "+stringBuffer);
    BigDecimal result=new BigDecimal(stringBuffer.toString());
    //log.debug("result1: "+result);
    result=result.setScale(0);
    //log.debug("result2: "+result);
    result=result.movePointLeft(cobolType.getVirtualDecimalPoint());
    //log.debug("result3: "+result);
    if (!positive){
      result=result.negate();
    }
    //log.debug("result4: "+result);
    return result;
  }

}

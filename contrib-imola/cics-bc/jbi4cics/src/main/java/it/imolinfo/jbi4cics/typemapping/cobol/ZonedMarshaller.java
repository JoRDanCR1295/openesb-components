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


public class ZonedMarshaller {  
  
  /**
   * The logger for this class and its instances.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(ZonedMarshaller.class);
  
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(ZonedMarshaller.class);
  
  private final static byte EBCDIC_PLUS = 0x4E;
  private final static byte EBCDIC_MINUS = 0x60;
  
  /**
   * void constructor.
   */
    public ZonedMarshaller(){
    }

  
  public static void marshallZoned(BigDecimal value, byte[] buffer, int startingOffset, int size, boolean signed, int virtualDecimalPoint, int signFormat, String codePage){
    //log.debug("value before moving point: "+value);
    value=value.movePointRight(virtualDecimalPoint);
    value=value.setScale(0);
    //log.debug("value after moving point: "+value);
    int signOffset=((signFormat==CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE)&&(signed))?1:0;
    int dataSize=size-signOffset;
    boolean isNegative=false;
    if (value.compareTo(new BigDecimal(0))==-1){
      value=value.abs();
      isNegative=true;
    }
    //log.debug("value after abs: "+value);
    String stringValue=value.toPlainString();
    StringBuffer stringBufferValue=new StringBuffer(stringValue);
    //log.debug("stringValue before padding: "+stringBufferValue);
//  at this point we don't have scale and comma and sign
    //log.debug("size: "+size+", signoffset: "+signOffset+",datasize: "+dataSize);
    for (int i=0;i<(dataSize-stringValue.length());i++){
      stringBufferValue.insert(0, '0');
    }
    // at this point we have the right length
    LOG.debug("stringValue after padding: "+stringBufferValue);    
    byte[] valueArray=stringBufferValue.toString().getBytes();
    //log.debug("valueArray: "+Arrays.toString(valueArray));
    int dataStartingOffset=startingOffset+signOffset;
    for (int i=dataSize-1;i>=0;i--){
      byte currentByte=(byte)(valueArray[i] & 0x0f);
      //log.debug("signFormat: "+signFormat+", signed: "+signed+", dataSize: "+dataSize+", i: "+i);
      if ((signFormat==CobolTypeDescriptor.SIGN_FORMAT_TRAILING && signed && i==dataSize-1) || (signFormat==CobolTypeDescriptor.SIGN_FORMAT_LEADING && signed && i==0)){
        if (isNegative){
          currentByte=(byte) ((PackedDecimalMarshaller.NEGATIVE_SIGN << 4) | currentByte);
        }
        else {
          currentByte=(byte) ((PackedDecimalMarshaller.POSITIVE_SIGN << 4) | currentByte);
        }        
      }
      else {
        currentByte=(byte)(0xf0 | currentByte);
      }
      buffer[dataStartingOffset+i]=currentByte;
    }
    if (signFormat==CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE && signed){
      if (isNegative){
        buffer[startingOffset]= EBCDIC_MINUS;
      }
      else {
        buffer[startingOffset]=EBCDIC_PLUS;        
      }
    }
    
  }
  public static BigDecimal unmarshallZoned(byte[] buffer, int startingOffset, int size, boolean signed, int virtualDecimalPoint, int signFormat, String codePage) throws FormatException{
    int signOffset=((signFormat==CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE)&&(signed))?1:0;
    int dataStartingOffset=startingOffset+signOffset;
    int dataSize=size-signOffset;
    boolean positive=true;
    if (signFormat==CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE && signed){
      byte signByte=buffer[startingOffset];
      switch (signByte) {
        case EBCDIC_PLUS  : {positive=true;break;}
        case EBCDIC_MINUS : {positive=false;break;}
        default : {
            LOG.error("CIC002119_Sign_not_recognized", signByte);
            throw new FormatException(MESSAGES.getString(
                    "CIC002119_Sign_not_recognized", signByte));
        }
      }
    }
    StringBuffer stringBuffer=new StringBuffer();
    for (int i=0;i<dataSize;i++){
      byte currentByte=buffer[dataStartingOffset+i];
      //log.debug("signFormat: "+signFormat+", signed: "+signed+", dataSize: "+dataSize+", i: "+i);
      if ((signFormat==CobolTypeDescriptor.SIGN_FORMAT_TRAILING && signed && i==dataSize-1) || (signFormat==CobolTypeDescriptor.SIGN_FORMAT_LEADING && signed && i==0)){
        byte signByte=(byte) ((currentByte & 0xf0)>>4);
        //log.debug("signByte: "+String.valueOf(signByte));
        if (signByte==PackedDecimalMarshaller.NEGATIVE_SIGN){
          positive=false;
        }
      }
      stringBuffer.append(String.valueOf(currentByte & 0x0f));
    }
    BigDecimal result=new BigDecimal(stringBuffer.toString());
    result=result.movePointLeft(virtualDecimalPoint);
    if (!positive){
      result=result.negate();
    }
    return result;
  }

}

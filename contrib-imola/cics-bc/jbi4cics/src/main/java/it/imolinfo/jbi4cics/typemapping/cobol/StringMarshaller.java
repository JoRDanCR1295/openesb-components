/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4cics.typemapping.cobol;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.jbi.Messages;

import java.io.UnsupportedEncodingException;

public class StringMarshaller {
    
  /**
   * The logger for this class and its instances.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(StringMarshaller.class);
    
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(StringMarshaller.class);
  
  /**
   * void constructor.
   */
    public StringMarshaller(){
    }

  public static void marshallStringIntoBuffer(String value, byte[] buffer, int startingOffset, String codePage, int size, int justification, String padCharStr) throws FormatException {
    byte[] valueArray = null;
    try {
      valueArray = value.getBytes(codePage);
    } catch (UnsupportedEncodingException e) {
      LOG.error("CIC002116_Code_page_not_supported", codePage);
      throw new FormatException(MESSAGES.getString("CIC002116_Code_page_not_supported", codePage));
    }
    // is the size corrext?
    if (valueArray.length > size) {
      LOG.error("CIC002117_Size_error", value, size);
      throw new FormatException(MESSAGES.getString("CIC002117_Size_error", value, size));
    }
    // do we need to pad?
    if (valueArray.length < size && padCharStr != null) {
      byte[] padChar;
      try {
        padChar = padCharStr.getBytes(codePage);
      } catch (UnsupportedEncodingException e) {
        LOG.error("CIC002116_Code_page_not_supported", codePage);
        throw new FormatException(MESSAGES.getString("CIC002116_Code_page_not_supported", codePage));
      }
      switch (justification) {
                case CobolTypeDescriptor.STRING_JUSTIFICATION_LEFT:
          int padlength = size - valueArray.length;
          byte[] padArray = new byte[padlength];
          fillArray(padArray, padChar);
          System.arraycopy(valueArray, 0, buffer, startingOffset, valueArray.length);
          System.arraycopy(padArray, 0, buffer, startingOffset + valueArray.length, padArray.length);
          break;
        case CobolTypeDescriptor.STRING_JUSTIFICATION_CENTER:
          int padlengthl = (size - valueArray.length) / 2;
          int padlengthr = (size - valueArray.length) / 2 + (size - valueArray.length) % 2;
          byte[] padArrayl = new byte[padlengthl];
          fillArray(padArrayl, padChar);
          byte[] padArrayr = new byte[padlengthr];
          fillArray(padArrayr, padChar);
          System.arraycopy(padArrayl, 0, buffer, startingOffset, padArrayl.length);
          System.arraycopy(valueArray, 0, buffer, startingOffset + padArrayl.length, valueArray.length);
          System.arraycopy(padArrayr, 0, buffer, startingOffset + padArrayl.length + valueArray.length, padArrayr.length);
          break;
        case CobolTypeDescriptor.STRING_JUSTIFICATION_RIGHT:
          int padlengthLocal = size - valueArray.length;
          byte [] padArrayLocal = new byte[padlengthLocal];
          fillArray(padArrayLocal, padChar);
          System.arraycopy(padArrayLocal,
                           0,
                           buffer,
                           startingOffset,
                           padArrayLocal.length);
          System.arraycopy(valueArray,
                           0,
                           buffer,
                           startingOffset + padArrayLocal.length,
                           valueArray.length);
          break;
        default :
          LOG.error("CIC002118_Error_code");
          throw new FormatException(MESSAGES.getString("CIC002118_Error_code"));
      }
    } else { //we don't need to pad
      System.arraycopy(valueArray, 0, buffer, startingOffset, valueArray.length);
    }
    

  }

  public static String unmarshallString(byte[] buffer, int startingOffset, String codePage, int size) throws FormatException {
    String result = null;
    try {
      result = new String(buffer, startingOffset, size, codePage);
    } catch (UnsupportedEncodingException exc) {
      LOG.error("CIC002116_Code_page_not_supported", codePage);
      throw new FormatException(MESSAGES.getString("CIC002116_Code_page_not_supported", codePage));
    }

    return result;

  }

  private static void fillArray(byte[] toBeFilled, byte[] filler) {
    for (int i = 0; i < toBeFilled.length; i += filler.length) {
      System.arraycopy(filler, 0, toBeFilled, i * filler.length, filler.length);
    }
  }

}

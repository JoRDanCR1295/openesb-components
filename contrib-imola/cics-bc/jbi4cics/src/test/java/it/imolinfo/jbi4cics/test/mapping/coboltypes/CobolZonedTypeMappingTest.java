/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.test.mapping.coboltypes;

import it.imolinfo.jbi4cics.typemapping.cobol.CobolFieldFormatter;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolType;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolTypeDescriptor;
import it.imolinfo.jbi4cics.typemapping.cobol.HexDump;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.Arrays;

import junit.framework.TestCase;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class CobolZonedTypeMappingTest extends TestCase {
  
  private static Log log=LogFactory.getLog(CobolZonedTypeMappingTest.class);
  public static final String DeafultHostCodePage="CP1144";
  
  public CobolZonedTypeMappingTest(String arg0){
    super(arg0);
  }
  
  /**
   * testo un zoned
   * PIC 9(10)
   * con valore 10
   *
   */
  public void testZoned9_10(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setPadCharacter(" ");
      BigInteger value=BigInteger.valueOf(10);
      byte[] buffer=new byte[10];   
      byte[] expectedBuffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xf0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'            

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }  
  
  /**
   * testo un zoned
   * PIC s9(10)
   * con valore 10
   *
   */
  public void testZonedsp9_10(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);      
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(10);
      byte[] buffer=new byte[10];   
      byte[] expectedBuffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xc0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'            

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }    
  
  /**
   * testo un zoned
   * PIC s9(10)
   * con valore 10
   *
   */
  public void testZonedsm9_10(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(-10);
      byte[] buffer=new byte[10];   
      byte[] expectedBuffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xd0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'            

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }   
  
  /**
   * testo un zoned
   * PIC 9(10)v9(2)
   * con valore 10.10
   *
   */
  public void testZoned9_10_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setDecimalPartLength(2);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setPadCharacter(" ");
      BigDecimal value=BigDecimal.valueOf(10.10);
      byte[] buffer=new byte[12];   
      byte[] expectedBuffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xf0,(byte)0xf1,(byte)0xf0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'        '1'        '0'           

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }   
  
  /**
   * testo un zoned
   * PIC s9(10)v9(2)
   * con valore 10.10
   *
   */
  public void testZonedsp9_10_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setDecimalPartLength(2);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setSigned(true);
      BigDecimal value=BigDecimal.valueOf(10.10);
      byte[] buffer=new byte[12];   
      byte[] expectedBuffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xf0,(byte)0xf1,(byte)0xc0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'        '1'        '0'           

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }   
  
  /**
   * testo un zoned
   * PIC s9(10)v9(2)
   * con valore 10.10
   *
   */
  public void testZonedsm9_10_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setDecimalPartLength(2);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage); 
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setSigned(true);
      BigDecimal value=BigDecimal.valueOf(-10.10);
      byte[] buffer=new byte[12];   
      byte[] expectedBuffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xf0,(byte)0xf1,(byte)0xd0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'        '1'        '0'           

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }  
  
  /**
   * testo un zoned
   * PIC 9(10)
   * con valore 10
   *
   */
  public void testUnformatZoned9_10(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setPadCharacter(" ");
      BigDecimal value=BigDecimal.valueOf(10);
         
      byte[] buffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xf0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'            

      long millis1=System.currentTimeMillis();
      Object result=CobolFieldFormatter.unformat(buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      
      log.debug("value ottenuto: ["+result.toString()+"]");
      log.debug("buffer esadecimale: ["+HexDump.toHex(buffer)+"]");
      
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertEquals("conversione non corretta",value,result);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }  
  
  /**
   * testo un zoned
   * PIC s9(10)
   * con valore 10
   *
   */
  public void testUnformatZonedsp9_10(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);      
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setSigned(true);
      BigDecimal value=BigDecimal.valueOf(10);
        
      byte[] buffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xc0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'            

      long millis1=System.currentTimeMillis();
      Object result=CobolFieldFormatter.unformat(buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      
      log.debug("value ottenuto: ["+result.toString()+"]");
      log.debug("buffer esadecimale: ["+HexDump.toHex(buffer)+"]");
      
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertEquals("conversione non corretta",value,result);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }    
  
  /**
   * testo un zoned
   * PIC s9(10)
   * con valore 10
   *
   */
  public void testUnformatZonedsm9_10(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setSigned(true);
      BigDecimal value=BigDecimal.valueOf(-10);
         
      byte[] buffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xd0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'            

      long millis1=System.currentTimeMillis();
      Object result=CobolFieldFormatter.unformat(buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      
      log.debug("value ottenuto: ["+result.toString()+"]");
      log.debug("buffer esadecimale: ["+HexDump.toHex(buffer)+"]");
      
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertEquals("conversione non corretta",value,result);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }   
  
  /**
   * testo un zoned
   * PIC 9(10)v9(2)
   * con valore 10.10
   *
   */
  public void testUnformatZoned9_10_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setDecimalPartLength(2);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setPadCharacter(" ");
      BigDecimal value=new BigDecimal(10.10,new MathContext(4));
         
      byte[] buffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xf0,(byte)0xf1,(byte)0xf0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'        '1'        '0'           

      long millis1=System.currentTimeMillis();
      Object result=CobolFieldFormatter.unformat(buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      
      log.debug("value ottenuto: ["+result.toString()+"]");
      log.debug("buffer esadecimale: ["+HexDump.toHex(buffer)+"]");
      
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertEquals("conversione non corretta",value,result);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }   
  
  /**
   * testo un zoned
   * PIC s9(10)v9(2)
   * con valore 10.10
   *
   */
  public void testUnformatZonedsp9_10_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setDecimalPartLength(2);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setSigned(true);
      BigDecimal value=new BigDecimal(10.10,new MathContext(4));
         
      byte[] buffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xf0,(byte)0xf1,(byte)0xc0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'        '1'        '0'           

      long millis1=System.currentTimeMillis();
      Object result=CobolFieldFormatter.unformat(buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      
      log.debug("value ottenuto: ["+result.toString()+"]");
      log.debug("buffer esadecimale: ["+HexDump.toHex(buffer)+"]");
      
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertEquals("conversione non corretta",value,result);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }   
  
  /**
   * testo un zoned
   * PIC s9(10)v9(2)
   * con valore 10.10
   *
   */
  public void testUnformatZonedsm9_10_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setDecimalPartLength(2);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage); 
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setSigned(true);
      // a causa del fatto che equals funziona se il valore e la precisione sono 
      // gli stessi e che da host ritorna una precisione 4 mentre in java a causa dello 0 la precisione va a 3, gliela forzo a 4.
      // non ho indagato se in futuro questo possa essere un baco.
      BigDecimal value=new BigDecimal(-10.10,new MathContext(4));     
         
      byte[] buffer=new byte[] {(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xf0,(byte)0xf1,(byte)0xd0};
      //                                        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'        '1'        '0'           

      long millis1=System.currentTimeMillis();
      Object result=CobolFieldFormatter.unformat(buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      
      log.debug("result ottenuto: ["+result.toString()+"]");
      log.debug("value originale: ["+value.toString()+"]");
      log.debug("buffer esadecimale: ["+HexDump.toHex(buffer)+"]");
      
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertEquals("conversione non corretta",value,result);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  } 
  
  /**
   * testo un zoned
   * PIC +9(10)
   * con valore 10
   *
   */
   
  public void testZonedpp9_10(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);      
      cobolTypeDescriptor.setPadCharacter(" ");
      //cobolTypeDescriptor.setZonedDecimalSign(CobolTypeDescriptor.EXTERNAL_DECIMAL_SIGN_ASCII);
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(10);
      byte[] buffer=new byte[11];   
      byte[] expectedBuffer=new byte[] {(byte)0x4e,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xf0};
      //                                        '+'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'            

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }  
  
  public void testZonedmm9_10(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);      
      cobolTypeDescriptor.setPadCharacter(" ");
      //cobolTypeDescriptor.setZonedDecimalSign(CobolTypeDescriptor.EXTERNAL_DECIMAL_SIGN_ASCII);
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(-10);
      byte[] buffer=new byte[11];   
      byte[] expectedBuffer=new byte[] {(byte)0x60,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf0,(byte)0xf1,(byte)0xf0};
      //                                        '-'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '0'        '1'        '0'            

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }  
  
}

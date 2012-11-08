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
import java.util.Arrays;

import junit.framework.TestCase;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * falliscono per problemi di approssimazione:
 * testPackedDecimal9_18
 * testPackedDecimal9_18_9_2
 * testInteger9_18
 * 
 * il problema sembra legato alla precisione intrinseca del tipo double,
 * la precisione del double sembra essere intorno alle 16/17 cifre. Ho deciso di accorciare i test a questa lunghezza
 * I test diventano dunque:
 * 
 * testPackedDecimal9_16
 * testPackedDecimal9_14_9_2
 * testInteger9_16
 * 
 * @author raffaele
 *
 */

public class JavaDoubleTypeMappingTest extends TestCase {
  
  private static Log log=LogFactory.getLog(JavaDoubleTypeMappingTest.class);
  
  public static final String DeafultHostCodePage="CP1144";
  
  /**
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC 9(16) comp-3 
   * valore 1234567891234567
   *
   */

  public void testPackedDecimal9_16(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(16);      
      Double value=Double.valueOf(1234567891234567L);
      
      log.debug("value.toString: "+value.toString());
      
      byte[] buffer=new byte[9];   
      byte[] expectedBuffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x7f};
           
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();    
      
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      // unformat      
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, ((BigDecimal)result).doubleValue());
      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }  
  
  /**
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC 9(14)v9(2) comp-3
   * valore 12345678912345.12
   *
   */

  public void testPackedDecimal9_14_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(14);
      cobolTypeDescriptor.setDecimalPartLength(2);      
      log.debug("virtual decimal point: "+cobolTypeDescriptor.getVirtualDecimalPoint());
       
      Double value=new Double("12345678912345.12");
      byte[] buffer=new byte[9];   
      byte[] expectedBuffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x51,(byte)0x2f};
      
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      // unformat      
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, ((BigDecimal)result).doubleValue());      
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
  public void testZoned9_10(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.ZONED);
      cobolTypeDescriptor.setIntegerPartLength(10);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setZonedSignFormat(CobolTypeDescriptor.SIGN_FORMAT_TRAILING);
      cobolTypeDescriptor.setPadCharacter(" ");
      Double value=Double.valueOf(10);
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
      
      // unformat      
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, ((BigDecimal)result).doubleValue());      
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
      Double value=Double.valueOf("10.10");
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
      
      // unformat      
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, ((BigDecimal)result).doubleValue());      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }   
  
  /**
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC 9(2) comp 
   * valore 123456789
   * 
   * PIC 9(1)-9(2) occupa 1 byte
   * PIC 9(3)-9(4) occupa 2 byte
   * PIC 9(5)-9(9) occupa 4 byte
   * PIC 9(10)-9(18) occupa 8 byte
   * PIC 9(19) .. occupa 16 byte
   * 
   * la differenza fra comp e comp-4 è che comp controlla che la reappresentazione in cifre non superi quello dichiarato
   */  

  public void testInteger9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(2);    
      cobolTypeDescriptor.setBigEndian(true);
      Double value=Double.valueOf(12L);
      
      //String value="pippo";
      
      byte[] buffer=new byte[1];   
      byte[] expectedBuffer=BigInteger.valueOf(12L).toByteArray(); //cosi' semplice??
      //byte[] expectedBuffer=value.getBytes(); //cosi' semplice??
           
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      // unformat      
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, ((BigInteger)result).doubleValue());      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }     
  
  /**
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC 9(4) comp 
   * valore 123456789
   * 
   * PIC 9(1)-9(2) occupa 1 byte
   * PIC 9(3)-9(4) occupa 2 byte
   * PIC 9(5)-9(9) occupa 4 byte
   * PIC 9(10)-9(18) occupa 8 byte
   * PIC 9(19) .. occupa 16 byte
   * 
   * la differenza fra comp e comp-4 è che comp controlla che la reappresentazione in cifre non superi quello dichiarato
   */

  public void testInteger9_4(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(4);    
      cobolTypeDescriptor.setBigEndian(true);
      Double value=Double.valueOf(1234L);
      
      //String value="pippo";
      
      byte[] buffer=new byte[2];   
      byte[] expectedBuffer=BigInteger.valueOf(1234L).toByteArray(); //cosi' semplice??
      //byte[] expectedBuffer=value.getBytes(); //cosi' semplice??
           
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      // unformat      
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, ((BigInteger)result).doubleValue());     
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }     
  
  /**
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC 9(9) comp 
   * valore 123456789
   * 
   * PIC 9(1)-9(2) occupa 1 byte
   * PIC 9(3)-9(4) occupa 2 byte
   * PIC 9(5)-9(9) occupa 4 byte
   * PIC 9(10)-9(18) occupa 8 byte
   * PIC 9(19) .. occupa 16 byte
   * 
   * la differenza fra comp e comp-4 è che comp controlla che la reappresentazione in cifre non superi quello dichiarato
   */

  public void testInteger9_9(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(9);    
      cobolTypeDescriptor.setBigEndian(true);
      Double value=Double.valueOf(123456789L);
      
      //String value="pippo";
      
      byte[] buffer=new byte[4];   
      byte[] expectedBuffer=BigInteger.valueOf(123456789L).toByteArray(); //cosi' semplice??
      //byte[] expectedBuffer=value.getBytes(); //cosi' semplice??
           
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      // unformat      
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, ((BigInteger)result).doubleValue());      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }     
  
  /**
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC 9(16) comp 
   * valore 1234567891234567
   * 
   * PIC 9(1)-9(2) occupa 1 byte
   * PIC 9(3)-9(4) occupa 2 byte
   * PIC 9(5)-9(9) occupa 4 byte
   * PIC 9(10)-9(18) occupa 8 byte
   * PIC 9(19) .. occupa 16 byte
   * 
   * la differenza fra comp e comp-4 è che comp controlla che la reappresentazione in cifre non superi quello dichiarato
   */

  public void testInteger9_16(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(16);    
      cobolTypeDescriptor.setBigEndian(true);
      Double value=Double.valueOf(1234567891234567L);
      
      byte[] buffer=new byte[8];   
      byte[] expectedBuffer=new byte[] {0x00, 0x04, 0x62, (byte)0xD5, 0x3C, (byte)0x9B, (byte)0xAF, 0x07};
           
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      // unformat      
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, ((BigInteger)result).doubleValue());      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }     
  
  public void testIntegerm9_16(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(16);    
      cobolTypeDescriptor.setBigEndian(true);
      Double value=Double.valueOf(-1234567891234567L);
      
      byte[] buffer=new byte[8];   
      byte[] expectedBuffer=new byte[] {(byte)0xFF, (byte)0xFB, (byte)0x9D, 0x2A, (byte)0xC3, 0x64, 0x50, (byte)0xF9};
           
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      // unformat      
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, ((BigInteger)result).doubleValue());      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }  

}

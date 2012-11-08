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
 * falliscono per problemi di approssimazione
 * testPackedDecimal9_18
 * testPackedDecimal9_18_9_2
 * testInteger9_9
 * testInteger9_18
 * 
 * la capacità di approssimazione del float è di circa 8/9 cifre quindi cambio i test in
 * testPackedDecimal9_8
 * testPackedDecimal9_6_9_2
 * testInteger9_8
 * testInteger9_8
 * @author raffaele
 *
 */

public class JavaFloatTypeMappingTest extends TestCase {
  
  private static Log log=LogFactory.getLog(JavaFloatTypeMappingTest.class);
  
  public static final String DeafultHostCodePage="CP1144";
  
  /**
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC 9(18) comp-3 
   * valore 123456789123456789
   *
   */

  public void testPackedDecimal9_8(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(8);      
      Float value=Float.valueOf(12345678L);
      
      byte[] buffer=new byte[5];   
      byte[] expectedBuffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x8f};
           
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();    
      
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      // unformat      
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, ((BigDecimal)result).floatValue());      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }  
  
  /**
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC 9(18)v9(2) comp-3
   * valore 123456789123456789.12
   *
   */

  public void testPackedDecimal9_6_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(6);
      cobolTypeDescriptor.setDecimalPartLength(2);      
      log.debug("virtual decimal point: "+cobolTypeDescriptor.getVirtualDecimalPoint());
       
      Float value=new Float("123456.12");
      byte[] buffer=new byte[5];   
      byte[] expectedBuffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x61,(byte)0x2f};
      
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("buffer expected in esadecimale:       ["+HexDump.toHex(expectedBuffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      // unformat      
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, ((BigDecimal)result).floatValue());      
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
      Float value=Float.valueOf(10);
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
      assertEquals(value, ((BigDecimal)result).floatValue());      
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
      Float value=Float.valueOf("10.10");
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
      assertEquals(value, ((BigDecimal)result).floatValue());      
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
      Float value=Float.valueOf(12L);
      
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
      assertEquals(value, ((BigInteger)result).floatValue());      
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
      Float value=Float.valueOf(1234L);
      
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
      assertEquals(value, ((BigInteger)result).floatValue());            
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

  public void testInteger9_8(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(8);    
      cobolTypeDescriptor.setBigEndian(true);
      Float value=Float.valueOf(12345678L);
      
      
      byte[] buffer=new byte[4];   
      byte[] expectedBuffer=BigInteger.valueOf(12345678L).toByteArray(); //cosi' semplice??
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
      assertEquals(value, ((BigInteger)result).floatValue());            
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }     
  
  /**
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC 9(18) comp 
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

  public void testIntegerm9_8(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(8);    
      cobolTypeDescriptor.setBigEndian(true);
      Float value=Float.valueOf(-12345678L);
      
      //String value="pippo";
      
      byte[] buffer=new byte[4];   
      byte[] expectedBuffer=BigInteger.valueOf(-12345678L).toByteArray();//cosi' semplice??
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
      assertEquals(value, ((BigInteger)result).floatValue());            
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }     
  


}

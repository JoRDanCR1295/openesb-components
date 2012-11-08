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

public class CobolPackedDecimalTypeMappingTest extends TestCase {
  
  private static Log log=LogFactory.getLog(CobolPackedDecimalTypeMappingTest.class);
  
  
  public CobolPackedDecimalTypeMappingTest(String arg0){
    super(arg0);
  }
  
  /**
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC 9(18) comp-3 
   * valore 123456789123456789
   *
   */

  public void testPackedDecimal9_18(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);      
      BigInteger value=BigInteger.valueOf(123456789123456789L);
      
      byte[] buffer=new byte[10];   
      byte[] expectedBuffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x9f};
           
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();    
      
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
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC s9(18) comp-3
   * valore +123456789123456789
   *
   */

  public void testPackedDecimalsp9_18(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(123456789123456789L);
      byte[] buffer=new byte[10];   
      byte[] expectedBuffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x9c};
       
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
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
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC s9(18) comp-3
   * valore -123456789123456789
   *
   */

  public void testPackedDecimalsm9_18(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(-123456789123456789L);
      byte[] buffer=new byte[10];   
      byte[] expectedBuffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x9d};
                  
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
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
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC 9(18)v9(2) comp-3
   * valore 123456789123456789.12
   *
   */

  public void testPackedDecimal9_18_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);
      cobolTypeDescriptor.setDecimalPartLength(2);      
      log.debug("virtual decimal point: "+cobolTypeDescriptor.getVirtualDecimalPoint());
      // uso il costruttore string come previsto dalla documentazione per evitare comportamenti "unpredictable" 
      BigDecimal value=new BigDecimal("123456789123456789.12");
      byte[] buffer=new byte[11];   
      byte[] expectedBuffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x91,(byte)0x2f};
      
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
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
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC s9(18)v9(2) comp-3
   * valore 123456789123456789.12
   *
   */

  public void testPackedDecimalsp9_18_9_2(){
     try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);
      cobolTypeDescriptor.setDecimalPartLength(2);  
      cobolTypeDescriptor.setSigned(true);
      log.debug("virtual decimal point: "+cobolTypeDescriptor.getVirtualDecimalPoint());
      // uso il costruttore string come previsto dalla documentazione per evitare comportamenti "unpredictable" 
      BigDecimal value=new BigDecimal("123456789123456789.12");
      byte[] buffer=new byte[11];   
      byte[] expectedBuffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x91,(byte)0x2c};
      
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
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
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC s9(18)v9(2) comp-3
   * valore -123456789123456789.12
   *
   */

  public void testPackedDecimalsm9_18_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);
      cobolTypeDescriptor.setDecimalPartLength(2);    
      cobolTypeDescriptor.setSigned(true);
      log.debug("virtual decimal point: "+cobolTypeDescriptor.getVirtualDecimalPoint());
      // uso il costruttore string come previsto dalla documentazione per evitare comportamenti "unpredictable" 
      BigDecimal value=new BigDecimal("-123456789123456789.12");
      byte[] buffer=new byte[11];   
      byte[] expectedBuffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x91,(byte)0x2d};
      
      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();           
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
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC 9(18) comp-3 
   * valore 123456789123456789
   *
   */

  public void testUnformatPackedDecimal9_18(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);      
      BigDecimal value=BigDecimal.valueOf(123456789123456789L);
         
      byte[] buffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x9f};
           
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
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC s9(18) comp-3
   * valore +123456789123456789
   *
   */

  public void testUnformatPackedDecimalsp9_18(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);
      cobolTypeDescriptor.setSigned(true);
      BigDecimal value=BigDecimal.valueOf(123456789123456789L);
   
      byte[] buffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x9c};
       
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
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC s9(18) comp-3
   * valore -123456789123456789
   *
   */

  public void testUnformatPackedDecimalsm9_18(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);
      cobolTypeDescriptor.setSigned(true);
      BigDecimal value=BigDecimal.valueOf(-123456789123456789L);
   
      byte[] buffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x9d};
                  
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
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC 9(18)v9(2) comp-3
   * valore 123456789123456789.12
   *
   */

  public void testUnformatPackedDecimal9_18_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);
      cobolTypeDescriptor.setDecimalPartLength(2);      
      log.debug("virtual decimal point: "+cobolTypeDescriptor.getVirtualDecimalPoint());
      // uso il costruttore string come previsto dalla documentazione per evitare comportamenti "unpredictable" 
      BigDecimal value=new BigDecimal("123456789123456789.12");
   
      byte[] buffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x91,(byte)0x2f};
      
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
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC s9(18)v9(2) comp-3
   * valore 123456789123456789.12
   *
   */

  public void testUnformatPackedDecimalsp9_18_9_2(){
     try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);
      cobolTypeDescriptor.setDecimalPartLength(2);  
      cobolTypeDescriptor.setSigned(true);
      log.debug("virtual decimal point: "+cobolTypeDescriptor.getVirtualDecimalPoint());
      // uso il costruttore string come previsto dalla documentazione per evitare comportamenti "unpredictable" 
      BigDecimal value=new BigDecimal("123456789123456789.12");
   
      byte[] buffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x91,(byte)0x2c};
      
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
   * testo un packed decimal -- comp3
   * la dichiarazione è 
   * PIC s9(18)v9(2) comp-3
   * valore -123456789123456789.12
   *
   */

  public void testUnformatPackedDecimalsm9_18_9_2(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      cobolTypeDescriptor.setIntegerPartLength(18);
      cobolTypeDescriptor.setDecimalPartLength(2);    
      cobolTypeDescriptor.setSigned(true);
      log.debug("virtual decimal point: "+cobolTypeDescriptor.getVirtualDecimalPoint());
      // uso il costruttore string come previsto dalla documentazione per evitare comportamenti "unpredictable" 
      BigDecimal value=new BigDecimal("-123456789123456789.12");
   
      byte[] buffer=new byte[] {(byte)0x01,(byte)0x23,(byte)0x45,(byte)0x67,(byte)0x89,(byte)0x12,(byte)0x34,(byte)0x56,(byte)0x78,(byte)0x91,(byte)0x2d};
      
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

//TODO aggiungere un po' di test bastardi  
//TODO aggiungere i test con Long Float e Double se si voglio suppotare  
  
}

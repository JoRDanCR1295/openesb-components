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

import java.util.Arrays;

import junit.framework.TestCase;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class CobolStringTypeMappingTest extends TestCase {
  
  private static Log log=LogFactory.getLog(CobolStringTypeMappingTest.class);
  
  public static final String DeafultHostCodePage="CP1144";
  
  public CobolStringTypeMappingTest(String arg0){
    super(arg0);
  }
  
  /**
   * testo un dato cobol di tipo
   * PIC X(10)
   * con valore "pippo"
   *
   */
  public void testStringX_10L(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.STRING);
      cobolTypeDescriptor.setStringLength(10);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_LEFT);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      String value="pippo";
      byte[] buffer=new byte[10]; 
      byte[] expectedBuffer=new byte[] {(byte)0x97,(byte)0x89,(byte)0x97,(byte)0x97,(byte)0x96,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40};
      //                                       'p'        'i'        'p'        'p'        'o'        ' '        ' '        ' '        ' '        ' '                    
      

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      //unformat
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, result);
      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }
  
  /**
   * test di unformat da cobol a java di un ipo stringa
   * PIC X(10)
   * con valore "pippo"
   *
   */
  
  public void testUnformatStringX_10L(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.STRING);
      cobolTypeDescriptor.setStringLength(10);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_LEFT);
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      String expectedValue="pippo";   
      byte[] buffer=new byte[] {(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x97,(byte)0x89,(byte)0x97,(byte)0x97,(byte)0x96};
      //                                        ' '        ' '        ' '        ' '        ' '        'p'        'i'        'p'        'p'        'o'            

      long millis1=System.currentTimeMillis();
      Object value=(String)CobolFieldFormatter.unformat(buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();   
      
      log.debug("value ottenuto: ["+value.toString()+"]");
      log.debug("buffer in esadecimale: ["+HexDump.toHex(buffer)+"]");

      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertEquals("conversione non corretta",expectedValue,value);
      
      //unformat
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, result);      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }          
  }
  
  /**
   * testo un dato cobol di tipo
   * PIC X(10)
   * con valore "pippo"
   *
   */
  public void testStringX_10C(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.STRING);
      cobolTypeDescriptor.setStringLength(10);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_CENTER);
      String value="pippo";
      byte[] buffer=new byte[10];   
      byte[] expectedBuffer=new byte[] {(byte)0x40,(byte)0x40,(byte)0x97,(byte)0x89,(byte)0x97,(byte)0x97,(byte)0x96,(byte)0x40,(byte)0x40,(byte)0x40};
      //                                        ' '        ' '        'p'        'i'        'p'        'p'        'o'        ' '        ' '        ' '                    

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      //unformat
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, result);      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }
  
  /**
   * test di unformat da cobol a java di un ipo stringa
   * PIC X(10)
   * con valore "pippo"
   *
   */
  
  public void testUnformatStringX_10C(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.STRING);
      cobolTypeDescriptor.setStringLength(10);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_CENTER);
      String expectedValue="pippo";   
      byte[] buffer=new byte[] {(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x97,(byte)0x89,(byte)0x97,(byte)0x97,(byte)0x96};
      //                                        ' '        ' '        ' '        ' '        ' '        'p'        'i'        'p'        'p'        'o'            

      long millis1=System.currentTimeMillis();
      Object value=(String)CobolFieldFormatter.unformat(buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();   
      
      log.debug("value ottenuto: ["+value.toString()+"]");
      log.debug("buffer in esadecimale: ["+HexDump.toHex(buffer)+"]");

      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertEquals("conversione non corretta",expectedValue,value);
      
      //unformat
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, result);      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }          
  }
  
  /**
   * testo un dato cobol di tipo
   * PIC X(10)
   * con valore "pippo"
   *
   */
  public void testStringX_10R(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.STRING);
      cobolTypeDescriptor.setStringLength(10);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_RIGHT);
      String value="pippo";
      byte[] buffer=new byte[10];   
      byte[] expectedBuffer=new byte[] {(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x97,(byte)0x89,(byte)0x97,(byte)0x97,(byte)0x96};
      //                                        ' '        ' '        ' '        ' '        ' '        'p'        'i'        'p'        'p'        'o'            

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      //unformat
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, result);      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }
  
  /**
   * test di unformat da cobol a java di un ipo stringa
   * PIC X(10)
   * con valore "pippo"
   *
   */
  
  public void testUnformatStringX_10R(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.STRING);
      cobolTypeDescriptor.setStringLength(10);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_RIGHT);
      String expectedValue="pippo";   
      byte[] buffer=new byte[] {(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x97,(byte)0x89,(byte)0x97,(byte)0x97,(byte)0x96};
      //                                        ' '        ' '        ' '        ' '        ' '        'p'        'i'        'p'        'p'        'o'            

      long millis1=System.currentTimeMillis();
      Object value=(String)CobolFieldFormatter.unformat(buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();   
      
      log.debug("value ottenuto: ["+value.toString()+"]");
      log.debug("buffer in esadecimale: ["+HexDump.toHex(buffer)+"]");

      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertEquals("conversione non corretta",expectedValue,value);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }          
  }
  
  /**
   * testo un dato cobol di tipo
   * PIC X(10)
   * con valore "pippo"
   *
   */
  public void testStringX_10(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.STRING);
      cobolTypeDescriptor.setStringLength(10);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      cobolTypeDescriptor.setJustification(CobolTypeDescriptor.STRING_JUSTIFICATION_RIGHT);
      String value="pippo";
      byte[] buffer=new byte[10];   
      byte[] expectedBuffer=new byte[] {(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x97,(byte)0x89,(byte)0x97,(byte)0x97,(byte)0x96};
      //                                        ' '        ' '        ' '        ' '        ' '        'p'        'i'        'p'        'p'        'o'            

      long millis1=System.currentTimeMillis();
      CobolFieldFormatter.format(value,buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();     
      log.debug("buffer dopo il format in string: ["+new String(buffer,DeafultHostCodePage)+"]");
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex(buffer)+"]");
      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertTrue("conversione non corretta",Arrays.equals(expectedBuffer,buffer));
      
      //unformat
      Object result=CobolFieldFormatter.unformat(buffer, cobolTypeDescriptor, 0);
      assertEquals(value, result);      
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }        
  }
  
  /**
   * test di unformat da cobol a java di un ipo stringa
   * PIC X(10)
   * con valore "pippo"
   *
   */
  
  public void testUnformatStringX_10(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.STRING);
      cobolTypeDescriptor.setStringLength(10);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      String expectedValue="pippo";   
      byte[] buffer=new byte[] {(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x40,(byte)0x97,(byte)0x89,(byte)0x97,(byte)0x97,(byte)0x96};
      //                                        ' '        ' '        ' '        ' '        ' '        'p'        'i'        'p'        'p'        'o'            

      long millis1=System.currentTimeMillis();
      Object value=(String)CobolFieldFormatter.unformat(buffer,cobolTypeDescriptor,0);
      long millis2=System.currentTimeMillis();   
      
      log.debug("value ottenuto: ["+value.toString()+"]");
      log.debug("buffer in esadecimale: ["+HexDump.toHex(buffer)+"]");

      log.debug("conversion time="+(millis2-millis1)+" millis");
      assertEquals("conversione non corretta",expectedValue,value);
           
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }          
  }  
  
}

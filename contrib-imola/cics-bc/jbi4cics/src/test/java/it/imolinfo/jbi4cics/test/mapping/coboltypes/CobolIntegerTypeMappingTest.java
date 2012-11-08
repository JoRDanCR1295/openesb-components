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

import java.math.BigInteger;
import java.util.Arrays;

import junit.framework.TestCase;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class CobolIntegerTypeMappingTest extends TestCase {  
  
  private static Log log=LogFactory.getLog(CobolIntegerTypeMappingTest.class);
  
  public CobolIntegerTypeMappingTest(String arg0){
    super(arg0);
  }
  
  /**
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC 9(9) comp 
   * valore 123456789
   * 
   * PIC 9(1)-9(4) occupa due byte
   * PIC 9(5)-9(9) occupa 4 byte
   * PIC 9(10)-9(??) occupa 8 byte
   * 
   * la differenza fra comp e comp-4 è che comp controlla che la reappresentazione in cifre non superi quello dichiarato
   */

  public void testInteger9_9(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(9);    
      cobolTypeDescriptor.setBigEndian(true);
      BigInteger value=BigInteger.valueOf(123456789L);
      
      //String value="pippo";
      
      byte[] buffer=new byte[4];   
      byte[] expectedBuffer=value.toByteArray(); //cosi' semplice??
      //byte[] expectedBuffer=value.getBytes(); //cosi' semplice??
           
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
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC s9(9) comp 
   * valore 123456789
   * 
   */

  public void testIntegersp9_9(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(9);    
      cobolTypeDescriptor.setBigEndian(true);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(123456789L);
      
      byte[] buffer=new byte[4];   
      byte[] expectedBuffer=value.toByteArray(); //cosi' semplice??
           
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
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC s9(9) comp 
   * valore -123456789
   * 
   */

  public void testIntegersm9_9(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(9);    
      cobolTypeDescriptor.setBigEndian(true);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(-123456789L);
      
      byte[] buffer=new byte[4];   
      byte[] expectedBuffer=value.toByteArray(); //cosi' semplice??
           
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
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC 9(4) comp 
   * valore 1234
   * 
   */

  public void testInteger9_4(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(4);    
      cobolTypeDescriptor.setBigEndian(true);      
      BigInteger value=BigInteger.valueOf(1234L);
      
      byte[] buffer=new byte[2];   
      byte[] expectedBuffer=value.toByteArray(); //cosi' semplice??
           
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
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC s9(4) comp 
   * valore 1234
   * 
   */

  public void testIntegersp9_4(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(4);    
      cobolTypeDescriptor.setBigEndian(true);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(1234L);
      
      byte[] buffer=new byte[2];   
      byte[] expectedBuffer=value.toByteArray(); //cosi' semplice??
           
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
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC s9(4) comp 
   * valore -1234
   * 
   */

  public void testIntegersm9_4(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(4);    
      cobolTypeDescriptor.setBigEndian(true);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(-1234L);
      
      byte[] buffer=new byte[2];   
      byte[] expectedBuffer=value.toByteArray(); //cosi' semplice??
           
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
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC 9(9) comp 
   * valore 123456789
   * 
   */

  public void testUnformatInteger9_9(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(9);    
      cobolTypeDescriptor.setBigEndian(true);      
      BigInteger value=BigInteger.valueOf(123456789L);      
         
      byte[] buffer=value.toByteArray(); //cosi' semplice??
           
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
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC s9(9) comp 
   * valore 123456789
   * 
   */

  public void testUnformatIntegersp9_9(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(9);    
      cobolTypeDescriptor.setBigEndian(true);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(123456789L);
      
      byte[] buffer=value.toByteArray(); //cosi' semplice??
      
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
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC s9(9) comp 
   * valore -123456789
   * 
   */

  public void testUnformatIntegersm9_9(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(9);    
      cobolTypeDescriptor.setBigEndian(true);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(-123456789L);
      
      byte[] buffer=value.toByteArray(); //cosi' semplice??
      
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
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC 9(4) comp 
   * valore 1234
   * 
   */

  public void testUnformatInteger9_4(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(4);    
      cobolTypeDescriptor.setBigEndian(true);      
      BigInteger value=BigInteger.valueOf(1234L);
      
      byte[] buffer=value.toByteArray(); //cosi' semplice??
      
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
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC s9(4) comp 
   * valore 1234
   * 
   */

  public void testUnformatIntegersp9_4(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(4);    
      cobolTypeDescriptor.setBigEndian(true);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(1234L);
      
      byte[] buffer=value.toByteArray(); //cosi' semplice??
      
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
   * testo un integer -- comp
   * la dichiarazione è 
   * PIC s9(4) comp 
   * valore -1234
   * 
   */

  public void testUnformatIntegersm9_4(){
    try{
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.INTEGER);
      cobolTypeDescriptor.setIntegerPartLength(4);    
      cobolTypeDescriptor.setBigEndian(true);
      cobolTypeDescriptor.setSigned(true);
      BigInteger value=BigInteger.valueOf(-1234L);
      
      byte[] buffer=value.toByteArray(); //cosi' semplice??
      
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
  
}

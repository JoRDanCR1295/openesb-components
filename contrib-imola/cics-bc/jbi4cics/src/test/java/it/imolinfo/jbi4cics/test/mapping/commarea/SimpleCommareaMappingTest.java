/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.test.mapping.commarea;

import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaFormatter;
import it.imolinfo.jbi4cics.service.ServiceContext;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolType;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolTypeDescriptor;
import it.imolinfo.jbi4cics.typemapping.cobol.HexDump;

import java.math.BigDecimal;
import java.math.BigInteger;

import junit.framework.TestCase;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class SimpleCommareaMappingTest extends TestCase {
  
  private static Log log=LogFactory.getLog(SimpleCommareaMappingTest.class);
  public static final String DeafultHostCodePage="CP1144";

  public SimpleCommareaMappingTest(String arg){
    super(arg);
  }
  /**
   * test sulla seguenta commarea
   * PIC x(10) valore pippo
   * 
   * il test esegue mapping e unmapping e poi controlla l'uguaglianza del bean di partenza con quello di arrivo
   * 
   */
  public final void testMapString(){
    try{
      //inizializzazione del cobolTypeDescriptor
      CobolTypeDescriptor cobolTypeDescriptor=new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.STRING);
      cobolTypeDescriptor.setStringLength(10);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);
      
      
      //inizializzazione del CommareaBeanMappingDescriptor
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor=new CommareaBeanMappingDescriptor();
      commareaBeanMappingDescriptor.addFieldMapping("string","STRING",cobolTypeDescriptor);
      commareaBeanMappingDescriptor.setBeanClass(SimpleBean.class);
      

      
      //inizializzazione CommareaFormatter
      CommareaFormatter commareaFormatter=new CommareaFormatter();
      
      //inizializzazione bean
      SimpleBean simpleBean=new SimpleBean();
      simpleBean.setString("pippo");
      
      //inizializzazione del service context
      ServiceContext serviceContext=new ServiceContext();
      serviceContext.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setOutputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setInputBean(simpleBean);
      
      // conversione in input
      long millis1=System.currentTimeMillis();
      commareaFormatter.mapInputBeanToInputMessage(serviceContext);      
      Object inputMessage=serviceContext.getInputMessage();
      long millis2=System.currentTimeMillis(); 
      
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex((byte[])inputMessage)+"]");
      log.debug("conversion time1="+(millis2-millis1)+" millis");
      
      serviceContext.setOutputMessage(inputMessage);
      //conversione in output
      millis1=System.currentTimeMillis();
      commareaFormatter.mapOutputMessageToOutputBean(serviceContext);
      Object outputBean=serviceContext.getOutputBean();
      millis2=System.currentTimeMillis();
      
      log.debug("bean ottenuto: ["+outputBean+"]");
      log.debug("conversion time2="+(millis2-millis1)+" millis");

      assertEquals("oggetti non uguali dopo la doppia conversione",simpleBean,outputBean);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }

  }
  
  /**
   * test sulla seguenta commarea
   * INT          PIC 9(10)       valore 10  -> _int
   * INTEGER      PIC 9(10)       valore 11  -> integer
   * INTCOMP      PIC 9(10) comp  valore 12  -> intcomp
   * INTEGERCOMP  PIC 9(10) comp  valore 13  -> integercomp
   * INTCOMP3     PIC 9(10) comp3 valore 14  -> intcomp3
   * INTEGERCOMP3 PIC 9(10) comp3 valore 15  -> integercomp3
   * 
   * il test esegue mapping e unmapping e poi controlla l'uguaglianza del bean di partenza con quello di arrivo
   * 
   */
  public final void testMapInteger(){
    try{
      //inizializzazione del cobolTypeDescriptor
      //Zoned
      CobolTypeDescriptor intCobolTypeDescriptor=new CobolTypeDescriptor();
      intCobolTypeDescriptor.setType(CobolType.ZONED);
      intCobolTypeDescriptor.setIntegerPartLength(10);    

      CobolTypeDescriptor integerCobolTypeDescriptor=new CobolTypeDescriptor();
      integerCobolTypeDescriptor.setType(CobolType.ZONED);
      integerCobolTypeDescriptor.setIntegerPartLength(10);    

      
      // comp
      CobolTypeDescriptor intcompCobolTypeDescriptor=new CobolTypeDescriptor();
      intcompCobolTypeDescriptor.setType(CobolType.INTEGER);
      intcompCobolTypeDescriptor.setIntegerPartLength(10);    
      intcompCobolTypeDescriptor.setBigEndian(true);

      CobolTypeDescriptor integercompCobolTypeDescriptor=new CobolTypeDescriptor();
      integercompCobolTypeDescriptor.setType(CobolType.INTEGER);
      integercompCobolTypeDescriptor.setIntegerPartLength(10);    
      integercompCobolTypeDescriptor.setBigEndian(true);

      
      //Packed decimal
      CobolTypeDescriptor intcomp3CobolTypeDescriptor=new CobolTypeDescriptor();
      intcomp3CobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      intcomp3CobolTypeDescriptor.setIntegerPartLength(10);    
      intcomp3CobolTypeDescriptor.setBigEndian(true);

      CobolTypeDescriptor integer3CobolTypeDescriptor=new CobolTypeDescriptor();
      integer3CobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      integer3CobolTypeDescriptor.setIntegerPartLength(10);    
      integer3CobolTypeDescriptor.setBigEndian(true);

      
      
      //inizializzazione del CommareaBeanMappingDescriptor
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor=new CommareaBeanMappingDescriptor();
      commareaBeanMappingDescriptor.addFieldMapping("_int","INT",intCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("integer","INTEGER",integerCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("intcomp","INTCOMP",intcompCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("integercomp","INTEGERCOMP",integercompCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("intcomp3","INTCOMP3",intcomp3CobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("integercomp3","INTEGERCOMP3",integer3CobolTypeDescriptor);
      commareaBeanMappingDescriptor.setBeanClass(SimpleBean.class);
      
      //inizializzazione CommareaFormatter
      CommareaFormatter commareaFormatter=new CommareaFormatter();
      
      //inizializzazione bean
      SimpleBean simpleBean=new SimpleBean();
      simpleBean.set_int(10);
      simpleBean.setInteger(new Integer(11));
      simpleBean.setIntcomp(12);
      simpleBean.setIntegercomp(new Integer(13));
      simpleBean.setIntcomp3(14);
      simpleBean.setIntegercomp3(new Integer(15));
      
      //inizializzazione del service context
      ServiceContext serviceContext=new ServiceContext();
      serviceContext.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setOutputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setInputBean(simpleBean);
      
      // conversione in input
      long millis1=System.currentTimeMillis();
      commareaFormatter.mapInputBeanToInputMessage(serviceContext);
      Object inputMessage=serviceContext.getInputMessage();
      long millis2=System.currentTimeMillis(); 
      
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex((byte[])inputMessage)+"]");
      log.debug("conversion time1="+(millis2-millis1)+" millis");
      
      serviceContext.setOutputMessage(inputMessage);
      //conversione in output
      millis1=System.currentTimeMillis();
      commareaFormatter.mapOutputMessageToOutputBean(serviceContext);
      Object outputBean=serviceContext.getOutputBean();
      millis2=System.currentTimeMillis();
      
      log.debug("bean ottenuto: ["+outputBean+"]");
      log.debug("conversion time2="+(millis2-millis1)+" millis");

      assertEquals("oggetti non uguali dopo la doppia conversione",simpleBean,outputBean);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }

  }  
  
  /**
   * test sulla seguenta commarea
   * LONG          PIC 9(10)       valore 10  -> _long
   * _LONG         PIC 9(10)       valore 11  -> __long
   * LONGCOMP      PIC 9(10) comp  valore 12  -> longcomp
   * _LONGCOMP     PIC 9(10) comp  valore 13  -> _longcomp
   * LONGCOMP3     PIC 9(10) comp3 valore 14  -> longcomp3
   * _LONGCOMP3    PIC 9(10) comp3 valore 15  -> _longcomp3
   * 
   * il test esegue mapping e unmapping e poi controlla l'uguaglianza del bean di partenza con quello di arrivo
   * 
   */
  public final void testMapLong(){
    try{
      //inizializzazione del cobolTypeDescriptor
      //Zoned
      CobolTypeDescriptor zonedPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      zonedPrimitiveCobolTypeDescriptor.setType(CobolType.ZONED);
      zonedPrimitiveCobolTypeDescriptor.setIntegerPartLength(10);    

      CobolTypeDescriptor zonedObjectCobolTypeDescriptor=new CobolTypeDescriptor();
      zonedObjectCobolTypeDescriptor.setType(CobolType.ZONED);
      zonedObjectCobolTypeDescriptor.setIntegerPartLength(10);    

      
      // comp
      CobolTypeDescriptor integerPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      integerPrimitiveCobolTypeDescriptor.setType(CobolType.INTEGER);
      integerPrimitiveCobolTypeDescriptor.setIntegerPartLength(10);    
      integerPrimitiveCobolTypeDescriptor.setBigEndian(true);

      CobolTypeDescriptor integerObjectCobolTypeDescriptor=new CobolTypeDescriptor();
      integerObjectCobolTypeDescriptor.setType(CobolType.INTEGER);
      integerObjectCobolTypeDescriptor.setIntegerPartLength(10);    
      integerObjectCobolTypeDescriptor.setBigEndian(true);

      
      //Packed decimal
      CobolTypeDescriptor packedDecimalPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      packedDecimalPrimitiveCobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      packedDecimalPrimitiveCobolTypeDescriptor.setIntegerPartLength(10);    
      packedDecimalPrimitiveCobolTypeDescriptor.setBigEndian(true);

      CobolTypeDescriptor packedDecimalObjectCobolTypeDescriptor=new CobolTypeDescriptor();
      packedDecimalObjectCobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      packedDecimalObjectCobolTypeDescriptor.setIntegerPartLength(10);    
      packedDecimalObjectCobolTypeDescriptor.setBigEndian(true);

      
      
      //inizializzazione del CommareaBeanMappingDescriptor
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor=new CommareaBeanMappingDescriptor();
      commareaBeanMappingDescriptor.addFieldMapping("_long","LONG",zonedPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("__long","_LONG",zonedObjectCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("longcomp","LONGCOMP",integerPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("_longcomp","_LONGCOMP",integerObjectCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("longcomp3","LONGCOMP3",packedDecimalPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("_longcomp3","_LONGCOMP3",packedDecimalObjectCobolTypeDescriptor);
      commareaBeanMappingDescriptor.setBeanClass(SimpleBean.class);
      
      //inizializzazione CommareaFormatter
      CommareaFormatter commareaFormatter=new CommareaFormatter();
      
      //inizializzazione bean
      SimpleBean inputBean=new SimpleBean();
      inputBean.set_long(10);
      inputBean.set__long(new Long(11));
      inputBean.setLongcomp(12);
      inputBean.set_longcomp(new Long(13));
      inputBean.setLongcomp3(14);
      inputBean.set_longcomp3(new Long(15));
      
      //inizializzazione del service context
      ServiceContext serviceContext=new ServiceContext();
      serviceContext.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setOutputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setInputBean(inputBean);
      
      // conversione in input
      long millis1=System.currentTimeMillis();
      commareaFormatter.mapInputBeanToInputMessage(serviceContext);
      Object inputMessage=serviceContext.getInputMessage();
      long millis2=System.currentTimeMillis(); 
      
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex((byte[])inputMessage)+"]");
      log.debug("conversion time1="+(millis2-millis1)+" millis");
      
      serviceContext.setOutputMessage(inputMessage);
      //conversione in output
      millis1=System.currentTimeMillis();
      commareaFormatter.mapOutputMessageToOutputBean(serviceContext);
      Object outputBean=serviceContext.getOutputBean();
      millis2=System.currentTimeMillis();
      
      log.debug("bean ottenuto: ["+outputBean+"]");
      log.debug("conversion time2="+(millis2-millis1)+" millis");

      assertEquals("oggetti non uguali dopo la doppia conversione",inputBean,outputBean);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }

  }   
  
    /**
   * test sulla seguenta commarea
   * FLOAT          PIC 9(9)       valore 10  -> _float
   * _FLOAT         PIC 9(9)       valore 11  -> __float
   * FLOATCOMP      PIC 9(9) comp  valore 12  -> floatcomp
   * _FLOATCOMP     PIC 9(9) comp  valore 13  -> _floatcomp
   * FLOATCOMP3     PIC 9(9) comp3 valore 14  -> floatcomp3
   * _FLOATCOMP3    PIC 9(9) comp3 valore 15  -> _floatcomp3
   * 
   * il test esegue mapping e unmapping e poi controlla l'uguaglianza del bean di partenza con quello di arrivo
   * 
   */
  public final void testMapFloat(){
    try{
      //inizializzazione del cobolTypeDescriptor
      //Zoned
      CobolTypeDescriptor zonedPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      zonedPrimitiveCobolTypeDescriptor.setType(CobolType.ZONED);
      zonedPrimitiveCobolTypeDescriptor.setIntegerPartLength(9);    

      CobolTypeDescriptor zonedObjectCobolTypeDescriptor=new CobolTypeDescriptor();
      zonedObjectCobolTypeDescriptor.setType(CobolType.ZONED);
      zonedObjectCobolTypeDescriptor.setIntegerPartLength(9);    

      
      // comp
      CobolTypeDescriptor integerPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      integerPrimitiveCobolTypeDescriptor.setType(CobolType.INTEGER);
      integerPrimitiveCobolTypeDescriptor.setIntegerPartLength(9);    
      integerPrimitiveCobolTypeDescriptor.setBigEndian(true);

      CobolTypeDescriptor integerObjectCobolTypeDescriptor=new CobolTypeDescriptor();
      integerObjectCobolTypeDescriptor.setType(CobolType.INTEGER);
      integerObjectCobolTypeDescriptor.setIntegerPartLength(9);    
      integerObjectCobolTypeDescriptor.setBigEndian(true);

      
      //Packed decimal
      CobolTypeDescriptor packedDecimalPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      packedDecimalPrimitiveCobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      packedDecimalPrimitiveCobolTypeDescriptor.setIntegerPartLength(9);    
      packedDecimalPrimitiveCobolTypeDescriptor.setBigEndian(true);

      CobolTypeDescriptor packedDecimalObjectCobolTypeDescriptor=new CobolTypeDescriptor();
      packedDecimalObjectCobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      packedDecimalObjectCobolTypeDescriptor.setIntegerPartLength(9);    
      packedDecimalObjectCobolTypeDescriptor.setBigEndian(true);
   
      
      
      //inizializzazione del CommareaBeanMappingDescriptor
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor=new CommareaBeanMappingDescriptor();
      commareaBeanMappingDescriptor.addFieldMapping("_float","FLOAT",zonedPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("__float","_FLOAT",zonedObjectCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("floatcomp","FLOATCOMP",integerPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("_floatcomp","_FLOATCOMP",integerObjectCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("floatcomp3","FLOATCOMP3",packedDecimalPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("_floatcomp3","_FLOATCOMP3",packedDecimalObjectCobolTypeDescriptor);
      commareaBeanMappingDescriptor.setBeanClass(SimpleBean.class);
      
      //inizializzazione CommareaFormatter
      CommareaFormatter commareaFormatter=new CommareaFormatter();
      
      //inizializzazione bean
      SimpleBean inputBean=new SimpleBean();
      inputBean.set_float(10);
      inputBean.set__float(new Float(11));
      inputBean.setFloatcomp(12);
      inputBean.set_floatcomp(new Float(13));
      inputBean.setFloatcomp3(14);
      inputBean.set_floatcomp3(new Float(15));
      
      //inizializzazione del service context
      ServiceContext serviceContext=new ServiceContext();
      serviceContext.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setOutputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setInputBean(inputBean);
      

      // conversione in input
      long millis1=System.currentTimeMillis();
      commareaFormatter.mapInputBeanToInputMessage(serviceContext);
      Object inputMessage=serviceContext.getInputMessage();
      long millis2=System.currentTimeMillis(); 
      
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex((byte[])inputMessage)+"]");
      log.debug("conversion time1="+(millis2-millis1)+" millis");
      
      serviceContext.setOutputMessage(inputMessage);
      //conversione in output
      millis1=System.currentTimeMillis();
      commareaFormatter.mapOutputMessageToOutputBean(serviceContext);
      Object outputBean=serviceContext.getOutputBean();
      millis2=System.currentTimeMillis();
      
      log.debug("bean ottenuto: ["+outputBean+"]");
      log.debug("conversion time2="+(millis2-millis1)+" millis");

      assertEquals("oggetti non uguali dopo la doppia conversione",inputBean,outputBean);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }

  }   
  
    /**
   * test sulla seguenta commarea
   * DOUBLE          PIC 9(9)       valore 10  -> _double
   * _DOUBLE         PIC 9(9)       valore 11  -> __double
   * DOUBLECOMP      PIC 9(9) comp  valore 12  -> doublecomp
   * _DOUBLECOMP     PIC 9(9) comp  valore 13  -> _doublecomp
   * DOUBLECOMP3     PIC 9(9) comp3 valore 14  -> doublecomp3
   * _DOUBLECOMP3    PIC 9(9) comp3 valore 15  -> _doublecomp3
   * 
   * il test esegue mapping e unmapping e poi controlla l'uguaglianza del bean di partenza con quello di arrivo
   * 
   */
  public final void testMapDouble(){
    try{
      //inizializzazione del cobolTypeDescriptor
      //Zoned
      CobolTypeDescriptor zonedPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      zonedPrimitiveCobolTypeDescriptor.setType(CobolType.ZONED);
      zonedPrimitiveCobolTypeDescriptor.setIntegerPartLength(9);    

      CobolTypeDescriptor zonedObjectCobolTypeDescriptor=new CobolTypeDescriptor();
      zonedObjectCobolTypeDescriptor.setType(CobolType.ZONED);
      zonedObjectCobolTypeDescriptor.setIntegerPartLength(9);    

      
      // comp
      CobolTypeDescriptor integerPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      integerPrimitiveCobolTypeDescriptor.setType(CobolType.INTEGER);
      integerPrimitiveCobolTypeDescriptor.setIntegerPartLength(9);    
      integerPrimitiveCobolTypeDescriptor.setBigEndian(true);

      CobolTypeDescriptor integerObjectCobolTypeDescriptor=new CobolTypeDescriptor();
      integerObjectCobolTypeDescriptor.setType(CobolType.INTEGER);
      integerObjectCobolTypeDescriptor.setIntegerPartLength(9);    
      integerObjectCobolTypeDescriptor.setBigEndian(true);

      
      //Packed decimal
      CobolTypeDescriptor packedDecimalPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      packedDecimalPrimitiveCobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      packedDecimalPrimitiveCobolTypeDescriptor.setIntegerPartLength(9);    
      packedDecimalPrimitiveCobolTypeDescriptor.setBigEndian(true);

      CobolTypeDescriptor packedDecimalObjectCobolTypeDescriptor=new CobolTypeDescriptor();
      packedDecimalObjectCobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      packedDecimalObjectCobolTypeDescriptor.setIntegerPartLength(9);    
      packedDecimalObjectCobolTypeDescriptor.setBigEndian(true);

      
      
      //inizializzazione del CommareaBeanMappingDescriptor
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor=new CommareaBeanMappingDescriptor();
      commareaBeanMappingDescriptor.addFieldMapping("_double","DOUBLE",zonedPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("__double","_DOUBLE",zonedObjectCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("doublecomp","DOUBLECOMP",integerPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("_doublecomp","_DOUBLECOMP",integerObjectCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("doublecomp3","DOUBLECOMP3",packedDecimalPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("_doublecomp3","_DOUBLECOMP3",packedDecimalObjectCobolTypeDescriptor);
      commareaBeanMappingDescriptor.setBeanClass(SimpleBean.class);

      
      //inizializzazione CommareaFormatter
      CommareaFormatter commareaFormatter=new CommareaFormatter();
      
      //inizializzazione bean
      SimpleBean inputBean=new SimpleBean();
      inputBean.set_double(10);
      inputBean.set__double(new Double(11));
      inputBean.setDoublecomp(12);
      inputBean.set_doublecomp(new Double(13));
      inputBean.setDoublecomp3(14);
      inputBean.set_doublecomp3(new Double(15));
      
      //inizializzazione del service context
      ServiceContext serviceContext=new ServiceContext();
      serviceContext.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setOutputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setInputBean(inputBean);
      

      // conversione in input
      long millis1=System.currentTimeMillis();
      commareaFormatter.mapInputBeanToInputMessage(serviceContext);
      Object inputMessage=serviceContext.getInputMessage();
      long millis2=System.currentTimeMillis(); 
      
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex((byte[])inputMessage)+"]");
      log.debug("conversion time1="+(millis2-millis1)+" millis");
      
      serviceContext.setOutputMessage(inputMessage);
      //conversione in output
      millis1=System.currentTimeMillis();
      commareaFormatter.mapOutputMessageToOutputBean(serviceContext);
      Object outputBean=serviceContext.getOutputBean();
      millis2=System.currentTimeMillis();
      
      log.debug("bean ottenuto: ["+outputBean+"]");
      log.debug("conversion time2="+(millis2-millis1)+" millis");

      assertEquals("oggetti non uguali dopo la doppia conversione",inputBean,outputBean);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }

  }   
  
  
  /**
   * test sulla seguenta commarea
   * BIGINTEGER          PIC 9(10)       valore 10  -> bigInteger
   * BIGINTEGERCOMP      PIC 9(10) comp  valore 12  -> bigIntegercomp
   * BIGINTEGERCOMP3     PIC 9(10) comp3 valore 14  -> bigIntegercomp3
   * 
   * il test esegue mapping e unmapping e poi controlla l'uguaglianza del bean di partenza con quello di arrivo
   * 
   */
  public final void testMapBigInteger(){
    try{
      //inizializzazione del cobolTypeDescriptor
      //Zoned
      CobolTypeDescriptor zonedPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      zonedPrimitiveCobolTypeDescriptor.setType(CobolType.ZONED);
      zonedPrimitiveCobolTypeDescriptor.setIntegerPartLength(10);    

      
      // comp
      CobolTypeDescriptor integerPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      integerPrimitiveCobolTypeDescriptor.setType(CobolType.INTEGER);
      integerPrimitiveCobolTypeDescriptor.setIntegerPartLength(10);    
      integerPrimitiveCobolTypeDescriptor.setBigEndian(true);

      
      //Packed decimal
      CobolTypeDescriptor packedDecimalPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      packedDecimalPrimitiveCobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      packedDecimalPrimitiveCobolTypeDescriptor.setIntegerPartLength(10);    
      packedDecimalPrimitiveCobolTypeDescriptor.setBigEndian(true);



      
      
      //inizializzazione del CommareaBeanMappingDescriptor
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor=new CommareaBeanMappingDescriptor();
      commareaBeanMappingDescriptor.addFieldMapping("bigInteger","BIGINTEGER",zonedPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("bigIntegercomp","BIGINTEGERCOMP",integerPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("bigIntegercomp3","BIGINTEGERCOMP3",packedDecimalPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.setBeanClass(SimpleBean.class);
      
      //inizializzazione CommareaFormatter
      CommareaFormatter commareaFormatter=new CommareaFormatter();
      
      //inizializzazione bean
      SimpleBean inputBean=new SimpleBean();
      inputBean.setBigInteger(BigInteger.valueOf(10));
      inputBean.setBigIntegercomp(BigInteger.valueOf(12));
      inputBean.setBigIntegercomp3(BigInteger.valueOf(14));
      
      //inizializzazione del service context
      ServiceContext serviceContext=new ServiceContext();
      serviceContext.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setOutputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setInputBean(inputBean);
      

      // conversione in input
      long millis1=System.currentTimeMillis();
      commareaFormatter.mapInputBeanToInputMessage(serviceContext);
      Object inputMessage=serviceContext.getInputMessage();
      long millis2=System.currentTimeMillis(); 
      
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex((byte[])inputMessage)+"]");
      log.debug("conversion time1="+(millis2-millis1)+" millis");
      
      serviceContext.setOutputMessage(inputMessage);
      //conversione in output
      millis1=System.currentTimeMillis();
      commareaFormatter.mapOutputMessageToOutputBean(serviceContext);
      Object outputBean=serviceContext.getOutputBean();
      millis2=System.currentTimeMillis();
      
      log.debug("bean ottenuto: ["+outputBean+"]");
      log.debug("conversion time2="+(millis2-millis1)+" millis");

      assertEquals("oggetti non uguali dopo la doppia conversione",inputBean,outputBean);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }

  }      
  
  /**
   * test sulla seguenta commarea
   * BIGDECIMAL          PIC 9(9)       valore 10  -> bigDecimal
   * BIGDECIMALCOMP      PIC 9(9) comp  valore 12  -> bigDecimalcomp
   * BIGDECIMALCOMP3     PIC 9(9) comp3 valore 14  -> bigDecimalcomp3
   * 
   * il test esegue mapping e unmapping e poi controlla l'uguaglianza del bean di partenza con quello di arrivo
   * 
   */
  public final void testMapBigDecimal(){
    try{
      //inizializzazione del cobolTypeDescriptor
      //Zoned
      CobolTypeDescriptor zonedPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      zonedPrimitiveCobolTypeDescriptor.setType(CobolType.ZONED);
      zonedPrimitiveCobolTypeDescriptor.setIntegerPartLength(9);    

      
      // comp
      CobolTypeDescriptor integerPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      integerPrimitiveCobolTypeDescriptor.setType(CobolType.INTEGER);
      integerPrimitiveCobolTypeDescriptor.setIntegerPartLength(9);    
      integerPrimitiveCobolTypeDescriptor.setBigEndian(true);

      
      //Packed decimal
      CobolTypeDescriptor packedDecimalPrimitiveCobolTypeDescriptor=new CobolTypeDescriptor();
      packedDecimalPrimitiveCobolTypeDescriptor.setType(CobolType.PACKED_DECIMAL);
      packedDecimalPrimitiveCobolTypeDescriptor.setIntegerPartLength(9);    
      packedDecimalPrimitiveCobolTypeDescriptor.setBigEndian(true);


      
      
      //inizializzazione del CommareaBeanMappingDescriptor
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor=new CommareaBeanMappingDescriptor();
      commareaBeanMappingDescriptor.addFieldMapping("bigDecimal","BIGDECIMAL",zonedPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("bigDecimalcomp","BIGDECIMALCOMP",integerPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.addFieldMapping("bigDecimalcomp3","BIGDECIMALCOMP3",packedDecimalPrimitiveCobolTypeDescriptor);
      commareaBeanMappingDescriptor.setBeanClass(SimpleBean.class);
      
      //inizializzazione CommareaFormatter
      CommareaFormatter commareaFormatter=new CommareaFormatter();
      
      //inizializzazione bean
      SimpleBean inputBean=new SimpleBean();
      inputBean.setBigDecimal(new BigDecimal("10"));
      inputBean.setBigDecimalcomp(new BigDecimal("12"));
      inputBean.setBigDecimalcomp3(new BigDecimal("14"));
      
      //inizializzazione del service context
      ServiceContext serviceContext=new ServiceContext();
      serviceContext.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setOutputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceContext.setInputBean(inputBean);

      
      // conversione in input
      long millis1=System.currentTimeMillis();
      commareaFormatter.mapInputBeanToInputMessage(serviceContext);
      Object inputMessage=serviceContext.getInputMessage();
      long millis2=System.currentTimeMillis(); 
      
      log.debug("buffer dopo il format in esadecimale: ["+HexDump.toHex((byte[])inputMessage)+"]");
      log.debug("conversion time1="+(millis2-millis1)+" millis");
      
      serviceContext.setOutputMessage(inputMessage);
      //conversione in output
      millis1=System.currentTimeMillis();
      commareaFormatter.mapOutputMessageToOutputBean(serviceContext);
      Object outputBean=serviceContext.getOutputBean();
      millis2=System.currentTimeMillis();
      
      log.debug("bean ottenuto   : ["+outputBean+"]");
      log.debug("bean di partenza: ["+inputBean+"]");
      log.debug("conversion time2="+(millis2-millis1)+" millis");

      assertEquals("oggetti non uguali dopo la doppia conversione",inputBean,outputBean);
    }
    catch(Exception e){
      e.printStackTrace();
      fail(e.getMessage());
    }

  }        

  public static class SimpleBean{
    //string
    private String string;
    
    //integer
    private int _int;
    private Integer integer;
    private int intcomp;
    private Integer integercomp;
    private int intcomp3;
    private Integer integercomp3;
    
    //long
    private long _long;
    private Long __long;
    private long longcomp;
    private Long _longcomp;
    private long longcomp3;
    private Long _longcomp3;
    
    //float
    private float _float;
    private Float __float;
    private float floatcomp;
    private Float _floatcomp;
    private float floatcomp3;
    private Float _floatcomp3;
    
    //double
    private double _double;
    private Double __double;
    private double doublecomp;
    private Double _doublecomp;
    private double doublecomp3;
    private Double _doublecomp3;
    
    //Biginteger
    private BigInteger bigInteger;
    private BigInteger bigIntegercomp;
    private BigInteger bigIntegercomp3;
    
    //BigDecimal
    private BigDecimal bigDecimal;
    private BigDecimal bigDecimalcomp;
    private BigDecimal bigDecimalcomp3;

    public SimpleBean(){
      
    }
    /**
     * @return Returns the string.
     */
    public String getString() {
      return string;
    }

    /**
     * @param string The string to set.
     */
    public void setString(String string) {
      this.string = string;
    }
    
    public String toString() {
      return ReflectionToStringBuilder.toString(this);
    }
    
    public boolean equals(Object obj) {
      return EqualsBuilder.reflectionEquals(this, obj);
    }
    public int hashCode() {
      return HashCodeBuilder.reflectionHashCode(this);
    }    
    /**
     * @return Returns the _int.
     */
    public int get_int() {
      return _int;
    }
    /**
     * @param _int The _int to set.
     */
    public void set_int(int _int) {
      this._int = _int;
    }
    /**
     * @return Returns the intcomp.
     */
    public int getIntcomp() {
      return intcomp;
    }
    /**
     * @param intcomp The intcomp to set.
     */
    public void setIntcomp(int intcomp) {
      this.intcomp = intcomp;
    }
    /**
     * @return Returns the intcomp3.
     */
    public int getIntcomp3() {
      return intcomp3;
    }
    /**
     * @param intcomp3 The intcomp3 to set.
     */
    public void setIntcomp3(int intcomp3) {
      this.intcomp3 = intcomp3;
    }
    /**
     * @return Returns the integer.
     */
    public Integer getInteger() {
      return integer;
    }
    /**
     * @param integer The integer to set.
     */
    public void setInteger(Integer integer) {
      this.integer = integer;
    }
    /**
     * @return Returns the integercomp.
     */
    public Integer getIntegercomp() {
      return integercomp;
    }
    /**
     * @param integercomp The integercomp to set.
     */
    public void setIntegercomp(Integer integercomp) {
      this.integercomp = integercomp;
    }
    /**
     * @return Returns the integercomp3.
     */
    public Integer getIntegercomp3() {
      return integercomp3;
    }
    /**
     * @param integercomp3 The integercomp3 to set.
     */
    public void setIntegercomp3(Integer integercomp3) {
      this.integercomp3 = integercomp3;
    }
    /**
     * @return Returns the __long.
     */
    public Long get__long() {
      return __long;
    }
    /**
     * @param __long The __long to set.
     */
    public void set__long(Long __long) {
      this.__long = __long;
    }
    /**
     * @return Returns the _long.
     */
    public long get_long() {
      return _long;
    }
    /**
     * @param _long The _long to set.
     */
    public void set_long(long _long) {
      this._long = _long;
    }
    /**
     * @return Returns the _longcomp.
     */
    public Long get_longcomp() {
      return _longcomp;
    }
    /**
     * @param _longcomp The _longcomp to set.
     */
    public void set_longcomp(Long _longcomp) {
      this._longcomp = _longcomp;
    }
    /**
     * @return Returns the _longcomp3.
     */
    public Long get_longcomp3() {
      return _longcomp3;
    }
    /**
     * @param _longcomp3 The _longcomp3 to set.
     */
    public void set_longcomp3(Long _longcomp3) {
      this._longcomp3 = _longcomp3;
    }
    /**
     * @return Returns the longcomp.
     */
    public long getLongcomp() {
      return longcomp;
    }
    /**
     * @param longcomp The longcomp to set.
     */
    public void setLongcomp(long longcomp) {
      this.longcomp = longcomp;
    }
    /**
     * @return Returns the longcomp3.
     */
    public long getLongcomp3() {
      return longcomp3;
    }
    /**
     * @param longcomp3 The longcomp3 to set.
     */
    public void setLongcomp3(long longcomp3) {
      this.longcomp3 = longcomp3;
    }
    /**
     * @return Returns the bigInteger.
     */
    public BigInteger getBigInteger() {
      return bigInteger;
    }
    /**
     * @param bigInteger The bigInteger to set.
     */
    public void setBigInteger(BigInteger bigInteger) {
      this.bigInteger = bigInteger;
    }
    /**
     * @return Returns the bigIntegercomp.
     */
    public BigInteger getBigIntegercomp() {
      return bigIntegercomp;
    }
    /**
     * @param bigIntegercomp The bigIntegercomp to set.
     */
    public void setBigIntegercomp(BigInteger bigIntegercomp) {
      this.bigIntegercomp = bigIntegercomp;
    }
    /**
     * @return Returns the bigIntegercomp3.
     */
    public BigInteger getBigIntegercomp3() {
      return bigIntegercomp3;
    }
    /**
     * @param bigIntegercomp3 The bigIntegercomp3 to set.
     */
    public void setBigIntegercomp3(BigInteger bigIntegercomp3) {
      this.bigIntegercomp3 = bigIntegercomp3;
    }
    /**
     * @return Returns the bigDecimal.
     */
    public BigDecimal getBigDecimal() {
      return bigDecimal;
    }
    /**
     * @param bigDecimal The bigDecimal to set.
     */
    public void setBigDecimal(BigDecimal bigDecimal) {
      this.bigDecimal = bigDecimal;
    }
    /**
     * @return Returns the bigDecimalcomp.
     */
    public BigDecimal getBigDecimalcomp() {
      return bigDecimalcomp;
    }
    /**
     * @param bigDecimalcomp The bigDecimalcomp to set.
     */
    public void setBigDecimalcomp(BigDecimal bigDecimalcomp) {
      this.bigDecimalcomp = bigDecimalcomp;
    }
    /**
     * @return Returns the bigDecimalcomp3.
     */
    public BigDecimal getBigDecimalcomp3() {
      return bigDecimalcomp3;
    }
    /**
     * @param bigDecimalcomp3 The bigDecimalcomp3 to set.
     */
    public void setBigDecimalcomp3(BigDecimal bigDecimalcomp3) {
      this.bigDecimalcomp3 = bigDecimalcomp3;
    }
    /**
     * @return Returns the __double.
     */
    public Double get__double() {
      return __double;
    }
    /**
     * @param __double The __double to set.
     */
    public void set__double(Double __double) {
      this.__double = __double;
    }
    /**
     * @return Returns the _double.
     */
    public double get_double() {
      return _double;
    }
    /**
     * @param _double The _double to set.
     */
    public void set_double(double _double) {
      this._double = _double;
    }
    /**
     * @return Returns the _doublecomp.
     */
    public Double get_doublecomp() {
      return _doublecomp;
    }
    /**
     * @param _doublecomp The _doublecomp to set.
     */
    public void set_doublecomp(Double _doublecomp) {
      this._doublecomp = _doublecomp;
    }
    /**
     * @return Returns the _doublecomp3.
     */
    public Double get_doublecomp3() {
      return _doublecomp3;
    }
    /**
     * @param _doublecomp3 The _doublecomp3 to set.
     */
    public void set_doublecomp3(Double _doublecomp3) {
      this._doublecomp3 = _doublecomp3;
    }
    /**
     * @return Returns the doublecomp.
     */
    public double getDoublecomp() {
      return doublecomp;
    }
    /**
     * @param doublecomp The doublecomp to set.
     */
    public void setDoublecomp(double doublecomp) {
      this.doublecomp = doublecomp;
    }
    /**
     * @return Returns the doublecomp3.
     */
    public double getDoublecomp3() {
      return doublecomp3;
    }
    /**
     * @param doublecomp3 The doublecomp3 to set.
     */
    public void setDoublecomp3(double doublecomp3) {
      this.doublecomp3 = doublecomp3;
    }
    /**
     * @return Returns the __float.
     */
    public Float get__float() {
      return __float;
    }
    /**
     * @param __float The __float to set.
     */
    public void set__float(Float __float) {
      this.__float = __float;
    }
    /**
     * @return Returns the _float.
     */
    public float get_float() {
      return _float;
    }
    /**
     * @param _float The _float to set.
     */
    public void set_float(float _float) {
      this._float = _float;
    }
    /**
     * @return Returns the _floatcomp.
     */
    public Float get_floatcomp() {
      return _floatcomp;
    }
    /**
     * @param _floatcomp The _floatcomp to set.
     */
    public void set_floatcomp(Float _floatcomp) {
      this._floatcomp = _floatcomp;
    }
    /**
     * @return Returns the _floatcomp3.
     */
    public Float get_floatcomp3() {
      return _floatcomp3;
    }
    /**
     * @param _floatcomp3 The _floatcomp3 to set.
     */
    public void set_floatcomp3(Float _floatcomp3) {
      this._floatcomp3 = _floatcomp3;
    }
    /**
     * @return Returns the floatcomp.
     */
    public float getFloatcomp() {
      return floatcomp;
    }
    /**
     * @param floatcomp The floatcomp to set.
     */
    public void setFloatcomp(float floatcomp) {
      this.floatcomp = floatcomp;
    }
    /**
     * @return Returns the floatcomp3.
     */
    public float getFloatcomp3() {
      return floatcomp3;
    }
    /**
     * @param floatcomp3 The floatcomp3 to set.
     */
    public void setFloatcomp3(float floatcomp3) {
      this.floatcomp3 = floatcomp3;
    }
    
  }

}

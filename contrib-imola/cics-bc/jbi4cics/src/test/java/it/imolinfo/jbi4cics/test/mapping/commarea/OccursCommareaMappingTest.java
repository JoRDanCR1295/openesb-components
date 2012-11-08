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
import junit.framework.TestCase;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class OccursCommareaMappingTest extends TestCase {
  private static Log log = LogFactory.getLog(OccursCommareaMappingTest.class);

  public static final String DeafultHostCodePage = "CP1144";

  public OccursCommareaMappingTest(String arg) {
    super(arg);
  }

  /**
   * questo metodo testa la seguente commarea
   * 
   * 01 PIPPO PICX(10) 
   * 01 NESTED OCCURS 10.
   * 02 PLUTO PICX(10)
   * 
   */
  public final void testOneNest() {
    try {
      // inizializzazione del cobolTypeDescriptor
      CobolTypeDescriptor cobolTypeDescriptor = new CobolTypeDescriptor();
      cobolTypeDescriptor.setType(CobolType.STRING);
      cobolTypeDescriptor.setStringLength(10);
      cobolTypeDescriptor.setPadCharacter(" ");
      cobolTypeDescriptor.setCodePage(DeafultHostCodePage);

      // inizializzazione del CommareaBeanMappingDescriptor External
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptorExternal = new CommareaBeanMappingDescriptor();
      commareaBeanMappingDescriptorExternal.addFieldMapping("pippo", "PIPPO", cobolTypeDescriptor);
      commareaBeanMappingDescriptorExternal.setBeanClass(ExternalBean.class);

      // inizializzazione del CommareaBeanMappingDescriptor Internal
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptorInternal = new CommareaBeanMappingDescriptor();
      commareaBeanMappingDescriptorInternal.addFieldMapping("pluto", "PLUTO", cobolTypeDescriptor);
      commareaBeanMappingDescriptorInternal.setBeanClass(InternalBean.class);
      
      // creo il field descriptor per l'internal mapping descriptor
      CobolTypeDescriptor cobolTypeDescriptorInternal = new CobolTypeDescriptor();
      cobolTypeDescriptorInternal.setType(CobolType.OCCURS);
      cobolTypeDescriptorInternal.setOccursSize(10);
      cobolTypeDescriptorInternal.setNestedCommarea(commareaBeanMappingDescriptorInternal);      
      
      commareaBeanMappingDescriptorExternal.addFieldMapping("nested", "NESTED", cobolTypeDescriptorInternal);
      
      //inizializzazione CommareaFormatter
      CommareaFormatter commareaFormatter=new CommareaFormatter();
      
      //inizializzazione bean
      InternalBean[] internalBeans=new InternalBean[10];
      for (int i=0;i<internalBeans.length;i++){
        internalBeans[i]=new InternalBean();
        internalBeans[i].setPluto("pluto"+i);      
      }
      ExternalBean externalBean=new ExternalBean();
      externalBean.setNested(internalBeans);
      externalBean.setPippo("pippo");
      
      log .debug("input bean: "+externalBean);
      
      //inizializzazione del service context
      ServiceContext serviceContext=new ServiceContext();
      serviceContext.setInputMappingDescriptor(commareaBeanMappingDescriptorExternal);
      serviceContext.setOutputMappingDescriptor(commareaBeanMappingDescriptorExternal);
      serviceContext.setInputBean(externalBean);
      
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

      assertEquals("oggetti non uguali dopo la doppia conversione",externalBean,outputBean);
      
    } catch (Exception e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
  }

  public static class InternalBean {
    private String pluto;

    public String getPluto() {
      return pluto;
    }

    public void setPluto(String pluto) {
      this.pluto = pluto;
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
  }

  public static class ExternalBean {
    private String pippo;
    private InternalBean[] nested;

    public InternalBean[] getNested() {
      return nested;
    }
    public void setNested(InternalBean[] nested) {
      this.nested = nested;
    }
    public String getPippo() {
      return pippo;
    }
    public void setPippo(String pippo) {
      this.pippo = pippo;
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
  }
}

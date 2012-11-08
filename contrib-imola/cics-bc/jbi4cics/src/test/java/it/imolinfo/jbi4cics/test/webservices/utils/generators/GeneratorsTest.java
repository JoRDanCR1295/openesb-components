/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.test.webservices.utils.generators;
 
import it.imolinfo.jbi4cics.commareaparser.CommareaLexer;
import it.imolinfo.jbi4cics.commareaparser.CommareaParser;
import it.imolinfo.jbi4cics.jbi.BCELClassLoader;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.test.BaseCommareaTest;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceBeanGenerator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceInterfaceGenerator;

import java.io.FileInputStream;
import java.io.InputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class GeneratorsTest extends BaseCommareaTest {
  
  private static Log log = LogFactory.getLog(GeneratorsTest.class);

  
  public GeneratorsTest(String arg) {
    super(arg);
  }
  
  public void testServiceBeanGenerator(){
    try {
      // genero il mapping descriptor
      InputStream is = new FileInputStream(testRootCommarea+"/"+"ProvaCommarea2.txt");
      CommareaLexer lexer = new CommareaLexer(is);
      CommareaParser parser = new CommareaParser(lexer);
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
      log.info("commareaBeanMappingDescriptor: " + commareaBeanMappingDescriptor);

      // creo il service desciptor
      ServiceDescriptor serviceDescriptor=new ServiceDescriptor();

      serviceDescriptor.setOperationName("provaOperation");
      serviceDescriptor.setServiceName("ProvaService");
      serviceDescriptor.setServiceInterfacePackageName("it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setInputBeanClassName("ProvaServiceBean");
      serviceDescriptor.setOutputBeanClassName("ProvaServiceBean");
      serviceDescriptor.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceDescriptor.setOutputMappingDescriptor(commareaBeanMappingDescriptor);
      
      // creo il service bean
      BCELClassLoader bcelClassLoader=new BCELClassLoader(Thread.currentThread().getContextClassLoader());
      ServiceBeanGenerator serviceBeanGenerator=new ServiceBeanGenerator(serviceDescriptor,true);
      Class loadedClass=serviceBeanGenerator.generateBeanClass(bcelClassLoader);
      
      log.info("loaded class: "+loadedClass);
      
      
    } catch (Throwable e) {
      log.error("errore in test testServiceBeanGenerator",e);
      fail(e.getMessage());
    }
  }
  
  public void testServiceInterfaceGenerator(){
    try {  
      // genero il mapping descriptor
      InputStream is = new FileInputStream(testRootCommarea+"/"+"ProvaCommarea2.txt");
      CommareaLexer lexer = new CommareaLexer(is);
      CommareaParser parser = new CommareaParser(lexer);
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
      log.info("commareaBeanMappingDescriptor: " + commareaBeanMappingDescriptor);
      
      // creo il service desciptor
      ServiceDescriptor serviceDescriptor=new ServiceDescriptor();

      serviceDescriptor.setOperationName("provaOperation");
      serviceDescriptor.setServiceName("ProvaService");
      serviceDescriptor.setServiceInterfacePackageName("it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setInputBeanClassName("ProvaServiceBean");
      serviceDescriptor.setOutputBeanClassName("ProvaServiceBean");
      serviceDescriptor.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceDescriptor.setOutputMappingDescriptor(commareaBeanMappingDescriptor);
      
      // creo il service bean
      BCELClassLoader bcelClassLoader=new BCELClassLoader(Thread.currentThread().getContextClassLoader());
      ServiceBeanGenerator serviceBeanGenerator=new ServiceBeanGenerator(serviceDescriptor,true);
      Class loadedClass=serviceBeanGenerator.generateBeanClass(bcelClassLoader);     
      
      log.info("loaded class: "+loadedClass);      
      
      // creo la service interface
      serviceDescriptor.setInputBean(loadedClass);
      serviceDescriptor.setOutputBean(loadedClass);
      ServiceInterfaceGenerator serviceInterfaceGenerator=new ServiceInterfaceGenerator(serviceDescriptor);      
      Class loadedInterface=serviceInterfaceGenerator.generateServiceInterface(bcelClassLoader);
      
      log.info("loaded interface: "+loadedInterface);
      
    } catch (Throwable e) {
      log.error("errore in test testServiceInterfaceGenerator",e);
      fail(e.getMessage());
    }
  }
}

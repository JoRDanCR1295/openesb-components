/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.test.webservices.runtime;

import it.imolinfo.jbi4cics.commareaparser.CommareaLexer;
import it.imolinfo.jbi4cics.commareaparser.CommareaParser;
import it.imolinfo.jbi4cics.jbi.BCELClassLoader;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.test.BaseCommareaTest;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import it.imolinfo.jbi4cics.webservices.runtime.ServiceCreator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceBeanGenerator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceInterfaceGenerator;

import java.io.FileInputStream;
import java.io.InputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.codehaus.xfire.XFire;
import org.codehaus.xfire.XFireFactory;
import org.codehaus.xfire.service.Service;


public class NestedServiceCreatorTest extends BaseCommareaTest {
  private static Log log = LogFactory.getLog(NestedServiceCreatorTest.class);



  public NestedServiceCreatorTest(String arg) {
    super(arg);
  }
  
  public void testNestedServiceCreator() {
    try {
      // genero il mapping descriptor
      InputStream is = new FileInputStream(testRootCommarea + "/" + "NestedCommarea.txt");
      CommareaLexer lexer = new CommareaLexer(is);
      CommareaParser parser = new CommareaParser(lexer);
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
      log.info("commareaBeanMappingDescriptor: " + commareaBeanMappingDescriptor);

      // creo il service desciptor
      ServiceDescriptor serviceDescriptor = new ServiceDescriptor();

      serviceDescriptor.setOperationName("provaOperation");
      serviceDescriptor.setServiceName("ProvaService");
      serviceDescriptor.setServiceInterfacePackageName("it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setServiceInterfaceName("ProvaServiceBeanInterface");
      serviceDescriptor.setServiceNameSpace("urn:it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setInputBeanClassName("ProvaServiceBean");
      serviceDescriptor.setOutputBeanClassName("ProvaServiceBean");
      serviceDescriptor.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceDescriptor.setOutputMappingDescriptor(commareaBeanMappingDescriptor);

      // creo il service bean
      BCELClassLoader bcelClassLoader = new BCELClassLoader(Thread.currentThread().getContextClassLoader());
      ServiceBeanGenerator serviceBeanGenerator = new ServiceBeanGenerator(serviceDescriptor, true);
      serviceBeanGenerator.generateBeanClass(bcelClassLoader);

      // creo la service interface
      ServiceInterfaceGenerator serviceInterfaceGenerator = new ServiceInterfaceGenerator(serviceDescriptor);
      serviceInterfaceGenerator.generateServiceInterface(bcelClassLoader);
      XFire xfire = XFireFactory.newInstance().getXFire();
      ServiceCreator serviceCreator = new ServiceCreator();
      Service service = serviceCreator.createService(serviceDescriptor, xfire);
      log.debug("servizio creato: " + service);

      // TODO fare degli assert ragionevoli

    } catch (Throwable e) {
      log.error("errore in test testNestedServiceCreator", e);
      fail(e.getMessage());
    }
  }
  
  public void testNested3ServiceCreator() {
    try {
      // genero il mapping descriptor
      InputStream is = new FileInputStream(testRootCommarea + "/" + "NestedCommarea3.txt");
      CommareaLexer lexer = new CommareaLexer(is);
      CommareaParser parser = new CommareaParser(lexer);
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
      log.info("commareaBeanMappingDescriptor: " + commareaBeanMappingDescriptor);

      // creo il service desciptor
      ServiceDescriptor serviceDescriptor = new ServiceDescriptor();

      serviceDescriptor.setOperationName("provaOperation");
      serviceDescriptor.setServiceName("ProvaService");
      serviceDescriptor.setServiceInterfacePackageName("it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setServiceInterfaceName("ProvaServiceBeanInterface");
      serviceDescriptor.setServiceNameSpace("urn:it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setInputBeanClassName("ProvaServiceBean");
      serviceDescriptor.setOutputBeanClassName("ProvaServiceBean");
      serviceDescriptor.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceDescriptor.setOutputMappingDescriptor(commareaBeanMappingDescriptor);

      // creo il service bean
      BCELClassLoader bcelClassLoader = new BCELClassLoader(Thread.currentThread().getContextClassLoader());
      ServiceBeanGenerator serviceBeanGenerator = new ServiceBeanGenerator(serviceDescriptor, true);
      serviceBeanGenerator.generateBeanClass(bcelClassLoader);

      // creo la service interface
      ServiceInterfaceGenerator serviceInterfaceGenerator = new ServiceInterfaceGenerator(serviceDescriptor);
      serviceInterfaceGenerator.generateServiceInterface(bcelClassLoader);
      XFire xfire = XFireFactory.newInstance().getXFire();
      ServiceCreator serviceCreator = new ServiceCreator();
      Service service = serviceCreator.createService(serviceDescriptor, xfire);
      log.debug("servizio creato: " + service);

      // TODO fare degli assert ragionevoli

    } catch (Throwable e) {
      log.error("errore in test testNestedServiceCreator", e);
      fail(e.getMessage());
    }
  }  

}

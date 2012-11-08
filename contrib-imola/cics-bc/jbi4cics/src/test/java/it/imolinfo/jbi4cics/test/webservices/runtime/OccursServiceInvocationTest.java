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
import it.imolinfo.jbi4cics.locator.ServiceLocation;
import it.imolinfo.jbi4cics.locator.SimpleLocation;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.test.BaseCommareaTest;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import it.imolinfo.jbi4cics.webservices.runtime.ServiceCreator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceBeanGenerator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceInterfaceGenerator;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.codehaus.xfire.XFire;
import org.codehaus.xfire.service.Service;
import org.jdom.Document;

public class OccursServiceInvocationTest extends BaseCommareaTest {

  private static Log log = LogFactory.getLog(ServiceInvocationTest.class);
  public static final String DeafultHostCodePage="CP037";

  public OccursServiceInvocationTest() {
  }


  public void testOccursCommareaServiceInvocation(){
    try {
      // genero il mapping descriptor
      InputStream is = new FileInputStream(testRootCommarea+"/"+"OccursCommarea.txt");
      CommareaLexer lexer = new CommareaLexer(is);
      CommareaParser parser = new CommareaParser(lexer);
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
      log.info("commareaBeanMappingDescriptor: " + commareaBeanMappingDescriptor);

      // creo il service desciptor
      ServiceDescriptor serviceDescriptor=new ServiceDescriptor();

      serviceDescriptor.setOperationName("provaOccursOperation");
      serviceDescriptor.setServiceName("ProvaOccurs");
      serviceDescriptor.setServiceInterfacePackageName("it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setServiceInterfaceName("ProvaOccursBeanInterface");
      serviceDescriptor.setServiceNameSpace("urn:it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setInputBeanClassName("ProvaOccursBean");
      serviceDescriptor.setOutputBeanClassName("ProvaOccursBean");
      serviceDescriptor.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceDescriptor.setOutputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceDescriptor.setCodePage(DeafultHostCodePage);

      // aggiungo le info del servizio
      //costruisco il locator
      SimpleLocation serviceLocation=new SimpleLocation();
      serviceLocation.setConnectionType(ServiceLocation.DUMMY);
      serviceDescriptor.setServiceLocation(serviceLocation);

      // creo il service bean, basta quello di input tanto in questo caso sono uguali
      BCELClassLoader bcelClassLoader = new BCELClassLoader(Thread.currentThread().getContextClassLoader());
      ServiceBeanGenerator serviceBeanGenerator=new ServiceBeanGenerator(serviceDescriptor,true);
      serviceBeanGenerator.generateBeanClass(bcelClassLoader);


      serviceBeanGenerator=new ServiceBeanGenerator(serviceDescriptor,false);
      serviceBeanGenerator.generateBeanClass(bcelClassLoader);


      // creo la service interface
      ServiceInterfaceGenerator serviceInterfaceGenerator=new ServiceInterfaceGenerator(serviceDescriptor);
      serviceInterfaceGenerator.generateServiceInterface(bcelClassLoader);

      //creo il servizio xfire
      ServiceCreator serviceCreator=new ServiceCreator();
      XFire xfire = getXFire();
      Service service=serviceCreator.createService(serviceDescriptor,xfire);
      service.getWSDLWriter().write(new FileOutputStream("target/test-wsdl-ext/"+service.getSimpleName()+".wsdl"));
      log.debug("created service: "+service);

      xfire.getServiceRegistry().register(service);
      log.debug("registry: "+xfire.getServiceRegistry());

      // invoco il servizio
      Document response = invokeService(service.getName().getLocalPart(), "/xmlmessages/testOccursMessage.xml" );
      printNode(response);

      addNamespace( "test", "urn:it.imolinfo.jbi4cics.test.webservices.utils.generators" );
      assertValid( "//test:provaOccursOperationResponse", response );
      //assertValid( "string(test:provaEchoOperationResponse/out/string)='Hello World'", response );
      //log.info("response document: "+response);

    } catch (Throwable e) {
      log.error("errore in test testOccursCommareaServiceInvocation",e);
      fail(e.getMessage());
    }
  }

  public void testOccursCommarea2ServiceInvocation(){
    try {
      // genero il mapping descriptor
      InputStream is = new FileInputStream(testRootCommarea+"/"+"OccursCommarea2.txt");
      CommareaLexer lexer = new CommareaLexer(is);
      CommareaParser parser = new CommareaParser(lexer);
      CommareaBeanMappingDescriptor commareaBeanMappingDescriptor = parser.commarea_definition();
      log.info("commareaBeanMappingDescriptor: " + commareaBeanMappingDescriptor);

      // creo il service desciptor
      ServiceDescriptor serviceDescriptor=new ServiceDescriptor();

      serviceDescriptor.setOperationName("provaOccurs2Operation");
      serviceDescriptor.setServiceName("ProvaOccurs2");
      serviceDescriptor.setServiceInterfacePackageName("it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setServiceInterfaceName("ProvaOccurs2BeanInterface");
      serviceDescriptor.setServiceNameSpace("urn:it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setInputBeanClassName("ProvaOccurs2Bean");
      serviceDescriptor.setOutputBeanClassName("ProvaOccurs2Bean");
      serviceDescriptor.setInputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceDescriptor.setOutputMappingDescriptor(commareaBeanMappingDescriptor);
      serviceDescriptor.setCodePage(DeafultHostCodePage);

      // aggiungo le info del servizio
      //costruisco il locator
      SimpleLocation serviceLocation=new SimpleLocation();
      serviceLocation.setConnectionType(ServiceLocation.DUMMY);
      serviceDescriptor.setServiceLocation(serviceLocation);

      // creo il service bean, basta quello di input tanto in questo caso sono uguali
      BCELClassLoader bcelClassLoader = new BCELClassLoader(Thread.currentThread().getContextClassLoader());
      ServiceBeanGenerator serviceBeanGenerator=new ServiceBeanGenerator(serviceDescriptor,true);
      serviceBeanGenerator.generateBeanClass(bcelClassLoader);


      serviceBeanGenerator=new ServiceBeanGenerator(serviceDescriptor,false);
      serviceBeanGenerator.generateBeanClass(bcelClassLoader);


      // creo la service interface
      ServiceInterfaceGenerator serviceInterfaceGenerator=new ServiceInterfaceGenerator(serviceDescriptor);
      serviceInterfaceGenerator.generateServiceInterface(bcelClassLoader);

      //creo il servizio xfire
      ServiceCreator serviceCreator=new ServiceCreator();
      XFire xfire = getXFire();
      Service service=serviceCreator.createService(serviceDescriptor,xfire);
      service.getWSDLWriter().write(new FileOutputStream("target/test-wsdl-ext/"+service.getSimpleName()+".wsdl"));
      log.debug("created service: "+service);

      xfire.getServiceRegistry().register(service);
      log.debug("registry: "+xfire.getServiceRegistry());

      // invoco il servizio
      Document response = invokeService(service.getName().getLocalPart(), "/xmlmessages/testOccursMessage2.xml" );
      printNode(response);

      addNamespace( "test", "urn:it.imolinfo.jbi4cics.test.webservices.utils.generators" );
      assertValid( "//test:provaOccurs2OperationResponse", response );
      //assertValid( "string(test:provaEchoOperationResponse/out/string)='Hello World'", response );
      //log.info("response document: "+response);

    } catch (Throwable e) {
      log.error("errore in test testServiceCreator",e);
      fail(e.getMessage());
    }
  }
}

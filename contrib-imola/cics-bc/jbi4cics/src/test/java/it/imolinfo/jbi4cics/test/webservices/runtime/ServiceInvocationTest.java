/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.test.webservices.runtime;

import it.imolinfo.jbi4cics.jbi.BCELClassLoader;
import it.imolinfo.jbi4cics.locator.ServiceLocation;
import it.imolinfo.jbi4cics.locator.SimpleLocation;
import it.imolinfo.jbi4cics.messageformat.NoOpMappingDescriptor;
import it.imolinfo.jbi4cics.test.BaseCommareaTest;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import it.imolinfo.jbi4cics.webservices.runtime.ServiceCreator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceBeanGenerator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceInterfaceGenerator;

import java.io.FileOutputStream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.codehaus.xfire.XFire;
import org.codehaus.xfire.service.Service;
import org.jdom.Document;

public class ServiceInvocationTest extends BaseCommareaTest {

  private static Log log = LogFactory.getLog(ServiceInvocationTest.class);

  public static final String DeafultHostCodePage="CP037";

  public ServiceInvocationTest() {
  }

  public void testEchoServiceInvocation(){
    try{
      // creo il service desciptor
      NoOpMappingDescriptor noOpMappingDescriptor=new NoOpMappingDescriptor();
      noOpMappingDescriptor.setBeanClass(StringBean.class);
      ServiceDescriptor serviceDescriptor=new ServiceDescriptor();
      serviceDescriptor.setOperationName("provaEchoOperation");
      serviceDescriptor.setServiceName("ProvaEcho");
      serviceDescriptor.setServiceInterfacePackageName("it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setServiceInterfaceName("ProvaServiceBeanInterface");
      serviceDescriptor.setServiceNameSpace("urn:it.imolinfo.jbi4cics.test.webservices.utils.generators");
      serviceDescriptor.setInputBeanClassName("ProvaEchoBean");
      serviceDescriptor.setOutputBeanClassName("ProvaEchoBean");
      serviceDescriptor.setInputMappingDescriptor(noOpMappingDescriptor);
      serviceDescriptor.setOutputMappingDescriptor(noOpMappingDescriptor);
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
      Document response = invokeService(service.getName().getLocalPart(), "/xmlmessages/testEchoMessage2.xml" );
      printNode(response);

      addNamespace( "test", "urn:it.imolinfo.jbi4cics.test.webservices.utils.generators" );
      assertValid( "//test:provaEchoOperationResponse", response );
      assertValid( "string(test:provaEchoOperationResponse/out/string)='Hello World'", response );
      log.info("response document: "+response);
    } catch (Throwable e) {
      log.error("errore in test testServiceCreator",e);
      fail(e.getMessage());
    }

  }

  public static class StringBean {
    private String string;

    /**
     *
     */
    public StringBean() {
      // TODO Auto-generated constructor stub
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

  }

}

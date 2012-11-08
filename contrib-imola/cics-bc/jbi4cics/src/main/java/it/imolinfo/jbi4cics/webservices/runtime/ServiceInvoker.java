/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.webservices.runtime;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.connection.ConnectionManager;
import it.imolinfo.jbi4cics.connection.ConnectionManagerFactory;
import it.imolinfo.jbi4cics.exception.ConnectionException;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.jbi.Messages;
import it.imolinfo.jbi4cics.messageformat.MessageFormatter;
import it.imolinfo.jbi4cics.messageformat.MessageFormatterFactory;
import it.imolinfo.jbi4cics.service.ServiceContext;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;

import java.lang.reflect.Method;
import java.util.Arrays;

import org.codehaus.xfire.MessageContext;
import org.codehaus.xfire.fault.XFireFault;
import org.codehaus.xfire.service.invoker.Invoker;

/**
 * This class handle invokation of web services and forward in to a legacy service invokation. 
 * @author raffaele
 *
 */

public class ServiceInvoker implements Invoker {
  
  /**
   * The logger for this class and its instances.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(ServiceInvoker.class);
  
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(ServiceInvoker.class);
    
  private ServiceDescriptor serviceDescriptor;

  public ServiceInvoker(ServiceDescriptor serviceDescriptor) {
    this.serviceDescriptor=serviceDescriptor;
  }

  public Object invoke(Method method, Object[] args, MessageContext messageContext) throws XFireFault {
    //we need to construct a ServiceContext and execute it
    LOG.debug("method: "+method+" with parameters: "+Arrays.toString(args)+" is being invoked on service: "+serviceDescriptor);
    ServiceContext serviceContext;
    serviceContext = new ServiceContext();
    
    serviceContext.setInputMappingDescriptor(serviceDescriptor.getInputMappingDescriptor());
    serviceContext.setOutputMappingDescriptor(serviceDescriptor.getOutputMappingDescriptor());
    
    serviceContext.setAccount(serviceDescriptor.getAccount());
    LOG.debug("Setting ServiceLocation: " + serviceDescriptor.getServiceLocation());
    serviceContext.setServiceLocation(serviceDescriptor.getServiceLocation());
    serviceContext.setInteractionDescription(serviceDescriptor.getInteractionDescription());
    
    serviceContext.setInputBean(args[0]);
    
    MessageFormatter inputMessageFormatter=MessageFormatterFactory.createMessageFormatter(serviceContext,true);
    try {
      // eseguo la formattazione in andata
      long millis1=System.currentTimeMillis();
      inputMessageFormatter.mapInputBeanToInputMessage(serviceContext);
      long millis2=System.currentTimeMillis();
      LOG.debug("input conversion time="+(millis2-millis1)+" millis");
      LOG.debug("input message: ["+serviceContext.getInputMessage()+"]");
    }
    catch (FormatException e){
      LOG.error("CIC002301_Formatting_input_bean_error", new Object[] {
          serviceContext.getInputBean(), 
          serviceContext.getInputMappingDescriptor(), 
          inputMessageFormatter}, e);
      throw new XFireFault(MESSAGES.getString(
          "CIC002301_Formatting_input_bean_error", 
          new Object[] {serviceContext.getInputBean(), 
          serviceContext.getInputMappingDescriptor(), 
          inputMessageFormatter
          }, e), e, XFireFault.RECEIVER);
    }
    
    ConnectionManager connectionManager=ConnectionManagerFactory.createConnectionManager(serviceContext);
    
    try {
      // eseguo la chiamata
      long millis1=System.currentTimeMillis();
      connectionManager.handleCall(serviceContext);
      long millis2=System.currentTimeMillis();
      LOG.debug("connection execution time="+(millis2-millis1)+" millis");
      LOG.debug("output message: ["+serviceContext.getOutputMessage()+"]");
    } catch (ConnectionException e) {
      LOG.error("CIC002302_Error_executing_call", new Object[] {
          serviceContext.getInputMessage(), 
          serviceContext.getInteractionDescription(), connectionManager}, e);
      throw new XFireFault(MESSAGES.getString("CIC002302_Error_executing_call", 
              new Object[] {serviceContext.getInputMessage(), 
              serviceContext.getInteractionDescription(), 
              connectionManager}), e, XFireFault.RECEIVER);
    }
    
    MessageFormatter outputMessageFormatter=MessageFormatterFactory.createMessageFormatter(serviceContext,false);
    try{
      // eseguo la formattazione all'indietro      
      long millis1=System.currentTimeMillis();
      outputMessageFormatter.mapOutputMessageToOutputBean(serviceContext);
      long millis2=System.currentTimeMillis();
      LOG.debug("output conversion time1="+(millis2-millis1)+" millis");
      LOG.debug("output bean : ["+serviceContext.getOutputBean()+"]");
    } catch (FormatException e) {
      LOG.error("CIC002303_IO_exception=CIC002303", new Object[] {
          serviceContext.getOutputMessage(), 
          serviceContext.getOutputMappingDescriptor(), 
          outputMessageFormatter}, e);
      throw new XFireFault(MESSAGES.getString(
              "CIC002303_IO_exception=CIC002303", new Object[] {
              serviceContext.getOutputMessage(), 
              serviceContext.getOutputMappingDescriptor(), 
              outputMessageFormatter}), e, XFireFault.RECEIVER);
    }
    
    return serviceContext.getOutputBean();
  }

}

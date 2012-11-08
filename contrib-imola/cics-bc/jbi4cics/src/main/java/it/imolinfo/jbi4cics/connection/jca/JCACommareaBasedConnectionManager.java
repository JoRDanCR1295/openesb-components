/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
/**
 * 
 */
package it.imolinfo.jbi4cics.connection.jca;

import it.imolinfo.jbi4cics.exception.ConnectionException;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.service.ServiceContext;

import javax.resource.cci.ConnectionSpec;
import javax.resource.cci.InteractionSpec;
import javax.resource.cci.Record;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;

/**
 * @author raffaele
 *
 */
public abstract class JCACommareaBasedConnectionManager extends JCAAbstractConnectionManager {

  /**
   * The logger for this class and its instances.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(JCACommareaBasedConnectionManager.class);

  /**
   * 
   */
  public JCACommareaBasedConnectionManager() {
    super();
    // TODO Auto-generated constructor stub
  }

  /* (non-Javadoc)
   * @see it.imolinfo.jbi4cics.connection.jca.JCAAbstractConnectionManager#createOutputMessage(it.imolinfo.jbi4cics.service.ServiceContext, javax.resource.cci.Record)
   */
  protected Object createOutputMessage(ServiceContext serviceContext, Record outputRecord) throws ConnectionException {
    if (!(outputRecord instanceof CommareaRecord)){
      //TODO loggare correttamente
      throw new ConnectionException("CIC000310_Expected_commarea_record", new Object[] {outputRecord.getClass()});
    }
    return ((CommareaRecord)outputRecord).getCommarea();
  }



  /* (non-Javadoc)
   * @see it.imolinfo.jbi4cics.connection.jca.JCAAbstractConnectionManager#createOutputRecord(it.imolinfo.jbi4cics.service.ServiceContext)
   */
  protected Record createOutputRecord(ServiceContext serviceContext) throws ConnectionException {
    CommareaRecord outputRecord=new CommareaRecord();
    //TODO ?? inputRecord.setRecordName();      
    //TODO ?? inputRecord.setRecordShortDescription();
    if (!(serviceContext.getOutputMappingDescriptor() instanceof CommareaBeanMappingDescriptor)){
      //TODO loggare correttamente
      throw new ConnectionException("CIC000311_Expected_commarea_bean_mapping_descriptor", new Object[] {serviceContext.getOutputMappingDescriptor().getClass()});
    }     
    CommareaBeanMappingDescriptor outputMappingDescriptor=(CommareaBeanMappingDescriptor)serviceContext.getOutputMappingDescriptor();
    try{
      outputRecord.setCommareaLength(outputMappingDescriptor.getBufferedLength());
    }
    catch (FormatException e){
      LOG.error("CIC000312_Error_setting_commarea_lengths", new Object[] {e.getMessage()}, e);
      throw new ConnectionException("CIC000312_Error_setting_commarea_lengths", new Object[] {e.getMessage()}, e);
    }
    return outputRecord;
  }

  /* (non-Javadoc)
   * @see it.imolinfo.jbi4cics.connection.jca.JCAAbstractConnectionManager#createInputRecord(it.imolinfo.jbi4cics.service.ServiceContext)
   */
  protected Record createInputRecord(ServiceContext serviceContext) throws ConnectionException {
    CommareaRecord inputRecord=new CommareaRecord();
    //TODO ?? inputRecord.setRecordName();      
    //TODO ?? inputRecord.setRecordShortDescription();
    Object inputMessage=serviceContext.getInputMessage();
    if (!(inputMessage instanceof byte[])){
      //TODO loggare correttamente
      throw new ConnectionException("CIC000313_Expected_byte[]_input_message", new Object[] {inputMessage.getClass()});
    }
    inputRecord.setCommarea((byte[])inputMessage);
    return inputRecord;
  }
  
  /* (non-Javadoc)
   * @see it.imolinfo.jbi4cics.connection.jca.JCAAbstractConnectionManager#createInteractionSpec(it.imolinfo.jbi4cics.service.ServiceContext)
   */
  protected abstract InteractionSpec createInteractionSpec(ServiceContext serviceContext) throws ConnectionException;

  /* (non-Javadoc)
   * @see it.imolinfo.jbi4cics.connection.jca.JCAAbstractConnectionManager#createConnectionSpec(it.imolinfo.jbi4cics.service.ServiceContext)
   */
  protected abstract ConnectionSpec createConnectionSpec(ServiceContext serviceContext) throws ConnectionException;

}

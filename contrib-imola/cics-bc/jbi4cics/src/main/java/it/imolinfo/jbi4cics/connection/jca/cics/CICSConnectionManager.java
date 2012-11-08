/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/

package it.imolinfo.jbi4cics.connection.jca.cics;

import it.imolinfo.jbi4cics.connection.jca.JCACommareaBasedConnectionManager;
import it.imolinfo.jbi4cics.exception.ConnectionException;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.service.ServiceContext;


import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.resource.cci.ConnectionSpec;
import javax.resource.cci.InteractionSpec;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.jbi.Messages;


/**
 * @author raffaele, marcopiraccini
 *
 */
public class CICSConnectionManager extends JCACommareaBasedConnectionManager {  

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(CICSConnectionManager.class);

    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES
            = Messages.getMessages(CICSConnectionManager.class);    
    
    /**
     * 
     */ 
    public CICSConnectionManager() {
        super();
    }

    /**
     * Inits and retruns a inited ConnectionSpec.
     * @param serviceContext    The service context
     * @throws ConnectionException    The connection exception
     * @return ConnectionSpec    The connection spec
     */
    protected ConnectionSpec createConnectionSpec(ServiceContext serviceContext) 
    throws ConnectionException {

        ConnectionSpec eciConnectionSpec = null;

        try {
            eciConnectionSpec = (ConnectionSpec)
            this.getConnectorClassLoader().loadClass("com.ibm.connector2.cics.ECIConnectionSpec").newInstance();
        } catch (InstantiationException e) {
            LOG.error("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);  				
        } catch (IllegalAccessException e) {
            LOG.error("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
        } catch (ClassNotFoundException e) {
            LOG.error("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
        }	

        // ECIConnectionSpec eciConnectionSpec = new ECIConnectionSpec();

        /*
         * property to set:
         * username
         * password
         * see http://publib.boulder.ibm.com/infocenter/cicstg60/index.jsp 
         */
        if (eciConnectionSpec != null) {
            // ((ECIConnectionSpec)eciConnectionSpec).setUserName(serviceContext.getAccount().getUsername());
            // ((ECIConnectionSpec)eciConnectionSpec).setPassword(serviceContext.getAccount().getPassword());		
            invokeMethodWithOneStringParam(eciConnectionSpec, "setUserName", serviceContext.getAccount().getUsername());
            invokeMethodWithOneStringParam(eciConnectionSpec, "setPassword", serviceContext.getAccount().getPassword());

        } else {  	  
            LOG.error("CIC000404_Connection_spec_not_found");
            throw new ConnectionException(MESSAGES.getString("CIC000404_Connection_spec_not_found"));  				
        }
        return eciConnectionSpec;

    }  

    /**
     * Returns a correctly configured <code>InteractionSpce</code>.
     * @param serviceContext    The service context
     * @throws ConnectionException    The connection exception
     * @return InteractionSpec    The interaction spec
     */
    protected InteractionSpec createInteractionSpec(ServiceContext serviceContext) throws ConnectionException {

        if (!(serviceContext.getInteractionDescription() instanceof CICSInteractionDescription)) {
            LOG.error("CIC000405_Cics_interaction_description_not_found", serviceContext.getInteractionDescription().getClass());
            throw new ConnectionException(MESSAGES.getString("CIC000405_Cics_interaction_description_not_found", new Object[] {serviceContext.getInteractionDescription().getClass()}));      
        }
        CICSInteractionDescription cicsInteractionDescription=(CICSInteractionDescription)serviceContext.getInteractionDescription();

        InteractionSpec eciInteractionSpec = null;

        try {
            eciInteractionSpec = (InteractionSpec)
            this.getConnectorClassLoader().loadClass("com.ibm.connector2.cics.ECIInteractionSpec").newInstance();
        } catch (InstantiationException e) {
            LOG.error("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);                 
        } catch (IllegalAccessException e) {
            LOG.error("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
        } catch (ClassNotFoundException e) {
            LOG.error("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000400_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
        }   

        /*
         * The properties to set are:
         * timeout
         * function name / program name
         * tpn name
         * tran name
         * see http://publib.boulder.ibm.com/infocenter/cicstg60/index.jsp
         */

        // eciInteractionSpec.setExecuteTimeout(cicsInteractionDescription.getTimeout());
        invokeMethodWithOneIntParam(eciInteractionSpec, "setExecuteTimeout", cicsInteractionDescription.getTimeout());

        // eciInteractionSpec.setFunctionName(cicsInteractionDescription.getProgramName());
        invokeMethodWithOneStringParam(eciInteractionSpec, "setFunctionName", cicsInteractionDescription.getProgramName());

        if (cicsInteractionDescription.isTpn()){
            // eciInteractionSpec.setTPNName(cicsInteractionDescription.getTransactionName());
            invokeMethodWithOneStringParam(eciInteractionSpec, "setTPNName", cicsInteractionDescription.getTransactionName());
        } else {
            // eciInteractionSpec.setTranName(cicsInteractionDescription.getTransactionName());
            invokeMethodWithOneStringParam(eciInteractionSpec, "setTranName", cicsInteractionDescription.getTransactionName());        
        }

        // le interazioni jbi4cics sono sono tutte sync send receive
        // eciInteractionSpec.setInteractionVerb(InteractionSpec.SYNC_SEND_RECEIVE);
        invokeMethodWithOneIntParam(eciInteractionSpec, "setInteractionVerb", InteractionSpec.SYNC_SEND_RECEIVE);      

        // setting della lunghezza delle commarea scambiate"Expected CommareaBeanMappingDescriptor, found: "+serviceContext.getInputMappingDescriptor().getClass()
        if (!(serviceContext.getInputMappingDescriptor() instanceof CommareaBeanMappingDescriptor)){
            LOG.error("CIC000401_Expected_commarea_bean_mapping_descriptor", new Object[] {serviceContext.getInputMappingDescriptor().getClass()});
            throw new ConnectionException("CIC000401_Expected_commarea_bean_mapping_descriptor", new Object[] {serviceContext.getInputMappingDescriptor().getClass()});
        }
        if (!(serviceContext.getOutputMappingDescriptor() instanceof CommareaBeanMappingDescriptor)){
            LOG.error("CIC000401_Expected_commarea_bean_mapping_descriptor", new Object[] {serviceContext.getOutputMappingDescriptor().getClass()});    	
            throw new ConnectionException("CIC000401_Expected_commarea_bean_mapping_descriptor", new Object[] {serviceContext.getOutputMappingDescriptor().getClass()});
        }   
        CommareaBeanMappingDescriptor inputMappingDescriptor=(CommareaBeanMappingDescriptor)serviceContext.getInputMappingDescriptor();
        CommareaBeanMappingDescriptor outputMappingDescriptor=(CommareaBeanMappingDescriptor)serviceContext.getOutputMappingDescriptor();
        try {
            //il CTG alloca solo una area di scambio quindi devo capire quale è la più grande e usare solo quella
            //in realtà se si usa sync_send_receive o sync_send vale la CommareaLength, altrimenti con sync_receive vale la ReplyLength, 
            //quindi commento il setting del reply length visto che siamo smepre in sync_send_receive 
            if (inputMappingDescriptor.getBufferedLength()>=outputMappingDescriptor.getBufferedLength()){

                // eciInteractionSpec.setCommareaLength(inputMappingDescriptor.getBufferedLength());
                //eciInteractionSpec.setReplyLength(inputMappingDescriptor.getBufferedLength());
                invokeMethodWithOneIntParam(eciInteractionSpec, "setCommareaLength", inputMappingDescriptor.getBufferedLength());

            }
            else {
                // eciInteractionSpec.setCommareaLength(outputMappingDescriptor.getBufferedLength());
                //eciInteractionSpec.setReplyLength(outputMappingDescriptor.getBufferedLength());    
                invokeMethodWithOneIntParam(eciInteractionSpec, "setCommareaLength", outputMappingDescriptor.getBufferedLength());
            }
        }
        catch (FormatException e){
            LOG.error("CIC000402_Error_setting_commarea_lengths", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000402_Error_setting_commarea_lengths", new Object[] {e.getMessage()}, e);
        }
        return eciInteractionSpec;
    }

    /**
     * Use reflection to invoke a void method with one <code>String</code> parameter.
     * @param obj    The object
     * @param methodName    The method name
     * @param methodParam    The method param
     * @throws ConnectionException    The connection exception
     */
    protected void invokeMethodWithOneStringParam(Object obj, String methodName, String methodParam) 
    throws ConnectionException {	  	

        Class[] parameterTypes = new Class[] {String.class};
        Method method;
        Object[] arguments = new Object[] {methodParam};
        Class objClass = obj.getClass();
        try {
            method = objClass.getMethod(methodName, parameterTypes);
            method.invoke(obj, arguments);
        } catch (NoSuchMethodException e) {
            LOG.error("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e);       
            throw new ConnectionException("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e);    
        } catch (IllegalAccessException e) {
            LOG.error("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e);       
            throw new ConnectionException("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e);    
        } catch (InvocationTargetException e) {                       
            // On a InvocationTarget Exception, we should extract the internal Exception
            LOG.error("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e.getCause());       
            throw new ConnectionException("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e.getCause());    
        }
    }  

    /**
     * Use reflection to invoke a void method with one <code>int</code> parameter.
     * @param obj    The object
     * @param methodName    The method name
     * @param methodParam    The method param
     * @throws ConnectionException    The connection exception
     **/
    protected void invokeMethodWithOneIntParam(Object obj, String methodName, int methodParam) 
    throws ConnectionException {	  	 

        Class[] parameterTypes = new Class[] {int.class};
        Method method;
        Object[] arguments = new Object[] {methodParam};
        Class objClass = obj.getClass();
        try {
            method = objClass.getMethod(methodName, parameterTypes);
            method.invoke(obj, arguments);
        } catch (NoSuchMethodException e) {
            LOG.error("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e);       
            throw new ConnectionException("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e);    
        } catch (IllegalAccessException e) {
            LOG.error("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e);       
            throw new ConnectionException("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e);    
        } catch (InvocationTargetException e) {
            // On a InvocationTarget Exception, we should extract the internal Exception
            LOG.error("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e.getCause());       
            throw new ConnectionException("CIC000403_Method_invocation_error", new Object[] {methodName, objClass, methodParam}, e.getCause());    
        }
    }    

}

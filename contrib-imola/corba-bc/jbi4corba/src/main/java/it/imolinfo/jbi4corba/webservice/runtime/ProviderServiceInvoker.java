 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.runtime;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.webservice.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.MethodSignature;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import net.java.hulp.measure.Probe;

import org.apache.cxf.frontend.MethodDispatcher;
import org.apache.cxf.helpers.CastUtils;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.FaultMode;
import org.apache.cxf.message.MessageContentsList;
import org.apache.cxf.service.Service;
import org.apache.cxf.service.invoker.AbstractInvoker;
import org.apache.cxf.service.model.BindingOperationInfo;
import org.apache.cxf.service.model.MessageInfo;
import org.apache.cxf.service.model.MessagePartInfo;
import org.omg.CORBA.SystemException;

/**
 * This class handle invokation of web services and forward in to a legacy
 * service invokation.
 */
public final class ProviderServiceInvoker extends AbstractInvoker {

    /**
     * Logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ProviderServiceInvoker.class);
    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES = Messages.getMessages(ProviderServiceInvoker.class);
    
    /**
     * The object proxed by the invoker
     */
    Object obj = null;
    
    private Probe mMeasurement;
    /**
     * The service descriptor.
     */
    private ProviderServiceDescriptor serviceDescriptor;
    /**
     * The Servant is a Corba Object.
     */
    boolean isCorbaServant = true;
    

    /**
     * Constructor.
     *
     * @param serviceDescriptor  The service descriptor
     */
    public ProviderServiceInvoker(
            final ProviderServiceDescriptor serviceDescriptor) {
        this.serviceDescriptor = serviceDescriptor;
        if (serviceDescriptor != null) {
            obj = serviceDescriptor.getCorbaObjectReference();
        }
    }

    /**
     * Constructor.
     *
     * @param serviceDescriptor  The service descriptor
     */
    public ProviderServiceInvoker(
            final ProviderServiceDescriptor serviceDescriptor, boolean isCorbaServant) {
        this.serviceDescriptor = serviceDescriptor;
        if (serviceDescriptor != null) {
            obj = serviceDescriptor.getCorbaObjectReference();
        }
        this.isCorbaServant = isCorbaServant;
    }

    /**
     * Creates and returns a service object depending on the scope.
     */
    public Object getServiceObject(final Exchange context) {
        LOG.debug("Returning object for exchange: " + context.getInMessage());
        return obj;
    }

    /**
     * Set the service object for testing pourpose
     */
    public void setServiceObject(Object obj) {
        this.obj = obj;
    }

    /**
     * Sets ths classloader to find the corba classes and invokes the superclass.
     */
    @Override
    public Object invoke(Exchange exchange, Object o) {
        
        Object dynamicObject=null;
        //Is used to use the correct Object for invokatio
        //It's the dynamic is dynamic is != null else is the obj. 
        Object currentObj=null;
        
        //If this request is dynamic the object used for the invocation is generated from the IOR
        
        if (exchange.get("EPR_IOR") != null) {
           try {
        	 
        	String ior=exchange.get("EPR_IOR").toString();
        	dynamicObject = serviceDescriptor.getEndpoint().getCorbaObjectReference(ior);
		  
           } catch (Jbi4CorbaException e) {
   			String msg = MESSAGES.getString("CRB000766_Unable_to_get_corba_object_from_IOR");
			LOG.error(msg, e.getMessage());
               exchange.getOutMessage().put(FaultMode.class, FaultMode.UNCHECKED_APPLICATION_FAULT);
               exchange.getOutFaultMessage().setContent(Exception.class, new Fault(e));                
               // e.printStackTrace();
               throw new Fault(e);
           }
            
        }

        LOG.debug("Invoking exchange: " + exchange + " with object: " + o.getClass().getName() + "[" + o + "] using CXF");

        // Stop chrono for measure of Denormalization      
        mMeasurement = (Probe) exchange.get("Measure-deN");
        mMeasurement.end();

        ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(
                serviceDescriptor.getOriginalClassLoader());
              
        // information needed for service to corba and corba to service object transformation
        RuntimeInformation runtimeInfo = new RuntimeInformation(serviceDescriptor.getUrlClassLoader(),
                serviceDescriptor.getOriginalClassLoader(), serviceDescriptor.getAllCorbaTypes(), serviceDescriptor.getEndpoint(),
                serviceDescriptor.getEndpoint().getOrb(), serviceDescriptor.getAllIDLTypes(), serviceDescriptor.getCorbaEnumMap(),
                serviceDescriptor.getIdToClassNameMap(), serviceDescriptor.getTypeDefs());
        

        try {
            // TODO: add i18 messages on exceptions
            BindingOperationInfo bop = exchange.get(BindingOperationInfo.class);
            MethodDispatcher md = (MethodDispatcher) exchange.get(Service.class).get(MethodDispatcher.class.getName());
            Method m = md.getMethod(bop);

            bop.getInput().getMessageInfo().getMessageParts();

            // Extract the Params. With CXF 2.1.x, the Object param is changed and contains
            // a wrapper of the 
            List<Object> params = extractParams(o, bop.getInput().getMessageInfo());

            Object[] paramArray = new Object[]{};
            if (params != null) {
                paramArray = params.toArray();
            }
            
            
            if (LOG.isDebugEnabled()) {
        	 
                LOG.debug("Parameters:");
                for (int i = 0; i < paramArray.length; i++) {
                    if (paramArray[i] != null) {
                        LOG.debug("Parameter #" + i + " of class:" + paramArray[i].getClass().getName());

                    } else {
                        LOG.debug("Parameter #" + i + " is null");
                    }

                }
            }
            try {

                LOG.debug(">>>> OBJ: " + obj);
                if (obj == null) {
                    if(serviceDescriptor.getLocalizationType()!=null){
                        if (serviceDescriptor.getCorbaObjectReference() == null) {
                            LOG.debug(">>>> Reconnect begin...");
                            serviceDescriptor.getEndpoint().locateCorbaService();
                            LOG.debug(">>>> Reconnect end...");
                        }
                        
                    }                        
                    //get corbaObjectReference
                    obj = serviceDescriptor.getCorbaObjectReference();
                   
                    
                }
                //Change the Invokation Object if there is a valid dynamic Object
                if(dynamicObject!=null){
                    currentObj=dynamicObject;    
                }else{
                    currentObj=obj;
                }
                
                //**********************************************************************
                // INOUT Gets the Object array with the correct Holder and gets the method from the stub 
                MethodSignature methodSignature = getMetodSignatureFromMethod(m);
                
                //**** Added for management of Array of length 0****************
                //**** Workaroud added to manage array of length 0
                for(int i=0;i<paramArray.length;i++){
                    if(methodSignature.getParameters().get(i).isArray()){
                        if(paramArray[i]==null){
                            LOG.debug("********************************************Instance new Array of size 0");
                            paramArray[i]=Array.newInstance(methodSignature.getParameters().get(i).getType(), 0);

                        }
                    }
                }
                //**************************************************************
                
                if (methodSignature.isContainsHolder()) {
                    // manage holders.             
                    paramArray = CorbaTransformationUtils.changeHoldersFromServiceToCorbaObjects(methodSignature, params, runtimeInfo);
                    m = getMethodFromStub(currentObj.getClass(), methodSignature);
                    LOG.debug("methodSignature:" + methodSignature.getMethod().toGenericString());
                    LOG.debug("M:" + m.toGenericString());

                }
                //**********************************************************************  
                //Runtime Generic Types
                //**********************************************************************
                // Get parameters about Corba Types Interface,Any or Union
                //if (methodSignature.hasCorbaTypes()) {
                    // TODO not completed or tested 
                    LOG.debug("***Retrieved  Union or Any or Interfaces ***");
                    LOG.debug("Param Array LENGTH " + paramArray.length);
                    
                  //  LOG.debug("Param  class" + paramArray[0].getClass());

                    paramArray = CorbaTransformationUtils.changeFromServiceToCorbaObjects(paramArray, runtimeInfo, methodSignature);
                    LOG.debug("Param  " + methodSignature);
                    LOG.debug("Param  " + currentObj.getClass());

                    m = getMethodFromStub(currentObj.getClass(), methodSignature);
                //}
                LOG.debug("Param  " + methodSignature);
                //**********************************************************************  

                try {
                    if (this.isCorbaServant && ((org.omg.CORBA.Object) currentObj)._non_existent()) {
                        LOG.debug(">>>> Reconnect begin...");
                        serviceDescriptor.getEndpoint().locateCorbaService();
                        LOG.debug(">>>> Reconnect end...");
                        //get corbaObjectReference
                        currentObj = serviceDescriptor.getCorbaObjectReference();
                    }
                } catch (SystemException se) {
                    LOG.debug(">>>> Reconnect begin...");
                    serviceDescriptor.getEndpoint().locateCorbaService();
                    LOG.debug(">>>> Reconnect end...");
                    //get corbaObjectReference
                    currentObj = serviceDescriptor.getCorbaObjectReference();
                }

                // Invocation
                // Invoke the stub method and pass the correct params
                //ADDED FOR Array OCTET TEST
                
                //LOG.debug("Param  " + paramArray[0]);
                //LOG.debug("Param  " + paramArray[0].getClass().getName());
                //byte[] par = ((byte[]) paramArray[0]);
                //LOG.info("PARAM ARRAY:" + par);
                //for (int i = 0; i < par.length; i++) {
                //LOG.info("PARAM ARRAY:" + i + ":" + par[i]);    
                //}
                
                
               // LOG.debug("Param  class" + paramArray[0].getClass());
                if(LOG.isDebugEnabled()){
                LOG.debug("ParamArray  ===================> " + Arrays.toString(paramArray) + " \n **********************************************************");
                LOG.debug("Exchange ======================>  " + exchange + "\n ************************************************************");
                LOG.debug("OBJ ===========================>  " + currentObj + "\n******************************************************************");
                LOG.debug("OBJ CLASSLOADER ===============>  " + currentObj.getClass().getClassLoader() + "\n******************************************************************");
          //      LOG.debug("PARAM CLASSLOADER ===============>  " + paramArray[0].getClass().getClassLoader() + "\n******************************************************************");
                
                LOG.debug("METHOD SIGNATURE ==============>  " + m + "\n********************************************************************");

                LOG.debug("**********************INVOCATION*****************************************************************************");
                LOG.debug("ParamArray  ====>" + paramArray.length);
               
//                LOG.debug("Param       ====>"+paramArray[0].getClass().getField("fieldEcho2").toString());
//                LOG.debug("Field Number " + paramArray[0].getClass().getFields().length);
//                for (Field f : paramArray[0].getClass().getFields()) {
//                    LOG.debug("field ===>" + f.getName() + " -- value: " + f.get(paramArray[0]));
//                    LOG.debug("field ===>" + f.toGenericString());
//                    LOG.debug("field ===>" + f.getType());
//                    LOG.debug("field ===>" + f.toString());
//                    LOG.debug("field ===>" + f.getClass().getClassLoader());
//                }
               
                LOG.debug("*************************************************************************************************************");
                }
                org.omg.CORBA_2_3.ORB orb=(org.omg.CORBA_2_3.ORB)serviceDescriptor.getEndpoint().getOrb();
                //LOG.debug("RAAAF: lookup value factory per MySequence: "+orb.lookup_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/MySequence:1.0"));
                //LOG.debug("CL stub: "+currentObj.getClass().getClassLoader());
                //LOG.debug("RAAAF: classloader value factory per MySequence: "+orb.lookup_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/MySequence:1.0").getClass().getClassLoader());
                Object res = performInvocation(exchange, currentObj, m, paramArray);
                
           
                LOG.debug("!!!! invokation successful with return:" + res);
                
                   //**********************************************************************
                // INOUT Change holder parameters
                if (methodSignature.isContainsHolder()) {
                    LOG.debug("The method contains holders: the parameters must be changed");
                    CorbaTransformationUtils.changeHoldersFromCorbaToServiceObjects(methodSignature, params, paramArray, runtimeInfo);
                }

                if (res != null) {
                    LOG.debug("TYPE"+m.getReturnType().getName());
                    res = CorbaTransformationUtils.changeFromCorbaToServiceObject(res, runtimeInfo, m.getReturnType());
                    LOG.debug("RESULT CLASS"+res.getClass().toString());
                    LOG.debug("RESULT CLASSLOADER"+res.getClass().getClassLoader());
                }
                LOG.debug("!!!! transformation successful");

                //Normalization timer starts
                String topic = new String("Normalization");
                String endpointName = (String) exchange.get("EndpointName");
                mMeasurement = Probe.fine(getClass(), endpointName, topic);
                //Add timer object to context, so it can be stopped in the other class
                exchange.put("Measure-N", mMeasurement);

                if (exchange.isOneWay()) {
                    return null;
                }

                return new MessageContentsList(res);
            } catch (InvocationTargetException e) {
                Throwable t = e.getCause();

                if (t == null) {
                    t = e;
                }


                LOG.debug(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    FAULT ");
                for (Class classException : m.getExceptionTypes()) {
                    LOG.debug("Exception Application : " + classException.getName());
                    if (classException.isInstance(t)) {
                        // Checked
                        LOG.debug(">>>>>>>>>> CHECKED_APPLICATION_FAULT");
                        try
                        {
                        t = (Throwable)CorbaTransformationUtils.changeFromCorbaToServiceObject(t, runtimeInfo, classException);
                        } catch (Exception ex) {              
                        exchange.getOutMessage().put(FaultMode.class, FaultMode.UNCHECKED_APPLICATION_FAULT);
                        exchange.getOutFaultMessage().setContent(Exception.class, new Fault(ex));
                        ex.printStackTrace();
                        throw new Fault(ex);
                        } 
                     
                        exchange.getOutMessage().put(FaultMode.class, FaultMode.CHECKED_APPLICATION_FAULT);
                        exchange.getOutFaultMessage().setContent(Exception.class, new Fault(t));
                        throw new Fault(t);
                    }
                }
                // Unchecked 
                LOG.debug(">>>>>>>>>> UNCHECKED_APPLICATION_FAULT");
                exchange.getOutMessage().put(FaultMode.class, FaultMode.UNCHECKED_APPLICATION_FAULT);
                exchange.getOutFaultMessage().setContent(Exception.class, new Fault(t));
                LOG.warn("error invokating stub: ", t);
                throw new Fault(t);
            // VERIFICARE SE I SEGUENTI CATCH SONO NECESSARI                                 
            } catch (Fault f) {
                exchange.getOutMessage().put(FaultMode.class, FaultMode.CHECKED_APPLICATION_FAULT);
                exchange.getOutFaultMessage().setContent(Exception.class, f);
                throw f;
            } catch (Exception e) {
                LOG.warn("error invokating stub: ", e);
                exchange.getOutMessage().put(FaultMode.class, FaultMode.UNCHECKED_APPLICATION_FAULT);
                exchange.getOutFaultMessage().setContent(Exception.class, new Fault(e));                
                // e.printStackTrace();
                throw new Fault(e);
            }

        } finally {
            Thread.currentThread().setContextClassLoader(oldClassLoader);
        }
    }
    

    /**
     * Creates the parameter arry from the parameter object (that contains a wrapper object for each part).
     * @param o
     * @param info
     * @return
     */
    private List<Object> extractParams(Object o, MessageInfo info) {

        List<MessagePartInfo> messageParts = info.getMessageParts();
        for (int i = 0; i < messageParts.size(); i++) {
            MessagePartInfo partInfo = messageParts.get(i);
        }
        List<Object> params = null;
        if (o instanceof List) {
            params = CastUtils.cast((List<?>) o);
        } else if (o != null) {
            params = new MessageContentsList(o);
        }
        return params;
    }

    /**
     * Get the provider service Descriptor object.
     * @return The provider service descriptor
     */
    public ProviderServiceDescriptor getServiceDescriptor() {
        return serviceDescriptor;
    }

    /**
     * Set the provider service Descriptor object.
     * @param serviceDescriptor The ProviderServiceDescriptor
     */
    public void setServiceDescriptor(ProviderServiceDescriptor serviceDescriptor) {
        this.serviceDescriptor = serviceDescriptor;
    }

    /**
     * Gets the <code>MethodSignature</code> object from the <code>Method</code> class.
     * The correspondace between the MethodSignature and the method can be saved for further optimization.
     * @param method
     * @return 
     */
    private MethodSignature getMetodSignatureFromMethod(Method method) {
        List<MethodSignature> methodSignatures = serviceDescriptor.getMethodSignatures();
        MethodSignature methodSignature = null;
        boolean found = false;
        for (int i = 0; i < methodSignatures.size() && (!found); i++) {
            methodSignature = (MethodSignature) methodSignatures.get(i);
            LOG.debug("METHOD SIGNATURE ===>"+methodSignature);
           
           // if(methodSignature.getChangedMethod()!=null){
                if (compareMethodsWithNameAndParameters(method, methodSignature.getChangedMethod())) {
                    found = true;
                    break;
                }      
           //}
        }
        if (methodSignature == null) {
			String msg = MESSAGES.getString(
					"CRB000767_No_method_signature_found_for_method",
					new java.lang.Object[] { method.toGenericString() });
			LOG.warn(msg);
		}
        return methodSignature;
    }

    

    /**
     * The method saved in the <code>MethodSignature</code> is of the corba interface. The object
     * used to invoke the method is the Stub reference. This method found the Stub method, comparing
     * the method name and the parameter types.
     * TODO: This operation is expensive, the correspondance between methods can be saved.
     * @param stub
     * @param signature
     * @return
     */
    public Method getMethodFromStub(Class stub, MethodSignature signature) {
        Method interfaceMethod = signature.getMethod();

        Method matchedMethod = null;
        // Get the stub methods
        Method[] methods = stub.getMethods();
        LOG.debug("********INTERFACE Method   ->" + signature.getMethod());
        LOG.debug("********INTERFACE STUB  ->" + stub);
        LOG.debug("********ClassLoader STUB  ->" + stub.getClassLoader());
        LOG.debug("********METHODS LENGTH  ->" + methods.length);

        for (int i = 0; i < methods.length; i++) {
            LOG.debug("********INTERFACE METHOD  ->" + methods[i].getName());
            boolean found = compareMethodsWithNameAndParameters(methods[i], interfaceMethod);
            if (found) {
                matchedMethod = methods[i];
                break;
            }
        }
        LOG.debug("********MATHCED METHOD  _->" + matchedMethod);
        return matchedMethod;

    }

    /**
     * Compares two method looking for the name and the parameters (not the original class).
     * @param methodA
     * @param methodB
     * @return
     */
    private boolean compareMethodsWithNameAndParameters(Method methodA, Method methodB) {
        boolean ret = false;
        LOG.debug("**************Method A" + methodA.toGenericString());
        LOG.debug("**************Method B" + methodB.toGenericString());
        boolean paramEquals = compareParameterTypes(methodA.getParameterTypes(), methodB.getParameterTypes());
        LOG.debug("**************Method A" + methodA.toGenericString());
        LOG.debug("**************Method B" + methodB.toGenericString());
        LOG.debug("**************PARAM EQUALS" + paramEquals);
        if ((paramEquals) &&
                (methodA.getName().equals(methodB.getName()))) {
            ret = true;
        }
        return ret;
    }

    /**
     * True if two <code>Class</code> array are of <code>Class</code> of the same type. 
     * @param typesA
     * @param typesB
     * @return
     */
    private boolean compareParameterTypes(Class[] typesA, Class[] typesB) {
        boolean ret = true;
        LOG.debug("***************TYpe A" + typesA.length);
        LOG.debug("***************Type B" + typesB.length);
        if (typesA.length != typesB.length) {
            ret = false;
        } else {
            for (int i = 0; i < typesA.length; i++) {
                if (!(typesA[i].getName().equals(typesB[i].getName()))) {
                    LOG.debug("***************Type A" + typesA[i].getName());
                    LOG.debug("***************Type B" + typesB[i].getName());
                    ret = false;
                    break;
                }
            }
        }
        return ret;
    }
    
   
    
}

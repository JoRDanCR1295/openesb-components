package net.openesb.jbi.restbc.jbiadapter.wsdl;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import net.openesb.jbi.restbc.jbiadapter.RestComponent;
import net.openesb.jbi.restbc.jbiadapter.ServiceUnit;
import net.openesb.jbi.restbc.jbiadapter.descriptor.EndpointIdentifier;
import net.openesb.jbi.restbc.jbiadapter.descriptor.RestSUDescriptor;

import org.w3c.dom.Document;
import org.xml.sax.EntityResolver;

import com.sun.wsdl4j.ext.WSDL4JExt;

/**
 * WSDLServiceUnitConfig.java
 *
 * @author Edward Chou
 */
public class WSDLServiceUnitConfig  {

    private final static Logger logger = Logger.getLogger(WSDLServiceUnitConfig.class.getName());
    
    private ServiceUnit serviceUnit;
    private RestComponent component;
    private ComponentContext context;
    private RestSUDescriptor suDescriptor;
    private File wsdlFile;
    private EntityResolver resolver = null;
    
    private List<WSDLEndpoint> wsdlEndpoints = new ArrayList<WSDLEndpoint> ();
    
    
    public WSDLServiceUnitConfig(ServiceUnit serviceUnit,
            RestComponent component, 
            ComponentContext context, 
            RestSUDescriptor suDescriptor, 
            File wsdlFile, 
            EntityResolver resolver) throws Exception {
        this.serviceUnit = serviceUnit;
        this.component = component;
        this.context = context;
        this.suDescriptor = suDescriptor;
        this.wsdlFile = wsdlFile;
        this.resolver = resolver;
        
        parseWSDL();
    }
    
    private void parseWSDL() throws Exception {
        WSDLReader reader = WSDL4JExt.newWSDLReader(resolver);
            
        // Suppress WSDL4j System.out logs and control the logging based on the current
        // logger logging level setting
        if (logger.isLoggable(Level.FINE)) {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, true);
        } else {
            reader.setFeature(com.ibm.wsdl.Constants.FEATURE_VERBOSE, false);
        }
        reader.setExtensionRegistry(new RestExtensionRegistry());
        Definition def = reader.readWSDL(wsdlFile.getAbsolutePath());
        
        for (EndpointIdentifier ei : suDescriptor.getServices()) {
            RestBinding binding = getRestBinding(def, ei.getServiceName().toString(), ei.getEndpointName());
            if (binding == null) {
                continue;
            }

            // look through all the definitions to see if we can find a
            // service binding that matches this endpoint.  If we find one,
            // extract file details

            // If we have a RestBinding, we must have an RestAddress.
            RestAddress address =
                getRestAddress(def, ei.getServiceName().toString(), ei.getEndpointName());


            // Create an WSDLEndpoint for each Port.  The endpoint should have the
            // correct WSDLBinding, the associated Operations, and the
            // associated OperationInput and OperationOutput
            
            WSDLEndpoint endpoint = new WSDLEndpoint(serviceUnit, context, component, ei.getServiceName(), ei.getEndpointName(), ei);

            // Store the Definition
            endpoint.setDefinition(def);

            endpoint.setIsOutbound(ei.isProvider());

            // Set the description
            DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder documentBuilder = docBuilderFactory.newDocumentBuilder();
            Document result = documentBuilder.parse(wsdlFile);
            endpoint.setServiceDescription(result);

            // Store our extensibility elements
            endpoint.setRestAddress(address);
            endpoint.setRestBinding(binding);

            // Traverse operations and their inputs and outputs and store them
            setRestOperationsAndInputsOutputs(def,
                    ei.getServiceName().toString(),
                    ei.getEndpointName(),
                    endpoint);

            // Now add the Endpoint to our list of Endpoints
            wsdlEndpoints.add(endpoint);
            
        }
    }

    private Binding getBinding(Definition def, String serviceName, String endpointName) {
        Service svc = getService(def,serviceName);
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port == null) {
            return null;
        } else {
            return port.getBinding();
        }
    }
    

    private Service getService(Definition def, String serviceName) {
        Map services = def.getServices();
        Service svc = (Service) services.get(QName.valueOf(serviceName));
        return svc;
    }

    private RestAddress getRestAddress(Definition def, String serviceName, String endpointName) {
        RestAddress address = null;
        Service svc = getService(def, serviceName);
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port != null) {
            List extElems = port.getExtensibilityElements();

            //Look for rest:address                
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && address == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (RestAddress.class.isInstance(ee)) {
                    address = (RestAddress) ee;
                }
            }
        }
        return address;
    }

    private RestBinding getRestBinding(Definition def, String serviceName, String endpointName) {
        RestBinding restBinding = null;
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();

            //Look for rest:binding
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext() && restBinding == null) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (RestBinding.class.isInstance(ee)) {
                    restBinding = (RestBinding) ee;
                }
            }
        }
        return restBinding;
    }
    
    private void setRestOperationsAndInputsOutputs(Definition def,
            String serviceName,
            String endpointName,
            WSDLEndpoint endpoint) throws Exception {
        Map<QName, RestOperation> restOperations = new HashMap<QName, RestOperation> ();
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List bindingOperations = binding.getBindingOperations();
            Iterator operIter = bindingOperations == null ? null : bindingOperations.iterator();
            while (operIter != null && operIter.hasNext()) {
                BindingOperation oper = (BindingOperation) operIter.next();
                List extElems = oper.getExtensibilityElements();
                // Look for rest:operation entries

                Iterator extIter = extElems == null ? null : extElems.iterator();
                while (extIter != null && extIter.hasNext()) {
                    ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                    if (RestOperation.class.isInstance(ee)) {
                        // We got a rest operation binding

                        //
                        // Collect the binding operation info
                        //
                        RestOperation restOp = (RestOperation) ee;
                        restOp.setBindingOperation(oper);
                        
                        Operation operation = oper.getOperation();
                        OperationType type = null;
                        if (operation.getInput() != null && operation.getOutput() != null) {
                            type = OperationType.REQUEST_RESPONSE;
                            
                        } else {
                            type = OperationType.ONE_WAY;
                        }
                        
                        restOp.setOperation(operation);
                        restOp.setMep(type);
                        restOperations.put(new QName(binding.getQName().getNamespaceURI(), oper.getName()), restOp);
                        
                    }
                }
            }
        }

        // Check for Rest Operations
        if (restOperations.size() == 0) {
            throw new Exception ("missing rest:operation for " + endpointName + ", " + serviceName);
        }
        
        endpoint.setRestOperations(restOperations);
    }
    
    public List<WSDLEndpoint> getWSDLEndpoints() {
        return Collections.unmodifiableList(wsdlEndpoints);
    }
    
}

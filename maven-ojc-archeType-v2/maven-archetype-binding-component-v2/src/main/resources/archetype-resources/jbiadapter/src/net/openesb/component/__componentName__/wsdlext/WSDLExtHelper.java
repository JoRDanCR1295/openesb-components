#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * WSDLExtHelper.java
 */
package net.openesb.component.${componentName}.wsdlext;

import java.util.List;
import javax.wsdl.Binding;
import javax.wsdl.BindingFault;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * This class is a helper class for processing the wsdl extensions. It has
 * methods that help locate the extension elements and return the java models
 * corresponds to the extension elements.
 *
 * @author chikkala
 */
public final class WSDLExtHelper {

    /**
     * creates the java object for the port extension element.
     *
     * @param def wsdl definition
     * @param serviceName QName of the service
     * @param endpointName name of the port under which to lookup for the port
     * extension.
     * @return java object for the port extension element
     */
    public static PortExt getPortExt(Definition def, QName serviceName, String endpointName) {
        PortExt portExt = null;
        Service wsdlService = def.getService(serviceName);
        Port port = wsdlService.getPort(endpointName);
        @SuppressWarnings("unchecked")
        List<ExtensibilityElement> extElList = port.getExtensibilityElements();
        for (ExtensibilityElement extEl : extElList) {
            if (extEl instanceof PortExt) {
                portExt = (PortExt) extEl;
                break;
            }
        }
        return portExt;
    }

    /**
     * creates the java object for the binding extension element.
     *
     * @param def wsdl definition
     * @param binding binding under which to lookup for the binding extension.
     * @return java object for the port extension element
     */
    public static BindingExt getBindingExt(Definition def, Binding binding) {
        BindingExt bindingExt = null;
        @SuppressWarnings("unchecked")
        List<ExtensibilityElement> extElList = binding.getExtensibilityElements();
        for (ExtensibilityElement extEl : extElList) {
            if (extEl instanceof BindingExt) {
                bindingExt = (BindingExt) extEl;
                break;
            }
        }
        return bindingExt;
    }

    /**
     * creates the java object for the binding extension element.
     *
     * @param def wsdl definition
     * @param serviceName QName of the service
     * @param endpointName name of the port that references the binding.
     * @return java object for the port extension element
     */
    public static BindingExt getBindingExt(Definition def, QName serviceName, String endpointName) {
        Service wsdlService = def.getService(serviceName);
        Port port = wsdlService.getPort(endpointName);
        Binding binding = port.getBinding();
        return getBindingExt(def, binding);
    }

    /**
     * @return OperationExt java object corresponding to the operation extension
     * element under binding
     */
    public static OperationExt getOperationExt(Definition def, Binding binding, String operation) {
        OperationExt operationExt = null;
        BindingOperation bindingOp = binding.getBindingOperation(operation, null, null);
        if (bindingOp != null) {
            @SuppressWarnings("unchecked")
            List<ExtensibilityElement> extElList = bindingOp.getExtensibilityElements();
            for (ExtensibilityElement extEl : extElList) {
                if (extEl instanceof OperationExt) {
                    operationExt = (OperationExt) extEl;
                    break;
                }
            }
        }
        return operationExt;
    }

    /**
     * @return OperationExt java object corresponding to the operation extension
     * element under binding
     */
    public static OperationExt getOperationExt(Definition def, QName serviceName, String endpointName, String operation) {
        Service wsdlService = def.getService(serviceName);
        Port port = wsdlService.getPort(endpointName);
        Binding binding = port.getBinding();
        return getOperationExt(def, binding, operation);
    }

    /**
     * @return InputExt java object corresponding to input extension element of
     * the the binding operation under binding
     */
    public static InputExt getInputExt(Definition def, Binding binding,
            String operation, String input) {
        InputExt inputExt = null;
        BindingOperation bindingOp = binding.getBindingOperation(operation, input, null);
        if (bindingOp != null) {
            BindingInput bindingInput = bindingOp.getBindingInput();
            @SuppressWarnings("unchecked")
            List<ExtensibilityElement> extElList = bindingInput.getExtensibilityElements();
            for (ExtensibilityElement extEl : extElList) {
                if (extEl instanceof InputExt) {
                    inputExt = (InputExt) extEl;
                    break;
                }
            }
        }
        return inputExt;
    }

    /**
     * @return InputExt java object corresponding to input extension element of
     * the the binding operation under binding
     */
    public static InputExt getInputExt(Definition def,
            QName serviceName, String endpointName, String operation, String input) {
        Service wsdlService = def.getService(serviceName);
        Port port = wsdlService.getPort(endpointName);
        Binding binding = port.getBinding();
        return getInputExt(def, binding, operation, input);
    }

    /**
     * @return OutputExt java object corresponding to output extension element
     * of the the binding operation under binding
     */
    public static OutputExt getOutputExt(Definition def, Binding binding,
            String operation, String output) {
        OutputExt outputExt = null;
        BindingOperation bindingOp = binding.getBindingOperation(operation, null, output);
        if (bindingOp != null) {
            BindingOutput bindingOutput = bindingOp.getBindingOutput();
            @SuppressWarnings("unchecked")
            List<ExtensibilityElement> extElList = bindingOutput.getExtensibilityElements();
            for (ExtensibilityElement extEl : extElList) {
                if (extEl instanceof OutputExt) {
                    outputExt = (OutputExt) extEl;
                    break;
                }
            }
        }
        return outputExt;
    }

    /**
     * @return OutputExt java object corresponding to output extension element
     * of the the binding operation under binding
     */
    public static OutputExt getOutputExt(Definition def,
            QName serviceName, String endpointName, String operation, String output) {
        Service wsdlService = def.getService(serviceName);
        Port port = wsdlService.getPort(endpointName);
        Binding binding = port.getBinding();
        return getOutputExt(def, binding, operation, output);
    }

    /**
     * @return FaultExt java object corresponding to fault extension element of
     * the the binding operation under binding
     */
    public static FaultExt getFaultExt(Definition def, Binding binding,
            String operation, String faultName) {
        FaultExt faultExt = null;
        BindingOperation bindingOp = binding.getBindingOperation(operation, null, null);
        if (bindingOp != null) {
            BindingFault bindingFault = bindingOp.getBindingFault(faultName);
            if (bindingFault != null) {
                @SuppressWarnings("unchecked")
                List<ExtensibilityElement> extElList = bindingFault.getExtensibilityElements();
                for (ExtensibilityElement extEl : extElList) {
                    if (extEl instanceof FaultExt) {
                        faultExt = (FaultExt) extEl;
                        break;
                    }
                }
            }
        }
        return faultExt;
    }

    /**
     * @return FaultExt java object corresponding to fault extension element of
     * the the binding operation under binding
     */
    public static FaultExt getFaultExt(Definition def,
            QName serviceName, String endpointName, String operation, String faultName) {
        Service wsdlService = def.getService(serviceName);
        Port port = wsdlService.getPort(endpointName);
        Binding binding = port.getBinding();
        return getFaultExt(def, binding, operation, faultName);
    }
}

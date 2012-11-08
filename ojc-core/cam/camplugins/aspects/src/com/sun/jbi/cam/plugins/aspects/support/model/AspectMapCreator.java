/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)AspectMapCreator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model;

import java.io.Serializable;
import java.util.List;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Output;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;

import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.WSDLModel;

/**
 * @author graj
 *
 */
public class AspectMapCreator implements Serializable {
	private static final long serialVersionUID = 1L;

    /**
     * 
     */
    public AspectMapCreator() {
    }

    /**
     * 
     * @param wsdlModel
     * @param providerConfiguration
     * @param partnerConfiguration
     * @param adviceList
     * @return
     */
    public AspectMap generateAspectMap(WSDLModel wsdlModel,
    		ProviderConfiguration providerConfiguration,
    		PartnerConfiguration partnerConfiguration,
            List<AspectAdvice> adviceList) {
        boolean transformJBI = true;
        String inputTransformationFile = "input.xsl";
        String outputTransformationFile = "output.xsl";
        
        AspectMap aspectMap = new AspectMap();
        Aspect aspect = null;
        AspectInput aspectInput = null;
        AspectOutput aspectOutput = null;
        Definition definition = null;
        Service service = null;
        Port port = null;
        Binding binding = null;
        PortType portType = null;
        Input input = null;
        Output output = null;
        ExchangeType pattern = null;
        Message inputMessage = null;
        Message outputMessage = null;
        
        QName serviceQName = providerConfiguration.getServiceQName(); 
        String portName = providerConfiguration.getPortName();
        QName partnerLinkQName = partnerConfiguration.getPartnerLinkQName();
        String partnerRoleName = partnerConfiguration.getRoleName();        
        
        definition = wsdlModel.getDefinition();
        service = definition.getService(serviceQName);
        port = service.getPort(portName);
        binding = port.getBinding();
        portType = binding.getPortType();
        
        List<Operation> operations = portType.getOperations();
        for(Operation operation : operations) {
            input = operation.getInput();
            output = operation.getOutput();
            if((input != null) && (output != null)) {
                // apply filterRequestReply pattern
                pattern = ExchangeType.FilterRequestReply;
            }
            if((input != null) && (output == null)) {
                // apply filterOneWay pattern
                pattern = ExchangeType.FilterOneWay;
            }
            if(pattern != null) {
                aspect = new Aspect();
                aspect.setExchangeType(pattern);
                if(input != null) {
                    inputMessage = input.getMessage();
                    aspectInput = new AspectInput(partnerLinkQName, 
                    		partnerRoleName, 
                            portType.getQName(), 
                            operation.getName(), 
                            inputMessage.getQName(), 
                            inputTransformationFile, 
                            transformJBI);
                    aspect.setInput(aspectInput);
                }
                if(output != null) {
                    outputMessage = output.getMessage();
                    aspectOutput = new AspectOutput("1", 
                            serviceQName, 
                            portName, 
                            portType.getQName(), 
                            operation.getName(), 
                            outputMessage.getQName(), 
                            outputTransformationFile, 
                            transformJBI);
                    
                    aspect.addOutput(aspectOutput);
                }
                for(AspectAdvice aspectAdvice : adviceList) {
                    if(true == pattern.equals(ExchangeType.FilterOneWay)) {
                        // Cache & Auto-Reconnect do not support 1-way
                        if(false == aspectAdvice.getAspectType().equals(AspectType.Cache) 
                           && (false == aspectAdvice.getAspectType().equals(AspectType.AutoReconnect))) {
                            aspect.addAdvice(aspectAdvice);
                        }
                    } else {
                        // Queue only supports 1-way
                        if(false == aspectAdvice.getAspectType().equals(AspectType.Queueing)) {
                            aspect.addAdvice(aspectAdvice);
                        }
                    } 
                }
                if(aspect.getOrderToAdviceMap().entrySet().size() > 0) {
                    aspectMap.addAspect(aspect);
                }
            }
        }
        return aspectMap;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}

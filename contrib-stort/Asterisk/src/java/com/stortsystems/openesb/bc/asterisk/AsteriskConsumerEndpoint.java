/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AsteriskConsumerEndpoint.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk;

import com.stortsystems.openesb.bc.asterisk.wsdlext.PortExt;
import com.stortsystems.openesb.bc.asterisk.wsdlext.InputExt;
import com.stortsystems.openesb.bc.asterisk.wsdlext.WSDLExtHelper;
import com.sun.jbi.sample.component.common.RuntimeHelper;
import com.sun.jbi.sample.component.common.deployment.ConsumerEndpoint;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor;
import com.sun.jbi.sample.component.common.deployment.ServiceUnit;
import javax.jbi.JBIException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import java.util.regex.*;
import java.util.*;

public class AsteriskConsumerEndpoint extends ConsumerEndpoint {
    
    private AsteriskConsumerProxy consumerproxy;
    
    public AsteriskConsumerEndpoint(SUDescriptor.Consumes consumes, Definition wsdlDef, ServiceUnit su) {
        super(consumes, wsdlDef, su);
     
    }
    
    public String getInputNameStr() {
        
        Map map = getWSDL().getBindings();
        Iterator it = map.keySet().iterator();
        Object key = it.next();
        String value = map.get(key).toString();  
        String regexp = "BindingInput: name=(.+)\n";
        Pattern pattern = Pattern.compile(regexp);
        
        Matcher matcher = pattern.matcher(value);
        boolean matchf = matcher.find();
        String input = matcher.group(1);
        
        return input;
        
    }
    
    public String getOperationNameStr() {
    
        Map map = getWSDL().getBindings();
        Iterator it = map.keySet().iterator();
        Object key = it.next();
        String value = map.get(key).toString();        
        String regexp = "BindingOperation: name=(.+)\n";
        Pattern pattern = Pattern.compile(regexp);
        
        Matcher matcher = pattern.matcher(value);
        boolean matchf = matcher.find();
        String operation = matcher.group(1);
        
        return operation;
        
    }
    
    protected String[] getProps() {
        
        String[] ret = new String[5];
        
        try {
            QName serviceName = this.getService().getServiceName();
            String endpointName = this.getService().getEndpointName();
            Definition def = this.getWSDL();
            PortExt address = WSDLExtHelper.getPortExt(def, serviceName, endpointName);
            if ( address != null ) {
                ret[0] = address.getAddress();
                ret[1] = address.getPort();
                ret[2] = address.getUsername();
                ret[3] = address.getPassword();        
            }
            String opstr = getOperationNameStr();
            String inputstr = getInputNameStr();
            InputExt config = WSDLExtHelper.getInputExt(def, serviceName, endpointName, opstr, inputstr );
            
            if ( config != null ) {
                ret[4] = config.getEventTypes();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return ret;
    }
    
    @Override
    protected void doInit() throws JBIException {
        RuntimeHelper.getLogger().fine("JMXBindingEndpoint(Consumer): init called");
                
        try {
            String[] props = getProps();
            consumerproxy = new AsteriskConsumerProxy( this, props );
            consumerproxy.start();
        } catch ( Exception e ) {
            RuntimeHelper.getLogger().fine("JMXBindingEndpoint(Consumer): " + e.getMessage());
        }
        
    }
    
    @Override
    protected void doActivate() throws JBIException {
        RuntimeHelper.getLogger().fine("JMXBindingEndpoint(Consumer): activate called");
        

    }
    
    @Override
    protected void doDeactivate() throws JBIException {
        RuntimeHelper.getLogger().fine("JMXBindingEndpoint(Consumer): deactivate called");
        
        
        
    }
    
    @Override
    protected void doClean() throws JBIException {
        RuntimeHelper.getLogger().fine("JMXBindingEndpoint(Consumer): clean called");
        try {
            consumerproxy.disconnect();
            consumerproxy.interrupt();
            consumerproxy = null;    
        } catch ( Exception e ) {
            RuntimeHelper.getLogger().fine("JMXBindingEndpoint(Consumer): " + e.getMessage());
        }
        
    }
    
}

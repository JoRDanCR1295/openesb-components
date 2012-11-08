/*
 * JMXBindingConsumerEndpoint.java
 */

package com.sun.jbi.sample.binding;

import com.sun.jbi.sample.binding.wsdlext.PortExt;
import com.sun.jbi.sample.binding.wsdlext.WSDLExtHelper;
import com.sun.jbi.sample.component.common.RuntimeHelper;
import com.sun.jbi.sample.component.common.deployment.ConsumerEndpoint;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor;
import com.sun.jbi.sample.component.common.deployment.ServiceUnit;
import javax.jbi.JBIException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.ObjectName;
import javax.management.StandardMBean;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

/**
 * This class extends ConsumerEndpoint to implement the component specific consumer endpoint
 * configuration. It uses JMXBindingConsumerProxy to receive messages from external service
 * consumers and initiate the InOut message exchange. This implementation shows how a synchronous
 * send/receive from the delivery channel will be used in InOut message exchange in a service
 * invocation by the external consumers.
 *
 * @author chikkala
 */
public class JMXBindingConsumerEndpoint extends ConsumerEndpoint {
    
    private ObjectName mJMXEndpointMBeanName;
    private JMXEndpointMBean mJMXEndpointMBean;
    
    
    /** Creates a new instance of XSLTProviderEndpoint */
    public JMXBindingConsumerEndpoint(SUDescriptor.Consumes consumes, Definition wsdlDef, ServiceUnit su) {
        super(consumes, wsdlDef, su);
    }
    
    @Override
    protected void doInit() throws JBIException {
        RuntimeHelper.getLogger().fine("JMXBindingEndpoint(Consumer): init called");
        this.initJMXEndpoint();
    }
    
    @Override
    protected void doActivate() throws JBIException {
        RuntimeHelper.getLogger().fine("JMXBindingEndpoint(Consumer): activate called");
        this.activateJMXEndpoint();
    }
    
    @Override
    protected void doDeactivate() throws JBIException {
        RuntimeHelper.getLogger().fine("JMXBindingEndpoint(Consumer): deactivate called");
        this.deactivateJMXEndpoint();
    }
    
    @Override
    protected void doClean() throws JBIException {
        RuntimeHelper.getLogger().fine("JMXBindingEndpoint(Consumer): clean called");
        this.cleanJMXEndpoint();
    }
    
    /** creates the JMXEndpointMBean implementation.
     */
    protected JMXEndpointMBean createJMXEndpointMBean() {
        // JMX EndpointMBean implementation that initiates in-out message exchange.
        return new JMXBindingConsumerProxy(this);
    }
    /**
     * get the mbean object name from the endpoint address of the jmx wsdl extension
     * element
     */
    protected ObjectName createJMXEndpointMBeanName() throws JBIException {
        ObjectName mbeanObjectName = null;
        try {
            mbeanObjectName = new ObjectName(findMBeanNameFromWSDL());
        } catch (Exception ex) {
            throw new JBIException(ex);
        }
        return mbeanObjectName;
    }
    
    public final ObjectName getJMXEndpointMBeanName() {
        return this.mJMXEndpointMBeanName;
    }
    public final JMXEndpointMBean getJMXEndpointMBean() {
        return this.mJMXEndpointMBean;
    }
    protected final void initJMXEndpoint() throws JBIException {
        // create jmx mbean resources
        this.mJMXEndpointMBeanName = createJMXEndpointMBeanName();
        this.mJMXEndpointMBean = createJMXEndpointMBean();
    }
    
    protected final void activateJMXEndpoint() throws JBIException {
        // open jmx connectivity to external consumers by registering the
        // external endpoint mbean implementation.
        try {
            StandardMBean mbean = new StandardMBean(this.mJMXEndpointMBean, JMXEndpointMBean.class);
            this.getComponentContext().getMBeanServer().registerMBean(mbean, this.mJMXEndpointMBeanName);
        } catch (Exception ex) {
            throw new JBIException(ex);
        }
    }
    
    protected final void deactivateJMXEndpoint() throws JBIException {
        // close the jmx  connectivity to external consumers by unregistering
        // the external endpoint mbean.
        try {
            this.getComponentContext().getMBeanServer().unregisterMBean(this.mJMXEndpointMBeanName);
        } catch (InstanceNotFoundException ex) {
            ex.printStackTrace();
        } catch (MBeanRegistrationException ex) {
            ex.printStackTrace();
        }
    }
    
    protected final void cleanJMXEndpoint() throws JBIException {
        // release jmx mbean resources
        this.mJMXEndpointMBean = null;
        this.mJMXEndpointMBeanName = null;
    }
    
    /**
     * get the mbean object name from the endpoint address of the jmx wsdl extension
     * element
     */
    protected String findMBeanNameFromWSDL() {
        String mbeanName = null;
        try {
            QName serviceName = this.getService().getServiceName();
            String endpointName = this.getService().getEndpointName();
            Definition def = this.getWSDL();
            PortExt address = WSDLExtHelper.getPortExt(def, serviceName, endpointName);
            if ( address != null ) {
                mbeanName = address.getMBean();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return mbeanName;
    }
    
}

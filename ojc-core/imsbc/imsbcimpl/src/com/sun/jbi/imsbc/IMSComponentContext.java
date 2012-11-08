package com.sun.jbi.imsbc;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;

/**
 * singleton used to share the component context 
 */
 public class IMSComponentContext {
    
    private static IMSComponentContext instance = new IMSComponentContext();
    
    private ComponentContext context;
    private MessagingChannel channel;
    private ComponentLifeCycle lifeCycle;
    private IMSBindingDeployer mDeployer;

    /**
     * Properties used for Dynamic addressing, where the user specifies
     * different set of properties in BPEL editor to route messages differently
     * than is configured in the wsdl.
     */

    public static final String NM_PROP_IMSBC_ADDRESS_IMSSERVER_LOCATION = "org.glassfish.openesb.imsbc.address.imsserverlocation";

    /** Creates a new instance of HttpSoapComponentContext */
    private IMSComponentContext() {
    }
    
    public static IMSComponentContext getInstance() {
        return instance;
    }

    public ComponentContext getContext() {
        return context;
    }

    public void setContext(ComponentContext context) {
        this.context = context;
    }
    
    
    /**
     * @return the component lifecycle associated with this context
     * if it has been initialized
     */
    public ComponentLifeCycle getAssociatedLifeCycle() {
        return lifeCycle;
    }
    
    /**
     * Set the component lifecycle associated with this context
     */
    public void setAssociatedLifeCycle(ComponentLifeCycle aLifeCycle) {
        lifeCycle = aLifeCycle;
    }
    
    /**
     * @return Obtain the channel associated with this context
     * if it has been initialized
     */
    public MessagingChannel getBindingChannel() {
        return channel;
    }
    
    /**
     * Set the initizalied channel associated with this context
     */
    public void setBindingChannel(MessagingChannel aChannel) {
        channel = aChannel;
    }

    public IMSBindingDeployer getDeployer() {
        return mDeployer;
    }
    
    public void setDeployer(IMSBindingDeployer deployer) {
        mDeployer = deployer;
    }
 }
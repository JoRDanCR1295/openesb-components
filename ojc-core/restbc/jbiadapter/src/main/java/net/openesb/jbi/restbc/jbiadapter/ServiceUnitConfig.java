package net.openesb.jbi.restbc.jbiadapter;

import javax.xml.namespace.QName;


/**
 * ServiceUnitConfig.java
 *
 * @author Edward Chou
 */
public interface ServiceUnitConfig {

    public void start() throws Exception;
    public void stop() throws Exception;
    public void shutdown() throws Exception;
    
    public QName getServiceName();
    public String getEndpointName();
    
}

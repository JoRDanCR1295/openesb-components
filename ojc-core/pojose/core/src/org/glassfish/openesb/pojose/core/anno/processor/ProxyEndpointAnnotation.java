/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor;

/**
 *
 * @author gpatil
 */
public interface ProxyEndpointAnnotation {
    public String inMessageTypeNS();
    public String intMessageType();
    public String interfaceNS();
    public String interfaceName();
    public String name();
    public String serviceNS();
    public String serviceName();
}

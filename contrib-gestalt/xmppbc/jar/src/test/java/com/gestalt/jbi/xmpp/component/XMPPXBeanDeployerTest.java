package com.gestalt.jbi.xmpp.component;

import com.gestalt.jbi.component.manager.Endpoint;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.xmpp.extensions.XMPPAddress;
import com.gestalt.jbi.xmpp.extensions.XMPPBinding;
import com.gestalt.jbi.xmpp.extensions.XMPPOperation;

import junit.framework.TestCase;

import java.io.File;

import java.net.URL;

import javax.jbi.messaging.MessageExchange;

import javax.xml.namespace.QName;


public class XMPPXBeanDeployerTest extends TestCase {
    private String domain = "localhost";
    private String username = "xmpp";
    private String password = "xmpp";
    private String resource = "resource";
    private String endpoint = "endpoint";
    private Integer port = 5222;

    public void testAddEndpoint() throws Exception {
        XMPPXBeanDeployer dep = new XMPPXBeanDeployer(null);
        ServiceUnit su = new ServiceUnit("", "", new XMPPComponent());
        URL url = this.getClass().getResource("/xbean.xml");
        assertNotNull(url);

        File file = new File(url.getFile());

        dep.addEndpoint(su, file, new QName("http://com.gestalt"), endpoint,
            new QName(""), MessageExchange.Role.PROVIDER);
        dep.addEndpoint(su, file, new QName("http://com.gestalt"), endpoint,
            new QName(""), MessageExchange.Role.CONSUMER);
        assertEquals(su.getEndpoints().size(),2);

        for (Endpoint e : su.getEndpoints()) {
            assertEquals(e.getEndpointName(), endpoint);

            XMPPAddress add = ((XMPPEndpoint) e).getXMPPAddress();
            XMPPBinding bin = ((XMPPEndpoint) e).getXMPPBinding();

            if(roleToString(((XMPPEndpoint) e).getRole()).equals("consumer")){
            assertEquals(((XMPPEndpoint) e).getOperations()
                          .get(new QName(endpoint)).getOperationName(),
                    XMPPOperation.OperationName.sendMessage);
            }else if(roleToString(((XMPPEndpoint) e).getRole()).equals("provider")){
            assertEquals(((XMPPEndpoint) e).getOperations()
                          .get(new QName(endpoint)).getOperationName(),
                    XMPPOperation.OperationName.receiveMessage);
            }else{
                assertTrue("invalid or missing role",Boolean.FALSE);
            }

            assertEquals(add.getPort(), port);
            assertEquals(add.getDomain(), domain);
            assertEquals(add.getUsername(), username);
            assertEquals(add.getPassword(), password);
            assertEquals(add.getResource(), resource);
            assertEquals(bin.getSaslEnabled().booleanValue(), false);
            assertEquals(bin.getTlsEnabled().booleanValue(), false);
        }
    }

    private String roleToString(MessageExchange.Role role){
        return role.equals(MessageExchange.Role.CONSUMER)? "consumer": role.equals(MessageExchange.Role.PROVIDER)? "provider": null;
    }
}

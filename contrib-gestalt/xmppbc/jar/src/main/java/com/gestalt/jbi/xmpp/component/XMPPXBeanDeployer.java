package com.gestalt.jbi.xmpp.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.component.manager.deployment.AbstractXBeanDeployer;
import com.gestalt.jbi.xmpp.extensions.XMPPAddress;
import com.gestalt.jbi.xmpp.extensions.XMPPBinding;
import com.gestalt.jbi.xmpp.extensions.XMPPOperation;

import org.xmlpull.mxp1.MXParser;

import org.xmlpull.v1.XmlPullParser;

import java.io.File;
import java.io.FileReader;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;

import javax.wsdl.Definition;

import javax.xml.namespace.QName;


public class XMPPXBeanDeployer extends AbstractXBeanDeployer {
    private static final String ENDPOINT = "endpoint";
    private static final String ADDRESS = "address";
    private static final String ROLE = "role";
    private static final String CONSUMER = "consumer";
    private static final String PROVIER = "provider";
    Logger log = Logger.getLogger(XMPPXBeanDeployer.class.getName());

    public XMPPXBeanDeployer(AbstractComponent abstractComponent) {
        super(abstractComponent);
    }

    protected void addEndpoint(ServiceUnit serviceUnit, File file,
        QName serviceName, String endpointName, QName interfaceName,
        MessageExchange.Role role) throws Exception {
        log.fine("Attempting to add an endpoint " + role);
        if (file != null ) {
            log.fine("adding endpoiong form file " + file.getAbsolutePath());
        }

        XMPPBinding binding = new XMPPBinding();
        XMPPAddress address = new XMPPAddress();
        Map<QName, XMPPOperation> operations = null;
        String sRole = role.equals(MessageExchange.Role.CONSUMER) ? CONSUMER
                                                                  : (role.equals(MessageExchange.Role.PROVIDER)
            ? PROVIER : null);
        String xbeanRole = "";
        String xbeanEndpoint = "";

        XmlPullParser xpp = new MXParser();
        xpp.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, true);
        xpp.setInput(new FileReader(file));

        boolean done = false;

        while (!done) {
            int eventType = xpp.next();

            if (eventType == XmlPullParser.START_TAG) {
                if (ENDPOINT.equals(xpp.getName()) &&
                        (xbeanRole = xpp.getAttributeValue(null, ROLE)).equalsIgnoreCase(
                            sRole) &&
                        (xbeanEndpoint = xpp.getAttributeValue(null, ENDPOINT)).equalsIgnoreCase(
                            endpointName)) {
                    binding.setTlsEnabled(Boolean.valueOf(xpp.getAttributeValue(
                                null,
                                (XMPPBinding.Attributes.tlsEnabled.toString()))));
                    binding.setSaslEnabled(Boolean.valueOf(
                            xpp.getAttributeValue(null,
                                XMPPBinding.Attributes.saslEnabled.toString())));

                    String x = xpp.getAttributeValue(null,
                            XMPPAttributes.OPERATION_NAME);

                    if (null != x) {
                        operations = new HashMap<QName, XMPPOperation>();

                        XMPPOperation op = new XMPPOperation();
                        op.setOperationName(XMPPOperation.OperationName.valueOf(
                                x));
                        operations.put(new QName(endpointName), op);
                    }
                } else if (ADDRESS.equals(xpp.getName()) &&
                        xbeanRole.equalsIgnoreCase(sRole) &&
                        xbeanEndpoint.equalsIgnoreCase(endpointName)) {
                    address.setDomain(xpp.getAttributeValue(null,
                            XMPPAddress.Attributes.domain.toString()));
                    address.setUsername(xpp.getAttributeValue(null,
                            XMPPAddress.Attributes.username.toString()));
                    address.setPassword(xpp.getAttributeValue(null,
                            XMPPAddress.Attributes.password.toString()));
                    address.setResource(xpp.getAttributeValue(null,
                            XMPPAddress.Attributes.resource.toString()));
                    address.setPort(Integer.valueOf(xpp.getAttributeValue(
                                null, XMPPAddress.Attributes.port.toString())));
                    address.setGroup(xpp.getAttributeValue(null,
                            XMPPAddress.Attributes.group.toString()));
                }
            } else if (eventType == XmlPullParser.END_DOCUMENT) {
                done = true;
            }
        }

        XMPPEndpoint endpoint = createEndpoint(serviceName, interfaceName,
                endpointName, serviceUnit, null, null, role);
        endpoint.setXMPPBinding(binding);
        endpoint.setXMPPAddress(address);

        if ((operations != null) && (operations.size() == 1)) {
            endpoint.setXMPPOperations(operations);
        }

        serviceUnit.addEndpoint(endpoint);
        log.fine("Added an endpoint " + role);
    }

    protected XMPPEndpoint createEndpoint(QName serviceName,
        QName interfaceName, String endpointName, ServiceUnit serviceUnit,
        File wsdl, Definition def, MessageExchange.Role role)
        throws Exception {
        log.fine("Creating XMPPEndpoint svn: " + serviceName + " intr: " + interfaceName + " ep: " + endpointName);

        return new XMPPEndpoint(serviceName, interfaceName, endpointName, role,
            null, null, serviceUnit, true);
    }
}

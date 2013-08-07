/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
/*
 *
 * Copyright 2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.openesb.components.camelse.camel;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import javax.xml.namespace.QName;
import org.apache.camel.CamelContext;
import org.apache.camel.Endpoint;
import org.apache.camel.impl.DefaultComponent;
import org.apache.camel.util.CamelContextHelper;

/**
 *
 * @author chikkala
 */
public class JBIBridgeComponent extends DefaultComponent {

    public static final String URI_SCHEME = "jbi:";
    private static final Logger LOG = Logger.getLogger(JBIBridgeComponent.class.getName());

    public JBIBridgeComponent() {
    }

    public JBIBridgeComponent(CamelContext context) {
        super(context);
    }

    protected Endpoint createEndpoint(String uri, String remaining, Map parameters)
            throws Exception {
        LOG.fine("Creating JBI Camel Endpiont URI:" + uri + "\n remaining: " + remaining);
        JBIBridgeEndpoint result = new JBIBridgeEndpoint(uri, remaining, this);
        setProperties(result, parameters);
        return result;
    }

    private static String getEndpointURI(QName servQName, String epName) {
        return URI_SCHEME + servQName.getNamespaceURI() + "/" + servQName.getLocalPart() +
                "/" + epName;
    }
    
    /**
     *  finds the camel endpoints for the given jbi endpoint with service qname + endpoint name.
     * @param ctx
     * @param servQName
     * @param epName
     * @return
     */
    public static HashMap<String, JBIBridgeEndpoint> findEndpoints(CamelContext ctx, QName servQName, String epName) {
        
       HashMap<String, JBIBridgeEndpoint> epMap = new HashMap<String, JBIBridgeEndpoint>();
       
       String epURI = getEndpointURI(servQName, epName);
       
       JBIBridgeEndpoint currentEP = CamelContextHelper.getMandatoryEndpoint(ctx, epURI, JBIBridgeEndpoint.class);
       
       /*List<JBIBridgeEndpoint> epList = CamelContextHelper.getSingletonEndpoints(ctx, JBIBridgeEndpoint.class);
        
        for (JBIBridgeEndpoint currentEP : epList) {
            String currentEPUri = currentEP.getEndpointUri();
//            LOG.finer("Current EP URI " + currentEPUri);
            if (currentEPUri.startsWith(epURI)) { // check if the ep uri starts with the jbi:<s-ns>/<s-n>/<ep-n>
                epMap.put(currentEP.getOperation(), currentEP); // maps null operation key also.
            }
        }*/
       epMap.put(currentEP.getOperation(), currentEP);
        return epMap;
    }    
    
    public static JBIBridgeEndpoint findEndpoint(CamelContext ctx, QName servQName, String epName, String operation) {
        HashMap<String, JBIBridgeEndpoint> epMap = findEndpoints(ctx, servQName, epName);
        return epMap.get(operation);
    }    
    
}

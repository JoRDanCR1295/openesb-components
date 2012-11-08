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
 * @(#)JBIConnectionsInformation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;


import com.sun.jbi.cam.manager.framework.common.Util;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Logger;

import org.apache.xpath.XPathAPI;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Sun MicrosystemInc.
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class JBIConnectionsInformation {

    private static final String CONSUMER = "consumer";
    private static final String PROVIDER = "provider";
    private Logger logger = 
            Logger.getLogger(JBIConnectionsInformation.class.getName());
   
    // map to hold connection object representing one entry
    // in the connection fragment of the SA jbi descriptor
    private Map<String,Connection> connectionsMap;
    private Map nameSpaces;
    // map to hold the last provides index used on a given SU. it is used
    // to define the next provides position. it is shared by all instances
    // of the SVGServiceUnit.
    private Map<SVGServiceUnit,Integer>  serviceUnitProvidesIndex =
            new HashMap<SVGServiceUnit,Integer> ();
    // Map that hold the FNQ of provides end point. it used to prevent the 
    // drawing of the point if it was previously drawn.
    private Map <String,SVGPointType> alreadyAssignedprovidesPointList =
            new HashMap<String, SVGPointType> ();

    /**
     * 
     */
    public JBIConnectionsInformation() {
        super();
        connectionsMap = new TreeMap<String,Connection>();
    }

    
    public void processConnectionInformation(Node root) throws Exception{
        nameSpaces = Util.getNameSpaces(root);
        NodeList nodesList = XPathAPI.selectNodeList(root,"//connections/connection");
        for (int index = 0; index < nodesList.getLength(); index++) {
            Connection connection = new Connection();
            Node node = nodesList.item(index);
            connection.processConnectionElement(node);
            connectionsMap.put(connection.getConsumerEndpointName() + "_" +
                    connection.getConsumerFQServiceName(),connection);    
        }

        
    }

    public String getProviderEndPointForConsumerEndPoint(String endPoint) {
        for (Iterator iter = connectionsMap.keySet().iterator(); iter.hasNext();) {
            String key = (String) iter.next();
            Connection connection = (Connection) connectionsMap.get(key);
            String connectionEndPoint = connection.getConsumerEndpointName() + "_" +
                connection.getConsumerFQServiceName();
            if(connectionEndPoint.equals(endPoint)) {
                return connection.getProviderEndpointName() + "_" +
                connection.getProviderFQServiceName();
            }
        }
        return null;
    }
    
    public String getConsumerEndPointForProviderEndPoint(String endPoint) {
        for (Iterator iter = connectionsMap.keySet().iterator(); iter.hasNext();) {
            String key = (String) iter.next();
            Connection connection = (Connection) connectionsMap.get(key);
            String connectionEndPoint = connection.getProviderEndpointName() + "_" +
                    connection.getProviderFQServiceName();
            if(connectionEndPoint.equals(endPoint)) {
                return connection.getConsumerEndpointName()+ "_" +
                    connection.getConsumerFQServiceName();
            }
        }
        return null;
    }
   
    public String[] getConsumersEndPoint() {
        String[] consumersEndPoint = new String[connectionsMap.size()];
        int index = 0;
        for (Iterator iter = connectionsMap.keySet().iterator(); iter.hasNext();) {
            String key = (String) iter.next();
            Connection connection = (Connection) connectionsMap.get(key);
            consumersEndPoint[index++] = connection.getConsumerEndpointName()+ "_" +
                connection.getConsumerFQServiceName();
        }
        return consumersEndPoint;
    }
   
    
    class Connection {
        String consumerServiceName;
        String consumerEndpointName;
        String providerServiceName;
        String providerEndpointName;
        String consumerFQServiceName;
        String providerFQServiceName;
       
        
        /**
         * @return Returns the consumerServiceName.
         */
        public String getConsumerServiceName() {
            return consumerServiceName;
        }
        
        /**
         * @return Returns the consumerEndpointName.
         */
        public String getConsumerEndpointName() {
            return consumerEndpointName;
        }
        /**
         * @return Returns the providerEndpointName.
         */
        public String getProviderEndpointName() {
            return providerEndpointName;
        }
         /**
         * @return Returns the providerServiceName.
         */
        public String getProviderServiceName() {
            return providerServiceName;
        }
        

        public String getConsumerFQServiceName() {
            return consumerFQServiceName;
        }
 
        public String getProviderFQServiceName() {
            return providerFQServiceName;
        }

 
        private void processConnectionElement(Node element) throws Exception{
            processElement(element,CONSUMER);
            processElement(element,PROVIDER);
        }
        
        private void processElement(Node element,String connectionType) throws Exception{
            String xpath = "./" + connectionType + "/@endpoint-name";
            Node attribNode = XPathAPI.selectSingleNode(element,xpath);
            if(attribNode == null) {
                //@todo should throw exception
                return;
            }
            String endpointNameValue = attribNode.getNodeValue();

            xpath = "./" +connectionType + "/@service-name";
            attribNode = XPathAPI.selectSingleNode(element,xpath);
            if(attribNode == null) {
                //@todo should throw exception
                return;
            }
 
            String serviceNameValue = attribNode.getNodeValue();
            String fqServiceNameValue = Util.resolveNameSpace(nameSpaces,serviceNameValue);
            if(connectionType.equals(CONSUMER)) {
                consumerServiceName = serviceNameValue;
                consumerEndpointName = endpointNameValue;
                consumerFQServiceName = fqServiceNameValue;
            } else {
                providerServiceName = serviceNameValue;
                providerEndpointName = endpointNameValue;
                providerFQServiceName = fqServiceNameValue;
            }            
        }


    }
    
    public void addToAssignedprovidesPointMap(String providePoint,SVGPointType point) {
    	alreadyAssignedprovidesPointList.put(providePoint,point);
    }
  
    public boolean hasAssignedprovidesPoint(String providePoint) {
    	return alreadyAssignedprovidesPointList.containsKey(providePoint);
    }

    public SVGPointType getAssignedprovidesPoint(String providePoint) {
    	return alreadyAssignedprovidesPointList.get(providePoint);
    }

    
    public void updateServiceUnitProvidesIndex(SVGServiceUnit su, int index) {
    	serviceUnitProvidesIndex.put(su, new Integer(index));
    }
 
    public boolean hasServiceUnitProvidesIndex(SVGServiceUnit su) {
    	return serviceUnitProvidesIndex.containsKey(su);
    }
    
    public int getServiceUnitProvidesIndex(SVGServiceUnit su) {
    	return serviceUnitProvidesIndex.get(su).intValue();
    
    }
}

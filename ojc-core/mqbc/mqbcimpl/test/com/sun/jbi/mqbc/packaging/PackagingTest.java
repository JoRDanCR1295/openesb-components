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
 * @(#)PackagingTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.packaging;


import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.xml.namespace.QName;

import com.sun.jbi.mqbc.Endpoint;
import com.sun.jbi.mqbc.Endpoint.EndpointType;
import com.sun.jbi.mqbc.mbeans.ApplicationConfigurationField;
import junit.framework.TestCase;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;


/**
 * * *
 * @author rchen
 */
public class PackagingTest extends TestCase
{

    EndpointConfiguration endpointConfig = null;

    WSDLConfigurations wsdlConfig = null;

    Collection mEndpoints = null;

    public PackagingTest( String testName )
    {
        super( testName );
    }

    protected void setUp()
        throws Exception
    {
        String rootDir = new File("test/com/sun/jbi/mqbc/packaging").getAbsolutePath();
        endpointConfig = EndpointConfigurationFactory.getEndpointConfiguration( rootDir );
        wsdlConfig = new WSDLConfigurations( rootDir );
        Map<String, String[]> envVarsMap = new HashMap<String, String[]>();
        Map<String, Collection<ApplicationConfigurationField>> appConfMap =
                new HashMap<String, Collection<ApplicationConfigurationField>>();
        mEndpoints = wsdlConfig.createEndpoints( endpointConfig.endpoints(),envVarsMap,appConfMap );
    }

    protected void tearDown()
        throws Exception
    {}

    /**
     * Test of newConfiguration method, of class com.sun.jbi.smtpbc.packaging.EndpointConfiguration.
     */
    public void testNewConfiguration()
        throws Exception
    {
        System.out.println( "newConfiguration" );
        assertNotNull( endpointConfig );
        assertNotNull( wsdlConfig );
        assertNotNull( mEndpoints );
    }

    /**
     * Test of addPortMap method, of class com.sun.jbi.smtpbc.packaging.EndpointConfiguration.
     */
    public void testAddPortMap()
    {
        System.out.println( "addPortMap" );
        // EndpointConfiguration.PortMap p = null;
        // EndpointConfiguration instance = null;
        // instance.addPortMap(p);
        // TODO review the generated test code and remove the default call to fail.
        // fail("The test case is a prototype.");
    }

    /**
     * Test of portMaps method, of class com.sun.jbi.smtpbc.packaging.EndpointConfiguration.
     */
    public void testPortMaps()
    {
        System.out.println( "testPortMaps" );
        List result = endpointConfig.endpoints();
        assertEquals( 4, result.size() );
        Iterator it = result.iterator();
        while (it.hasNext()) {
            EndpointData portMap = (EndpointData) it.next();
            String service = portMap.getService();
            String endPoint = portMap.getEndpoint();
            int endPointType = portMap.getDirection();
            String endpttype = "";
            if (endPointType == EndpointType.INBOUND) {
                endpttype = "EndpointType.INBOUND";
            } else if (endPointType == EndpointType.OUTBOUND) {
                endpttype = "EndpointType.OUTBOUND";
            } else {
                fail( "Wrong end point type" );
            }
            System.out.println( "portmap service:" + service + "  :endpoint:" + endPoint + "  :endpointtype:" + endpttype );
        }
    }

    public void testEndPoints()
    {
        System.out.println( "testEndPoints" );
        assertEquals( 4, mEndpoints.size() );
        Iterator it = mEndpoints.iterator();
        while (it.hasNext()) {
            Endpoint endpoint = (Endpoint) it.next();
            // Store the status...
            QName serviceName = endpoint.getServiceName();
            String portName = endpoint.getEndpointName();
            System.out.println( "Endpoint serviceName:" + serviceName + "   :portName:" + portName );
        }
    }

}
